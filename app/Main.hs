{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.List qualified as L
import Data.List.Extra (stripSuffix)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.SparseGraph
import Data.Vector.Unboxed qualified as U
import Language.Haskell.Exts qualified as H
import Language.Haskell.TH (runIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Exit (exitFailure)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)

-- TODO: Refactor
-- TODO: Filter dependencies and do topological sort

main :: IO ()
main = do
  let srcDir = installPath ++ "/src"
  files <- collectSourceFiles srcDir

  -- Because `haskell-src-exts` does not understand `GHC2021`, collect language extensions enalbed
  -- by `GHC2021` and give them manually to the the parser:
  let ghc2021File = installPath ++ "/template/GHC2021.hs"
  (!ghc2021Extensions, !_) <- parseFile [] ghc2021File

  -- parse source files
  parsedFiles <- forM files $ \path -> do
    (path,) <$> parseFile ghc2021Extensions path
  let (!failures, !successes) = partitionParseResults parsedFiles

  unless (null failures) $ do
    forM_ failures print
    exitFailure

  -- parse template
  let template = installPath ++ "/template/Main.hs"
  (!templateExtensions, !parsed) <- parseFile ghc2021Extensions template

  header <- readFile $ installPath ++ "/template/Header.hs"
  macros <- readFile $ installPath ++ "/template/Macros.hs"
  body <- readFile $ installPath ++ "/template/Body.hs"

  case parsed of
    H.ParseOk templateAst -> do
      let toylib = generateLibrary ghc2021Extensions successes
      putStr $ generateTemplate templateExtensions templateAst toylib header macros body
    failure -> do
      putStrLn "Failed to parse template:"
      print failure
  where
    partitionParseResults :: [(a, ([H.Extension], H.ParseResult b))] -> ([(a, [H.Extension], (H.SrcLoc, String))], [(a, [H.Extension], b)])
    partitionParseResults = foldr step ([], [])
      where
        step (f, (exts, H.ParseFailed loc s)) (accL, accR) = ((f, exts, (loc, s)) : accL, accR)
        step (f, (exts, H.ParseOk l)) (accL, accR) = (accL, (f, exts, l) : accR)

-- | Recursively collects @.hs@ files.
collectSourceFiles :: FilePath -> IO [FilePath]
collectSourceFiles dir = do
  contents <- map ((dir ++ "/") ++) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
  let sourceFiles = filter filterSourceFiles contents
  subDirs <- filterM doesDirectoryExist contents
  L.foldl' (++) sourceFiles <$> mapM collectSourceFiles subDirs

filterSourceFiles :: String -> Bool
filterSourceFiles s = (".hs" `L.isSuffixOf` s) && (not $ ("Macro.hs" `L.isSuffixOf` s))

-- | Collects declaratrions from a Haskell source file and minify them into one line.
parseFile :: [H.Extension] -> String -> IO ([H.Extension], H.ParseResult (H.Module H.SrcSpanInfo))
parseFile ghc2021Extensions absPath = do
  code <- readFile absPath

  -- Pre-processing CPP:
  -- TODO: why removing macros?
  processed <- withTempDirectory "." "toy-lib-cpp" $ \tmpDir -> do
    let originalPath = tmpDir ++ "/define-removed.hs"
    let processedPath = tmpDir ++ "/cpp-processed.hs"
    writeFile originalPath $ removeDefineMacros code

    _exitCode <- rawSystem "stack" ["ghc", "--", "-E", originalPath, "-o", processedPath]

    removeMacros <$> readFile processedPath

  -- Collect language extensions:
  let extensions = case H.readExtensions processed of
        Just (_, exts) -> exts
        Nothing -> []

  let parseOption =
        H.defaultParseMode
          { H.parseFilename = absPath,
            H.extensions = extensions ++ ghc2021Extensions
          }

  return (extensions, H.parseModuleWithMode parseOption processed)

-- | Returns the root directory.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

-- | Converts module name to source file path.
modulePath :: String -> FilePath
modulePath name = concat [installPath, "/src/", map tr name, ".hs"]
  where
    tr '.' = '/'
    tr c = c

-- | Converts source file path to module name
moduleName :: FilePath -> Maybe String
moduleName name = do
  name1 <- L.stripPrefix (installPath ++ "/src/") name
  name2 <- stripSuffix ".hs" name1
  return $ map (\c -> if c == '/' then '.' else c) name2

removeDefineMacros :: String -> String
removeDefineMacros = unlines . filter (not . L.isPrefixOf "#define") . lines

removeMacros :: String -> String
removeMacros = unlines . filter (not . L.isPrefixOf "#") . lines

-- | Geneartes `toy-lib` in one line.
generateLibrary :: [H.Extension] -> [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> [(FilePath, String)]
generateLibrary ghc2021Extensions parsedFiles =
  let files = topSortSourceFiles parsedFiles
   in map
        ( \(path, exts, module_) ->
            (path, minifyDeclarations (exts ++ ghc2021Extensions) module_)
        )
        files
  where
    topSortSourceFiles :: [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)]
    topSortSourceFiles input =
      let edges = U.fromList $ concatMap (\(!path, _, module_) -> edgesOf path module_) input
          gr = buildSG (0, pred (length input)) edges
          vs = topSortSG gr
       in map (input !!) vs
      where
        moduleNameToVert :: M.Map String Int
        !moduleNameToVert = M.fromList $ zip (map (\(!path, _, _) -> fromJust $ moduleName path) input) [0 :: Int ..]

        -- edge from depended vertex to dependent vertex
        edgesOf :: FilePath -> H.Module a -> [(Int, Int)]
        edgesOf path (H.Module _ _ _ !imports _) =
          let !v1 = moduleNameToVert M.! fromJust (moduleName path)
              !v2s = mapMaybe ((moduleNameToVert M.!?) . (\(H.ModuleName _ s) -> s) . H.importModule) imports
           in map (,v1) v2s
        edgesOf _ _ = error "unexpected module data"

    minifyDeclarations :: [H.Extension] -> H.Module l -> String
    minifyDeclarations _ ast = minify ast
      where
        pretty :: H.Module l -> String
        pretty (H.Module _ _ _ _ !decls) = unlines $ map (H.prettyPrintWithMode pphsMode) decls
        pretty _ = ""

        minify :: H.Module l -> String
        minify (H.Module _ _ _ _ !decls) = L.intercalate ";" (map (hack . H.prettyPrintWithMode pphsMode) decls)
          where
            -- \| `deriving newtype` is not handled correctly by `haskell-src-exts`.
            -- Here we remove newline characters, but there's some needless many spaces before `deriving newtype`:
            hack :: String -> String
            hack = filter (/= '\n')
        minify _ = ""

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode {H.layout = H.PPNoLayout}

generateTemplate :: [H.Extension] -> H.Module H.SrcSpanInfo -> [(FilePath, String)] -> String -> String -> String -> String
generateTemplate extensions (H.Module _ _ _ imports _) toylib header macros body =
  unlines [header, pre, disableFormat, exts, imports', rules, macros', toylib', enableFormat, post, "", body]
  where
    exts :: String
    exts = "{-# LANGUAGE " ++ es ++ " #-}"
      where
        es = L.intercalate ", " . L.sort $ map H.prettyExtension extensions

    imports' :: String
    imports' = L.intercalate ";" [L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) imports)]

    pre = "-- {{{ toy-lib: https://github.com/toyboot4e/toy-lib"
    post = "-- }}}"
    rules = "{-# RULES \"Force inline VAI.sort\" VAI.sort = VAI.sortBy compare #-}"

    -- remove newline character
    macros' = init macros

    disableFormat = "{- ORMOLU_DISABLE -}"
    enableFormat = "{- ORMOLU_ENABLE -}"

    toylib' = L.intercalate ";" (map snd toylib)
