{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import qualified Data.List as L
import Data.Maybe
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.TH (runIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)

-- TODO: Refactor

main :: IO ()
main = do
  -- (name : opts) <- getArgs
  let srcDir = installPath ++ "/src"
  files <- collectSourceFiles srcDir
  (failures, successes) <- partitionResults <$> mapM (\f -> (f,) <$> minifyAsOneline f) files

  when ((not . null) failures) $ do
    forM_ failures print
    exitFailure

  let template = installPath ++ "/template/Main.hs"
  (extensions, parsed) <- parseFile template

  header <- readFile $ installPath ++ "/template/Header.hs"
  macros <- readFile $ installPath ++ "/template/Macros.hs"
  body <- readFile $ installPath ++ "/template/Body.hs"

  case parsed of
    H.ParseOk templateAst -> do
      putStr $ generate extensions templateAst (map snd successes) header macros body
    failure -> do
      putStrLn $ "Failed to parse template:"
      print failure
  where
    partitionResults :: [(a, Either l r)] -> ([(a, l)], [(a, r)])
    partitionResults = foldr step ([], [])
      where
        step :: (a, Either l r) -> ([(a, l)], [(a, r)]) -> ([(a, l)], [(a, r)])
        step (f, Left l) (accL, accR) = ((f, l) : accL, accR)
        step (f, Right r) (accL, accR) = (accL, (f, r) : accR)

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
parseFile :: String -> IO ([H.Extension], H.ParseResult (H.Module H.SrcSpanInfo))
parseFile absPath = do
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
            H.extensions = extensions
          }

  return (extensions, H.parseModuleWithMode parseOption processed)

-- | Collects declaratrions from a Haskell source file and minify them into one line.
minifyAsOneline :: String -> IO (Either String String)
minifyAsOneline absPath = do
  (extensions, result) <- parseFile absPath
  return $ case result of
    H.ParseOk ast -> Right $ minifyDeclarations extensions ast
    failed -> Left $ show failed

-- | Returns the root directory.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

-- | Converts module name to source file path.
modulePath :: String -> FilePath
modulePath name = concat [installPath, "/src/", map tr name, ".hs"]
  where
    tr '.' = '/'
    tr c = c

removeDefineMacros :: String -> String
removeDefineMacros = unlines . filter (not . L.isPrefixOf "#define") . lines

removeMacros :: String -> String
removeMacros = unlines . filter (not . L.isPrefixOf "#") . lines

minifyDeclarations :: [H.Extension] -> H.Module l -> String
minifyDeclarations extensions ast = minify ast
  where
    pretty :: H.Module l -> String
    pretty (H.Module _ _ _ _ decls) = unlines $ map (H.prettyPrintWithMode pphsMode) decls
    pretty _ = ""

    minify :: H.Module l -> String
    minify (H.Module _ _ _ _ decls) = L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) decls)
    minify _ = ""

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode {H.layout = H.PPNoLayout}

generate :: [H.Extension] -> H.Module H.SrcSpanInfo -> [String] -> String -> String -> String -> String
generate extensions (H.Module _ _ _ imports _) toylib header macros body =
  unlines [header, exts, "", pre, disableFormat, imports', "",macros, toylib', enableFormat, post, "", body]
  where
    exts :: String
    exts = "{-# LANGUAGE " ++ es ++ " #-}"
      where
        es = L.intercalate ", " $ map H.prettyExtension extensions

    imports' :: String
    imports' = L.intercalate ";" [L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) imports)]

    pre = "-- {{{ toy-lib: <https://github.com/toyboot4e/toy-lib>"
    post = "-- }}}"

    disableFormat = "{- ORMOLU_DISABLE -}"
    enableFormat = "{- ORMOLU_ENABLE -}"

    toylib' = L.intercalate ";" toylib
