{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad
import Data.Graph.Sparse
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Language.Haskell.Exts qualified as H
import Lib qualified
import Lib.Parse qualified
import Lib.Write qualified
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  getArgs >>= \case
    [a] | a `elem` ["-h", "--help"] -> do
      putStrLn =<< Lib.readRootFile "/app/help"
    [] -> do
      -- Import style template
      mainGenTemplate
    ["--stand-alone"] -> do
      -- Embedded template
      mainGenTemplateStandAlone
    ["-m"] -> do
      putStrLn "Not given module names to minify."
    ("-m" : modules) -> do
      putStrLn =<< mainMinifyLibrary modules
    ["-e"] -> do
      putStrLn "Not given file name to embed toy-lib modules."
    ["-e", file] -> do
      putStrLn =<< mainEmbedLibrary file
    ["-u", file] -> do
      putStrLn =<< mainUpdateLibraryLine file
    args -> do
      putStrLn $ "Given unknown arguments: " ++ show args

-- | Sub command for generating a Haskell template with imports style: `-- {{{ toy-lib imports`.
mainGenTemplate :: IO ()
mainGenTemplate = do
  writeOutTemplate Nothing

-- | Sub command for generating a Haskell template with the whole @toy-lib@ embedded.
mainGenTemplateStandAlone :: IO ()
mainGenTemplateStandAlone = do
  (!parsedFiles, !gr) <- getSourceFileGraph
  let sortedParsedFiles = map (parsedFiles !!) $ topSortSG gr
  writeOutTemplate $ Just sortedParsedFiles

-- | Sub command for embedding toy-lib.
mainMinifyLibrary :: [String] -> IO String
mainMinifyLibrary moduleNames = do
  (!parsedFiles, !gr) <- getSourceFileGraph
  let moduleNameToVertex =
        M.fromList $
          zip
            ( map
                (\(!path, _, _) -> fromJust $ Lib.moduleName path)
                parsedFiles
            )
            [0 :: Int ..]

  let sourceVerts =
        map
          ( \name ->
              fromMaybe
                (error ("error, cannot resolve toy-lib module: `" ++ name ++ "`"))
                (moduleNameToVertex M.!? name)
          )
          moduleNames

  -- TODO: faster
  let targetSourceFiles =
        let reachables = componentsSG (revSG gr) $ U.fromList sourceVerts
            topSortVerts = topSortSG gr
            sortedReachables = filter (`U.elem` reachables) topSortVerts
         in map (parsedFiles !!) sortedReachables

  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions
  return $ Lib.Write.minifyLibrary ghc2021Extensions targetSourceFiles

getSourceFileGraph :: IO ([(FilePath, [H.Extension], H.Module H.SrcSpanInfo)], SparseGraph Int)
getSourceFileGraph = do
  parsedFiles <- getAllTheSourceFiles
  let gr = Lib.Parse.buildDepGraph parsedFiles
  return (parsedFiles, gr)
  where
    -- Gets all the source files of @toy-lib@.
    getAllTheSourceFiles :: IO [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)]
    getAllTheSourceFiles = do
      ghc2021Extensions <- Lib.Parse.getGhc2021Extensions
      files <- collectSourceFiles $ Lib.rootPath "/src"

      -- [(path, fileContent)]
      (!failures, !successes) <- Lib.Parse.parseFiles $ map (ghc2021Extensions,) files

      -- TODO: use ExceptT or something
      unless (null failures) $ do
        putStrLn "Failed to parse source files:"
        forM_ failures print
        exitFailure

      return successes

    -- Recursively collects @.hs@ files.
    collectSourceFiles :: FilePath -> IO [FilePath]
    collectSourceFiles dir = do
      contents <- map ((dir ++ "/") ++) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
      let sourceFiles = filter filterSourceFiles contents
      subDirs <- filterM doesDirectoryExist contents
      L.foldl' (++) sourceFiles <$> mapM collectSourceFiles subDirs
      where
        -- Filter Haskell source files, ignoring `Macro.hs`
        filterSourceFiles :: String -> Bool
        filterSourceFiles s = (".hs" `L.isSuffixOf` s) && not ("Macro.hs" `L.isSuffixOf` s)

-- | Replaces imports
--
-- = Example file
-- @
-- -- {{{ toy-lib imports
-- import Data.Graph.UnionFind
-- import Data.Graph.Sparse;import Data.Graph.Tree
-- -- }}} toy-lib imports
-- @
mainEmbedLibrary :: FilePath -> IO String
mainEmbedLibrary file = do
  -- TODO: handle failures
  s <- readFile file

  -- For simplicity, resolve `#include`s manualy:
  lns <- forM (lines s) $ \line -> do
    if "#include" `L.isPrefixOf` line
      then do
        let importPath = takeWhile (/= '"') $ drop 1 $ dropWhile (/= '"') line
        let path = reverse (dropWhile (/= '/') (reverse file)) ++ importPath
        readFile path
      else return line

  let front = fromMaybe (error "cannot find front header") $ L.findIndex ("-- {{{ toy-lib import" `L.isPrefixOf`) lns
  let back = fromMaybe (error "cannot find back header") $ L.findIndex ("-- }}} toy-lib import" `L.isPrefixOf`) lns

  let importLines = unlines . filter (not . ("-" `L.isPrefixOf`)) . take (back - front - 1) $ drop (front + 1) lns
  -- FIXME: ToyLib.Debug resolution
  let moduleNames = filter (/= "import") $ words $ map (\case ';' -> ' '; c -> c) importLines
  toylib <- mainMinifyLibrary moduleNames

  let lns' = take front lns ++ [toylib] ++ drop (back + 1) lns
  return $ L.intercalate "\n" lns'

-- | Replace line 15 with new library. That should be the line of minified library.
mainUpdateLibraryLine :: FilePath -> IO String
mainUpdateLibraryLine file = do
  s <- readFile file

  (!parsedFiles, !_gr) <- getSourceFileGraph
  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions
  let !toylib = Lib.Write.minifyLibrary ghc2021Extensions parsedFiles

  let i = 15
  let lns = lines s

  let s' = L.intercalate "\n" $ take (i - 1) lns ++ [toylib] ++ drop i lns
  return s'

-- | Geneartes toy-lib template and writes it out to the stdout.
writeOutTemplate :: Maybe [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> IO ()
writeOutTemplate embeddedModules = do
  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions

  -- parse template
  let template = Lib.rootPath "/template/Main.hs"
  (!templateExtensions, !parsedTemplate) <- Lib.Parse.parseFile ghc2021Extensions template

  case parsedTemplate of
    H.ParseOk templateAst -> do
      toylib <- case embeddedModules of
        Nothing -> do
          -- Import style ({{{ toy-lib imports):
          Lib.readRootFile "/template/Imports.hs"
        Just sortedParsedFiles -> do
          -- Embedded style:
          return $ Lib.Write.minifyLibrary ghc2021Extensions sortedParsedFiles

      macros <- Lib.readRootFile "/template/Macros.hs"
      body <- Lib.readRootFile "/template/Body.hs"
      putStr $
        Lib.Write.generateTemplate
          templateExtensions
          templateAst
          toylib
          macros
          body
    failure -> do
      putStrLn "Failed to parse template:"
      print failure
