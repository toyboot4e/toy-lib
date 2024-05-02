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
main =
  getArgs >>= \case
    [] -> mainGenTemplate
    ["-e"] -> putStrLn "Not given module names to embed."
    ("-e" : rest) -> mainEmbedLibrary rest
    args -> putStrLn $ "Given unknown arguments: " ++ show args

-- | Sub command for generating a Haskell template.
mainGenTemplate :: IO ()
mainGenTemplate = do
  (!parsedFiles, !gr) <- getSourceFileGraph
  let sortedParsedFiles = map (parsedFiles !!) $ topSortSG gr
  generateTemplateFromInput sortedParsedFiles

-- | Sub command for embedding toy-lib.
mainEmbedLibrary :: [String] -> IO ()
mainEmbedLibrary moduleNames = do
  (!parsedFiles, !gr) <- getSourceFileGraph
  let moduleNameToVertex =
        M.fromList $
          zip
            ( map
                (\(!path, _, _) -> fromJust $ Lib.moduleName path)
                parsedFiles
            )
            [0 :: Int ..]

  -- TODO: warn instead of die on mismatch
  let sourceVerts = map (moduleNameToVertex M.!) moduleNames

  -- TODO: faster
  let targetSourceFiles =
        let reachables = componentsSG (revSG gr) $ U.fromList sourceVerts
            topSortVerts = topSortSG gr
            sortedReachables = filter (`U.elem` reachables) topSortVerts
         in map (parsedFiles !!) sortedReachables

  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions
  let toylib = Lib.Write.minifyLibrary ghc2021Extensions targetSourceFiles
  putStrLn toylib

getSourceFileGraph :: IO ([(FilePath, [H.Extension], H.Module H.SrcSpanInfo)], SparseGraph Int ())
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

-- | Geneartes toy-lib template and Writes it out to the stdout.
generateTemplateFromInput :: [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> IO ()
generateTemplateFromInput sortedParsedFiles = do
  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions

  -- parse template
  let template = Lib.rootPath "/template/Main.hs"
  (!templateExtensions, !parsedTemplate) <- Lib.Parse.parseFile ghc2021Extensions template

  case parsedTemplate of
    H.ParseOk templateAst -> do
      header <- readFile $ Lib.rootPath "/template/Header.hs"
      macros <- readFile $ Lib.rootPath "/template/Macros.hs"
      body <- readFile $ Lib.rootPath "/template/Body.hs"
      let toylib = Lib.Write.minifyLibrary ghc2021Extensions sortedParsedFiles
      putStr $ Lib.Write.generateTemplate templateExtensions templateAst toylib header macros body
    failure -> do
      putStrLn "Failed to parse template:"
      print failure
