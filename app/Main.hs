module Main (main) where

import Control.Monad
import Data.List qualified as L
import Language.Haskell.Exts qualified as H
import Lib qualified
import Lib.Parse qualified
import Lib.Write qualified
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mainGenTemplate
    else mainEmbedLibrary

-- | Sub command for generating a Haskell template.
mainGenTemplate :: IO ()
mainGenTemplate = do
  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions
  files <- collectSourceFiles $ Lib.rootPath "/src"

  -- [(path, fileContent)]
  (!failures, !successes) <- Lib.Parse.parseFiles $ map (ghc2021Extensions,) files
  if null failures
    then do
      generateTemplateFromInput successes
    else do
      putStrLn "Failed to parse source files:"
      forM_ failures print
      exitFailure

-- | Recursively collects @.hs@ files.
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
generateTemplateFromInput parsedFiles = do
  ghc2021Extensions <- Lib.Parse.getGhc2021Extensions

  -- parse template
  let template = Lib.rootPath "/template/Main.hs"
  (!templateExtensions, !parsedTemplate) <- Lib.Parse.parseFile ghc2021Extensions template

  case parsedTemplate of
    H.ParseOk templateAst -> do
      header <- readFile $ Lib.rootPath "/template/Header.hs"
      macros <- readFile $ Lib.rootPath "/template/Macros.hs"
      body <- readFile $ Lib.rootPath "/template/Body.hs"
      let sourceFiles = Lib.Parse.topSortSourceFiles parsedFiles
      let toylib = Lib.Write.generateLibrary ghc2021Extensions sourceFiles
      putStr $ Lib.Write.generateTemplate templateExtensions templateAst toylib header macros body
    failure -> do
      putStrLn "Failed to parse template:"
      print failure

-- | Sub command for embedding toy-lib.
mainEmbedLibrary :: IO ()
mainEmbedLibrary = do
  putStrLn "TODO"
  return ()
