module Main (main) where

import Control.Monad
import Data.Graph.Sparse
import Data.List qualified as L
import Data.List.Extra (nubSort, stripSuffix)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Language.Haskell.Exts qualified as H
import Language.Haskell.TH (runIO)
import Lib qualified
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- TODO: Refactor

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mainGenTemplate
    else mainEmbedLibrary

-- | Sub command for generating a Haskell template.
mainGenTemplate :: IO ()
mainGenTemplate = do
  ghc2021Extensions <- getGhc2021Extensions
  files <- collectSourceFiles $ Lib.rootPath "/src"

  -- [(path, fileContent)]
  parsedFiles <- forM files $ \path -> do
    (path,) <$> parseFile ghc2021Extensions path

  -- TODO: throw instead
  let (!failures, !successes) = partitionParseResults parsedFiles
  if null failures
    then do
      generateTemplateFromInput successes
    else do
      putStrLn "Failed to parse source files:"
      forM_ failures print
      exitFailure

generateTemplateFromInput :: [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> IO ()
generateTemplateFromInput parsed = do
  ghc2021Extensions <- getGhc2021Extensions

  -- parse template
  let template = Lib.rootPath "/template/Main.hs"
  (!templateExtensions, !parsedTemplate) <- parseFile ghc2021Extensions template

  case parsedTemplate of
    H.ParseOk templateAst -> do
      header <- readFile $ Lib.rootPath "/template/Header.hs"
      macros <- readFile $ Lib.rootPath "/template/Macros.hs"
      body <- readFile $ Lib.rootPath "/template/Body.hs"
      let toylib = Lib.generateLibrary ghc2021Extensions parsed
      putStr $ Lib.generateTemplate templateExtensions templateAst toylib header macros body
    failure -> do
      putStrLn "Failed to parse template:"
      print failure

-- | Sub command for embedding toy-lib.
mainEmbedLibrary :: IO ()
mainEmbedLibrary = do
  putStrLn "TODO"
  return ()

-- | Because `haskell-src-exts` does not understand `GHC2021`, collect language extensions enalbed
-- by `GHC2021` and give them to the the parser manually:
getGhc2021Extensions :: IO [H.Extension]
getGhc2021Extensions = do
  let ghc2021File = Lib.rootPath "/template/GHC2021.hs"
  (!ghc2021Extensions, !_) <- parseFile [] ghc2021File
  return ghc2021Extensions

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

-- | Collects declaratrions from a Haskell source file and minify them into one line.
parseFile :: [H.Extension] -> String -> IO ([H.Extension], H.ParseResult (H.Module H.SrcSpanInfo))
parseFile ghc2021Extensions absPath = do
  code <- readFile absPath

  -- Collect language extensions:
  let extensions = case H.readExtensions code of
        Just (_, exts) -> exts
        Nothing -> []

  let parseOption =
        H.defaultParseMode
          { H.parseFilename = absPath,
            H.extensions = nubSort $ extensions ++ ghc2021Extensions
          }

  return (extensions, H.parseModuleWithMode parseOption code)

partitionParseResults :: [(a, ([H.Extension], H.ParseResult b))] -> ([(a, [H.Extension], (H.SrcLoc, String))], [(a, [H.Extension], b)])
partitionParseResults = foldr step ([], [])
  where
    step (f, (exts, H.ParseFailed loc s)) (accL, accR) = ((f, exts, (loc, s)) : accL, accR)
    step (f, (exts, H.ParseOk l)) (accL, accR) = (accL, (f, exts, l) : accR)

