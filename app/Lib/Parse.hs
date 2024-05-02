module Lib.Parse where

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
import Lib.Write qualified
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Because `haskell-src-exts` does not understand `GHC2021`, collect language extensions enalbed
-- by `GHC2021` and give them to the the parser manually:
getGhc2021Extensions :: IO [H.Extension]
getGhc2021Extensions = do
  let ghc2021File = Lib.rootPath "/template/GHC2021.hs"
  (!ghc2021Extensions, !_) <- parseFile [] ghc2021File
  return ghc2021Extensions
parseFiles :: [([H.Extension], FilePath)] -> IO ([(FilePath, [H.Extension], (H.SrcLoc, String))], [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)])
parseFiles files = do
  parsedFiles <- forM files $ \(!exts, !path) -> do
    (path,) <$> Lib.Parse.parseFile exts path
  return $ partitionParseResults parsedFiles

-- | Collects declaratrions from a Haskell source file and minify them into one line.
parseFile :: [H.Extension] -> FilePath -> IO ([H.Extension], H.ParseResult (H.Module H.SrcSpanInfo))
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
