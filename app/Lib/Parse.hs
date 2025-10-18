module Lib.Parse where

import AtCoder.Extra.Graph qualified as Gr
import Control.Monad
import Data.List.Extra (nubSort)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import GHC.Stack
import Language.Haskell.Exts qualified as H
import Lib qualified

-- | Language extensions enabled by @GHC2021@ collected mannualy for @haskell-src-exts@.
getGhc2021Extensions :: IO [H.Extension]
getGhc2021Extensions = do
  let ghc2021File = Lib.rootPath "/template/GHC2021.hs"
  (!ghc2021Extensions, !_) <- parseFile [] ghc2021File
  return ghc2021Extensions

parseFiles :: [([H.Extension], FilePath)] -> IO ([(FilePath, [H.Extension], (H.SrcLoc, String))], [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)])
parseFiles files = do
  parsedFiles <- forM files $ \(!exts, !path) -> do
    (path,) <$> parseFile exts path
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
    step (!f, (!exts, H.ParseFailed loc s)) (!accL, !accR) = ((f, exts, (loc, s)) : accL, accR)
    step (!f, (!exts, H.ParseOk l)) (!accL, !accR) = (accL, (f, exts, l) : accR)

buildDepGraph :: (HasCallStack) => [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> Gr.Csr ()
buildDepGraph input = Gr.build' (length input) edges
  where
    edges :: U.Vector (Int, Int)
    edges = U.fromList $ concatMap (\(!path, _, module_) -> edgesOf path module_) input

    -- edge from depended vertex to dependent vertex
    edgesOf :: FilePath -> H.Module a -> [(Int, Int)]
    edgesOf path (H.Module _ _ _ !imports _) =
      let !v1 = moduleNameToVertex M.! fromJust (Lib.moduleName path)
          !v2s = mapMaybe ((moduleNameToVertex M.!?) . (\(H.ModuleName _ s) -> s) . H.importModule) imports
       in map (,v1) v2s
    edgesOf _ _ = error "unexpected module data"

    moduleNameToVertex :: M.Map String Int
    !moduleNameToVertex = M.fromList $ zip (map (\(!path, _, _) -> fromJust $ Lib.moduleName path) input) [0 :: Int ..]
