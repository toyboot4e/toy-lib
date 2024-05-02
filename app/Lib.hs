{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Library for the command line app.
module Lib where

import Control.Monad
import Data.Graph.Sparse
import Data.List qualified as L
import Data.List.Extra (nubSort, stripSuffix)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Vector.Unboxed qualified as U
import Language.Haskell.Exts qualified as H
import Language.Haskell.TH (runIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | Returns the root directory of the project.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

rootPath :: FilePath -> FilePath
rootPath = (installPath ++)

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
            -- `deriving newtype` is not handled correctly by `haskell-src-exts`.
            -- Here we remove newline characters, but there's some needless many spaces before `deriving newtype`:
            hack :: String -> String
            hack = filter (/= '\n')
        minify _ = ""

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode {H.layout = H.PPNoLayout}

generateTemplate :: [H.Extension] -> H.Module H.SrcSpanInfo -> [(FilePath, String)] -> String -> String -> String -> String
generateTemplate extensions (H.Module _ _ _ imports _) toylib header macros body =
  unlines [header, pre1, pre2, disableFormat, exts, imports', rules, macros', toylib', enableFormat, post, "", body]
  where
    exts :: String
    exts = "{-# LANGUAGE " ++ es ++ " #-}"
      where
        es = L.intercalate ", " . L.sort $ map H.prettyExtension extensions

    imports' :: String
    imports' = L.intercalate ";" [L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) imports)]

    pre1 = "-- {{{ toy-lib: https://github.com/toyboot4e/toy-lib"
    pre2 = "{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds -Wno-orphans #-}"
    post = "-- }}}"
    rules = "{-# RULES \"Force inline VAI.sort\" VAI.sort = VAI.sortBy compare #-}"

    -- remove newline character
    macros' = init macros

    disableFormat = "{- ORMOLU_DISABLE -}"
    enableFormat = "{- ORMOLU_ENABLE -}"

    toylib' = L.intercalate ";" (map snd toylib)
