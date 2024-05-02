-- | Library for the command line app.
module Lib.Write where

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
import System.Exit (exitFailure)
import Lib qualified

-- | Geneartes `toy-lib` in one line.
generateLibrary :: [H.Extension] -> [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> [(FilePath, String)]
generateLibrary ghc2021Extensions = map
        ( \(path, exts, module_) ->
            (path, minifyDeclarations (exts ++ ghc2021Extensions) module_)
        )
  where
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
