-- | Library for the command line app.
module Lib.Write where

import Data.List qualified as L
import Language.Haskell.Exts qualified as H

-- | Minifies `toy-lib` modules into one line.
minifyLibrary :: [H.Extension] -> [(FilePath, [H.Extension], H.Module H.SrcSpanInfo)] -> String
minifyLibrary ghc2021Extensions =
  L.intercalate ";"
    . map
      ( \(!_path, !exts, !module_) ->
          minifyDeclarations (exts ++ ghc2021Extensions) module_
      )
  where
    minifyDeclarations :: [H.Extension] -> H.Module l -> String
    minifyDeclarations _ ast = minify ast
      where
        -- pretty :: H.Module l -> String
        -- pretty (H.Module _ _ _ _ decls) = unlines $ map (H.prettyPrintWithMode pphsMode) decls
        -- pretty _ = ""

        minify :: H.Module l -> String
        minify (H.Module _ _ _ _ decls) = L.intercalate ";" (map (hack . H.prettyPrintWithMode pphsMode) decls)
          where
            -- `deriving newtype` is not handled correctly by `haskell-src-exts`.
            -- Here we remove newline characters, but there's some needless many spaces before `deriving newtype`:
            hack :: String -> String
            hack = filter (/= '\n')
        minify _ = ""

pphsMode :: H.PPHsMode
pphsMode = H.defaultMode {H.layout = H.PPNoLayout}

generateTemplate :: [H.Extension] -> H.Module H.SrcSpanInfo -> String -> String -> String -> String
generateTemplate extensions (H.Module _ _ _ imports _) toylib macros body =
  init $ unlines
    [ "{- ORMOLU_DISABLE -}",
      exts,
      imports',
      "{-# RULES \"Force inline VAI.sort\" VAI.sort = VAI.sortBy compare #-}",
      init macros, -- `init` removes newline character
      init toylib, -- embedded library or imports of toy-lib
      "{- ORMOLU_ENABLE -}",
      "",
      body
    ]
  where
    exts :: String
    exts = "{-# LANGUAGE " ++ es ++ " #-}"
      where
        es = L.intercalate ", " . L.sort $ map H.prettyExtension extensions

    imports' :: String
    imports' = L.intercalate ";" [L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) imports)]
