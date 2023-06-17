{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.List as L
import qualified Language.Haskell.Exts as H
import Language.Haskell.TH (runIO)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)

-- NOTE: On NixOS, purity is disabled as in `stack.yaml` so that we can find `stack` in user path.

main :: IO ()
main = do
  (name : opts) <- getArgs
  let path = modulePath name
  code <- readFile path

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
          { H.parseFilename = path,
            H.extensions = extensions
          }

  case H.parseModuleWithMode parseOption processed of
    H.ParseOk ast -> case opts of
      [] -> putStrLn $ generate name extensions ast
      [dst] -> do
        appendFile dst $ generate name extensions ast
      _ -> pure ()
    failed -> print failed

installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

modulePath :: String -> FilePath
modulePath name = concat [installPath, "/src/", map convert name, ".hs"]
  where
    convert '.' = '/'
    convert c = c

lineWidth :: Int
lineWidth = 100

removeDefineMacros :: String -> String
removeDefineMacros = unlines . filter (not . L.isPrefixOf "#define") . lines

removeMacros :: String -> String
removeMacros = unlines . filter (not . L.isPrefixOf "#") . lines

generate :: String -> [H.Extension] -> H.Module l -> String
generate name extensions ast =
  unlines [scriptHeader, "-- -*- vimish-evil-mode -*-", "-- {{{ Template", "{- ORMOLU_DISABLE -}", extensionLine, minify ast, "{- ORMOLU_ENABLE -}", "-- }}}"]
  where
    extensionLine :: String
    extensionLine = "{-# LANGUAGE " ++ exts ++ " #-}"
      where
        exts = L.intercalate ", " $ map H.prettyExtension extensions

    pretty :: H.Module l -> String
    pretty (H.Module _ _ _ _ decls) = unlines $ map (H.prettyPrintWithMode pphsMode) decls
    pretty _ = ""

    minify :: H.Module l -> String
    minify (H.Module _ _ _ imports decls) =
      L.intercalate
        " ; "
        [ L.intercalate " ; " (map (H.prettyPrintWithMode pphsMode) imports),
          L.intercalate " ; " (map (H.prettyPrintWithMode pphsMode) decls)
        ]
    minify _ = ""

    pphsMode :: H.PPHsMode
    pphsMode = H.defaultMode {H.layout = H.PPNoLayout}

    header :: String -> String
    header name =
      unlines
        [ "-- " ++ replicate (lineWidth - 3) '-',
          "-- " ++ name,
          "-- " ++ replicate (lineWidth - 3) '-'
        ]

    scriptHeader :: String
    scriptHeader =
      unlines
        [ "#!/usr/bin/env stack",
          "{- stack script --resolver lts-16.31 --package array --package bytestring --package containers --package extra --package hashable --package unordered-containers --package heaps --package utility-ht --package vector --package vector-th-unbox --package vector-algorithms --package primitive --package transformers --ghc-options \"-D DEBUG\" -}"
        ]
