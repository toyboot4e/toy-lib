{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad
import Data.Either
import qualified Data.List as L
import Data.Maybe
import qualified Language.Haskell.Exts as H
import Language.Haskell.TH (runIO)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)

-- TODO: Refactor

main :: IO ()
main = do
  -- (name : opts) <- getArgs
  let srcDir = installPath ++ "/src"
  files <- collectSourceFiles srcDir
  (failures, successes) <- partResults <$> mapM (\f -> (f,) <$> minifyAsOneline f) files
  if (not . null) failures
    then do
      forM_ failures print
      exitFailure
    else do
      forM_ successes $ \(path, result) -> do
        putStrLn $ "-- " ++ modulePath path
        putStrLn result
  where
    partResults :: [(a, Either l r)] -> ([(a, l)], [(a, r)])
    partResults = foldr step ([], [])
      where
        step :: (a, Either l r) -> ([(a, l)], [(a, r)]) -> ([(a, l)], [(a, r)])
        step (f, Left l) (accL, accR) = ((f, l) : accL, accR)
        step (f, Right r) (accL, accR) = (accL, (f, r) : accR)

-- | Recursively collects @.hs@ files.
collectSourceFiles :: FilePath -> IO [FilePath]
collectSourceFiles dir = do
  contents <- map ((dir ++ "/") ++) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
  let sourceFiles = filter (".hs" `L.isSuffixOf`) contents
  subDirs <- filterM doesDirectoryExist contents
  L.foldl' (++) sourceFiles <$> mapM collectSourceFiles subDirs

-- | Collects declaratrions from a Haskell source file and minify them into one line.
minifyAsOneline :: String -> IO (Either String String)
minifyAsOneline absPath = do
  let name = fromJust $ L.stripPrefix (installPath ++ "/") absPath
  code <- readFile absPath

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
          { H.parseFilename = absPath,
            H.extensions = extensions
          }

  return $ case H.parseModuleWithMode parseOption processed of
    H.ParseOk ast -> Right $ generate name extensions ast
    failed -> Left $ show failed

helpCommand :: IO ()
helpCommand = putStrLn "Available commands: oneline"

-- | Returns the root directory.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

-- | Converts module name to source file path.
modulePath :: String -> FilePath
modulePath name = concat [installPath, "/src/", map tr name, ".hs"]
  where
    tr '.' = '/'
    tr c = c

lineWidth :: Int
lineWidth = 100

removeDefineMacros :: String -> String
removeDefineMacros = unlines . filter (not . L.isPrefixOf "#define") . lines

removeMacros :: String -> String
removeMacros = unlines . filter (not . L.isPrefixOf "#") . lines

generate :: String -> [H.Extension] -> H.Module l -> String
generate name extensions ast = minify ast
  -- unlines [scriptHeader, extensionLine, "-- {{{ Template", "{- ORMOLU_DISABLE -}", minify ast, "{- ORMOLU_ENABLE -}", "-- }}}"]
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
        ";"
        [ L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) imports),
          L.intercalate ";" (map (H.prettyPrintWithMode pphsMode) decls)
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
