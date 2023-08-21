{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.List qualified as L
import Data.List.Extra (stripSuffix)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.SparseGraph
import Data.Vector.Unboxed qualified as VU
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified
import GHC.Driver.Session (DynFlags, extensionFlags)
import GHC.Driver.Session qualified
import GHC.Hs (HsModule (..), hsmodDecls, hsmodImports, ideclName)
import GHC.LanguageExtensions
import GHC.Parser.Header qualified
import GHC.Parser.Lexer qualified
import GHC.Types.SourceError (handleSourceError)
import GHC.Types.SrcLoc (Located, unLoc)
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Utils.Error qualified
import GHC.Utils.Outputable qualified
import GHC.Utils.Panic (handleGhcException)
import GHC.Utils.Ppr qualified as Pretty
import Language.Haskell.GhclibParserEx.GHC.Parser qualified as GHC.Parser.Ex
import Language.Haskell.GhclibParserEx.GHC.Settings.Config qualified as GHC.Settings.Config.Ex
import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import System.Exit (exitFailure)
import System.IO.Temp (withTempDirectory)
import System.Process (rawSystem)
import "template-haskell" Language.Haskell.TH (runIO)

-- TODO: Need of keep `Located`?

main :: IO ()
main = do
  let srcDir = installPath ++ "/src"
  files <- collectSourceFiles srcDir

  -- parse source files
  parsedFiles <- fmap ((\x -> [x]) . (!! 1)) . forM files $ \absPath -> do
    (absPath,) <$> parseFile absPath
  let (!failures, !successes) = partitionParseResults parsedFiles

  -- exit on any failure
  unless (null failures) $ do
    forM_ failures $ \(!_, !dynFlags, !ps) ->
      putStrLn $ renderParseErrors dynFlags ps
    exitFailure

  -- parse template
  let templatePath = installPath ++ "/template/Main.hs"
  (templateDynFlags, parsed) <- parseFile templatePath

  -- generate the output file
  case parsed of
    (POk _ templateAst) -> do
      let successes' = map (\(!path, !dynFlags, !ast) -> (path, dynFlags, unLoc ast)) successes
      let toylib = sortModules successes'

      header <- readFile $ installPath ++ "/template/Header.hs"
      macros <- readFile $ installPath ++ "/template/Macros.hs"
      body <- readFile $ installPath ++ "/template/Body.hs"
      putStr $ generateTemplate (templateDynFlags, (unLoc templateAst)) toylib header macros body
    (PFailed ps) -> do
      putStrLn "Failed to parse the template file"
      putStrLn $ renderParseErrors templateDynFlags ps
  where
    partitionParseResults = L.foldr step ([], [])
      where
        step (!path, (!dynFlags, PFailed ps)) (!accL, !accR) = ((path, dynFlags, ps) : accL, accR)
        step (!path, (!dynFlags, POk _ ast)) (!accL, !accR) = (accL, (path, dynFlags, ast) : accR)

-- --------------------------------------------------------------------------------
-- IO
-- --------------------------------------------------------------------------------

-- | Recursively collects @.hs@ files.
collectSourceFiles :: FilePath -> IO [FilePath]
collectSourceFiles dir = do
  contents <- map ((dir ++ "/") ++) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
  let sourceFiles = filter filterSourceFiles contents
  subDirs <- filterM doesDirectoryExist contents
  L.foldl' (++) sourceFiles <$> mapM collectSourceFiles subDirs

filterSourceFiles :: String -> Bool
filterSourceFiles = (&&) <$> (".hs" `L.isSuffixOf`) <*> (not . ("Macro.hs" `L.isSuffixOf`))

-- | Reads a file with CPP preprocessed
readFileWithCPP :: FilePath -> IO String
readFileWithCPP absPath = do
  withTempDirectory "." "toy-lib-cpp" $ \tmpDir -> do
    let processedPath = tmpDir ++ "/cpp-processed.hs"
    -- TODO: Just get `stdout` on execution
    _exitCode <- rawSystem "stack" ["ghc", "--", "-E", absPath, "-o", processedPath]
    readFile processedPath

-- | Returns the root directory.
installPath :: FilePath
installPath = $(do dir <- runIO getCurrentDirectory; [e|dir|])

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

renderParseErrors :: DynFlags -> GHC.Parser.Lexer.PState -> String
renderParseErrors dynFlags =
  GHC.Utils.Outputable.renderWithContext
    (GHC.Driver.Session.initDefaultSDocContext dynFlags)
    . GHC.Utils.Error.pprMessages
    . snd
    . GHC.Parser.Lexer.getPsMessages

-- --------------------------------------------------------------------------------
-- Parse / generate
-- --------------------------------------------------------------------------------

defaultDynFlags :: DynFlags
defaultDynFlags =
  GHC.Driver.Session.defaultDynFlags
    GHC.Settings.Config.Ex.fakeSettings
    GHC.Settings.Config.Ex.fakeLlvmConfig

-- | Boxed (lifted) alternative to `GHC.Parser.Lexer.ParseResult`
data BoxedParseResult a = POk GHC.Parser.Lexer.PState a | PFailed GHC.Parser.Lexer.PState

unliftParseResult :: GHC.Parser.Lexer.ParseResult a -> BoxedParseResult a
unliftParseResult (GHC.Parser.Lexer.POk ps a) = POk ps a
unliftParseResult (GHC.Parser.Lexer.PFailed ps) = PFailed ps

-- | Collects declaratrions from a Haskell source file.
parseFile :: FilePath -> IO (DynFlags, BoxedParseResult (Located HsModule))
parseFile absPath = do
  fileContent <- readFileWithCPP absPath
  -- TODO: No need of `defaultDynFlags`?
  dynFlags <- fromJust <$> parsePragmasIntoDynFlags defaultDynFlags absPath fileContent
  let res = GHC.Parser.Ex.parseFile absPath dynFlags fileContent
  return (dynFlags, unliftParseResult res)

-- TODO: what is `str`? why IO?
parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filePath str =
  catchErrors $ do
    let opts = GHC.Driver.Config.Parser.initParserOpts flags
    let (_warnings, !pragmas) = GHC.Parser.Header.getOptions opts (stringToStringBuffer str) filePath
    (flags', _left_pragmas, _warnings) <- GHC.Driver.Session.parseDynamicFilePragma flags pragmas
    return $ Just flags'
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act =
      handleGhcException
        reportErr
        (handleSourceError reportErr act)
    reportErr e = do putStrLn $ "error : " ++ show e; return Nothing

-- | Renders module declarations into one line.
renderDecls :: DynFlags -> HsModule -> String
renderDecls dynFlags =
  L.intercalate ";"
    . map (inOneline . GHC.Utils.Outputable.ppr)
    . GHC.Hs.hsmodDecls
  where
    ctx = GHC.Driver.Session.initDefaultSDocContext dynFlags
    -- Whitespace-separated and not parsable!
    -- inOneline = GHC.Utils.Outputable.showSDocOneLine context
    -- inOneline = L.intercalate ";" . lines . GHC.Utils.Outputable.showSDocOneLine context

    -- ダメだわ。。
    -- TODO: Faster implementation or builtin parsable oneline output?
    inOneline sdoc =
      -- L.intercalate ";" . lines $
        foldr mergeLines ""
        . lines
        . Pretty.fullRender
          Pretty.LeftMode
          -- (GHC.Utils.Outputable.sdocLineLength ctx)
          1_000_000_000
          1.5 -- ribbon?
          Pretty.txtPrinter
          ""
        $ GHC.Utils.Outputable.runSDoc sdoc ctx

    mergeLines "where" ln = "where" ++ (' ' : ln)
    mergeLines ln "where" = ln ++ " where"
    mergeLines fn ln@('=' : _) = fn ++ ln
    mergeLines ln1 ln2 = ln1 ++ (';' : ln2)

-- Semicolon-separated and not parsable:
-- inOneline sdoc =
--   Pretty.fullRender
--     (Pretty.PageMode True)
--     -- (GHC.Utils.Outputable.sdocLineLength ctx)
--     1_000_000_000
--     1.5 -- ribbon?
--     Pretty.txtPrinter
--     ""
--     $ GHC.Utils.Outputable.runSDoc sdoc ctx

-- TODO: stringify qualified imports
importModuleNames :: HsModule -> [String]
importModuleNames = map (moduleNameString . unLoc . ideclName . unLoc) . hsmodImports

importModuleStrings :: HsModule -> [String]
importModuleStrings = map (GHC.Utils.Outputable.showSDocUnsafe . GHC.Utils.Outputable.ppr) . hsmodImports

-- | Geneartes `toy-lib` in one line.
sortModules :: [(FilePath, DynFlags, HsModule)] -> [(FilePath, DynFlags, HsModule)]
sortModules input =
  let edges = VU.fromList $ concatMap (\(!path, !_, !ast) -> edgesOf path ast) input
      gr = buildUSG (0, pred (length input)) edges
      vs = topSortSG gr
   in map (input !!) vs
  where
    moduleNameToVert :: M.Map String Int
    moduleNameToVert = M.fromList $ zip (map (\(!path, _, _) -> fromJust $ moduleName path) input) [0 :: Int ..]

    -- edge from depended vertex to dependent vertex
    edgesOf :: FilePath -> HsModule -> [(Int, Int)]
    edgesOf path hsModule =
      let !v1 = moduleNameToVert M.! fromJust (moduleName path)
          !v2s = mapMaybe (moduleNameToVert M.!?) (importModuleNames hsModule)
       in map (,v1) v2s
    edgesOf _ _ = error "unexpected module data"

generateTemplate :: (DynFlags, HsModule) -> [(FilePath, DynFlags, HsModule)] -> String -> String -> String -> String
generateTemplate (!templateDynFlags, !hsModule) toylib header macros body =
  unlines [header, pre, disableFormat, exts, imports, "", macros, toylib', enableFormat, post, "", body]
  where
    exts :: String
    exts = "{-# LANGUAGE " ++ es ++ " #-}"
      where
        es = L.intercalate ", " . L.sort . map toName . EnumSet.toList $ extensionFlags templateDynFlags
        toName :: Extension -> String
        toName ext = case show ext of
          "Cpp" -> "CPP"
          s -> s

    imports :: String
    imports = L.intercalate ";" $ importModuleStrings hsModule

    pre = "-- {{{ toy-lib: https://github.com/toyboot4e/toy-lib"
    post = "-- }}}"

    disableFormat = "{- ORMOLU_DISABLE -}"
    enableFormat = "{- ORMOLU_ENABLE -}"

    toylib' = L.intercalate ";" $ map (\(_, !dynFlags, !ast) -> renderDecls dynFlags ast) toylib
