module TaskTest (testTask) where

import GHC
    ( DynFlags(hscTarget, ghcLink, packageEnv),
      HscEnv,
      Module(moduleName),
      simpleImportDecl,
      unLoc,
      SuccessFlag(..),
      HscTarget(HscInterpreted),
      GhcLink(LinkInMemory),
      GhcMonad(..),
      ModSummary(ms_mod, ms_textual_imps),
      addTarget,
      defaultErrorHandler,
      getModuleGraph,
      getModuleInfo,
      guessTarget,
      modInfoTopLevelScope,
      runGhc,
      setSessionDynFlags,
      load,
      getSessionDynFlags,
      mgModSummaries,
      compileExpr,
      setContext,
      mkModuleName,
      LoadHowMuch(LoadAllTargets),
      InteractiveImport(IIDecl) )
import GHC.LanguageExtensions.Type ()
import DynFlags ( defaultFatalMessager, defaultFlushOut )
import Outputable ()
import Pretty ()
import OccName ( HasOccName(occName), mkOccName, varName )

import Data.List (find, isInfixOf)

import Control.Exception (finally)
import Control.Monad (join, when, unless)
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import System.Exit (exitFailure)
import System.Directory (doesFileExist, removeFile)

import GHC.Paths (libdir)

import Unsafe.Coerce (unsafeCoerce)

{-
  GHC related code follows Stephen Diehl's "Dive into GHC" Overview.
  http://www.stephendiehl.com/posts/ghc_01.html
-}

testTask :: FilePath -> FilePath -> IO ()
testTask task solution = do
    putStrLn $ "\ESC[34mTesting file " ++ task ++ "\ESC[0m"
    runMain task solution Nothing

runMain :: FilePath -> FilePath -> Maybe [String] -> IO ()
runMain task solution typeHoles = do
  (template,tests) <- splitTask task typeHoles
  flip finally (mapM_ removeFile (template:tests)) $ do
    let configDir = ".test-task"
    pkgEnvExists <- doesFileExist $ configDir ++ "/pkg-env"
    let envFile =
          if pkgEnvExists
            then Just $ configDir ++ "/pkg-env"
            else Nothing
    env <- setupEnv envFile
    -- test compile template
    -- don't load the first (primary) test module but load other hidden modules so that the template can import them
    unless ("Err" `isInfixOf` task) $ do
      (sflagTemplate,_) <- compileFiles env $ template : tail tests
      reportOutcome "template" sflagTemplate
    -- test compile solution and tests
    (sflagSolution,env) <- compileFiles env $ [configDir ++ "/TestHelper", configDir ++ "/TestHarness", solution] ++ tests
    reportOutcome "solution and tests" sflagSolution
    testRes <- testFiles env configDir
    case testRes of
      Just err -> do
        putStrLn "testing solution failed:"
        putStrLn err
        exitFailure
      Nothing -> putStrLn "successfully tested solution"

reportOutcome :: String -> SuccessFlag -> IO ()
reportOutcome target Succeeded =
  putStrLn $ target ++ " complilation successfull"
reportOutcome target Failed = do
  putStrLn "Error locations are relative to configuration boundaries"
  putStrLn $ target ++ " compilation failed"
  exitFailure

setupEnv :: Maybe FilePath -> IO HscEnv
setupEnv env = defaultErrorHandler defaultFatalMessager defaultFlushOut $
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                , packageEnv = env
                                }
    getSession

compileFiles :: HscEnv -> [FilePath] -> IO (SuccessFlag,HscEnv)
compileFiles env fs =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      setSession env
      mapM_ addTargetFile fs
      sflag <- load LoadAllTargets
      env <- getSession
      return (sflag,env)

addTargetFile ::  GhcMonad m => FilePath -> m ()
addTargetFile file = do
  target <- guessTarget file Nothing
  addTarget target

type TestFailure = String

testFiles :: HscEnv -> FilePath -> IO (Maybe TestFailure)
testFiles env configDir = runGhc (Just libdir) $ do
  setSession env
  -- look for a Main.main function
  modules <- mgModSummaries <$> getModuleGraph
  let mMod = find ((mkModuleName "Main" ==) . moduleName . ms_mod) modules
  runMain <- case mMod of
    Just modSum -> do
      topLevelScope <- join <$> modInfoTopLevelScope <$$> getModuleInfo (ms_mod modSum)
      let mainName = mkOccName varName "main"
          containsMain = maybe False ((mainName `elem`) . map occName) topLevelScope
          isCodeWorldTask = mkModuleName "CodeWorld" `elem` map (unLoc . snd) (ms_textual_imps modSum)
      return $ containsMain && not isCodeWorldTask
    Nothing -> return False
  -- compile test runner
  setContext $
    [ IIDecl $ simpleImportDecl (mkModuleName "Prelude")
    , IIDecl $ simpleImportDecl (mkModuleName "Test.HUnit.Base")
    , IIDecl $ simpleImportDecl (mkModuleName "Test")
    , IIDecl $ simpleImportDecl (mkModuleName "TestHarness")
    ] ++
    [ IIDecl $ simpleImportDecl (mkModuleName "Main") | runMain ]
  -- run public test suite (Main.main) if present
  when runMain $ do
    hValue <- compileExpr "Main.main"
    liftIO $ do
      putStrLn "found public test suite\nrunning Main.main:"
      unsafeCoerce hValue
  -- run internal test suite
  hValue <- compileExpr $
    "let (Counts {failures=n},s) = TestHarness.run Test.test"
    ++ " in if n > 0 then return (Just $ s []) else (return Nothing :: IO (Maybe String))"
  liftIO (unsafeCoerce hValue)

splitTask :: FilePath -> Maybe [String] ->  IO (FilePath,[FilePath])
splitTask file typeHoles = do
  (template,tests) <- splitConfig <$> readFile file
  let fileBaseName = take (length file - 3) file
      templateFile = fileBaseName ++ "-template.hs"
      testFiles = [fileBaseName ++ "-tests"++ show n ++ ".hs" | (n,_) <- zip [1..] tests]
  writeFile templateFile $ insertHoledTypes template typeHoles
  mapM_ (uncurry writeFile) $ zip testFiles tests
  return (templateFile,testFiles)

insertHoledTypes :: String -> Maybe [String] -> String
insertHoledTypes template Nothing = template
insertHoledTypes template (Just tys) =
  template ++ "\n" ++
  unlines (map ("data " ++) tys)

splitConfig :: String -> (String, [String])
splitConfig x =
  -- dropWhile discards the YAML part of the config
  let ~(ls1,_sep:ls2) = break isSep . tail . dropWhile (not . isSep) $ lines x
  in (unlines ls1, splitTests ls2)

splitTests :: [String] -> [String]
splitTests x = case break isSep x of
  (ls1,[]) -> [unlines ls1]
  (ls1,_sep:ls2) -> unlines ls1 : splitTests ls2

isSep :: String -> Bool
isSep ('-':'-':'-':_) = True
isSep _ = False

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
