{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Inter ( interFile ) where

import System.IO ( hClose, hFlush, hPutStr, openTempFile )
import System.Directory ( removeFile )
import Control.Applicative ()
import DynFlags
    ( DynFlags(ghcLink, hscTarget),
      HscTarget(HscInterpreted),
      GhcLink(LinkInMemory),
      defaultFatalMessager,
      defaultFlushOut )
import GHC
    ( defaultErrorHandler,
      guessTarget,
      runGhc,
      setSessionDynFlags,
      setTargets,
      load,
      getSessionDynFlags,
      compileExpr,
      setContext,
      mkModuleName,
      LoadHowMuch(LoadAllTargets),
      InteractiveImport(IIDecl), simpleImportDecl, SuccessFlag (Failed, Succeeded) )
import GHC.Paths ( libdir )
import Unsafe.Coerce ( unsafeCoerce )
import Control.Exception ( finally )
import Control.Monad (join)

interFile :: String -> String -> IO String
interFile name code = do
    tmp <- createTempFile code
    join $ finally (inter name tmp) (removeFile tmp)


inter :: Show a => String -> FilePath -> IO (IO a)
inter name file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget file Nothing]
        l <- load LoadAllTargets
        case l of
          Failed -> error "Compilation failed!"
          Succeeded -> do
            setContext [ IIDecl $ simpleImportDecl (mkModuleName "Snippet")
                       , IIDecl $ simpleImportDecl (mkModuleName "Prelude")
                       ]
            value <- compileExpr name
            do let value' = unsafeCoerce value
               return value'


createTempFile :: String -> IO FilePath
createTempFile code = do
        (tfile, h) <- openTempFile "." "tmpFile.hs"
        hPutStr h code
        hFlush h
        hClose h
        return tfile