module Lit.ErrorHandling where

import           Control.Exception (catches, Handler(..))
import qualified Control.Exception as E
import           Control.Monad     (void)
import           System.Exit       (exitFailure)
import           System.IO         (stderr, hPutStr, hPutStrLn, hPrint)
import qualified System.IO.Error   as IO.Error

funnel :: IO a -> IO ()
funnel a = void $ catches a
    [ Handler $ \e -> do
        if IO.Error.isUserError e
            then do
                hPutStr stderr "hlit: "
                hPutStrLn stderr $ IO.Error.ioeGetErrorString e
            else hPrint stderr e
        exitFailure
    , Handler $ \e -> do
        hPutStr stderr "hlit: "
        hPrint stderr (e :: E.SomeException)
        exitFailure
    ]
