{-# LANGUAGE DeriveDataTypeable #-}

module Lit.ErrorHandling
    ( context
    , Context
    , Details
    , litThrow
    , litThrowIO
    , litThrow'
    , litThrowIO'
    , funnel
    ) where

import           Control.Exception (Exception, SomeException, catch, throw,
                                    Handler(..), catches, fromException)
import qualified Control.Exception as E
import           Control.Monad     (void)
import           Data.Typeable     (Typeable)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStr, hPutStrLn, stderr)
import qualified System.IO.Error   as IO.Error
import           Data.Foldable     (for_)

type Context = String
type Details = String

data LitError = LitError Details String | LitErrorWrap E.SomeException
data LitExcp = LitExcp
    { getContext :: [Context]
    , getError :: LitError
    } deriving Typeable

instance Show LitExcp where
    show (LitExcp _ (LitError _ s)) = s
    show (LitExcp _ (LitErrorWrap e)) = errStr e

instance Exception LitExcp

errStr :: SomeException -> String
errStr e = case fromException e of
    Just ioe | IO.Error.isUserError ioe -> IO.Error.ioeGetErrorString ioe
    _ -> show e

errContext :: SomeException -> [Context]
errContext = maybe [] getContext . fromException

errDetails :: SomeException -> Maybe String
errDetails e = case fromException e of
    Just (LitExcp _ (LitError d _)) -> Just d
    _ -> Nothing

context :: Context -> IO a -> IO a
context c a = catches a
    [ Handler $ \(LitExcp cs e) -> throw $ LitExcp (c:cs) e
    , Handler $ throw . LitExcp [c] . LitErrorWrap
    ]

printException :: SomeException -> IO ()
printException e = do
    hPutStr stderr "hlit: "
    hPutStrLn stderr $ errStr e
    for_ (errContext e) $ \c ->
        hPutStrLn stderr $ "  * while " ++ c ++ "."
    for_ (errDetails e) $ \details -> do
        hPutStrLn stderr "details:"
        for_ (lines details) $ hPutStrLn stderr . ("> " ++)

handler :: SomeException -> IO ()
handler e = printException e >> exitFailure

funnel :: IO a -> IO ()
funnel a = void a `catch` handler

litThrow :: String -> a
litThrow = E.throw . LitExcp [] . LitError ""

litThrowIO :: String -> IO a
litThrowIO = E.throwIO . LitExcp [] . LitError ""

litThrow' :: String -> Details -> a
litThrow' s d = E.throw . LitExcp [] $ LitError d s

litThrowIO' :: String -> Details -> IO a
litThrowIO' s d = E.throwIO . LitExcp [] $ LitError d s
