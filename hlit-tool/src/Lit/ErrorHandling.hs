{-# LANGUAGE DeriveDataTypeable #-}

module Lit.ErrorHandling
    ( context
    , funnel
    , litThrow
    , litThrowIO
    , litThrow'
    , litThrowIO'
    ) where

import           Control.Exception (Exception, catch, throw, Handler(..), catches)
import qualified Control.Exception as E
import           Control.Monad     (void)
import           Data.Typeable     (Typeable, cast)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStr, hPutStrLn, stderr)
import qualified System.IO.Error   as IO.Error
import           Data.Foldable     (traverse_, for_)

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

errStr :: (Typeable e, Show e) => e -> String
errStr e = case cast e of
    Just ioe | IO.Error.isUserError ioe -> IO.Error.ioeGetErrorString ioe
    _ -> show e

errContext :: Typeable e => e -> [Context]
errContext = maybe [] getContext . cast

errDetails :: Typeable e => e -> Maybe String
errDetails e = case cast e of
    Just (LitExcp _ (LitError d _)) -> Just d
    _ -> Nothing

context :: Context -> IO a -> IO a
context c a = catches a
    [ Handler $ \(LitExcp cs e) -> throw $ LitExcp (c:cs) e
    , Handler $ throw . LitExcp [c] . LitErrorWrap
    ]

printException :: Exception e => e -> IO ()
printException e = do
    hPutStr stderr "hlit: "
    hPutStrLn stderr $ errStr e
    for_ (errContext e) $ \c ->
        hPutStrLn stderr $ "  while " ++ c
    traverse_ (hPutStrLn stderr) $ errDetails e

handler :: Exception e => e -> IO ()
handler e = printException e >> exitFailure

funnel :: IO a -> IO ()
funnel a = void a `catch` (handler :: E.SomeException -> IO ())

litThrow :: String -> a
litThrow = E.throw . LitExcp [] . LitError ""

litThrowIO :: String -> IO a
litThrowIO = E.throwIO . LitExcp [] . LitError ""

litThrow' :: String -> Details -> a
litThrow' s d = E.throw . LitExcp [] $ LitError d s

litThrowIO' :: String -> Details -> IO a
litThrowIO' s d = E.throwIO . LitExcp [] $ LitError d s
