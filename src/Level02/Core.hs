{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           Level02.Types            (ContentType (..), Error (..), RqType (..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

import           Control.Applicative      (liftA2)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse s c = responseLBS s [("Content-Type ",renderContentType c)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 = mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 = mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 = mkResponse status400

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t b = let topic = mkTopic t
                       comment = mkCommentText (lazyByteStringToStrictText b)
                   in liftA2 AddRq topic comment

-- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
lazyByteStringToStrictText :: LBS.ByteString -> Text
lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

textToLazyByteString :: Text -> LBS.ByteString
textToLazyByteString = LBS.fromStrict . encodeUtf8

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest = fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse (Err t) = mkResponse
                            status400
                            ContentTypeJson
                            (textToLazyByteString t)

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest req =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case pathInfo req of
    [topic:"add"] -> _add
    [topic:"view"] -> _view
    ["list"] -> _list

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest =
  error "handleRequest not implemented"

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app
  :: Application
app =
  error "app not reimplemented"

runApp :: IO ()
runApp = run 3000 app
