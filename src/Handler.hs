module Handler ( HTTPMethod (..)
               , Request (..)
               , Response (..)
               , Handler
               , Headers
               , HeaderName
               , HeaderValue
               , HeaderField
               , ResponseData (..)
               ) where

import Data.Text
import Data.ByteString.Lazy

import ProjectPrelude

type HeaderName = Text
type HeaderValue = Text
type HeaderField = (HeaderName, HeaderValue)

type Headers = [HeaderField]

data HTTPMethod = HTTP_GET | HTTP_POST | HTTP_HEAD | HTTP_PUT | HTTP_DELETE

data Request = Request
                { reqMethod :: HTTPMethod
                , reqPath :: Text
                , reqHeaders :: Headers
                , reqAuthority :: Maybe Text
                , reqDataChunk :: Maybe (Int -> IO ByteString)
                , reqScheme  :: Text
                }

data ResponseData = ResponseComplete ByteString | ResponseChunked (Word32 -> IO ByteString) | ResponseHeadOnly

data Response = Response
                { respStatus :: Word16
                , respHeaders :: Headers
                , respData :: ResponseData
                }

type Handler = Request -> IO Response
