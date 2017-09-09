module ErrorCodes
   ( ErrorCode (..)
   , ErrorType (..)
   , ConnError (..)
   , errorCodeToWord32
   , errorCodeFromWord32
   ) where

import ProjectPrelude

data ErrorCode = NoError
               | ProtocolError
               | InternalError
               | FlowControlError
               | SettingsTimeout
               | StreamClosedError
               | FrameSizeError
               | RefusedStream
               | CancelStream
               | CompressionError
               | ConnectError
               | EnhanceYourClam
               | InadequateSecurity
               | Http11Reqired
               | EndOfConn
               | UnknownError Word32
                    deriving (Show, Eq)

data ErrorType = ConnectionError | StreamError StreamId deriving (Show, Eq)



data ConnError = ConnError ErrorType ErrorCode deriving (Show, Eq)

errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 NoError = 0x0
errorCodeToWord32 ProtocolError = 0x1
errorCodeToWord32 InternalError = 0x2
errorCodeToWord32 FlowControlError = 0x3
errorCodeToWord32 SettingsTimeout = 0x4
errorCodeToWord32 StreamClosedError = 0x5
errorCodeToWord32 FrameSizeError = 0x6
errorCodeToWord32 RefusedStream = 0x7
errorCodeToWord32 CancelStream = 0x8
errorCodeToWord32 CompressionError = 0x9
errorCodeToWord32 ConnectError = 0xa
errorCodeToWord32 EnhanceYourClam = 0xb
errorCodeToWord32 InadequateSecurity = 0xc
errorCodeToWord32 Http11Reqired = 0xd
errorCodeToWord32 EndOfConn = undefined -- Only interal
errorCodeToWord32 (UnknownError w) = w

errorCodeFromWord32 :: Word32 -> ErrorCode
errorCodeFromWord32 0x0 = NoError
errorCodeFromWord32 0x1 = ProtocolError
errorCodeFromWord32 0x2 = InternalError
errorCodeFromWord32 0x3 = FlowControlError
errorCodeFromWord32 0x4 = SettingsTimeout
errorCodeFromWord32 0x5 = StreamClosedError
errorCodeFromWord32 0x6 = FrameSizeError
errorCodeFromWord32 0x7 = RefusedStream
errorCodeFromWord32 0x8 = CancelStream
errorCodeFromWord32 0x9 = CompressionError
errorCodeFromWord32 0xa = ConnectError
errorCodeFromWord32 0xb = EnhanceYourClam
errorCodeFromWord32 0xc = InadequateSecurity
errorCodeFromWord32 0xd = Http11Reqired
errorCodeFromWord32 w   = UnknownError w

