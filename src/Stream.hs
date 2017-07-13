module Stream where

data StreamState = StrIdle 
                 | StrReservedLocal
                 | StrReservedRemote
                 | StrOpen
                 | StrHalfClosedLocal
                 | StrHalfClosedRemote
                 | StrClosed
