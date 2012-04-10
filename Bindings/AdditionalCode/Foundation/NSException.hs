--X catchNS

import HOC.Exception          ( WrappedNSException(..) )
import Control.Exception as E ( catch )
-- CUT HERE

catchNS :: IO a -> (NSException () -> IO a) -> IO a
catchNS action handler
    = action `E.catch` \(WrappedNSException exc) -> handler (castObject exc)
