--X catchNS

import Foreign.ObjC ( ObjCException(..) )
import Control.Exception as E ( catch )
-- CUT HERE

-- TODO: find out whether the exception needs to be released
catchNS :: IO a -> (NSException () -> IO a) -> IO a
catchNS action handler
    = action `E.catch` \(ObjCException exc) -> importArgument exc Prelude.>>= handler
