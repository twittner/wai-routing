-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Network.Wai.Predicate.Error where

import Data.ByteString (ByteString)
import Network.HTTP.Types.Status

-- | The error type used as 'F' meta-data in all snap predicates.
data Error = Error
    { status  :: !Status          -- ^ (HTTP) status code
    , message :: Maybe ByteString -- ^ optional status message
    } deriving (Eq, Show)

-- | Convenience function to construct 'Error' values from
-- status code and body message.
err :: Status -> ByteString -> Error
err s = Error s . Just
