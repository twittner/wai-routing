-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Network.Wai.Predicate.Doc
    ( Doc
    , doc
    ) where

import Data.Predicate
import Data.Predicate.Descr

data Doc = Doc String

doc :: String -> Doc
doc = Doc

instance Predicate Doc b where
    type FVal Doc = ()
    type TVal Doc = ()
    apply _ _ = T 0 ()

instance Show Doc where
    show (Doc d) = d

instance Description Doc where
    describe (Doc d) = DDoc d
