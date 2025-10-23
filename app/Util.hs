module Util where

import Control.Concurrent
import Control.Monad.IO.Class
import Language.Javascript.JSaddle hiding (obj, val)
import Miso
import Miso.String

select :: MisoString -> JSM ()
select a = () <$ jsg1 "callSelect" a