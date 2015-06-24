module Geb.Utils where

import           Control.Monad
import           Control.Monad.Catch

catchConst :: (Exception e, Eq e, MonadCatch m) => e -> m a -> m a -> m a
catchConst exc f def = catchJust (guard . (== exc))
                                 f
                                 (const def)
