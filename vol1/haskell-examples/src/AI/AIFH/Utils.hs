-----------------------------------------------------------------------------
--
-- Module      :  AI.AIFH.Utils
-- Copyright   :
-- License     :  Apache
--
-- Maintainer  :  JP Moresmau <jp@moresmau.fr>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module AI.AIFH.Utils where


roundTo :: (Fractional a, Integral b, RealFrac r) =>
                 b -> r -> a
roundTo n f=  (fromInteger $ round $ f * (10^n)) / (10.0^^n)
