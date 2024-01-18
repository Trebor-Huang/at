{-# LANGUAGE UndecidableInstances #-}

-- | Normalised chain complex of a SSet
-- See, eg. <https://kerodon.net/tag/00QH>
module Math.Topology.SSet.NChains where

import Control.Category.Constrained
import Data.Coerce
import Prelude hiding (Bounded, Functor, return)

import Math.Algebra.ChainComplex as CC hiding (Bounded, FiniteType, UMorphism (..), amplitude)
import qualified Math.Algebra.ChainComplex as CC (Bounded, FiniteType (..), UMorphism (..), amplitude)
import Math.Algebra.Combination
import Math.Topology.SSet

-- | Normalised chain complex of a `SSet`
newtype NChains a = NChains a

instance Show a => Show (NChains a) where
  show (NChains a) = "N(" ++ show a ++ ")"

newtype BasisSimplex a = BasisSimplex a
  deriving (Eq, Ord, Show) via a

instance SSet a => CC.ChainComplex (NChains a) where
  type Basis (NChains a) = BasisSimplex (GeomSimplex a)

  isBasis (NChains a) (BasisSimplex s) = isGeomSimplex a s

  degree (NChains a) = coerce $ geomSimplexDim a

  diff (NChains a) = CC.Morphism (-1) (coerceCombination act)
    where
      act v = sum [c .* singleComb s | (c, FormalDegen s d) <- zip signs $ geomFaces a v, d == NonDegen]
      signs = cycle [1, -1]

instance FiniteType a => CC.FiniteType (NChains a) where
  dim (NChains a) i = length (geomBasis a i)
  basis (NChains a) i = coerce $ geomBasis a i

instance Bounded a => CC.Bounded (NChains a) where
  amplitude (NChains a) = amplitude a

instance Functor UMorphism (CC.UMorphism Int) BasisSimplex where
  fmap m = CC.Morphism 0 $ \(BasisSimplex s) ->
    let FormalDegen g d = m `onGeomSimplex` s in
    if d == NonDegen then
      singleComb (BasisSimplex g)
    else
      zeroComb
