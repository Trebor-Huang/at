{-# LANGUAGE UndecidableInstances #-}

-- | (Suspension of) Pushouts of surjections
--
-- The suspension avoids some special casing with dimension 0, and
-- means no desuspension is necessary to define the projections.
module Math.Algebra.ChainComplex.Bicone where

import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.Combination
import Prelude hiding (return)

data Bicone b c d = Bicone b c d (Morphism b c) (Morphism d c)

data BiconeBasis b c d = FromB b | FromC c | FromD d
  deriving (Eq, Ord)

instance (ChainComplex b, ChainComplex c, ChainComplex d) => ChainComplex (Bicone b c d) where
  type Basis (Bicone b c d) = BiconeBasis (Basis b) (Basis c) (Basis d)

  isBasis (Bicone b _ _ _ _) (FromB s) = isBasis b s
  isBasis (Bicone _ c _ _ _) (FromC s) = isBasis c s
  isBasis (Bicone _ _ d _ _) (FromD s) = isBasis d s

  degree (Bicone b _ _ _ _) (FromB s) = degree b s
  degree (Bicone _ c _ _ _) (FromC s) = degree c s - 1
  degree (Bicone _ _ d _ _) (FromD s) = degree d s

  diff (Bicone b c d f g) = Morphism (-1) go
    where
      go (FromB s) = (FromB <$> diff b `onBasis` s) + (FromC <$> f `onBasis` s)
      go (FromC s) = - (FromC <$> diff c `onBasis` s)
      go (FromD s) = (FromD <$> diff d `onBasis` s) + (FromC <$> g `onBasis` s)

instance (FiniteType b, FiniteType c, FiniteType d) => FiniteType (Bicone b c d) where
  dim (Bicone b c d _ _) n = dim b n + dim c (n + 1) + dim d n
  basis (Bicone b c d _ _) n = (FromB <$> basis b n) ++ (FromC <$> basis c (n + 1)) ++ (FromD <$> basis d n)

-- Can all be defined without reference to the differentials, so we
-- use UMorphism.
projLeft :: (Num deg, Ord b) => UMorphism deg (BiconeBasis b c d) b
projLeft = Morphism 0 (\case FromB b -> singleComb b; _ -> 0)

projRight :: (Num deg, Ord d) => UMorphism deg (BiconeBasis b c d) d
projRight = Morphism 0 (\case FromD d -> singleComb d; _ -> 0)

projRedLeft :: (Ord b, Ord c, Ord d) => UReduction b c -> UReduction d c -> UReduction (BiconeBasis b c d) b
projRedLeft (Reduction f1 g1 h1) (Reduction f2 g2 h2) = Reduction projLeft (Morphism 0 g) (Morphism 1 h)
  where
    g b = singleComb (FromB b) - (FromD <$> g2 `onComb` (f1 `onBasis` b))
    h (FromB b) = 0
    h (FromC c) = FromD <$> g2 `onBasis` c
    h (FromD d) = FromD <$> h2 `onBasis` d

projRedRight :: (Ord b, Ord c, Ord d) => UReduction b c -> UReduction d c -> UReduction (BiconeBasis b c d) d
projRedRight (Reduction f1 g1 h1) (Reduction f2 g2 h2) = Reduction projRight (Morphism 0 g) (Morphism 1 h)
  where
    g d = singleComb (FromD d) - (FromB <$> g1 `onComb` (f2 `onBasis` d))
    h (FromB b) = FromB <$> h1 `onBasis` b
    h (FromC c) = FromB <$> g1 `onBasis` c
    h (FromD d) = 0
