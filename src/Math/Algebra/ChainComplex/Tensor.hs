{-# LANGUAGE UndecidableInstances #-}

-- | Tensor product of chain complexes of free Z-modules
module Math.Algebra.ChainComplex.Tensor where

import Control.Category.Constrained hiding (fmap, return)
import qualified Data.Map.Lazy as Map
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.Combination
import Prelude hiding (id, (.))

data Tensor a b = Tensor a b

instance (ChainComplex a, ChainComplex b) => ChainComplex (Tensor a b) where
  type Basis (Tensor a b) = (Basis a, Basis b)
  isBasis (Tensor a b) (s, t) = isBasis a s && isBasis b t
  degree (Tensor a b) (s, t) = degree a s + degree b t
  diff (Tensor a b) = Morphism (-1) go
    where
      go (s, t) =
        fmap (,t) (diff a `onBasis` s)
          + kozulRule negate (degree a s) (fmap (s,) (diff b `onBasis` t))

-- TODO: this assumes that a and b are null below degree 0
instance (FiniteType a, FiniteType b) => FiniteType (Tensor a b) where
  dim (Tensor a b) n = sum [dim a i * dim b (n - i) | i <- [0 .. n]]
  basis (Tensor a b) n =
    [(s, t) | i <- [0 .. n], s <- basis a i, t <- basis b (n - i)]

tensorCombination :: (Eq a, Eq b) => Combination a -> Combination b -> Combination (a, b)
tensorCombination (Combination as) (Combination bs) =
  Combination $ Map.fromAscList $ do
    (a, c) <- Map.toAscList as
    (b, d) <- Map.toAscList bs
    return ((a, b), c * d)

tensorFunc ::
  (ChainComplex a1, ChainComplex a2, Eq (Basis b1), Eq (Basis b2)) =>
  a1 ->
  a2 ->
  Morphism a1 b1 ->
  Morphism a2 b2 ->
  Morphism (Tensor a1 a2) (Tensor b1 b2)
tensorFunc a1 a2 (Morphism deg f1) (Morphism _ f2) = Morphism deg ft
  where
    ft (x1, x2) = kozulRule negComb (degree a1 x1 * degree a2 x2) (tensorCombination (f1 x1) (f2 x2))

tensorAssoc :: Morphism (Tensor (Tensor a b) c) (Tensor a (Tensor b c))
tensorAssoc = fmapBasis $ \((a, b), c) -> (a, (b, c))

tensorAssocInv :: Morphism (Tensor a (Tensor b c)) (Tensor (Tensor a b) c)
tensorAssocInv = fmapBasis $ \(a, (b, c)) -> ((a, b), c)

tensorUnitL :: Morphism (Tensor () a) a
tensorUnitL = fmapBasis snd

tensorUnitLInv :: Morphism a (Tensor () a)
tensorUnitLInv = fmapBasis $ \a -> ((), a)

tensorUnitR :: Morphism (Tensor a ()) a
tensorUnitR = fmapBasis fst

tensorUnitRInv :: Morphism a (Tensor a ())
tensorUnitRInv = fmapBasis $ \a -> (a, ())

tensorReduction ::
  (ChainComplex a1, ChainComplex a2, ChainComplex b1, ChainComplex b2) =>
  a1 ->
  a2 ->
  b1 ->
  b2 ->
  Reduction a1 b1 ->
  Reduction a2 b2 ->
  Reduction (Tensor a1 a2) (Tensor b1 b2)
tensorReduction a1 a2 b1 b2 (Reduction f1 g1 h1) (Reduction f2 g2 h2) = Reduction f' g' h'
  where
    (⊗) = tensorFunc a1 a2
    (⊗^) = tensorFunc a1 a2
    (⊗^^) = tensorFunc b1 b2

    f' = f1 ⊗ f2
    g' = g1 ⊗^^ g2
    h' = (h1 ⊗^ (g2 . f2)) + (id ⊗^ h2)

-- Convenience:
tensorFuncArr ::
  (ChainComplex a1, ChainComplex a2, Eq (Basis b1), Eq (Basis b2)) =>
  ClosedMorphism a1 b1 ->
  ClosedMorphism a2 b2 ->
  ClosedMorphism (Tensor a1 a2) (Tensor b1 b2)
tensorFuncArr (ClosedMorphism a1 m1 b1) (ClosedMorphism a2 m2 b2) = ClosedMorphism (Tensor a1 a2) (tensorFunc a1 a2 m1 m2) (Tensor b1 b2)

-- tensorReductionArr ::
--   (ChainComplex a1, ChainComplex a2) =>
--   ClosedReduction a1 b1 ->
--   ClosedReduction a2 b2 ->
--   ClosedReduction (Tensor a1 a2) (Tensor b1 b2)
-- tensorReductionArr (ClosedReduction a1 m1 b1) (ClosedReduction a2 m2 b2) = ClosedReduction (Tensor a1 a2) (tensorReduction a1 a2 b1 b2 m1 m2) (Tensor b1 b2)

tensorEquiv ::
  (ChainComplex a1, ChainComplex a2, ChainComplex b1, ChainComplex b2) =>
  Equivalence a1 b1 ->
  Equivalence a2 b2 ->
  Equivalence (Tensor a1 a2) (Tensor b1 b2)
tensorEquiv (Equivalence a1 l1 x1 r1 b1) (Equivalence a2 l2 x2 r2 b2) =
  Equivalence (Tensor a1 a2) (tensorReduction x1 x2 a1 a2 l1 l2) (Tensor x1 x2) (tensorReduction x1 x2 b1 b2 r1 r2) (Tensor b1 b2)

-- TODO: this could use a group homomorphism type
crossProduct :: (Eq (Basis a), Eq (Basis a')) =>
  HomologyClass a -> HomologyClass a' -> HomologyClass (Tensor a a')
crossProduct (HomologyClass x) (HomologyClass y) = HomologyClass (tensorCombination x y)
