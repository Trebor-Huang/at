{-# LANGUAGE UndecidableInstances #-}

-- | A strong deformation retract of chain complexes.  We follow Kenzo
-- and call these 'reductions'. In other places these are called
-- 'contractions' or 'SDR-data'.
module Math.Algebra.ChainComplex.Reduction where

import Control.Category.Constrained
import Math.Algebra.ChainComplex
import Math.Algebra.Combination (singleComb, coerceCombination)

import Prelude hiding (id, (.), fmap)

data UReduction a b = Reduction
  { reductionF :: UMorphism Int a b, -- degree 0
    reductionG :: UMorphism Int b a, -- degree 0
    reductionH :: UMorphism Int a a -- degree 1
  }

type Reduction a b = UReduction (Basis a) (Basis b)

instance Semigroupoid UReduction where
  type Object UReduction a = Ord a
  (Reduction f1 g1 h1) . (Reduction f2 g2 h2) = Reduction (f1 . f2) (g2 . g1) (h2 + (g2 . h1 . f2))

instance Category UReduction where
  id = Reduction id id (morphismZeroOfDeg 1)

isoToReduction :: Ord a => UMorphism Int a b -> UMorphism Int b a -> UReduction a b
isoToReduction f g = Reduction f g 0

data Perturbed a = Perturbed { perturbedOrig :: a,
                               perturbedDiff :: Morphism a a }

newtype PerturbedBasis a = PerturbedBasis a
  deriving (Eq, Ord, Show)

instance (ChainComplex a) => ChainComplex (Perturbed a) where
  type Basis (Perturbed a) = PerturbedBasis (Basis a)
  degree (Perturbed a _) (PerturbedBasis b) = degree a b
  diff (Perturbed a delta) = Morphism (-1) $ coerceCombination $
    \b -> diff a `onBasis` b + delta `onBasis` b

instance (FiniteType a) => FiniteType (Perturbed a) where
  dim (Perturbed a _) = dim a
  basis (Perturbed a _) n = fmap PerturbedBasis (basis a n)

-- | The Basic Perturbation Lemma
-- The recursion only terminates if (deltahat . h) is
-- pointwise nilpotent, and this is not checked!.
perturb ::
  (Ord (Basis a), Ord (Basis b)) =>
  a ->
  b ->
  Reduction a b ->
  Morphism a a ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturb a b (Reduction f g h) deltahat =
  (Perturbed a deltahat, Perturbed b delta,
    Reduction (coerceCombination f') (coerceCombination g') (coerceCombination h'))
  where
    sigmaimp d = singleComb d - fmap sigma ( (h . deltahat) `onBasis` d)
    sigma = Morphism 0 sigmaimp
    f' = f . (id - (deltahat . sigma . h))
    g' = sigma . g
    h' = sigma . h
    delta = f . deltahat . g'

-- | Use the BPL to set the differential of `a` to a particular
-- morphism. Again, the nilpotence condition of the BPL must be
-- satisfied.
perturbTo ::
  (Ord (Basis a), Ord (Basis b), ChainComplex a) =>
  a ->
  b ->
  Reduction a b ->
  Morphism a a ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbTo a b r d = perturb a b r (d - diff a)

-- | The Easy Perturbation Lemma
perturbBottom ::
  (Ord (Basis a), Ord (Basis b)) =>
  a ->
  b ->
  Reduction a b ->
  Morphism b b ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbBottom a b (Reduction f g h) delta =
  (Perturbed a deltahat, Perturbed b delta,
    Reduction (coerceCombination f) (coerceCombination g) (coerceCombination h))
  where
    deltahat = g . delta . f

-- | Use the EPL to set the differential of `b` to a particular
-- morphism.
perturbBottomTo ::
  (Ord (Basis a), Ord (Basis b), ChainComplex b) =>
  a ->
  b ->
  Reduction a b ->
  Morphism b b ->
  (Perturbed a, Perturbed b, Reduction (Perturbed a) (Perturbed b))
perturbBottomTo a b r d = perturbBottom a b r (d - diff b)
