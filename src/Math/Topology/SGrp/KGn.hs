{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The space \(K(\mathbb{Z}, 1)\), homotopy equivalent to the
-- circle. We cannot use the same method as `Wbar` to show that this
-- simplicial set is effective, because \(\mathbb{Z}\) (as a discrete
-- \(sGrp\)) is not 0-reduced.
--
-- A good reference for the DVF used here is
-- <https://doi.org/10.1007/s10208-013-9159-7>
module Math.Topology.SGrp.KGn where

import Control.Category.Constrained ((.))
import Data.Coerce
import Data.List
import Math.Algebra.Combination
import Math.Algebra.ChainComplex as CC hiding (FiniteType, Morphism)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.DVF hiding (DVF, vf)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Shift
import Math.Algebra.ChainComplex.Sum
import Math.Algebra.Group
import Math.Topology.SGrp
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Prelude hiding ((.))

type KZ1 = WbarDiscrete Z

kz1 :: WbarDiscrete Z
kz1 = WbarDiscrete Z

type CircleComplex = () `Sum` Shift ()

instance Algebra CircleComplex where
  unitMor _ = CC.Morphism 0 (const (singleComb (Left ())))
  muMor _ = CC.Morphism 0 go
    where go (Left _, Left _) = singleComb (Left ())
          go (Left _, Right _) = singleComb (Right (ShiftBasis ()))
          go (Right _, Left _) = singleComb (Right (ShiftBasis ()))
          go (Right _, Right _) = 2 .* singleComb (Right (ShiftBasis ()))

instance DVF KZ1 where
  vf _ [] = Critical
  vf _ [1] = Critical
  vf _ (1 : a1 : as)
    | a1 < 0 = Target (a1 : as) Pos
    | otherwise = Target (a1 + 1 : as) Neg
  vf _ (a1 : as)
    | a1 < 0 = Source (1 : a1 : as) Pos
    | otherwise = Source (1 : a1 - 1 : as) Neg

criticalIso :: CC.Morphism (CriticalComplex (NChains KZ1)) CircleComplex
criticalIso = fmapBasis $
  coerce $ \case
    [] -> Left ()
    [1 :: Integer] -> Right (ShiftBasis ())
    _ -> error "impossible"

criticalIsoInv :: CC.Morphism CircleComplex (CriticalComplex (NChains KZ1))
criticalIsoInv = fmapBasis $
  coerce $ \case
    Left () -> []
    Right (ShiftBasis ()) -> [1 :: Integer]

instance Effective KZ1 where
  type Model KZ1 = CircleComplex

  eff _ =
    fromRedLeft
      (NChains (WbarDiscrete Z))
      (Sum () (Shift ()))
      (isoToReduction criticalIso criticalIsoInv . dvfReduction (NChains (WbarDiscrete Z)))

-- This seems to work... TODO: give a reduction from this to a chain
-- complex with one basis element in each dimension, no need to
-- generate all simplices and filter
instance DVF (WbarDiscrete Zmod) where
  vf (WbarDiscrete (Zmod n)) [] = Critical
  vf (WbarDiscrete (Zmod n)) [1] = Critical
  vf (WbarDiscrete (Zmod n)) (1 : a1 : as)
    | a1 == n - 1 = fmap (\x -> 1 : a1 : x) (vf (WbarDiscrete (Zmod n)) as)
    | otherwise = Target (a1 + 1 : as) Neg
  vf (WbarDiscrete (Zmod n)) (a1 : as) = Source (1 : a1 - 1 : as) Neg

instance Effective (WbarDiscrete Zmod) where
  type Model (WbarDiscrete Zmod) = CriticalComplex (NChains (WbarDiscrete Zmod))
  model w = CriticalComplex (NChains w)
  eff w = dvfEquivalence (NChains w)

-- | An efficient version of \(K(\mathbb{Z}/2, 1)\)
-- Ugly name, but what can you do?
data KZmod2_1 = KZmod2_1
  deriving (Show)

instance SSet KZmod2_1 where
  type GeomSimplex KZmod2_1 = Int

  isGeomSimplex KZmod2_1 s = s >= 0

  geomSimplexDim _ s = s

  geomFace _ 0 _ = error "KZmod2_1 geomFace: Attempt to calculate face of a point"
  geomFace _ s i | 0 == i || s == i = nonDegen (s - 1)
  geomFace _ s i = FormalDegen (s-2) (primDegen (i-1))

instance Pointed KZmod2_1 where
  basepoint _ = 0

instance ZeroReduced KZmod2_1

instance FiniteType KZmod2_1 where
  geomBasis _ i = [i]

instance Effective KZmod2_1

instance SGrp KZmod2_1 where
  prodMor _ = Morphism $ \ (s, t) ->
    let bs = toBits (degenSymbol s) in
    let bt = toBits (degenSymbol t) in
    let b = take (simplexDim KZmod2_1 s) $ zipWith (/=) bs bt in
      FormalDegen (length (filter id b)) $ fromBits $ b
    where
      toBits :: DegenSymbol -> [Bool]
      toBits = intercalate [True]
             . map (\d -> replicate (d-1) False)
             . (++ repeat 1)  -- make it infinite
             . dsymbol

      fromBits :: [Bool] -> DegenSymbol
      fromBits xs =  -- expects a finite result
        let (rs, xs') = span not xs in
          (length rs + 1) ?:
            case xs' of
              [] -> NonDegen
              (_:xs'') -> fromBits xs''
  invMor _ = Morphism nonDegen

instance SAb KZmod2_1
