{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for discrete groups
-- Wbar : Grp -> 0-reduced sSet_*
-- Much easier than the case of general simplicial groups
-- See also https://dl.acm.org/doi/10.1145/1576702.1576744
module Math.Topology.SGrp.WbarDiscrete where

import Math.Algebra.Group
import qualified Math.Topology.SGrp as S
import Math.Topology.SSet

-- If a is a discrete group, things get much easier.
newtype WbarDiscrete a = WbarDiscrete a

-- The group need not be ordered, i.e. the order doesn't respect multiplication
instance (Group a, Ord (Element a)) => SSet (WbarDiscrete a) where
  -- A non-degenerate n-simplex is a list of n non-identity elements
  -- of `a`
  type GeomSimplex (WbarDiscrete a) = [Element a]

  isGeomSimplex (WbarDiscrete a) ss = unit a `notElem` ss

  geomSimplexDim _ ss = length ss

  geomFace _ [] _ = error "WbarDiscrete geomFace: Trying to compute the face of a vertex"
  geomFace (WbarDiscrete a) ss i = normalise a (underlying ss i)
    where
      underlying (_:ss) 0 = ss
      underlying [s] 1 = []
      underlying (s : s' : ss) 1 = prod a s s' : ss
      underlying (s : ss) i = s : underlying ss (i - 1)
      underlying _ _ = error "WbarDiscrete geomFace: impossible" -- can't happen

normalise :: (Group a, Eq (Element a)) => a -> [Element a] -> Simplex (WbarDiscrete a)
normalise a [] = nonDegen []
normalise a (e : es)
  | e == unit a = degen (normalise a es) 0
  | otherwise = fmap (e :) (downshift (normalise a es))

unnormalise :: (Group a, Eq (Element a)) => a -> Simplex (WbarDiscrete a) -> [Element a]
unnormalise a (FormalDegen g d) = helper (unit a) g (dsymbol d)
  where
    helper :: el -> [el] -> [Int] -> [el]
    helper u xs [] = xs
    helper u [] [i] = replicate i u
    helper u [] _ = error "WbarDiscrete unnormalise: impossible"
    helper u (x:xs) (i:is) = replicate (i-1) u ++ x : helper u xs is

instance (Group a, Ord (Element a)) => Pointed (WbarDiscrete a) where
  basepoint (WbarDiscrete a) = []

instance (Group a, Ord (Element a)) => ZeroReduced (WbarDiscrete a)

instance (FiniteGroup a, Ord (Element a)) => FiniteType (WbarDiscrete a) where
  geomBasis (WbarDiscrete a) i = sequence (replicate i nonident)
    where
      nonident = filter (\x -> x /= unit a) (elements a)

instance (Abelian a, Ord (Element a)) => S.SGrp (WbarDiscrete a) where
  prodMor (WbarDiscrete a) = Morphism $ \(s, t) -> normalise a $ fmap (uncurry (prod a)) (zip (unnormalise a s) (unnormalise a t))
  invMor (WbarDiscrete a) = Morphism $ \s -> nonDegen $ fmap (inv a) s

instance (Abelian a, Ord (Element a)) => S.SAb (WbarDiscrete a)
