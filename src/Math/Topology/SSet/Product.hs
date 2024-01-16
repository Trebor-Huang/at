{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Cartesian product of simplicial sets See as:dvf, as:ez-dvf
--
-- WARNING: You can define a DVF on the product by searching the path
-- (0,0) to (p,q) forwards or backwards. Some resources use forwards,
-- some backwards, we follow Kenzo by going backwards.
module Math.Topology.SSet.Product where

import Control.Category.Constrained (fmap, (.))
import Data.Coerce
import Prelude hiding (fmap, id, return, (.))

import Math.Algebra.ChainComplex hiding (FiniteType, Morphism)
import qualified Math.Algebra.ChainComplex as CC
import Math.Algebra.ChainComplex.Coalgebra
import Math.Algebra.ChainComplex.DVF hiding (DVF)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Algebra.ChainComplex.Tensor
import Math.Algebra.Combination
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains

data Product a b = Product a b

instance (Show a, Show b) => Show (Product a b) where
  show (Product a b) = show a ++ " × " ++ show b

instance (SSet a, SSet b) => SSet (Product a b) where
  type GeomSimplex (Product a b) = (Simplex a, Simplex b)
  isGeomSimplex (Product a b) (s, t) =
    simplexDim a s == simplexDim b t
      && jointlyNonDegen (s, t)
      && isSimplex a s
      && isSimplex b t

  geomSimplexDim (Product a _) (s, _) = simplexDim a s

  geomFace (Product a b) (s, t) i = prodNormalise (face a s i, face b t i)

-- NOTE: In bit-field form we can use "Parallel Bits Extract" or
-- similar to do this efficiently. Single x86 instruction!
-- https://stackoverflow.com/questions/21144237/standard-c11-code-equivalent-to-the-pext-haswell-instruction-and-likely-to-be

-- | Join two degeneracies p : [n] -> [i] and q : [n] -> [j].
-- More precisely, it now pinches together points that are pinched
-- together by both maps simultaneously.
-- It returns (j : [n] -> [?], p', q') satisfying
-- -
-- - j <> p' = p
-- - j <> q' = q
joinDegen :: DegenSymbol -> DegenSymbol -> (DegenSymbol, DegenSymbol, DegenSymbol)
joinDegen (DegenSymbol (x:xs)) (DegenSymbol (y:ys))
  | x > y =
    let (j, p', q') = ((x-y) ?: DegenSymbol xs) `joinDegen` DegenSymbol ys in
      (y ?: j, (1 ?: p') <> DegenSymbol [2], 1 ?: q')
  | x < y =
    let (j, p', q') = DegenSymbol xs `joinDegen` ((y-x) ?: DegenSymbol ys) in
      (x ?: j, 1 ?: p', (1 ?: q') <> DegenSymbol [2])
  | otherwise =
    let (j, p', q') = DegenSymbol xs `joinDegen` DegenSymbol ys in
      (x ?: j, 1 ?: p', 1 ?: q')
joinDegen p NonDegen = (NonDegen, p, NonDegen)
joinDegen NonDegen q = (NonDegen, NonDegen, q)

prodNormalise :: (Simplex a, Simplex b) -> Simplex (Product a b)
prodNormalise (FormalDegen a d1, FormalDegen b d2) =
  let (d, p, q) = joinDegen d1 d2 in
    FormalDegen (FormalDegen a p, FormalDegen b q) d

prodUnnormalise :: Simplex (Product a b) -> (Simplex a, Simplex b)
prodUnnormalise s = (s >>= fst, s >>= snd) -- nice!

jointlyNonDegen :: (Simplex a, Simplex b) -> Bool
jointlyNonDegen ss = not $ isDegen (prodNormalise ss)

instance (Pointed a, Pointed b) => Pointed (Product a b) where
  basepoint (Product a b) = (nonDegen $ basepoint a, nonDegen $ basepoint b)

instance (ZeroReduced a, ZeroReduced b) => ZeroReduced (Product a b)

instance (OneReduced a, OneReduced b) => OneReduced (Product a b)

instance (FiniteType a, FiniteType b) => FiniteType (Product a b) where
  geomBasis (Product a b) n = [(s, t) | s <- allSimplices a n, t <- allSimplices b n, isGeomSimplex (Product a b) (s, t)]

prodSym :: Morphism (Product a b) (Product b a)
prodSym = Morphism $ \(s, t) -> nonDegen (t, s)

prodAssoc :: Morphism (Product (Product a b) c) (Product a (Product b c))
prodAssoc = Morphism $ \(st, r) ->
  let (s, t) = prodUnnormalise st
   in prodNormalise (s, prodNormalise (t, r))

prodAssocInv :: Morphism (Product a (Product b c)) (Product (Product a b) c)
prodAssocInv = Morphism $ \(s, tr) ->
  let (t, r) = prodUnnormalise tr
   in prodNormalise (prodNormalise (s, t), r)

prodFunc :: Morphism a a' -> Morphism b b' -> Morphism (Product a b) (Product a' b')
prodFunc m m' = Morphism $ \(s, t) -> prodNormalise (m `onSimplex` s, m' `onSimplex` t)

-- TODO: there is probably some kind of typeclass trickery we could
-- use to implement 'coherence'. It would be nice to specify a
-- type-level mapping (1, (2, 3)) -> ((2, 1), 3) and have it put
-- together the above maps as required.

instance (SSet a, SSet b) => DVF (Product a b) where
  vf = status where
    incidenceFor :: Int -> Incidence  -- which face
    incidenceFor ix = if even ix then Pos else Neg

    -- Takes a reversed coordinate list
    search :: [(Int, Int)] -> Int -> Status [(Int, Int)]
    search ((x0, y0):(x1, y1):(x2, y2):xs) i
      | x0 == x1 + 1 &&
        x1 == x2 &&
        y0 == y1 &&
        y1 == y2 + 1
        = Target ((x0,y0):(x2,y2):xs) (incidenceFor (i-1))
    search ((x0, y0):(x1, y1):xs) i
      | x0 == x1 + 1 &&
        y0 == y1 + 1
        = Source ((x0,y0):(x1,y0):(x1,y1):xs) (incidenceFor i)
    search (c:coords) i = fmap (c:) $ search coords (i-1)
    search [] i = Critical

    status :: (SSet a, SSet b) => Product a b -> (Simplex a, Simplex b) -> Status (Simplex a, Simplex b)
    status (Product a b) (FormalDegen s d, FormalDegen t d') =
      let dim = simplexDim a (FormalDegen s d) in
      let sd = reverse $ take (dim+1) $ slowDegen 0 d in
      let sd' = reverse $ take (dim+1) $ slowDegen 0 d' in
      let wrap (sd, sd') =
            (FormalDegen s (quickDegen (reverse sd)),
             FormalDegen t (quickDegen (reverse sd'))) in
      fmap (wrap . unzip) $ search (zip sd sd') dim


stripProduct :: (Simplex a, Simplex b) -> (GeomSimplex a, GeomSimplex b)
stripProduct (s, t) = (underlyingGeom s, underlyingGeom t)

reconstructProduct :: (SSet a, SSet b) => a -> b -> (GeomSimplex a, GeomSimplex b) -> (Simplex a, Simplex b)
reconstructProduct a b (s, t) =
  let n = geomSimplexDim a s
      m = geomSimplexDim b t
  in (downshiftN n (constantAt s m), constantAt t n)

criticalIso ::
  forall a b.
  CC.Morphism
    (CriticalComplex (NChains (Product a b)))
    (Tensor (NChains a) (NChains b))
criticalIso = fmapBasis $ coerce @((Simplex a, Simplex b) -> _) stripProduct

criticalIsoInv ::
  (SSet a, SSet b) =>
  a ->
  b ->
  CC.Morphism
    (Tensor (NChains a) (NChains b))
    (CriticalComplex (NChains (Product a b)))
criticalIsoInv a b = fmapBasis $ coerce $ reconstructProduct a b

ezReduction ::
  (SSet a, SSet b) =>
  Product a b ->
  Reduction
    (NChains (Product a b))
    (Tensor (NChains a) (NChains b))
ezReduction p@(Product a b) =
  isoToReduction criticalIso (criticalIsoInv a b)
    . dvfReduction (NChains p)

diagMor :: Morphism a (Product a a)
diagMor = Morphism $ \s -> nonDegen (nonDegen s, nonDegen s)

instance (SSet a, Eq (GeomSimplex a)) => Coalgebra (NChains a) where
  counitMor a = CC.Morphism 0 $ \s -> if degree a s == 0 then singleComb () else 0
  delMor (NChains a) = reductionF (ezReduction (Product a a)) . fmap diagMor

instance (Effective a, Effective b) => Effective (Product a b) where
  type Model (Product a b) = Tensor (Model a) (Model b)

  model (Product a b) = Tensor (model a) (model b)

  eff p@(Product a b) =
    tensorEquiv (eff a) (eff b)
      . fromRedLeft
        (NChains (Product a b))
        (Tensor (NChains a) (NChains b))
        (ezReduction p)
