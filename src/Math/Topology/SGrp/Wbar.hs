{-# LANGUAGE UndecidableInstances #-}

-- | Classifying spaces for simplicial groups
-- Wbar : sGrp -> 0-reduced sSet_*
-- See <https://ncatlab.org/nlab/show/simplicial+classifying+space>
-- In the Kenzo source, this is spread over
-- classifying-spaces.lisp, classifying-spaces-dvf.lisp, cl-space-efhm.lisp
-- Also anromero/resolutions.lisp in the fork
module Math.Topology.SGrp.Wbar where

import Control.Category.Constrained ((.))
import Data.Coerce
import Prelude hiding (id, return, (.))

import qualified Math.Algebra.Bicomplex as Bi
import qualified Math.Algebra.ChainComplex as CC (Morphism, fmapBasis)
import Math.Algebra.ChainComplex.Algebra
import Math.Algebra.ChainComplex.Algebra.Bar
import Math.Algebra.ChainComplex.DVF hiding (DVF, vf)
import Math.Algebra.ChainComplex.Equivalence
import Math.Algebra.ChainComplex.Reduction
import Math.Topology.SGrp
import Math.Topology.SSet
import Math.Topology.SSet.DVF
import Math.Topology.SSet.Effective
import Math.Topology.SSet.NChains
import Math.Topology.SSet.Product hiding (criticalIso, criticalIsoInv)
import Math.Topology.SSet.TwistedProduct

newtype Wbar g = Wbar g
  deriving (Show)

newtype WbarSimplex a = WbarSimplex a
  deriving (Show) via a
  deriving (Functor)
  deriving (Eq, Ord)

instance (SGrp g) => SSet (Wbar g) where
  -- A non-degenerate simplex is a list of simplices of `g`
  -- (Wbar G)_n = G_n-1 x G_n-2 x ... x G_0
  -- meeting a slightly complicated condition on whether the list
  -- contains a unit, and the things proceding it are all degeneracies
  type GeomSimplex (Wbar g) = WbarSimplex [Simplex g]

  isGeomSimplex (Wbar g) (WbarSimplex ss) =
    all (\(i, s) -> simplexDim g s == i) (zip (reverse [0..length ss-1]) ss)
    && normalise g ss == nonDegen (WbarSimplex ss)
    && all (isSimplex g) ss

  geomSimplexDim _ (WbarSimplex ss) = length ss

  -- TODO: need to make sure this matches with Kenzo's conventions,
  -- multiplying on which side (for abelian groups of course it
  -- doesn't matter)
  geomFace (Wbar g) (WbarSimplex ss) i = normalise g (underlying ss i)
    where
      underlying []          _ = error "Wbar geomFace: impossible"
      underlying (s:ss)      0 = ss
      underlying [s]         1 = []
      underlying (s:s':rest) 1 =
        (prodMor g `onSimplex` prodNormalise (face g s 0, s')) : rest
      underlying (s:rest)    i =
        face g s (i - 1) : underlying rest (i - 1)

-- TODO: there are probably efficient algorithms for this in terms of bit fields.
-- 1. Create a bit field marking which positions are the unit
-- 2. Intersect this with the shifted degeneracy operators for each entry
-- 3. Delete all the surviving units and bit-extract every degeneracy operator appropriately

normalise :: (Pointed g) => g -> [Simplex g] -> Simplex (Wbar g)
normalise g [] = nonDegen $ WbarSimplex []
normalise g (s : ss) | isUnit g s = degen (normalise g ss) 0
normalise g (s : ss) = downshift $ fmap (\(s, t) -> WbarSimplex (s : unnormalise g t)) p
  where
    p = prodNormalise (s, normalise g ss)

unnormalise :: (Pointed g) => g -> Simplex (Wbar g) -> [Simplex g]
unnormalise g (FormalDegen (WbarSimplex s) d)
  = helper g s (dsymbol d)
  where
    helper :: (Pointed g) => g -> [Simplex g] -> [Int] -> [Simplex g]
    helper g ss [] = ss
    helper g (s:ss) (x:xs) =
      let mindim = length ss + sum xs - length xs + 1 in
      let maxdim = mindim + x - 2 in
      map (constantAt (basepoint g))
        [maxdim,maxdim-1..mindim] ++
      degen' (DegenSymbol xs) s :
      helper g ss xs
    helper g [] [x] = map (constantAt (basepoint g)) [x-2,x-3..0]
    helper _ _ _ = error "Wbar unnormalise: impossible"

instance SGrp g => Pointed (Wbar g) where
  basepoint (Wbar g) = WbarSimplex []

instance (SGrp g) => ZeroReduced (Wbar g)

instance (SGrp g, ZeroReduced g) => OneReduced (Wbar g) -- Not a typo!

instance (SGrp g, ZeroReduced g, FiniteType g) => FiniteType (Wbar g) where
  geomBasis (Wbar g) n =
    filter (isGeomSimplex (Wbar g)) $ fmap WbarSimplex $ sequence $ allSimplices g <$> reverse [0 .. (n - 1)]

instance (SAb g) => SGrp (Wbar g) where
  -- TODO: can be more efficient, everywhere there is a degeneracy
  -- there is no need to actually compute the product.
  prodMor (Wbar g) = Morphism $ \(gs1, gs2) ->
    normalise g $
      (onSimplex (prodMor g) . prodNormalise)
        <$> zip (unnormalise g gs1) (unnormalise g gs2)

  invMor (Wbar g) = Morphism $ nonDegen . fmap (fmap (invMor g `onSimplex`))

instance (SAb g) => SAb (Wbar g)

-- instance (SGrp g) => Kan (Wbar g)

-- Kenzo implements this via DVF when `g` is a 0-reduced simplicial
-- abelian group. This should be enough to compute homotopy groups of
-- 1-reduced simplicial sets, as the K(G,n)s involved should all be of
-- that type.

-- Other simplicial groups will need the more complicated method
-- described in serre.lisp and cl-space-efhm.lisp

instance (SAb g, ZeroReduced g) => DVF (Wbar g) where
  vf (Wbar g) (WbarSimplex []) = Critical
  vf (Wbar g) (WbarSimplex (s : ss)) | nss <- normalise g ss =
    case vf (Product (Wbar g) g) (nss, s) of
      Source (ts', t') i -> Source (WbarSimplex (t' : unnormalise g ts')) (flipIncidence i)
      Target (ss', s') i -> Target (WbarSimplex (s' : unnormalise g ss')) (flipIncidence i)
      Critical -> case vf (Wbar g) (underlyingGeom nss) of
        Source nss' i -> Source (WbarSimplex (degen s 0 : unnormalise g (downshift (fmap (const nss') nss)))) (flipIncidence i)
        Target ntt' i -> Target (WbarSimplex (upshift s : unnormalise g (upshift (fmap (const ntt') nss)))) (flipIncidence i)
        Critical -> Critical

stripBar :: Pointed g => g -> GeomSimplex (Wbar g) -> [GeomSimplex g]
stripBar g (WbarSimplex as) = filter (/= basepoint g) $ fmap underlyingGeom as

reconstructBar :: SGrp g => g -> [GeomSimplex g] -> GeomSimplex (Wbar g)
reconstructBar _ [] = WbarSimplex []
reconstructBar g (a:as) = WbarSimplex (a':nb')
  where rest = reconstructBar g as
        (b', a') = reconstructProduct (Wbar g) g (rest, a)
        nb' = unnormalise g b'

criticalIso ::
  forall g.
  (Pointed g) =>
  g ->
  CC.Morphism
    (CriticalComplex (NChains (Wbar g)))
    (Bar (NChains g))
criticalIso g = CC.fmapBasis $ coerce @(GeomSimplex (Wbar g) -> _) (stripBar g)

criticalIsoInv ::
  (SGrp g) =>
  g ->
  CC.Morphism
    (Bar (NChains g))
    (CriticalComplex (NChains (Wbar g)))
criticalIsoInv g = CC.fmapBasis $ coerce $ reconstructBar g

wbarReduction ::
  (SAb g, ZeroReduced g) =>
  Wbar g ->
  Reduction
    (NChains (Wbar g))
    (Bar (NChains g))
wbarReduction p@(Wbar g) =
  isoToReduction (criticalIso g) (criticalIsoInv g)
    . dvfReduction (NChains p)

instance (SAb g, Effective g, ZeroReduced g, Algebra (Model g)) => Effective (Wbar g) where
  type Model (Wbar g) = Perturbed (TensorSusp (Model g))
  eff (Wbar g) = barEquiv (eff g) . fromRedLeft (NChains (Wbar g)) (Bar (NChains g)) (wbarReduction (Wbar g))

-- | Canonical twisting function \(\bar W G \rightsquigarrow G\),
-- corresponding to the fibre sequence \( G \hookrightarrow W G
-- \twoheadrightarrow \bar W G\). The total space \(W G\) is
-- contractible.

canonicalTwist :: (SGrp g) => g -> Twist (Wbar g) g
canonicalTwist g = Twist $ \(WbarSimplex s) -> case s of
  [] -> basepointSimplex g
  (s0:_) -> s0
