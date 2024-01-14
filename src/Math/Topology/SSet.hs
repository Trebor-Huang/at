module Math.Topology.SSet where

import qualified Control.Category.Constrained as Constrained
import Control.Monad (ap)
import Data.List
import Prelude hiding (Bounded)

-- A degeneracy symbol is a list of positive integers.
-- [3,4,2] means the map {0,...,8} -> {0,1,2} mapping the first three numbers
-- to 0, the next four to 1, and the last two to 2.
-- Trailing 1s are pruned, this is an invariant.
-- Note that we don't store the dom/cod dimensions, only the difference!
newtype DegenSymbol = DegenSymbol { dsymbol :: [Int] } deriving (Eq)

instance Show DegenSymbol where
  show (DegenSymbol d) = show d

primDegen :: Int -> DegenSymbol
primDegen n = DegenSymbol $ replicate n 1 ++ [2] -- TODO more efficient

-- They form a ValueCategory, but let's make less fuss
instance Semigroup DegenSymbol where
  -- p : [m] -> [n], q : [n] -> [k]
  DegenSymbol [] <> q = q
  p <> DegenSymbol [] = p
  p <> DegenSymbol (x:xs) =
    let (p1, p2) = splitAt x (dsymbol p) in
      DegenSymbol (sum p1: p2 <> xs)

instance Monoid DegenSymbol where
  mempty = DegenSymbol []

-- Dually, a face symbol is a list of natural numbers.
-- [3,4,0] means a map {0,...,9} -> {0,...,7}, by discarding
-- 3, 8 and 9, moving the others forward.

newtype FaceSymbol = FaceSymbol { fsymbol :: [Int] } deriving (Eq)

instance Show FaceSymbol where
  show (FaceSymbol d) = show d

-- TODO instance monoid facesymbol

primFace :: Int -> FaceSymbol
primFace n = FaceSymbol [n]

-- exchanges --f-> --d-> to --d'-> --f'->
(#) :: FaceSymbol -> DegenSymbol -> (DegenSymbol, FaceSymbol)
FaceSymbol [] # d = (d, FaceSymbol [])
f # DegenSymbol [] = (DegenSymbol [], f)
FaceSymbol (i:f) # DegenSymbol (j:d)
  | i >= j =
    let (DegenSymbol d', FaceSymbol f') = FaceSymbol (i-j:f) # DegenSymbol d in
    case f' of
      []      -> (DegenSymbol (j:d'), FaceSymbol [])
      (k:f'') -> (DegenSymbol (j:d'), FaceSymbol (k+1:f''))
  | otherwise =
    let d_reduced = if i == j+1 then d else (j-i-1:d) in
    let u@(DegenSymbol d', FaceSymbol f') = FaceSymbol f # DegenSymbol d_reduced in
    if i == 0 then u else
    case d' of
      []      -> (DegenSymbol (if i == 1 then [] else [i]), FaceSymbol f')
      (k:d'') -> case f' of
        (l:_) | l > 0 -> (DegenSymbol (i:k:d''), FaceSymbol f')
        _ -> (DegenSymbol (k+i:d''), FaceSymbol f')

data FormalDegen a  -- writer monad
  = FormalDegen {
    underlyingGeom :: a,  -- Dimension n
    degenSymbol :: DegenSymbol -- [m] -> [n]
  }
  deriving (Eq, Functor)
  deriving (Constrained.Functor (->) (->)) via (Constrained.Wrapped FormalDegen)

instance Show a => Show (FormalDegen a) where
  show b@(FormalDegen a d) =
    if isDegen b then
      show a
    else
      "s_" ++ show d ++ " " ++ show a

instance Applicative FormalDegen where
  pure a = FormalDegen a mempty
  (<*>) = ap

instance Monad FormalDegen where
  (FormalDegen a d) >>= f =
    let FormalDegen b d' = f a in
      FormalDegen b (d <> d')

-- These functions don't depend on a at all.
-- TODO separate
isDegen :: FormalDegen a -> Bool
isDegen (FormalDegen _ (DegenSymbol [])) = True
isDegen _ = False

degen :: FormalDegen a -> Int -> FormalDegen a
degen (FormalDegen a d) i = FormalDegen a (primDegen i <> d)

nonDegen :: a -> FormalDegen a
nonDegen a = FormalDegen a mempty

degenList :: FormalDegen a -> [Int]
degenList = helper 0 . dsymbol . degenSymbol
  where
    helper :: Int -> [Int] -> [Int]
    helper _ [] = []
    helper n (x:xs) = [n..n+x-1] ++ helper (n+x) xs

degenCount :: DegenSymbol -> Int
degenCount (DegenSymbol d) = sum d - length d

-- In this representation, we just need to check that the index is
-- somewhere in the list. (Not necessarily the first thing)
isImageOfDegen :: FormalDegen a -> Int -> Bool
isImageOfDegen (FormalDegen a (DegenSymbol d)) = helper d
  where
    helper :: [Int] -> Int -> Bool
    helper [] _ = False
    helper (x:xs) n | n  <  x-1 = True
                    | n  == x-1 = False
                    | otherwise = helper xs (n-x)

constantAt :: a -> Int -> FormalDegen a
constantAt a n =
  if n == 0 then
    FormalDegen a (DegenSymbol [])
  else
    FormalDegen a (DegenSymbol [n])

-- `allDegens m n` lists all the degeneracy symbols [m] -> [n]
allDegens :: Int -> Int -> [DegenSymbol]
allDegens m n
  | m <  n = []
  | m == n = [mempty] -- shortcut
  | otherwise = do
  k <- [1..m+1]
  DegenSymbol d <- allDegens (m-k) (n-1)
  return $ DegenSymbol (k:d)


-- The following are dangerous and only make sense in certain situations.
downshiftN :: Int -> FormalDegen a -> FormalDegen a  -- seems to assume n >= 0
downshiftN n (FormalDegen a (DegenSymbol d)) = FormalDegen a (DegenSymbol $ helper n d)
  where
    helper :: Int -> [Int] -> [Int]
    helper 0 xs = xs
    helper n [] = []
    helper n xs =
      if last xs == 2 then
        init xs ++ replicate n 1 ++ [2]
      else
        init xs ++ [last xs - 1] ++ replicate (n-1) 1 ++ [2]

downshift :: FormalDegen a -> FormalDegen a
downshift = downshiftN 1

-- Composition with a face map, represented as a decreasing sequence of
-- generating face maps
unDegen :: FormalDegen a -> [Int] -> FormalDegen a
unDegen (FormalDegen a d) f = FormalDegen a (DegenSymbol (helper (differ f) d))
  where
    helper f d =
      let (d', f') = f # d in
      if f' /= FaceSymbol [] then
        error "IMPOSSIBLE: unDegen"
      else
        dsymbol d'

    differ f = let f' = reverse f in
      FaceSymbol $ zipWith (\ a b -> a - b - 1) f' (0:f')

type Simplex a = FormalDegen (GeomSimplex a)

class Eq (GeomSimplex a) => SSet a where
  -- NOTE: Maybe this shouldn't be an associated type, instead just
  -- another parameter to the typeclass

  -- NOTE: Or we could even reverse things, so that GeomSimplex is the
  -- class and SSet is the associated type.
  type GeomSimplex a = s | s -> a

  -- In a language with dependent types, this could be folded into the
  -- GeomSimplex type.
  isGeomSimplex :: a -> GeomSimplex a -> Bool
  isGeomSimplex _ _ = True

  geomSimplexDim :: a -> GeomSimplex a -> Int

  geomFace :: a -> GeomSimplex a -> Int -> Simplex a

  -- geomSimplexDim a s = length (geomFaces a s)
  geomFaces :: a -> GeomSimplex a -> [Simplex a]
  geomFaces a s =
    let d = geomSimplexDim a s
     in if d == 0 then [] else fmap (geomFace a s) [0 .. d]

  -- TODO: for efficiency?
  -- nonDegenFaces :: a -> GeomSimplex a -> [(Int, Simplex a)]

isSimplex :: SSet a => a -> Simplex a -> Bool
isSimplex a (FormalDegen g d)
  = isGeomSimplex a g && length (dsymbol d) <= geomSimplexDim a g + 1

simplexDim :: SSet a => a -> Simplex a -> Int
simplexDim a (FormalDegen g d) = geomSimplexDim a g + degenCount d

-- Apply a face operator
geomFace' :: SSet a => FaceSymbol -> a -> GeomSimplex a -> Simplex a
geomFace' (FaceSymbol []) _ g = nonDegen g
geomFace' (FaceSymbol [x]) a g = geomFace a g x
geomFace' (FaceSymbol (x:xs@(y:ys))) a g =
  face' (FaceSymbol (x+y:ys)) a $ geomFace a g x

face' :: SSet a => FaceSymbol -> a -> Simplex a -> Simplex a
face' f a (FormalDegen g d) =
  let (d', f') = f # d in
  let FormalDegen g' d'' = geomFace' f' a g in
    FormalDegen g' (d' <> d'')

face :: SSet a => a -> Simplex a -> Int -> Simplex a
face a s i = face' (primFace i) a s

hasFace :: SSet a => a -> GeomSimplex a -> GeomSimplex a -> Bool
hasFace a t s = nonDegen s `elem` geomFaces a t

frontFace :: SSet a => a -> Simplex a -> Simplex a
frontFace a s = face a s 0

backFace :: SSet a => a -> Simplex a -> Simplex a
backFace a s = face a s (simplexDim a s)

class SSet a => FiniteType a where
  -- * `all isSimplex (geomBasis n)`
  geomBasis :: a -> Int -> [GeomSimplex a]

someSimplices :: (SSet a) => a -> Int -> (Int -> [GeomSimplex a]) -> [Simplex a]
someSimplices a n f | n < 0 = []
someSimplices a n f = [FormalDegen g d | k <- [0 .. n], g <- f n, d <- allDegens n k]

allSimplices :: (FiniteType a) => a -> Int -> [Simplex a]
allSimplices a n = someSimplices a n (geomBasis a)

class SSet a => Bounded a where
  amplitude :: a -> [Int]

class SSet a => Pointed a where
  basepoint :: a -> GeomSimplex a

basepointSimplex :: (Pointed a) => a -> Simplex a
basepointSimplex a = nonDegen (basepoint a)

-- TODO: move Pointed to its own file to import Morphism
-- basepointMor :: a -> Morphism () a

-- | SSet with a unique 0-simplex.
class Pointed a => ZeroReduced a

-- | SSets with no non-degenerate 1-simplices.
class ZeroReduced a => OneReduced a

-- | Simplicial morphisms
newtype UMorphism a b = Morphism {onGeomSimplex :: a -> FormalDegen b}

type Morphism a b = UMorphism (GeomSimplex a) (GeomSimplex b)

onSimplex :: UMorphism a b -> FormalDegen a -> FormalDegen b
onSimplex (Morphism f) (FormalDegen a d) =
  let FormalDegen b d' = f a in
    FormalDegen b (d <> d')

instance Constrained.Semigroupoid UMorphism where
  f2 . (Morphism f1) = Morphism $ \s -> f2 `onSimplex` f1 s

instance Constrained.Category UMorphism where
  id = Morphism $ \s -> nonDegen s

instance Constrained.Functor UMorphism (->) FormalDegen where
  fmap = onSimplex

-- Reid Barton:
-- https://categorytheory.zulipchat.com/#narrow/stream/241590-theory.3A-
-- algebraic.20topology.20.26.20homological.20algebra/topic/describing.
-- 20simplicial.20sets/near/260675092
--
-- There's a lot more interesting stuff to say about this situation.
--
-- If we want to understand the category of semisimplicial sets
-- relative to the category of simplicial sets via the left adjoint
-- you mentioned, we should answer three questions: 1) Which
-- simplicial sets lie in the image of this functor?  2) Which
-- morphisms lie in the image of this functor?  3) When do two
-- parallel morphisms of semisimplicial sets become equal when we
-- apply this functor?
--
-- I think, though I haven't carefully checked, that the answers are:
--
-- 1) The simplicial sets in which every face of a nondegenerate simplex is
--    nondegenerate.
-- 2) The morphisms which send nondegenerate simplices to nondegenerate
--    simplices.
-- 3) Only if the maps were already equal, i.e., the functor is faithful.
--
-- There's also a more efficient way to describe what this left
-- adjoint produces, related to the kerodon proposition that Daniel
-- linked to, and using the notion of a "degeneracy operation". A
-- degeneracy operation is an operation taking nn-simplices to
-- mm-simplices for some fixed nn and mm, for which the corresponding
-- map [m]→[n] of Δ is surjective. (So in particular, n≤m.) The
-- operations s_i are the generating degeneracy opaerations, and the
-- degeneracy options are all compositions of the s_is, but quotiented
-- by the simplicial relations involving the s_i.
--
-- The linked proposition says that every simplex of a simplicial set
-- can be expressed as a degeneracy operation applied to a
-- nondegenerate simplex in a unique way.
--
-- Now if we start with a semisimplicial set X, we can describe the
-- "free" simplicial set Y it generates as follows:
--
-- - The simplices of Y are formal applications of a degeneracy
--   operation to a simplex of X.
-- - The structure maps of X are computed as follows. Suppose we want
--   to compute the action of a simplicial operator ff on a formal
--   degeneracy sx. The combined operation fsfs corresponds to some
--   map of Δ which we can refactor as a surjection followed by an
--   injection. Then f(sx) is given by formally applying the
--   degeneracy operator corresponding to the surjection to the value
--   of the face operator corresponding to the injection on x
--   (computed in the semisimplicial set X).
--
-- A more syntactic way to describe the action in terms of the
-- generating face and degenerating operators is:
--
-- - If we want to apply s_i to a formal degeneracy sx, we just form (s_i s) x
-- - If we want to apply d_i to a formal degeneracy sx, then we use
--   the simplicial identities to rewrite d_i s as a composition s' d'
--   s "moving ds to the left". Since we started with a single d_i ,
--   what will happen is that either d_i will pass through all the ss
--   (possibly changing indices in the process) so that d = d_j or the
--   d_i ​ will cancel with some s, so that d = id.  Then we compute x'
--   = d' x in X and form s' x'.
--
-- There is also a way to specify an arbitrary simplicial set
-- in terms of only its nondegenerate simplices and its face maps, but
-- with the caveat that the face of a nondegenerate simplex can be a
-- formal degeneracy of another nondegenerate simplex. The full
-- simplicial structure is recovered by the same process as above
-- except that when we take the face of a nondegenerate simplex (in
-- what would have been X above), it may come as a formal degeneracy
-- to which we have to apply another degeneracy operator to--which is
-- no problem.
--
-- The other caveat is that because of the original question 2, in
-- order to recover the correct maps of simplicial sets, we also need
-- to allow a map to send a nondegenerate simplex to a formal
-- degeneracy in the target simplicial set.
--
-- The program Kenzo uses this representation of simplicial sets.
