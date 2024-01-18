module Math.Algebra.Combination where

import Control.Category.Constrained (join, return)
import qualified Control.Category.Constrained as Constrained
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Unsafe.Coerce
import Data.Map.Merge.Lazy
import Data.Maybe (fromMaybe)
import Prelude hiding (id, return)

-- | Z-linear combinations
newtype Combination b = Combination {coeffs :: Map b Int}
  deriving (Eq)

-- | This is used in the case that two types are coercible
-- except for the type role declaration on `Map`, appearing
-- in the `Combination` type. To use this, ensure that the
-- coercion is monotonic.
coerceCombination :: a -> b
coerceCombination = unsafeCoerce  -- TODO can we type-safeguard this?

-- Collection of stuff that doesn't require the order instance
zeroComb :: Combination b
zeroComb = Combination $ Map.empty

singleComb :: b -> Combination b
singleComb a = Combination $ Map.singleton a 1

singleComb' :: Int -> b -> Combination b
singleComb' c b = Combination $ Map.singleton b c

negComb :: Combination b -> Combination b
negComb (Combination a) = Combination $ Map.map negate a

support :: Combination b -> [b]
support = Map.keys . coeffs

(.*) :: Int -> Combination b -> Combination b
0 .* (Combination bs) = zeroComb
n .* (Combination bs) = Combination $ Map.map (n *) bs

-- This is dangerous and requires `f` to be monotonic
instance Functor Combination where
  fmap f (Combination m) = Combination $ Map.mapKeysMonotonic f m

-- TODO: obviously make this a hashmap, possibly with special cases
-- for very small combinations? Unless hashmap alreayd does this.

instance Show b => Show (Combination b) where
  show (Combination c) = helper (Map.toList c)
    where
      showAddTerm :: (b, Int) -> String
      showAddTerm (b, 0) = error "showTerm: 0 coefficient"
      showAddTerm (b, 1) = " + " ++ show b
      showAddTerm (b, -1) = " - " ++ show b
      showAddTerm (b, c)
        | c < 0 = " - " ++ show (-c) ++ "·" ++ show b
        | otherwise = " + " ++ show c ++ "·" ++ show b

      showSoloTerm :: (b, Int) -> String
      showSoloTerm (b, 0) = error "showSoloTerm: 0 coefficient"
      showSoloTerm (b, 1) = show b
      showSoloTerm (b, -1) = "-" ++ show b
      showSoloTerm (b, c) = show c ++ "·" ++ show b

      showTail :: [(b, Int)] -> String
      showTail = foldMap showAddTerm

      helper [] = "0"
      helper (t : cs) = showSoloTerm t ++ showTail cs

coeffOf :: (Ord b) => Combination b -> b -> Int
coeffOf (Combination l) b = fromMaybe 0 $ Map.lookup b l

-- | Prunes away the zeroes
normalise :: (Ord b) => Combination b -> Combination b
normalise (Combination m) = Combination $ Map.filter (/= 0) m

instance Constrained.Functor (Constrained.Sub Ord (->)) (->) Combination where
  fmap (Constrained.Sub f) (Combination cs) = normalise $ Combination $
    Map.mapKeysWith (+) f cs

instance Constrained.Functor (Constrained.Sub Ord (->)) (Constrained.Sub Ord (->)) Combination where
  fmap f = Constrained.Sub $ Constrained.fmap f

instance Constrained.Monad (Constrained.Sub Ord (->)) Combination where
  return = Constrained.Sub $ singleComb
  join = Constrained.Sub $ \(Combination cs) ->
    Map.foldlWithKey (\ cum el coe -> cum + coe .* el) 0 cs

liftA2Comb :: (Ord c) => (a -> b -> c) -> Combination a -> Combination b -> Combination c
liftA2Comb f (Combination as) (Combination bs) = normalise $ Combination $ -- got to be a better way
  Map.fromListWith (+) [(f a b, ca * cb) | (a, ca) <- Map.toList as, (b, cb) <- Map.toList bs]

-- TODO: generalise via Constrained.Traversable
traverseComb :: (Ord b) => (a -> Combination b) -> [a] -> Combination [b]
traverseComb f [] = 0
traverseComb f (a:as) = liftA2Comb (:) (f a) (traverseComb f as)

instance Ord b => Num (Combination b) where
  fromInteger 0 = zeroComb
  fromInteger _ = error "Combination: fromInteger"

  Combination cs + Combination cs' = Combination $
    merge
      preserveMissing
      preserveMissing
      (zipWithMaybeMatched $ \ _ x y ->
        let r = x + y in if r == 0 then Nothing else Just r) cs cs'
  negate = negComb

  (*) = error "Combination: (*)"
  abs = error "Combination: abs"
  signum = error "Combination: signum"
