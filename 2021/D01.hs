{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- Day 1: Very overcomplicated Haskell
-- run with `stack runhaskell D01.hs`

module D01 where

import Data.Kind (Type)
import Data.Monoid (Sum (..))
import Text.Read (readMaybe)

data Nat = Zero | Suc Nat

type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Suc n) a

infixr 5 :>

singleton :: a -> Vec (Suc Zero) a
singleton = (:> Nil)

instance Functor (Vec n) where
  fmap _ Nil = Nil
  fmap f (x :> xs) = f x :> fmap f xs

instance Foldable (Vec n) where
  foldMap _ Nil = mempty
  foldMap f (x :> xs) = f x <> foldMap f xs

instance Show a => Show (Vec n a) where
  show Nil = "Nil"
  show (x :> xs) = show x <> " :> " <> show xs

type SNat :: Nat -> Type
data SNat n where
  SZero :: SNat Zero
  SSuc :: SNat n -> SNat (Suc n)

windows :: [a] -> SNat n -> [Vec n a]
windows [] _ = []
windows (x : xs) n = case go n (x : xs) of
  Just a -> a : windows xs n
  Nothing -> []
  where
    go :: SNat m -> [a] -> Maybe (Vec m a)
    go SZero _ = Just Nil
    go (SSuc m) [] = Nothing
    go (SSuc m) (x : xs) = (x :>) <$> go m xs

withSNat :: Int -> (forall n. SNat n -> a) -> a
withSNat 0 f = f SZero
withSNat n f = withSNat (n - 1) (f . SSuc)

data Diff = Increase | Decrease | NoDifference
  deriving (Show, Eq)

diff :: (Num a, Ord a) => [Vec n a] -> [Diff]
diff = go . fmap (foldMap Sum)
  where
    go xs = NoDifference : zipWith cmp xs (tail xs)
    cmp a b = case compare a b of
      GT -> Decrease
      LT -> Increase
      _ -> NoDifference

solve1 :: [Int] -> Int
solve1 = length . filter (== Increase) . diff . fmap singleton

solve2 :: [Int] -> Int
solve2 xs = length . filter (== Increase) $ withSNat 3 (diff . windows xs)

main :: IO ()
main = do
  input <- pure . traverse readMaybe . lines =<< readFile "D01.txt"
  putStrLn "Problem 1:"
  print $ solve1 <$> input
  putStrLn "Problem 2:"
  print $ solve2 <$> input
