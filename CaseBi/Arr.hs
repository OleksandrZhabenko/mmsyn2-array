{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module    :  CaseBi.Arr
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- A library that can be used as a @case ... of@ constuction analogue for the multiple @Ord a => a -> b@
-- transformations and data representation. Uses 'Array' internally. If you use the module in GHCi, then, please,
-- run the interpreter with the flag @-fobject-code@.
--

module CaseBi.Arr (
{- |  * Functions that can be used instead of
@ case var of { a1 -> b1 ; a2 -> b2 ; a3 -> b3 ; ... ; an -> bn ; ~z -> def } @
for efficiency or other data representation
-}
  -- * Basic functions
  getBFst''
  , getBFst'
  -- * With a transformation to the array
  , listArrSortedByFst
  , getBFstLSorted'
  , getBFstL'
) where

import qualified Data.List as L (sortBy)
import GHC.Arr
import Data.Ord (comparing)

{- | The function that can be used instead of the 'case ... of' function
@
case var of { a1 -> b1 ; a2 -> b2 ; a3 -> b3 ; ... ; an -> bn ; ~z -> defaultValue }
@
If we follow a lot of teaching materials that explain the workflow of the construction
we think that the complexity of it is about /O(n)/ for the transformation of @a@ to @b@ here.
David Feuer (david.feuer (at) gmail.com) said that 'case ... of' is already optimized in GHC.
Some benchmarks show that its behaviour  tends to be about of /O(log n)/ complexity, the same as
the proposed function 'getBFst''. Nevertheless, the last one shows better performance in some situations,
is rather general and can be used for another data representation. Therefore, it can be preferred in
some situations. 'getBFst'' uses binary search algorithm and an 'Array' of @(a, b)@ as somewhat like a complicated filter or like a special sieve. The array must be sorted in ascending order here for the algorithm to be used correctly. For this you can use the function 'listArrSortedByFst' or the similar ones. If you would like to
use them both, please, consider usage of the 'getBFstLSorted'' or 'getBFstL'' instead.
The function 'getBFst''' is used internally in the 'getBFst'' that is recommended to be used in most cases. Nevertheless,
it is provided here if you have precomputed the first two arguments or at least some parts of them so that
you can reduce the needed amount of computations in the 'getBFst''.
-}
getBFst''
  :: Ord a => (# Int, (a, b) #) -- ^ The first unboxed tuple of the index and the element of the array.
  -> (# Int, (a, b) #) -- ^ The second unboxed tuple of the index and the element of the array.
  -> Array i (a, b) -- ^ The array of the pairs of the compared value and the result that is used in case the last argument is equal to the compared value.
  -> b -- ^ The default value that is used if no first element in the array tuples equals to the compared value.
  -> a -- ^ The compared value, well, the @main@ function argument, to which it is applied.
  -> b -- ^ The resulting branch value.
getBFst'' (# i, k #) (# j, m #) arr def x
 | if x < fst k then True else x > fst m = def
 | otherwise = gBF3 (# i, k #) (# j, m #) arr def x
{-# INLINE getBFst'' #-}

-- | The meaning of the arguments is the same as for 'getBFst'''. Is used internally in it.
gBF3 :: Ord a => (# Int, (a, b) #) -> (# Int, (a, b) #) -> Array i (a, b) -> b -> a -> b
gBF3 (# i, k #) (# j, m #) arr def x
 | j - i > 1 = let !n = (i + j) `quot` 2 in let !p = unsafeAt arr n in
    case compare x (fst p) of
     GT -> gBF3 (# n, p #) (# j, m #) arr def x
     EQ -> snd p
     _  -> gBF3 (# i, k #) (# n, p #) arr def x
 | x == fst m = snd m
 | x == fst k = snd k
 | otherwise = def
{-# INLINABLE gBF3 #-}

-- | A generally written without extending variant of the 'getBFst'''.
getBFst' :: Ord a => (b, Array Int (a, b)) -> a -> b
getBFst' (def, arr) = getBFst'' (# i, k #) (# j, m #) arr def
  where (!i,!j) = bounds arr
        !k = unsafeAt arr i
        !m = unsafeAt arr j
{-# INLINE getBFst' #-}

{- | If the list argument is sorted in the ascending order by the first element in every tuple, then to reduce
computations instead of
@
\\def xs x -> getBFst' (def, listArray (0,length xs - 1) xs) x
@
you can use this function.
-}
getBFstLSorted'
  :: Ord a => b
  -> [(a, b)]
  -> a
  -> b
getBFstLSorted' def xs = getBFst'' (# 0, k #) (# l, m #) arr def
  where !l = length xs - 1
        !arr = listArray (0,l) xs
        !k = unsafeAt arr 0
        !m = unsafeAt arr l
{-# INLINE getBFstLSorted' #-}

{- | If it is unknown whether the list argument is sorted in the ascending order by the first element in every
tuple (or, definitely, it is not, speaking generally), then instead of
@ \\def xs x -> getBFst' (def, listArray (0,length xs - 1) . sortBy (comparing fst) $ xs) x @
you can use this function.
-}
getBFstL'
  :: Ord a => b
  -> [(a, b)]
  -> a
  -> b
getBFstL' def xs = getBFst'' (# 0, k #) (# l, m #) arr def
  where !l = length xs - 1
        !arr = listArray (0,l) . L.sortBy (comparing fst) $ xs
        !k = unsafeAt arr 0
        !m = unsafeAt arr l
{-# INLINE getBFstL' #-}

{- | Sorts the list of pairs by the first element in the tuples, then transforms them into an immutable array. Can be
used only if the list contains no more than 2^31 - 1 elements though this is not checked, it is up to user
to check this constraint before or provide its correctness by design.
-}
listArrSortedByFst :: Ord a => [(a,b)] -> Array Int (a,b)
listArrSortedByFst xs = listArray (0,l - 1) ys
  where !ys = L.sortBy (comparing fst) xs
        !l = length ys
{-# INLINE listArrSortedByFst #-}
