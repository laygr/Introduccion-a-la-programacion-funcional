module Main where

main =
  do
    solution1
    solution2
    solution3

{-
1.-
  Make the type Month member of the type class Ord such that:
  January < February < March < April < May < June < July < August < September < October < November < December

  Hint:
    Create a mapping Month -> Int, "monthToInt".
    Create a mapping Int -> Month, "intToMonth".
    Remember that Int already is a member of Ord
-}

data Month = January | February | March | April | May | June | July | August | September | October | November | December

monthToInt m =
  case m of
  January   -> 1
  February  -> 2
  March     -> 3
  April     -> 4
  May       -> 5
  June      -> 6
  July      -> 7
  August    -> 8
  September -> 9
  October   -> 10
  November  -> 11
  December  -> 12

intToMonth m =
  case m of
  1  -> January
  2  -> February
  3  -> March
  4  -> April
  5  -> May
  6  -> June
  7  -> July
  8  -> August
  9  -> September
  10 -> October
  11 -> November
  12 -> December

instance Eq Month where
  m1 == m2 = (monthToInt m1) == (monthToInt m2)
  m1 /= m2 = (monthToInt m1) /= (monthToInt m2)

instance Ord Month where
  compare m1 m2 = compare (monthToInt m1) (monthToInt m2)
  m1 < m2 = (monthToInt m1) < (monthToInt m2)
  m1 <= m2 = (monthToInt m1) < (monthToInt m2)
  m1 > m2 = (monthToInt m1) > (monthToInt m2)
  m1 >= m2 = (monthToInt m1) >= (monthToInt m2)
  max m1 m2 = intToMonth (max (monthToInt m1) (monthToInt m2))
  min m1 m2 = intToMonth (min (monthToInt m1) (monthToInt m2))

solution1 =
  do
    putStrLn "Does September comes before November?"
    putStrLn (show (September < November))
{-
2.-
  About the merge sort algorithm:
    Merge sort is an algorithm for sorting through the technique of divide and conquer.

      The "divide" part is done by the function "split".
        "split" receives a list of things and splits it in two lists, half the size each.
        split :: [a] -> ([a], [a])

      The "conquer" part is done by the function "merge".
        "merge" receives two ordered lists and merges them into one ordered list.
        merge :: [a] -> [a] -> [a]

    (The time complexity is O(n) = nlog(n): There ocurr log(n) splits and merges, each split/merge taking at most n units of time)

  Assignment:
    Write the function "mergesort" that implements the merge sort algorithm; the functions "split" and "merge" are already given to you.

    Hint:
    "pred" is used for pattern matching with any function (provided by the user of "mergesort") of type (a -> a -> Bool).
      Given two values of type "a", "pred" returns "True" if the first value should come first than the second
      or "False" otherwise in the sorted list.

    solution1 (given) will showcase the use of mergesort by
      sorting a list of ints ([Int]) in ascending order and
      sorting a list of chars (String or [Char]) in descending order.
-}

solution2 =
  do
    putStr "[1,4,5,2,10] in ascending order: "
    putStrLn (show aListInAscendingOrder)
    putStr "Hello, World! in descending order: "
    putStrLn helloWorldInDescendingOrder
    where
      aList      = [1,4,5,2,10]
      helloWorld = "Hello, World!"
      aListInAscendingOrder       = mergesort (<=) aList
      helloWorldInDescendingOrder = mergesort (>=) helloWorld

      mergesort :: (a -> a -> Bool) -> [a] -> [a]
      mergesort pred []  = []
      mergesort pred [x] = [x]
      mergesort pred xs  = merge pred firstHalfSorted secondHalfSorted
        where
          (firstHalf, secondHalf) = split xs
          firstHalfSorted  = mergesort pred firstHalf
          secondHalfSorted = mergesort pred secondHalf


    -- split and merge are given:

      -- This implementation of "split" uses a clever technique for spliting a list in one past.
      -- It basically assigns the first half of the list while skipping elements by 2 in a copy of the list.
      --   When the skipping reaches the end of the list, the remaining list is assigned as the second half.
      split :: [a] -> ([a], [a])
      split xs = go xs xs
        where
          go (x:xs) (_:_:zs) = (x:us,vs) where (us,vs) = go xs zs
          go    xs   _       = ([],xs)

      merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
      merge pred xs [] = xs
      merge pred [] ys = ys
      merge pred (x:xs) (y:ys)
        | pred x y  = x : merge pred xs (y:ys)
        | otherwise = y : merge pred (x:xs) ys

{-
3.- Rewrite "mergesort" but using the type class "Ord".
      The new types for "mergesort", "split" and "merge" are given;
      you will need to modify "mergesort", "split" and "merge" to work using their new type.
    This new version of "mergesort" will actually result in more code, but perhaps code more easy to understand.
    It is a matter of taste.
-}
data Sorting = Ascending | Descending
solution3 =
  do
    putStr "[1,4,5,2,10] in ascending order: "
    putStrLn (show aListInAscendingOrder)
    putStr "Hello, World! in descending order: "
    putStrLn helloWorldInDescendingOrder
    where
      aList = [1,4,5,2,10]
      helloWorld = "Hello, World!"
      aListInAscendingOrder       = mergesort Ascending aList
      helloWorldInDescendingOrder = mergesort Descending helloWorld

      mergesort :: (Ord a) => Sorting -> [a] -> [a]
      mergesort _ []  = []
      mergesort _ [x] = [x]
      mergesort sorting xs  = merge sorting firstHalfSorted secondHalfSorted
        where
          (firstHalf, secondHalf) = split xs
          firstHalfSorted  = mergesort sorting firstHalf
          secondHalfSorted = mergesort sorting secondHalf

      split :: [a] -> ([a], [a])         -- The type could have been (Ord a) => [a] -> ([a], [a]), but since we
      split xs = go xs xs                -- are not using any property of Ord in split, it makes no difference.
        where
          go (x:xs) (_:_:zs) = (x:us,vs) where (us,vs) = go xs zs
          go    xs   _       = ([],xs)

      merge :: (Ord a) => Sorting -> [a] -> [a] -> [a]
      merge _ xs [] = xs
      merge _ [] ys = ys
      merge Ascending (x:xs) (y:ys)
        | x <= y    = x : merge Ascending xs (y:ys)
        | otherwise = y : merge Ascending (x:xs) ys
      merge Descending (x:xs) (y:ys)
        | x <= y    = y : merge Descending (x:xs) ys
        | otherwise = x : merge Descending (xs) (y:ys)