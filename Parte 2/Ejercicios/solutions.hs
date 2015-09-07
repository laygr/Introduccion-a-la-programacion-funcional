-- Solutions to the second tutorial of "Introducción a la Programación Funcional"

main :: IO ()
main = do
    solution1
    solution2
    solution3

-------------------------------------------------------------------------------------------------------------
{-
1.- Model the months of the year using addition of data constructors in a type constructor named "Month".
    Then write a function "monthToStr" that given a month "m", it returns the name of "m" as a String.
    Then write a function "solution1" that prints the String of the month you were born.
-}

data Month = January | February | March | April | May | June | July | August | September | October | November | December

monthToStr m = case m of
    January   -> "January"
    February  -> "February"
    March     -> "March"
    April     -> "April"
    May       -> "May"
    June      -> "June"
    July      -> "July"
    August    -> "August"
    September -> "September"
    October   -> "October"
    November  -> "November"
    December  -> "December"

solution1 = putStrLn (monthToStr November)
-------------------------------------------------------------------------------------------------------------
{-
2.- Model a date using multiplication of data constructors in a type constructor named "Date".
      Use the type Int for modeling the year.
      Use the type Month you wrote in exercise 1 for modeling the month.
      Use the type Int for modeling the day.
    Then write a function "dateToStr" that given a date "d", it returns a String representing that date
        following this format: "dd/month/year"
    Then write a function "solution2" that prints the String of the date you were born.
-}

data Date = Date Int Month Int

dateToStr (Date year month day) = (show year) ++ "/" ++ (monthToStr month) ++ "/" ++ (show day)

solution2 = putStrLn (dateToStr (Date 1990 November 18))
-------------------------------------------------------------------------------------------------------------
{-
3.- Write a function printLBTree that prints all the values in the given LBTree.
      Use recursion.
    Write a function solution3 that prints the example labeled binary tree using printLBTree.
-}

data LBTree a = InternalNode a (LBTree a) (LBTree a) | Leaf (Maybe a)

example = InternalNode 2
            (InternalNode 7
              (Leaf (Just 2))
              (InternalNode 6
                (Leaf (Just 5))
                (Leaf (Just 11))))
            (InternalNode 5
              (Leaf Nothing)
              (InternalNode 9
                (Leaf (Just 4))
                (Leaf Nothing)))

printLBTree (InternalNode v left right) =
    do
        putStrLn (show v)
        printLBTree left
        printLBTree right
printLBTree (Leaf Nothing) = putStr ""
printLBTree (Leaf (Just v)) = putStrLn (show v)

solution3 = printLBTree example
-------------------------------------------------------------------------------------------------------------