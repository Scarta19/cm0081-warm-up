module Main where
import Test.QuickCheck


-- | Append two lists.

myAppend :: [a] -> [a] -> [a]
myAppend xs j = foldr (:) j xs


-- | Extract the last element of a list, which must be finite and
-- non-empty. The function generates an error if the list is empty.

myLast :: [a] -> a
myLast [] = error "Empty lists are not allowed"
myLast [x]  = x
myLast (x:xs) = myLast xs


-- | Test whether a list is empty.

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False


-- | Testing "myAppend function.

propMyAppend :: [Int] -> [Int] -> Bool
propMyAppend i j = i ++ j == myAppend i j


-- | Testing "myNull" function.

propMyNull :: [Int] -> Bool
propMyNull x = null x == myNull x

-- | Testing "myLast" function.

propMyLast :: NonEmptyList Int -> Bool
propMyLast xs = myLast x == last x where
    x :: [Int]
    x = getNonEmpty xs 

main :: IO ()

main = do
    
    quickCheck propMyAppend
    quickCheck propMyNull
    quickCheck propMyLast 
