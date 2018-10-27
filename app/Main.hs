module Main where

import Lib
import System.Random

main :: IO ()
main = someFunc

lastMy :: [a] -> Maybe a
lastMy x = case x of
    [] -> Nothing
    x:[] -> Just x
    _:xs -> lastMy xs

lastButOneMy :: [a] -> Maybe a
lastButOneMy x = ans x
    where ans (y:[_]) = Just y
          ans (_:xs) = lastButOneMy xs
          ans _ = Nothing

findK :: Int -> [a] -> a
findK _ [] = error "empty list"
findK 1 (x:_) = x
findk y (_:xs) = findk (y-1) xs 

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) `myOp` [x] 

myOp :: [a] -> [a] -> [a]
myOp [] ys = ys
myOp (x:xs) ys = x:(myOp xs ys)

myPalindrome :: (Eq a) => [a] -> Bool
myPalindrome xs = if xs == myReverse xs then True else False

data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten l = case(l) of
    Elem a ->  [a]
    List as -> listFlatten as 

listFlatten :: [NestedList a] -> [a]
listFlatten [] = []
listFlatten (x:xs) = myFlatten x ++ listFlatten xs

myConsecutiveDupes :: (Eq a) => [a] -> [a]
myConsecutiveDupes (x:y:as) 
    | x == y = myConsecutiveDupes (y:as)
    | otherwise = x:myConsecutiveDupes (y:as)
myConsecutiveDupes x = x

myPack :: (Eq a) => [a] -> [[a]]
myPack list = case list of
    (x:rest@(y:as)) -> if x==y then let (ys:res) = myPack rest in
                ((x:ys):res) else ([x]:myPack rest)
    otherwise -> [list]

myEncoding :: (Eq a) => [a] -> [(Int,a)]
myEncoding x = myEncodingHelp.myPack$x 

myEncodingHelp :: [[a]] -> [(Int,a)]
myEncodingHelp (a@(x:_):as) =  (length a,x):(myEncodingHelp as)
myEncodingHelp [] = []

data Workaround a = Multiple (Int,a) | Single a deriving Show

mySecondEncoding :: (Eq a) => [a] -> [Workaround a]
mySecondEncoding x = mySecondEncodingHelp.myPack$x   

mySecondEncodingHelp :: [[a]] -> [Workaround a]
mySecondEncodingHelp (a@(x:_):as) =  if length a > 1 
    then Multiple (length a,x):mySecondEncodingHelp as 
    else Single x: mySecondEncodingHelp as
mySecondEncodingHelp [] = []

myDecode :: [Workaround a] -> [a]
myDecode x = case x of
    [] -> []
    Single y:xs -> y:myDecode xs
    Multiple (l,y):xs -> myDuplicate l y ++ myDecode xs

myDuplicate :: Int -> a -> [a]
myDuplicate a b = if a == 0 then [] else b:myDuplicate (a-1) b

myDirectEncode :: (Eq a) => [a] -> [Workaround a]
myDirectEncode x = encodeHelper x []

encodeHelper :: (Eq a) => [a] -> [Workaround a] -> [Workaround a]
encodeHelper x acc = case x of
    [] -> acc
    (a:list) ->
        case acc of
        Multiple (d,e):rest -> if e==a then encodeHelper list (Multiple (d+1,e):rest)
            else encodeHelper list (Single a:acc)
        Single e:rest -> if e==a then encodeHelper list (Multiple (2,e):rest)
            else encodeHelper list (Single a:acc)
        [] -> encodeHelper list [Single a]
    
dupli :: [a] -> [a]
dupli [] = []
dupli (a:as) = a:a:(dupli as)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (a:as) d = myMult a d ++ repli as d

myMult :: a -> Int -> [a]
myMult a 0 = []
myMult a d = a:myMult a (d-1) 

myDrop :: [a] -> Int-> [a]
myDrop a d = myDrop' a d d 

myDrop' :: [a] -> Int -> Int -> [a]
myDrop' (x:a) d 1 =  myDrop' a d d
myDrop' (x:a) d c =  x:myDrop' a d (c-1)
myDrop' [] _ _ = []

mySplit :: [a] -> Int -> ([a],[a])
mySplit x 0 = ([],x)
mySplit (x:xs) d = let (a,b) = mySplit xs (d-1) in
    (x:a,b)
mySplit [] _ = ([],[])

mySlice :: [a] -> Int -> Int -> [a]
mySlice (x:xs) 1 1 = [x]
mySlice (x:xs) 1 d = x:mySlice xs 1 (d-1)
mySlice (_:xs) d e = mySlice xs (d-1) e

myRotate :: [a] -> Int -> [a]
myRotate x 0 = x
myRotate a@(x:xs) d = if d > 0 
    then myRotate (xs ++ [x]) (d-1)
    else let (x,y:_) = mySplit a (length a - 1) in
        myRotate (y:x) (d+1)

myRemove :: [a] -> Int -> (a,[a])
myRemove (x:xs) 1 = (x,xs)
myRemove (x:xs) d = let (y,ys) = myRemove xs (d-1) in
        (y,x:ys)

myInsert :: [a] -> a -> Int -> [a]
myInsert xs a 1 = a:xs
myInsert (x:xs) a d = x:myInsert xs a (d-1)  

myRange :: (Ord a,Eq a,Enum a) => a -> a -> [a]
myRange a b = if a == b then [a] else a: myRange (succ a) b

myRandom = random (mkStdGen 100) :: (Int, StdGen)

myRandomExtract :: [a] -> Int -> StdGen-> IO [a]
myRandomExtract x 0 _ = return []
myRandomExtract x d gen =  let (a,b) = randomR (1,length x) (gen) in
    do
        c <- myRandomExtract x (d-1) b 
        return ((x !! (a-1)):c)

myRndSelect :: [a] -> Int -> IO [a]
myRndSelect x d = do
    gen <- getStdGen
    myRandomExtract x d gen

