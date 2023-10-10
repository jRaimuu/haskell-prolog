module Asgn2 where

{-
    1. a) 
    Base cases:
        y is empty and x is _, then return x
        y is _ and x is empty, then return y
    
    This will account for lists that are larger than others

    Inductive step:
        Otherwise, concat thr head of list y with the head of list x, and also concat that
        with recursive call of x's tail and y's tail
-}
riffle :: [a] -> [a] -> [a]
riffle [] x = x
riffle y [] = y
riffle (x:xs) (y:ys) = y : x : riffle xs ys

{-
    1. b)
    Base cases:
        If the param is an empty list, then we want to return a list
        containing the empty list (subset of empty list is the empty list)

    Inductive step:
        For a non-empty list, recurce on the tail of the list and concat it
        with the head and the power set of the tail included in each subset

-}
powerSet :: [a] -> [[a]]
powerSet []     = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)



data Nat = Z | S Nat deriving(Show)

{-
    2. a)
    Base cases:
        If both params are zero, then True is returned, indicating both equal numbers

        If both params are different values from each other, then False is returned,
        indicating they are not equal

    Inductive step:
        If we pattern match a successor wrapping its predecessor (x and y), 
        then we recurse on the predecessor x and y. This effectively reduces 
        our function by one
-}
natEq :: Nat -> Nat -> Bool
natEq Z Z = True
natEq (S x) (S y) = natEq x y
natEq _ _ = False 


{-  
    2. b)
    Base cases:
        If both params are Zero then return Zero
    
        If x param contains some Nat and y param is zero, then return x param
        If y param is zero and x param is some Nat, then return y param
    
    Inductive step:
        If x >= 1 || y >= 1 then recurse on predecessors of (S x) and (S y).
        In the case of x = 1 and y = 1, the predecessors are 0 and 0
        therefore when it returns we get S ( S (Z)) which is equal to 2

-}
addNat :: Nat -> Nat -> Nat
addNat Z Z     = Z     -- 0 + 0 = 0
addNat (S x) Z = (S x) -- identity case: x + 0 = x
addNat Z (S y) = (S y) -- identity case: 0 + y = y
addNat (S x) (S y) = S (S (addNat x y)) 


{-
    2. c)
    Base cases:
        If the param is Z (i.e. 0) then True b/c even
        If the param is (S Z) (i.e. 1) then False b/c odd

    Inductive step:
        If the param is > 1, then call recursively on reduceHelper, which takes
        the predecessor as input and returns the predecessor of the predecessor 
        (effectively reducing the function by 2). This will continue until base
        case is met
-}
reduceHelper :: Nat -> Nat
reduceHelper (S x) = x

isNatEven :: Nat -> Bool
isNatEven Z = True
isNatEven (S Z) = False
isNatEven (S x) = isNatEven(reduceHelper x)



{-
    2. d)
    # NatToInt
    Base cases:
        If the param is Z, then output 0
        If param is any successor pattern, then call helper function with successor
        and starting counter
        
        ## counterHelper
        If counterHelper first param eventually reduces to Z, then return total

    Inductive step:
        If param is some successor pattern and some total, then recurse  on the
        predecessor x (effectively reducing the function) along with the newly 
        incremented total
    ---
    # IntToNat
    Base cases:
        If first param is 0, then return Z data type

    Inductive step:
        If any other pattern, then recurse  on one less than the input number
        wrapped in the successor data type
-}

countHelper :: Nat -> Int -> Int
countHelper Z total = total
countHelper (S x) total = countHelper x (total + 1)

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S x) = countHelper (S x) 0


intToNat :: Int -> Nat
intToNat 0 = Z
intToNat x = S(intToNat(x-1)) 




{-
    3. 
        a) Filters a list based on if each element is positive, then applies
            foldl1 with addition operator on each (negative) element in the filtered list

        b) For each element in the list, the function applied in the foldl1
            checks if one element is greater than the next. Eventually returning
            the maximum element.

-}
--Sum of only negative numbers in a list
negativeSum :: [Integer] -> Integer --use foldl or foldr
negativeSum xs = foldl1 (+) (filter (\n -> n < 0) xs)


--Maximum element in list
myMaximum :: [Integer] -> Integer
myMaximum xs = foldl1 (\ n m -> if n > m then n else m) xs




{-
    4. Implomenting typeclass called Shape and providing instances
        for Circle and Rectangle Types. The shape typecalss
        defines two functions, area and perimeter.
-}

--Define the Shape TYPECLASS
class Shape a where
    area :: a -> Double
    perimeter :: a -> Double

--Define the Circle TYPE
data Circle = Circle Double

--Implement the Shape typeclass for Circle
instance Shape Circle where
    -- Calc the area of a Circle
    area (Circle r) = pi * r * r
    -- Calc the perimeter of a Circle
    perimeter (Circle r) = 2 * pi * r


--Define the Rectangle TYPE
data Rectangle = Rectangle Double Double

--Implement the Shape typeclass for Rectanglez
instance Shape Rectangle where
    -- Calc the area of a Rectangle
    area (Rectangle w h) = w * h
    -- Calc the perimeter of a Rectangle
    perimeter (Rectangle w h) = 2 * (w + h)



{-
    5.
-}
data GTree a = Leaf a | Gnode [GTree a]


{-
    a)
    Base cases:
        If param is a Leaf with any value, the return 1 (indicating that
        this component of the Gtree is a leaf)

        If param is an empty Gnode, then return 0 (indicating this should
        not be added to the counter of number of leaves)
    
    Inductive step:
        If Gnode contains a subtree then take the sum of mapping the
        recursive call on the subtree. Put more succinctly, we take the
        sum of all leaves (which carry a 1) over the list of GTrees.
        Making the recursive call with the subtree effectively reduces 
        our function by one level. Map helps us apply the recursive function 
        to every element of the subtree (list of Gtrees). 

-}
numberOfLeaves :: GTree a -> Int
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Gnode []) = 0
numberOfLeaves (Gnode subtree) = sum (map numberOfLeaves subtree)

-- Gnode[Leaf 10, Gnode[...], Leaf 14]
-- m = [Leaf 10, Gnode[...], Leaf 14] -- list of GTrees but we want a single Gtree

{-
    b)
    Base cases:
        If param is a Leaf node with some value, then return 0
        If para is an empty Gnode, then also return 0

    Inductive step:
        If param is Gnode with a subtree, then we want to take the
        maximum of the depth among all subtrees. Then, add 1 to account 
        for the current level of the Gnode itself. Recursing on the subtree 
        effectively reduces the function by one level. 
-}
depth :: GTree a -> Int
depth (Leaf _) = 0
depth (Gnode[]) = 0
depth (Gnode subtree) = maximum (map depth subtree)+1



{-
    c)
    Base cases:
        If second param is a Leaf, then compare the node value to the
        target value. If they are equivalent, return true. Otherwise,
        return false.

        If second param is an empty Gnode, then retun false (can't compare target
        to a component that contains no value)
    
    Inductive step:
        If the second param is a Gnode with a subtree, we want to:
            - Apply findVal with target value to the subtree m. This will
                effectively return a list of booleans -- False and possibly True
                depending on in the target value is contained within the nodes
            - Take the foldl, applying OR (lambda expression) over the values of 
                the list
        If the target find a matching leaf node (meaning the list contains True), 
        then the OR expression will return True
    
-}
findVal :: Ord a => a -> GTree a -> Bool
findVal target (Leaf nodeVal)    =  nodeVal == target
findVal target (Gnode [])        =  False
findVal target (Gnode m)         =  foldl (\ acc x -> acc || x) False (map (findVal target) m)
