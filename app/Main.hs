module Main where

data MyTuple a b where
    MkTuple :: a -> b -> MyTuple a b
    deriving (Show)

data MyMaybe a where
    MkJust :: a -> MyMaybe a
    MkNothing :: MyMaybe a
    deriving (Show)

data MyEither a b where
    MkLeft :: a -> MyEither a b
    MkRight :: b -> MyEither a b
    deriving (Show)

data MyList a where
    (:|) :: a -> MyList a -> MyList a
    Nil :: MyList a
    deriving (Show)

data MyBool where
    MkTrue :: MyBool
    MkFalse :: MyBool
    deriving (Show)

infixr :|

someList :: MyList Int
someList = 1 :| 2 :| 3 :| Nil 

myHead ::  MyList a -> a
-- myHead Nil = Nil
myHead (x :| xs) = x

(+++) :: MyList a -> MyList a -> MyList a
Nil +++ ys = ys
(x :| xs) +++ ys =  x :| (xs +++ ys)    


myReverse ::  MyList a -> MyList a
myReverse Nil = Nil
myReverse (x :| xs) =  myReverse xs +++ mySingleton x

mySingleton :: a -> MyList a
mySingleton a = a :| Nil


myTail ::  MyList a -> MyList a
myTail (x :| xs) = xs

myLast ::  MyList a -> a
myLast (x :| xs) = myHead $  myReverse xs

myNull :: MyList a -> MyBool
myNull Nil = MkTrue
myNull _ = MkFalse

-- myInit ::  MyList a -> MyList a
-- myInit (x :| xs) = 

myMap :: (a -> b) -> MyList a -> MyList b
myMap f Nil = Nil
myMap f ( x :| xs ) = f x :| myMap f xs

myFst :: MyTuple a b -> a
myFst (MkTuple x y) = x

mySnd :: MyTuple a b -> b
mySnd (MkTuple x y) = y

main :: IO ()
main = undefined
