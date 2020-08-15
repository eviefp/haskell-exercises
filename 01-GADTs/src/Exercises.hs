-- Tom did this:
{-# LANGUAGE GADTs #-}
-- I did this:
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
-- :)
module Exercises where

import Data.Foldable (fold)
import Data.Monoid (Any(..), Sum)



{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountableNil :: CountableList
  CountableCons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList =
  \case
    CountableNil       -> 0
    CountableCons x xs -> count x + countList xs

-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero =
  \case
    CountableNil -> CountableNil
    CountableCons x xs ->
        if count x == 0
            then dropZero xs
            else CountableCons x (dropZero xs)


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "No we can't, because we can't pattern match on the type."




{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList
  -- ...

-- | b. How many of the following functions can we implement for an 'AnyList'?

concatAnyList :: AnyList -> AnyList -> AnyList
concatAnyList AnyNil ys = ys
concatAnyList (AnyCons x xs) ys = AnyCons x (concatAnyList xs ys)

reverseAnyList :: AnyList -> AnyList
reverseAnyList =
  \case
    AnyNil -> AnyNil
    AnyCons x xs -> reverseAnyList xs `concatAnyList` AnyCons x AnyNil

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = error "I don't think you can write this because the caller determines the 'a'. It needs to be (forall a. a -> Bool)"

filterPlus :: (forall a. a -> Bool) -> AnyList -> AnyList
filterPlus f =
  \case
    AnyNil -> AnyNil
    AnyCons x xs ->
      if f x
        then AnyCons x (filterPlus f xs)
        else filterPlus f xs

lengthAnyList :: AnyList -> Int
lengthAnyList =
  \case
    AnyNil -> 0
    AnyCons _ xs -> 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList _ = mempty

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList =
  \case
    AnyNil -> True
    AnyCons _ _ -> False

instance Show AnyList where
  show =
    \case
      AnyNil -> "[]"
      AnyCons _ xs -> "<some element here> :: " <> show xs



{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it?
-- A: 'input' is the existential variable and all we can do is pass it to the
-- wrapped function to obtain an 'output' -- which is NOT existsentially
-- quantified.

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq output => Eq (TransformableTo output) where
    TransformWith f x == TransformWith h y = f x == h y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where
    fmap f (TransformWith t x) = TransformWith (f . t) x

one :: output -> TransformableTo output
one = TransformWith id

extract :: TransformableTo output -> output
extract (TransformWith f x) = f x



{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?

isTrue :: EqPair -> Bool
isTrue (EqPair a b) = a == b

isFalse :: EqPair -> Bool
isFalse = not . isTrue

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair1 a where
  EqPair1 :: Eq a => a -> a -> EqPair1 a
-- ????????


-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

data EqPair2 a = Eq a => EqPair2 a a

isTrue2 :: EqPair2 a -> Bool
isTrue2 (EqPair2 a b) = a == b



{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox int _)) = int

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers =
  \case
    EmptyBox      -> 0
    IntBox _ _    -> 1
    StringBox _ _ -> 2
    BoolBox _ _   -> 3

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

class PeelLayer a b | a -> b, b -> a where
  peel :: MysteryBox a -> MysteryBox b

instance PeelLayer Bool [Char] where
  peel :: MysteryBox Bool -> MysteryBox String
  peel (BoolBox _ b) = b

instance PeelLayer String Int where
  peel :: MysteryBox String -> MysteryBox Int
  peel (StringBox _ b) = b

instance PeelLayer Int () where
  peel :: MysteryBox Int -> MysteryBox ()
  peel (IntBox _ b) = b

peelLayer :: PeelLayer a b => MysteryBox a -> MysteryBox b
peelLayer = peel

-- THIS IS SUPER COOL
-- You use GADT to encode a relationship between a and b
data Layer a b where -- We can use a GADT to encode the layers...
  Int'    :: Layer Int ()
  String' :: Layer String Int
  Bool'   :: Layer Bool String

-- And now we can write this function:
unpeel :: Layer a b -> MysteryBox a -> MysteryBox b
unpeel Int'    (IntBox    _ xs) = xs
unpeel String' (StringBox _ xs) = xs
unpeel Bool'   (BoolBox   _ xs) = xs


{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

head :: HList (a, rest) -> a
head =
  \case
    HCons head _ -> head

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

patternMatchMe :: HList (Int, String, Bool, ()) -> Int
patternMatchMe = error "I don't think this is possible."

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

class Append l r result | l r -> result where
  append :: HList l -> HList r -> HList result

instance Append () r r where
  append :: HList () -> HList r -> HList r
  append _ = id

instance
    Append lrest r result
    => Append (lhead, lrest) r (lhead, result) where
  append :: HList (lhead, lrest) -> HList r -> HList (lhead, result)
  append (HCons head rest) hlist = HCons head (append rest hlist)

-- Bonus: Show instance :D
class ShowHList l where
  showH :: HList l -> String

instance ShowHList () where
  showH _ = "[]"

instance (Show a, ShowHList b) => ShowHList (a, b) where
  showH (HCons x xs) = show x <> " :: " <> showH xs

appendHList :: Append a b r => HList a -> HList b -> HList r
appendHList = append




{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty  :: HTree Empty
  HBranch :: HTree a -> b -> HTree c -> HTree (Branch a b c)
  -- ...

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (Branch a b c) -> HTree (Branch Empty b c)
deleteLeft (HBranch _ b right) = HBranch HEmpty b right

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  _ == _ = True

instance (Eq (HTree a), Eq b, Eq (HTree c)) => Eq (HTree (Branch a b c)) where
  HBranch a1 b1 c1 == HBranch a2 b2 c2 =
    a1 == a2 && b1 == b2 && c1 == c2


{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
f :: AlternatingList Any (Sum Int)
f = ACons (Any True) (ACons 1 (ACons (Any False) (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

skip1 :: AlternatingList a b -> AlternatingList b a
skip1 =
  \case
    ANil         -> ANil
    ACons _ next -> next

getFirsts :: AlternatingList a b -> [a]
getFirsts =
  \case
    ANil -> []
    ACons a b -> a : getFirsts (skip1 b)

getSeconds :: AlternatingList a b -> [b]
getSeconds = getFirsts . skip1

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues l = (fold $ getFirsts l, fold $getSeconds l)

singlePass :: forall a b.(Monoid a, Monoid b) => AlternatingList a b -> (a, b)
singlePass list = go list (mempty, mempty)
  where
    go :: AlternatingList a b -> (a, b) -> (a, b)
    go list (a, b) =
      case list of
        ANil -> (a, b)
        ACons a' next ->
           case next of
               ANil -> (a <> a', b)
               ACons b' next' -> go next' (a <> a', b <> b')

codeGolf :: forall a b. (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
codeGolf list =
  case list of
     ANil -> mempty
     ACons a ANil -> (a, mempty)
     ACons a (ACons b next) ->
       let (a', b') = codeGolf next
        in (a <> a', b <> b')


{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Fun       :: (a -> b)                         -> Expr (a -> b)
  Apply     :: Expr (a -> Expr b) -> Expr a     -> Expr b
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval =
    \case
        Equals l r  -> eval l == eval r
        Add l r     -> eval l + eval r
        If cond a b -> if eval cond then eval a else eval b
        IntValue i  -> i
        BoolValue b -> b
        Apply f x   -> eval $ eval f (eval x)
        Fun f       -> f

expr :: Expr Int
expr = Apply (Fun (IntValue . (+1))) (IntValue 1)

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

dirtyExpr :: DirtyExpr
dirtyExpr =
    DirtyIf
        (DirtyIf (DirtyBoolValue False) (DirtyBoolValue True) (DirtyBoolValue False))
        (DirtyIntValue 1)
        (DirtyAdd
            (DirtyIntValue 1)
            (DirtyIntValue 2)
        )

parseBool :: DirtyExpr -> Maybe (Expr Bool)
parseBool =
    \case
        DirtyEquals a b -> Equals <$> parse a <*> parse b
        DirtyBoolValue b -> Just $ BoolValue b
        DirtyIf cond a b   -> do
            cond' <- parseBool cond
            (l, r) <- (,) <$> parseBool a <*> parseBool b
            pure $ If cond' l r
        _ -> Nothing


parse :: DirtyExpr -> Maybe (Expr Int)
parse =
    \case
        DirtyEquals d1 d2 -> Nothing
        DirtyAdd d1 d2    -> Add <$> parse d1 <*> parse d2
        DirtyIf cond a b   -> do
            cond' <- parseBool cond
            (l, r) <- (,) <$> parse a <*> parse b
            pure $ If cond' l r
        DirtyIntValue i -> pure $ IntValue i
        _ -> Nothing

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
    TANil :: TypeAlignedList a a
    TACons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c
  -- ...

-- | b. Which types are existential?
-- The types in the 'middle'.

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs b2cl a2bl =
    case (a2bl, b2cl) of
        (TANil, _)           -> b2cl
        (TACons a2a' a'2bl, _) ->
            TACons a2a' (composeTALs b2cl a'2bl)

tl1 :: TypeAlignedList Int String
tl1 = TACons (+1) (TACons show TANil)

tl2 :: TypeAlignedList String [String]
tl2 = TACons (<> "!") (TACons pure (TACons (<>["hi"]) TANil))

runTAL :: a -> TypeAlignedList a b -> b
runTAL a =
    \case
        TANil -> a
        TACons f next -> runTAL (f a) next
