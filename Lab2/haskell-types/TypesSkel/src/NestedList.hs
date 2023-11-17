module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a
    = EmptyList
    | Elem a
    | List [NestedList a]


instance (Show a) => Show (NestedList a) where
    show = []
    show (Elem a) = show a
    show (List l) = show l 

instance Functor NestedList where
    fmap _ EmptyList = EmptyList
    fmap f (Elem a) = Elem $ f a
    fmap f (List l) = List $ map (fmap f) l 

instance Container NestedList where
    contents EmptyList = []
    contents (Elem a) = [a]
    contents (List l) = foldl (++) [] $ fmap contents l

instance Invertible a => Invertible (NestedList a) where
    invert EmptyList = EmptyList
    invert (Elem a) = Elem $ invert a
    invert (List l) = List $ invert $ fmap invert l 