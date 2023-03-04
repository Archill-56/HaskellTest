{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Api2
    ( hahaa
    ) where

hahaa :: IO String
hahaa = pure "hello"

data a :+ b = InLeft a | InRight b
    deriving Show

data a :* b = a :*: b
    deriving Show

first :: a :* b -> a
first (a :*: _) = a