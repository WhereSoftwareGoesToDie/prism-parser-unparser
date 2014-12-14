--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module PrismParserUnparser
(
    ApplicativePrismSyntax(..)
) where

import           Control.Lens

class ApplicativePrismSyntax f v where
    -- Functor from Prisms to Hask restricted to f
    (<$$>) :: Prism' b a -> f a -> f b

    -- The opposite of above. That thing.
    (<$$$>) :: Prism' a b -> f a -> f b

    -- Applicative
    (<**>) :: f a -> f b -> f (a, b)

    -- Choice
    (<||>) :: f a -> f a -> f a

    value :: f v

class Flat a where
  type TupleTree a
  _Flat :: Iso' a (TupleTree a)


instance Flat (a,b) where
  type TupleTree (a,b) = (a,b)
  _Flat = iso id id

instance Flat (a,b,c) where
  type TupleTree (a,b,c) = (a,(b,c))
  _Flat = iso (\(a,b,c) -> (a,(b,c))) (\(a,(b,c)) -> (a,b,c))


instance Flat (a,b,c,d) where
  type TupleTree (a,b,c,d) = (a,(b,(c,d)))
  _Flat = iso (\(a,b,c,d) -> (a,(b,(c,d)))) (\(a,(b,(c,d))) -> (a,b,c,d))

instance Flat (a,b,c,d,e) where
  type TupleTree (a,b,c,d,e) = (a,(b,(c,(d,e))))
  _Flat = iso (\(a,b,c,d,e) -> (a,(b,(c,(d,e))))) (\(a,(b,(c,(d,e)))) -> (a,b,c,d,e))
