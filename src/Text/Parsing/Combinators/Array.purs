module Text.Parsing.Combinators.Array (
  many, sepBy, sepBy1, some
) where

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Apply ((<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Functor ((<$>))
import Data.Array ((:))
import Data.Array (many, some) as Array

many :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
many = Array.many

sepBy :: forall f a sep. (Alternative f, Lazy (f (Array a)))
      => f a -> f sep -> f (Array a)
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: forall f a sep. (Alternative f, Lazy (f (Array a)))
       => f a -> f sep -> f (Array a)
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

some :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
some = Array.some
