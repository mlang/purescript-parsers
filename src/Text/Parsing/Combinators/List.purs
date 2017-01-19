module Text.Parsing.Combinators.List (
  many, sepBy, sepBy1, some
) where

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Apply ((<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Functor ((<$>))
import Data.List (List(Cons, Nil))
import Data.List (many, some) as List

many :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
many = List.many

sepBy :: forall f a sep. (Alternative f, Lazy (f (List a)))
      => f a -> f sep -> f (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

sepBy1 :: forall f a sep. (Alternative f, Lazy (f (List a)))
       => f a -> f sep -> f (List a)
sepBy1 p sep = Cons <$> p <*> many (sep *> p)

some :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
some = List.some
