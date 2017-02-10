module Text.Parsing.Combinators.Array (
  many, sepBy, sepBy1, some
) where

import Control.Alternative (class Alternative)
import Control.Apply ((<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Functor ((<$>))
import Data.Array ((:))
import Data.Array (many, some) as Array
import Data.Function (($))
import Text.Parsing.Combinators (option)

many :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
many = Array.many

sepBy :: forall f a sep. (Alternative f, Lazy (f (Array a)))
      => f a -> f sep -> f (Array a)
sepBy p sep = option [] $ sepBy1 p sep

sepBy1 :: forall f a sep. (Alternative f, Lazy (f (Array a)))
       => f a -> f sep -> f (Array a)
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

some :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
some = Array.some
