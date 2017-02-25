module Text.Parsing.Combinators.List (
  many, sepBy, sepBy1, sepEndBy, sepEndBy1, some
) where

import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Apply ((<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Function (flip, ($), (#))
import Data.Functor ((<$>))
import Data.List (List(Cons, Nil))
import Data.List (many, some) as List
import Text.Parsing.Combinators (option)

many :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
many = List.many

sepBy :: forall f a sep. (Alternative f, Lazy (f (List a)))
      => f a -> f sep -> f (List a)
sepBy p sep = option Nil $ sepBy1 p sep

sepBy1 :: forall f a sep. (Alternative f, Lazy (f (List a)))
       => f a -> f sep -> f (List a)
sepBy1 p sep = Cons <$> p <*> many (sep *> p)

sepEndBy :: forall f a sep. (Alternative f, Lazy (f (List a)))
          => f a -> f sep -> f (List a)
sepEndBy p sep = option Nil $ sepEndBy1 p sep

sepEndBy1 :: forall f a sep. (Alternative f, Lazy (f (List a)))
          => f a -> f sep -> f (List a)
sepEndBy1 p sep = (#) <$> p <*> option pure (flip Cons <$> (sep *> sepEndBy p sep))

some :: forall f a. (Alternative f, Lazy (f (List a))) => f a -> f (List a)
some = List.some
