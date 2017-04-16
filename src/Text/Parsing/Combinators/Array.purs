module Text.Parsing.Combinators.Array (
  many, sepBy, sepBy1, sepEndBy, sepEndBy1, some
) where

import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Apply (class Apply, lift2, (<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Array ((:))
import Data.Array (many, some) as Array
import Data.Function (flip, ($), (#))
import Data.Functor ((<$>))
import Text.Parsing.Combinators (option)

many :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
many = Array.many

sepBy :: forall f a sep. Alternative f => Lazy (f (Array a))
      => f a -> f sep -> f (Array a)
sepBy p sep = option [] $ sepBy1 p sep

sepBy1 :: forall f a sep. Alternative f => Lazy (f (Array a))
       => f a -> f sep -> f (Array a)
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepEndBy :: forall f a sep. Alternative f => Lazy (f (Array a))
         => f a -> f sep -> f (Array a)
sepEndBy p sep = option [] $ sepEndBy1 p sep

sepEndBy1 :: forall f a sep. Alternative f => Lazy (f (Array a))
          => f a -> f sep -> f (Array a)
sepEndBy1 p sep = p <**> option pure (flip (:) <$> (sep *> sepEndBy p sep))

some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
some = Array.some

applyFlipped :: forall f a b. Apply f => f a -> f (a -> b) -> f b
applyFlipped = lift2 (#)
infixl 4 applyFlipped as <**>
