module Text.Parsing.Combinators (
  between, either, option, optional, optionMaybe, sepBy, sepBy1
) where

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*), (<*>), (*>))
import Control.Lazy (class Lazy)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor (class Functor, void, (<$>))
import Data.List (List(Nil), many, some, (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)

between :: forall f open close a. Apply f => f open -> f close -> f a -> f a
between open close p = open *> p <* close

either :: forall f a b. (Alt f, Functor f)
       => f a -> f b -> f (Either a b)
either a b = Left <$> a <|> Right <$> b

option :: forall f a. (Alt f, Applicative f)
       => a -> f a -> f a
option a p = p <|> pure a

optional :: forall f a. (Alt f, Applicative f)
         => f a -> f Unit
optional p = void p <|> pure unit

optionMaybe :: forall f a. (Alt f, Applicative f)
            => f a -> f (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

sepBy :: forall f a sep. (Alternative f, Lazy (f (List a)))
      => f a -> f sep -> f (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

sepBy1 :: forall f a sep. (Alternative f, Lazy (f (List a)))
       => f a -> f sep -> f (List a)
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
