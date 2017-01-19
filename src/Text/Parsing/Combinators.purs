module Text.Parsing.Combinators (
  between, either, option, optional, optionMaybe,
  module Text.Parsing.Combinators.List
) where

import Control.Alt (class Alt, (<|>))
import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply, (<*), (*>))
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor (class Functor, void, (<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)
import Text.Parsing.Combinators.List (many, sepBy, sepBy1, some)

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
