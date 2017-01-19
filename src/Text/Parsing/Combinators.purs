module Text.Parsing.Combinators where

import Control.Alt (class Alt, (<|>))
import Control.Applicative (class Applicative, pure)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Unit (Unit, unit)

option :: forall f a. (Alt f, Applicative f) => a -> f a -> f a
option a p = p <|> pure a

optional :: forall f a. (Alt f, Applicative f) => f a -> f Unit
optional p = void p <|> pure unit

optionMaybe :: forall f a. (Alt f, Applicative f)
            => f a -> f (Maybe a)
optionMaybe p = option Nothing $ Just <$> p

either :: forall f a b. (Alt f, Applicative f)
       => f a -> f b -> f (Either a b)
either a b = Left <$> a <|> Right <$> b
