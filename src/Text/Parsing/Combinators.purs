module Text.Parsing.Combinators (
  between, chainr, chainr1, either, oneOf, option, optional, optionMaybe
) where

import Control.Alt         ( class Alt, (<|>) )
import Control.Applicative ( class Applicative, pure )
import Control.Apply       ( class Apply, (<*), (<*>), (*>) )
import Control.Bind        ( class Bind, (>>=) )
import Control.Plus        ( class Plus )
import Data.Either         ( Either(Left, Right) )
import Data.Foldable       ( class Foldable )
import Data.Foldable       ( oneOf )                          as Foldable
import Data.Function       ( ($) )
import Data.Functor        ( class Functor, void, (<$>) )
import Data.Maybe          ( Maybe(Just, Nothing) )
import Data.Unit           ( Unit, unit )

between :: forall f a l r. Apply f => f l -> f r -> f a -> f a
between l r p = l *> p <* r

-- | Parse phrases delimited by a right-associative operator.
chainr :: forall f a. (Alt f, Applicative f, Bind f)
       => f a -> f (a -> a -> a) -> a -> f a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a right-associative operator,
-- | requiring at least one match.
chainr1 :: forall f a. (Alt f, Applicative f, Bind f)
        => f a -> f (a -> a -> a) -> f a
chainr1 p f = p >>= \ a -> (_ $ a) <$> f <*> chainr1 p f <|> pure a

either :: forall f a b. (Alt f, Functor f) => f a -> f b -> f (Either a b)
either a b = Left <$> a <|> Right <$> b

oneOf :: forall f g a. (Foldable f, Plus g) => f (g a) -> g a
oneOf = Foldable.oneOf

option :: forall f a. (Alt f, Applicative f)
       => a -> f a -> f a
option a p = p <|> pure a

optional :: forall f a. (Alt f, Applicative f)
         => f a -> f Unit
optional p = void p <|> pure unit

optionMaybe :: forall f a. (Alt f, Applicative f)
            => f a -> f (Maybe a)
optionMaybe p = option Nothing $ Just <$> p
