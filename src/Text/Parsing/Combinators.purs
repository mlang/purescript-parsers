module Text.Parsing.Combinators (
  between,
  chainl, chainl1, chainr, chainr1,
  either,
  oneOf,
  option, optional, optionMaybe
) where

import Control.Alternative ( class Alternative, pure, (<|>) )
import Control.Apply       ( class Apply, lift2, (<*), (<*>), (*>) )
import Data.Either         ( Either(Left, Right) )
import Data.Foldable       ( class Foldable )
import Data.Foldable       ( oneOf )                          as Foldable
import Data.Function       ( flip, id, ($), (#) )
import Data.Functor        ( void, (<$>) )
import Data.Maybe          ( Maybe(Just, Nothing) )
import Data.Unit           ( Unit, unit )

between :: forall m open close a. Apply m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

chainl :: forall m a. Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op a = option a $ chainl1 p op

chainl1 :: forall m a. Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = lift2 (#) p $ scan id where
  scan def = option def $ (lift2 (\f y g x -> g $ f x y) op p) <*> scan def

-- | Parse phrases delimited by a right-associative operator.
chainr :: forall m a. Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p f a = option a $ chainr1 p f

-- | Parse phrases delimited by a right-associative operator,
-- | requiring at least one match.
chainr1 :: forall m a. Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = lift2 (#) p $ option id $ lift2 flip op $ chainr1 p op

either :: forall m a b. Alternative m => m a -> m b -> m (Either a b)
either a b = Left <$> a <|> Right <$> b

oneOf :: forall f m a. (Foldable f, Alternative m) => f (m a) -> m a
oneOf = Foldable.oneOf

option :: forall m a. Alternative m => a -> m a -> m a
option a p = p <|> pure a

optional :: forall m a. Alternative m => m a -> m Unit
optional p = void p <|> pure unit

optionMaybe :: forall m a. Alternative m => m a -> m (Maybe a)
optionMaybe p = option Nothing $ Just <$> p
