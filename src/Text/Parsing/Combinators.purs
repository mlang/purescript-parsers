module Text.Parsing.Combinators (
  between, chainr, chainr1, either, oneOf, option, optional, optionMaybe
) where

import Control.Alternative ( class Alternative, pure, (<|>) )
import Control.Apply       ( class Apply, (<*), (<*>), (*>) )
import Data.Either         ( Either(Left, Right) )
import Data.Foldable       ( class Foldable )
import Data.Foldable       ( oneOf )                          as Foldable
import Data.Function       ( flip, id, ($), (#) )
import Data.Functor        ( void, (<$>) )
import Data.Maybe          ( Maybe(Just, Nothing) )
import Data.Unit           ( Unit, unit )

between :: forall m open close a. Apply m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

-- | Parse phrases delimited by a right-associative operator.
chainr :: forall m a. Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a right-associative operator,
-- | requiring at least one match.
chainr1 :: forall m a. Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = (#) <$> p <*> ((flip <$> op <*> chainr1 p op) <|> pure id)

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
