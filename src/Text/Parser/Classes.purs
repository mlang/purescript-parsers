module Text.Parser.Classes where

import Control.Alt (class Alt)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (class Monad)
import Control.Monad.RWS.Trans (RWST(..), RWSResult(..))
import Control.Monad.Trans.Class (lift)
import Data.Eq ((/=))
import Data.Function
import Data.Functor ((<$>))
import Data.Monoid (class Monoid, mempty)
import Data.Show (class Show)
import Data.Unit (Unit)
import Text.Parsing.Parser (ParserT) as P
import Text.Parsing.Parser.Combinators (lookAhead, notFollowedBy, try, withErrorMessage) as P
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy) as P

class Alt m <= Parsing m where
  try :: forall a. m a -> m a
  withErrorMessage :: forall a. m a -> String -> m a
  --unexpected :: forall a. String -> m a
  eof :: m Unit
  notFollowedBy :: forall a. Show a => m a -> m Unit

instance parsingParser :: (Monad m, P.StringLike s)
                       => Parsing (P.ParserT s m) where
  try = P.try
  withErrorMessage = P.withErrorMessage
  eof = P.eof
  notFollowedBy = P.notFollowedBy

instance parsingRWST :: (Parsing m, Monad m, Monoid w)
                     => Parsing (RWST r w s m) where
  try (RWST m) = RWST \ r s -> try $ m r s
  withErrorMessage (RWST m) l = RWST \ r s -> withErrorMessage (m r s) l
  --unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy (RWST m) = RWST \ r s -> do
    x <- notFollowedBy $ (\(RWSResult _ a _) -> a) <$> m r s
    pure $ RWSResult s x mempty

class Parsing m <= CharParsing m where
  satisfy :: (Char -> Boolean) -> m Char
  char :: Char -> m Char
  notChar :: Char -> m Char
  anyChar :: m Char

instance charParsingParser :: (P.StringLike s, Monad m)
                           => CharParsing (P.ParserT s m) where
  satisfy = P.satisfy
  char = P.char
  notChar c = satisfy (c /= _)
  anyChar = P.anyChar

instance charParsingRWST :: (CharParsing m, Monad m, Monoid w)
                         => CharParsing (RWST r w s m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

class Parsing m <= LookAheadParsing m where
  lookAhead :: forall a. m a -> m a

instance lookAheadParsingParser :: (P.StringLike s, Monad m) => LookAheadParsing (P.ParserT s m) where
  lookAhead = P.lookAhead

instance lookAheadParsingRWST :: (LookAheadParsing m, Monad m, Monoid w)
                              => LookAheadParsing (RWST r w s m) where
  lookAhead (RWST m) = RWST $ \ r s -> lookAhead $ m r s
