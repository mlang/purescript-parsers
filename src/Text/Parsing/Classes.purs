module Text.Parsing.Classes where

import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Apply ((*>))
import Control.Bind (bind, (>>=))
import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT, mapExceptT, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT, mapMaybeT, runMaybeT)
import Control.Monad.Reader.Trans (ReaderT, mapReaderT)
import Control.Monad.RWS.Trans (RWST(), RWSResult(RWSResult), mapRWST, runRWST)
import Control.Monad.State.Trans (StateT, mapStateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT, mapWriterT, runWriterT)
import Control.Plus (empty)
import Data.Either (either)
import Data.Eq ((/=))
import Data.Function
import Data.Functor ((<$>))
import Data.Maybe (maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (wrap)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(Tuple), fst)
import Data.Unit (Unit, unit)
import Text.Parsing.Parser (ParserT, fail) as P
import Text.Parsing.Parser.Combinators (lookAhead, notFollowedBy, try, withErrorMessage) as P
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy) as P
import Text.Parsing.StringParser (Parser, fail, try) as SP
import Text.Parsing.StringParser.Combinators (lookAhead, withError) as SP
import Text.Parsing.StringParser.String (anyChar, char, eof, satisfy) as SP

class Alternative m <= Parsing m where
  try :: forall a. m a -> m a
  withErrorMessage :: forall a. m a -> String -> m a
  unexpected :: forall a. String -> m a
  eof :: m Unit
  notFollowedBy :: forall a. m a -> m Unit

asErrorMessage :: forall m a. Parsing m => String -> m a -> m a
asErrorMessage = flip withErrorMessage

infix 3 withErrorMessage as <?>
infix 3 asErrorMessage as <??>

instance parsingParser :: (P.StringLike s, Monad m)
                       => Parsing (P.ParserT s m) where
  try = P.try
  withErrorMessage = P.withErrorMessage
  unexpected = P.fail <<< ("Unexpected " <> _)
  eof = P.eof
  notFollowedBy = P.notFollowedBy

instance parsingStringParser :: Parsing SP.Parser where
  try = SP.try
  withErrorMessage = SP.withError
  unexpected = SP.fail <<< ("Unexpected " <> _)
  eof = SP.eof
  notFollowedBy p = p *> SP.fail "Negated parser succeded" <|> pure unit

instance parsingExceptT :: (Monoid e, Monad m, Parsing m) => Parsing (ExceptT e m) where
  try = mapExceptT try
  withErrorMessage r l = mapExceptT (asErrorMessage l) r
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy m = wrap do
    u <- notFollowedBy $ runExceptT m >>= either (const empty) pure
    pure $ pure u

instance parsingMaybeT :: (Monad m, Parsing m) => Parsing (MaybeT m) where
  try = mapMaybeT try
  withErrorMessage r l = mapMaybeT (asErrorMessage l) r
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy m = wrap do
    u <- notFollowedBy $ runMaybeT m >>= maybe empty pure
    pure $ pure u

instance parsingReaderT :: (Parsing m, Monad m)
                        => Parsing (ReaderT e m) where
  try = mapReaderT try
  withErrorMessage r l = mapReaderT (flip withErrorMessage l) r
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy = mapReaderT notFollowedBy

instance parsingRWST :: (Parsing m, Monad m, Monoid w)
                     => Parsing (RWST r w s m) where
  try = mapRWST try
  withErrorMessage r l = mapRWST (asErrorMessage l) r
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy m = wrap \ r s -> do
    u <- notFollowedBy $ (\(RWSResult _ a _) -> a) <$> runRWST m r s
    pure $ RWSResult s u mempty

instance parsingStateT :: (Parsing m, Monad m)
                       => Parsing (StateT s m) where
  try = mapStateT try
  withErrorMessage s l = mapStateT (flip withErrorMessage l) s
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy m = wrap \ s -> do
    u <- notFollowedBy $ fst <$> runStateT m s
    pure $ Tuple u s
 
instance parsingWriterT :: (Parsing m, Monad m, Monoid w)
                        => Parsing (WriterT w m) where
  try = mapWriterT try
  withErrorMessage m msg = mapWriterT (asErrorMessage msg) m
  unexpected = lift <<< unexpected
  eof = lift eof
  notFollowedBy m = wrap do
    u <- notFollowedBy $ fst <$> runWriterT m
    pure $ Tuple u mempty

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

instance charParsingStringParser :: CharParsing SP.Parser where
  satisfy = SP.satisfy
  char = SP.char
  notChar c = satisfy (c /= _)
  anyChar = SP.anyChar

instance charParsingExceptT :: (Monoid e, CharParsing m, Monad m)
                            => CharParsing (ExceptT e m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

instance charParsingMaybeT :: (CharParsing m, Monad m)
                           => CharParsing (MaybeT m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

instance charParsingReaderT :: (CharParsing m, Monad m)
                            => CharParsing (ReaderT e m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

instance charParsingRWST :: (CharParsing m, Monad m, Monoid w)
                         => CharParsing (RWST r w s m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

instance charParsingStateT :: (CharParsing m, Monad m)
                           => CharParsing (StateT s m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

instance charParsingWriterT :: (CharParsing m, Monad m, Monoid w)
                            => CharParsing (WriterT w m) where
  satisfy = lift <<< satisfy
  char = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar

class Parsing m <= LookAheadParsing m where lookAhead :: forall a. m a -> m a

instance lookAheadParsingParser :: (P.StringLike s, Monad m)
                                => LookAheadParsing (P.ParserT s m) where
  lookAhead = P.lookAhead

instance lookAheadParsingStringParser :: LookAheadParsing SP.Parser where
  lookAhead = SP.lookAhead

instance lookAheadParsingEitherT :: (Monoid e, LookAheadParsing m, Monad m)
                                 => LookAheadParsing (ExceptT e m) where
  lookAhead = mapExceptT lookAhead

instance lookAheadParsingMaybeT :: (LookAheadParsing m, Monad m)
                                => LookAheadParsing (MaybeT m) where
  lookAhead = mapMaybeT lookAhead

instance lookAheadParsingReaderT :: (LookAheadParsing m, Monad m)
                                 => LookAheadParsing (ReaderT e m) where
  lookAhead = mapReaderT lookAhead

instance lookAheadParsingRWST :: (LookAheadParsing m, Monad m, Monoid w)
                              => LookAheadParsing (RWST r w s m) where
  lookAhead = mapRWST lookAhead

instance lookAheadParsingStateT :: (LookAheadParsing m, Monad m)
                                => LookAheadParsing (StateT e m) where
  lookAhead = mapStateT lookAhead

instance lookAheadParsingWriterT :: (LookAheadParsing m, Monad m, Monoid w)
                                 => LookAheadParsing (WriterT w m) where
  lookAhead = mapWriterT lookAhead
