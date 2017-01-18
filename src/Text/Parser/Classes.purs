module Text.Parser.Classes where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (class Monad)
import Control.Monad.Reader.Trans (ReaderT(ReaderT), mapReaderT)
import Control.Monad.RWS.Trans (RWST(RWST), RWSResult(RWSResult), mapRWST)
import Control.Monad.State.Trans (StateT(StateT), mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT(WriterT), mapWriterT)
import Data.Eq ((/=))
import Data.Function
import Data.Functor ((<$>))
import Data.Monoid (class Monoid, mempty)
import Data.Show (class Show)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Unit (Unit, unit)
import Text.Parsing.Parser (ParserT) as P
import Text.Parsing.Parser.Combinators (lookAhead, notFollowedBy, try, withErrorMessage) as P
import Text.Parsing.Parser.String (class StringLike, anyChar, char, eof, satisfy) as P

class Parsing m where
  try :: forall a. m a -> m a
  withErrorMessage :: forall a. m a -> String -> m a
  eof :: m Unit
  notFollowedBy :: forall a. Show a => m a -> m Unit

instance parsingParser :: (P.StringLike s, Monad m)
                       => Parsing (P.ParserT s m) where
  try = P.try
  withErrorMessage = P.withErrorMessage
  eof = P.eof
  notFollowedBy = P.notFollowedBy

instance parsingReaderT :: (Parsing m, Monad m)
                        => Parsing (ReaderT e m) where
  try = mapReaderT try
  withErrorMessage (ReaderT m) l = ReaderT $ \ e -> withErrorMessage (m e) l
  eof = lift eof
  notFollowedBy = mapReaderT notFollowedBy

instance parsingRWST :: (Parsing m, Monad m, Monoid w)
                     => Parsing (RWST r w s m) where
  try = mapRWST try
  withErrorMessage (RWST m) l = RWST \ r s -> withErrorMessage (m r s) l
  eof = lift eof
  notFollowedBy (RWST m) = RWST \ r s -> do
    x <- notFollowedBy $ (\(RWSResult _ a _) -> a) <$> m r s
    pure $ RWSResult s x mempty

instance parsingStateT :: (Parsing m, Monad m) => Parsing (StateT s m) where
  try = mapStateT try
  withErrorMessage (StateT m) l = StateT $ \s -> withErrorMessage (m s) l
  eof = lift eof
  notFollowedBy (StateT m) = StateT \ s -> do
    notFollowedBy $ fst <$> m s
    pure $ Tuple unit s
 
instance parsingWriterT :: (Parsing m, Monad m, Monoid w)
                        => Parsing (WriterT w m) where
  try = mapWriterT try
  withErrorMessage (WriterT m) l = WriterT $ withErrorMessage m l
  eof = lift eof
  notFollowedBy (WriterT m) = WriterT do
    x <- notFollowedBy $ fst <$> m
    pure $ Tuple x mempty

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

class Parsing m <= LookAheadParsing m where
  lookAhead :: forall a. m a -> m a

instance lookAheadParsingParser :: (P.StringLike s, Monad m)
                                => LookAheadParsing (P.ParserT s m) where
  lookAhead = P.lookAhead

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
