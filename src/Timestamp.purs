module Timestamp where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (fromFoldable)
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowDateTime)
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

iso8601Format :: Formatter
iso8601Format = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  , Placeholder "T"
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  , Placeholder "."
  , Milliseconds
  , Placeholder "Z"
  ]

newtype Timestamp = Timestamp DateTime
derive newtype instance eqTimestamp :: Eq Timestamp
derive newtype instance ordTimestamp :: Ord Timestamp
instance readTimestamp :: ReadForeign Timestamp where
  readImpl v = do
    s <- readImpl v
    case unformat iso8601Format s of
      Right d -> pure (Timestamp d)
      Left e -> throwError $ pure $ ForeignError e

instance writeTimestamp :: WriteForeign Timestamp where
  writeImpl (Timestamp dt) =
    writeImpl $ format iso8601Format dt

derive instance genericTimestamp :: Generic Timestamp _
derive instance newtypeTimestamp :: Newtype Timestamp _
instance showTimestamp :: Show Timestamp where
  show = genericShow

nowTimestamp :: ∀ m. MonadEffect m => m Timestamp
nowTimestamp = liftEffect $ Timestamp <$> nowDateTime