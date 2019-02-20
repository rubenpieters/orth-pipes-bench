{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import EventRecord

import Control.Monad
import Data.Monoid
import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text)
import Kafka.Consumer as KC
import Data.Map              (Map)
import qualified Data.Map.Strict as M
import Pipes (MonadIO, liftIO, lift)
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Data.Text.Encoding as T
import Text.Read             (readMaybe)
import qualified Database.Redis as R
import Data.UUID as UUID
import Data.UUID.V4 as UUID4
import Data.UnixTime
import Data.Aeson

-- src/redis-server

-- bin/zookeeper-server-start.sh config/zookeeper.properties
-- bin/kafka-server-start.sh config/server.properties
-- bin/kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic ad-events --from-beginning

main = run pipesCode

-- consumer configuration
consumerProps :: ConsumerProperties
consumerProps = KC.brokersList [BrokerAddress "localhost:9092"]
             <> groupId (ConsumerGroupId "consumer-id")
             -- prevent auto commit, so group id can be reused
             <> noAutoCommit

consumerSub :: Subscription
consumerSub = topics [TopicName "ad-events"]
          -- read messages from start of queue
          <> offsetReset Earliest

mkConsumer :: IO (Either KafkaError KafkaConsumer)
mkConsumer = newConsumer consumerProps consumerSub

-- NOTE: nicely closing consumers should normally be done with some kind of 'bracket' function
run :: (KafkaConsumer -> R.Connection -> IO ()) -> IO ()
run code = do
  eKafkaConsumer <- mkConsumer
  redisConnection <- R.checkedConnect R.defaultConnectInfo
  case eKafkaConsumer of
    Left err -> putStrLn (show err)
    Right kafkaConsumer -> do
      code kafkaConsumer redisConnection
      closeConsumer kafkaConsumer
      R.disconnect redisConnection

parseJson :: Maybe ByteString -> Maybe EventRecord
parseJson x = join (decodeStrict <$> x)

viewFilter :: EventRecord -> Bool
viewFilter r = event_type r == "view"

-- (ad_id, event_time)
eventProjection :: EventRecord -> (String, String)
eventProjection r = (ad_id r, event_time r)

-- (campaign_id, ad_id, event_time)
queryRedis :: (MonadIO m) => R.Connection -> (String, String) -> m (String, String, String)
queryRedis conn (ad_id, event_time) = do
  eMByteString <- liftIO $ R.runRedis conn $ R.get (pack ad_id)
  liftIO $ putStrLn ("AID: " ++ ad_id)
  case eMByteString of
    Left err -> do
      liftIO $ putStrLn ("redis error: " ++ show err)
      return ("error", ad_id, event_time)
    Right (Nothing) -> return ("nothing", ad_id, event_time)
    Right (Just campaign_id) -> return (unpack campaign_id, ad_id, event_time)

-- (campaign_id, window_time)
campaignTime :: (String, String, String) -> (String, Int)
campaignTime (campaign_id, ad_id, event_time) = let
  time_divisor = 10000
  event_time_num = case (readMaybe event_time) of
    Nothing -> 0
    Just x -> x
  in (campaign_id, time_divisor * (event_time_num `quot` time_divisor))

writeRedis :: (MonadIO m) => R.Connection -> ((String, Int), Int) -> m ()
writeRedis conn ((campaign_id, window_time), count) = liftIO $ R.runRedis conn $ do
  eMWindowUUID <- R.hget (pack campaign_id) (pack (show window_time))
  liftIO $ putStrLn ("CID: " ++ campaign_id)
  windowUUID <- case eMWindowUUID of
    Left err -> do
      liftIO $ putStrLn ("redis error: " ++ show err)
      uuid <- liftIO (UUID.toASCIIBytes <$> UUID4.nextRandom)
      R.hset (pack campaign_id) (pack (show window_time)) uuid
      return uuid
    Right (Nothing) -> do
      uuid <- liftIO (UUID.toASCIIBytes <$> UUID4.nextRandom)
      R.hset (pack campaign_id) (pack (show window_time)) uuid
      return uuid
    Right (Just uuid) -> do
      return uuid
  R.hincrby windowUUID "seen_count" (toInteger count)
  unixtime <- liftIO getUnixTime
  R.hset windowUUID "time_updated" (formatUnixTimeGMT webDateFormat unixtime)
  return ()

-- Pipes Code

countByKey :: (Monad m) => P.Producer (String, Int) m () -> P.Producer ((String, Int), Int) m ()
countByKey prod = do
  resultMap <- lift $ P.fold (\map key -> M.insertWith (+) key (1 :: Int) map) M.empty id prod
  P.each (M.toList resultMap)

pipesConsumer :: (MonadIO m) => KafkaConsumer -> P.Producer (ConsumerRecord (Maybe ByteString) (Maybe ByteString)) m ()
pipesConsumer kafkaConsumer = forever $ do
  eMsg <- liftIO (pollMessage kafkaConsumer (Timeout 1000))
  case eMsg of
    Left err -> liftIO (putStrLn (show err))
    Right msg -> P.yield msg

pipesCode :: KafkaConsumer -> R.Connection -> IO ()
pipesCode kafkaConsumer redisConnection = P.runEffect $
  (countByKey (pipesConsumer kafkaConsumer P.>->
    P.map crValue P.>->
    P.map parseJson P.>->
    P.concat P.>->
    P.filter viewFilter P.>->
    P.map eventProjection P.>->
    P.mapM (queryRedis redisConnection) P.>->
    P.map campaignTime P.>->
    P.take 1000
  )) P.>-> P.mapM (writeRedis redisConnection) P.>-> forever P.await

-- Conduit Code



-- Orthogonal Pipes Code


