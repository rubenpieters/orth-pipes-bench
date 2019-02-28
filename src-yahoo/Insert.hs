{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Insert where

import EventRecord

import Control.Exception (bracket)
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Monoid
import Data.ByteString       (ByteString)
import Data.ByteString.Lazy  (toStrict)
import Data.ByteString.Char8 (pack, unpack)
import Data.Aeson
import Test.QuickCheck
import qualified Database.Redis as R
import Kafka.Producer as KP

genEventRecord :: Gen [EventRecord]
genEventRecord = do
  user_id <- create "user_id" [1..100]
  page_id <- create "page_id" [1..100]
  ad_id <- create "aid_" [1..100]
  ad_type <- elements ["banner", "modal", "sponsored-search", "mail", "mobile"]
  event_time <- show <$> choose (1 :: Int, 1000000)
  ip_address <- create "ip_address" [1..100]
  return
    [ EventRecord user_id page_id ad_id ad_type "view" event_time ip_address
    , EventRecord user_id page_id ad_id ad_type "click" event_time ip_address
    , EventRecord user_id page_id ad_id ad_type "purchase" event_time ip_address
    ]

create :: String -> [Int] -> Gen String
create prefix postfixes = let
  values = (\postfix -> prefix ++ (show postfix)) <$> postfixes
  in oneof (return <$> values)

redisData :: [(String, String)]
redisData = do
  index :: Int <- [1..100]
  return ("aid_" ++ show index, "cid_" ++ show index)

main :: IO ()
main = do
  putStrLn "insert Redis data..."
  runRedisProducer
  putStrLn "insert Kafka data..."
  runKafkaProducer
  putStrLn "done"

-- Redis Producer code

runRedisProducer :: IO ()
runRedisProducer = let
  mkRedis = R.checkedConnect R.defaultConnectInfo
  closeRedis conn = R.disconnect conn
  runRedis conn = sendRedis conn
  in bracket mkRedis closeRedis runRedis >>= print

sendRedis :: R.Connection -> IO ()
sendRedis redisConnection =
  R.runRedis redisConnection $ do
  forM_ redisData $ \(key, val) -> do
    R.set (pack key) (pack val)

-- Kafka Producer code

producerProps :: ProducerProperties
producerProps = KP.brokersList [BrokerAddress "localhost:9092"]
             <> KP.logLevel KafkaLogDebug

targetTopic :: TopicName
targetTopic = TopicName "ad-events"

runKafkaProducer :: IO ()
runKafkaProducer = let
  mkKafka = newProducer producerProps
  closeKafka (Left _) = return ()
  closeKafka (Right prod) = closeProducer prod
  runKafka (Left err)   = return $ Left err
  runKafka (Right prod) = Right <$> sendKafka prod
  in bracket mkKafka closeKafka runKafka >>= print

sendKafka :: KafkaProducer -> IO ()
sendKafka prod = do
  records <- replicateM 1000000 (generate genEventRecord)
  forM_ (concat records) $ \record -> do
    mErr <- produceMessage prod (mkMessage Nothing (Just (toStrict $ encode record)))
    case mErr of
      Just (KafkaResponseError RdKafkaRespErrQueueFull) -> do
        putStrLn "queue full, waiting 1s ..."
        threadDelay 1000000
      Just err -> putStrLn (show err)
      Nothing -> return ()

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
                  { prTopic = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = k
                  , prValue = v
                  }