 {-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Connection
import Data.Default
import Data.Char

server = "irc.server.com"
port   = 9999
chan   = "#channel1,#channel2"
keys   = " key1,key2"
nick   = "myBot"

main = do
    ctx <- initConnectionContext
    conn <- connectTo ctx $ ConnectionParams
                       { connectionHostname  = server
                       , connectionPort      = port
                       , connectionUseSecure = Just def -- set to 'Nothing' to not use ssl
                       , connectionUseSocks  = Nothing
                       }
    write conn "USER" $ B.append nick " 0 * :creatorName"
    write conn "NICK" nick
    write conn "JOIN" $ B.append chan keys
    listen conn

write :: Connection -> B.ByteString -> B.ByteString -> IO ()
write conn s t = do
    connectionPut conn $ B.concat [s, " ", t, "\r\n"] --output to irc
    B.putStr $ B.concat ["> ", s, " ", t, "\n"] -- output to console

privmsg :: Connection -> B.ByteString -> B.ByteString -> IO ()
privmsg conn channel s = write conn "PRIVMSG" $ B.concat [channel, " :", s]

listen :: Connection -> IO ()
listen conn = forever $ do
  s <- connectionGetLine 1024 conn -- get the string from server
  eval conn s -- evaluate the recieved string
  putStrLn $ show s -- output to console
    where
      forever a = a >> forever a -- loop forever

getChannel :: B.ByteString -> B.ByteString
getChannel s
  | chan == nick = user
  | otherwise = chan
  where
    chan = C.takeWhile (/= ' ') $ B.drop 1 $ C.takeWhile (/= ':') $ C.dropWhile (/= ' ') $ B.drop 1 $ C.dropWhile (/= ' ') s
    user =  C.takeWhile (/= '!') $ B.drop 1 s

eval :: Connection -> B.ByteString -> IO ()
eval conn s
  | "PING :" `B.isPrefixOf` s = write conn "PONG" (":" `B.append` B.drop 6 s)
  | "PRIVMSG" `B.isInfixOf` s = respond conn message nick (getChannel s)
  | otherwise = return ()
  where
    nick =  C.takeWhile (/= '!') $ B.drop 1 s
    message =  C.map toLower $ C.takeWhile (/= '\r') $ B.drop 1 $ C.dropWhile (/= ':') $ B.drop 1 $ C.dropWhile (/= ' ') s

respond :: Connection -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
respond conn message nick channel
  | "!hi" `B.isInfixOf` message = privmsg conn channel "Hello"
  | otherwise                    = return () --ignore everything else
