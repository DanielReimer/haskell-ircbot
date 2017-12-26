 {-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Connection
import Data.Default

server = "irc.server.com"
port   = 9999
chan   = "#channel1,#channel2"
keys   = " key1,key2" --leave blank if there are no keys
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

eval :: Connection -> B.ByteString -> IO ()
eval conn s -- add your own pattern matching here
  | "PING :" `B.isPrefixOf` s                       = write conn "PONG" (":" `B.append` B.drop 6 s)
  | "!quit" == getMessage                           = write conn "QUIT" ":Exiting"
  | "!hi" `B.isPrefixOf` getMessage                 = privmsg conn getChannel $ "hi " `B.append` getNick
  | otherwise                                       = return () --ignore everything else                                                                       
  where
    getNick =  C.takeWhile (/= '!') $ B.drop 1 s
    getMessage = C.takeWhile (/= '\r') $ B.drop 1 $ C.dropWhile (/= ':') $ B.drop 1 s
    getChannel = C.takeWhile (/= ' ') $ C.dropWhile (/= '#') s
