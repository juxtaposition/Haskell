import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit

server = "irc.twitch.tv"
port = 6667
chan = "#langenscheidt"
nick = "Langenscheidt"
privmsg = "PRIVMSG "


main = do
       h <- connectTo server (PortNumber (fromIntegral port))
       hSetBuffering h NoBuffering
       write h "PASS " "oauth:"
       write h "NICK " nick
       write h "JOIN " chan
       listen h
       

write :: Handle -> String -> String -> IO ()
write h s t = do
                hPrintf h "%s %s\r\n" s t
                printf "> %s %s\n" s t
                

listen :: Handle -> IO ()
listen h = forever $ do
            s <- hGetLine h
            if ping s then pong s else eval h (clean s)
            putStrLn s
           where 
                clean  = drop 1 . dropWhile (/= ':') . drop 1 
                ping x = isPrefixOf "PING :" x
                pong x = write h " " x 
                forever a = do a; forever a 
            -- :jachupawa!jachupawa@jachupawa.tmi.twitch.tv PRIVMSG #lindsan :RareParrot 
            

eval :: Handle -> String -> IO ()   
eval h    "!quit"                = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x = write h privmsg (drop 4 x)
eval _   _                       = return () -- ignore everything else         
