import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import System.IO (getLine)

-- writer gets a line from console and puts it in mailbox
writer mailbox = do
    line <- getLine
    putMVar mailbox line
    writer mailbox

-- reader takes a line from mailbox and prints it
reader rId mailbox = do
    line <- takeMVar mailbox
    putStrLn $ "Reader " ++ rId ++ ": " ++ line
    reader rId mailbox

main = do
    -- mailbox connects readers and writers
    mailbox <- newEmptyMVar -- :: IO (MVar String)

    -- the mailbox is shared by the readers and the writer
    forkIO $ reader "A" mailbox
    forkIO $ reader "B" mailbox
    forkIO $ writer mailbox

    -- wait
    threadDelay 1000000000
