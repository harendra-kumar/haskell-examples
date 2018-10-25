import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

getSequence :: IO Bool
getSequence = do
    r <- getLine
    if r /= "x"
    then return False
    else do
        r <- getLine
        if r /= "y"
        then return False
        else do
            r <- getLine
            if r /= "z"
            then return False
            else return True

mainIO :: IO ()
mainIO = do
    r <- getSequence
    if r
    then putStrLn "Right"
    else putStrLn "Wrong"

getSequenceMaybe :: MaybeT IO ()
getSequenceMaybe = do
    r <- liftIO getLine
    when (r /= "x") mzero

    r <- liftIO getLine
    when (r /= "y") mzero

    r <- liftIO getLine
    when (r /= "z") mzero

mainMaybe :: IO ()
mainMaybe = do
    r <- runMaybeT getSequenceMaybe
    case r of
        Just _ -> putStrLn "Right"
        Nothing -> putStrLn "Wrong"

getSequenceEither :: ExceptT String IO ()
getSequenceEither = do
    r <- liftIO getLine
    when (r /= "x") $ throwE $ "Expecting x got: " ++ r

    r <- liftIO getLine
    when (r /= "y") $ throwE $ "Expecting y got: " ++ r

    r <- liftIO getLine
    when (r /= "z") $ throwE $ "Expecting z got: " ++ r

mainEither :: IO ()
mainEither = do
    r <- runExceptT getSequenceEither
    case r of
        Right _ -> putStrLn "Right"
        Left s  -> putStrLn s

main = mainEither
