import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Cont

-- We illustrate common control flow manipulation mechanisms using a simple
-- example. The user enters a sequence on the console. The sequence is "x"
-- followed by a newline and then "y" followed by newline and then "z" followed
-- by newline. If the sequence is correct the program ends otherwise it prints
-- an error wherever the expected character is not entered.
--
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

-- terminate the control flow at any point
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

-- terminate the control flow at any point with an error return
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

-- Continuation monad without CallCC looks like just plain IO monad.
getSequenceCont1 :: ContT r IO (Either String ())
getSequenceCont1 = do
    r <- liftIO getLine
    if (r /= "x")
    then return $ Left $ "Expecting x got: " ++ r
    else do
        r <- liftIO getLine
        if (r /= "y")
        then return $ Left $ "Expecting y got: " ++ r
        else do
            r <- liftIO getLine
            if (r /= "z")
            then return $ Left $ "Expecting z got: " ++ r
            else return $ Right ()

-- CallCC is the goto/setjmp/longjmp equivalent
-- Allows us to manipulate the control flow in arbitrary ways
getSequenceCont2 :: ContT r IO (Either String ())
getSequenceCont2 =
    callCC $ \exit -> do
        r <- liftIO getLine
        if (r /= "x") then do
            exit $ Left $ "Expecting x got: " ++ r
        else return $ Right ()

        r <- liftIO getLine
        if (r /= "y") then do
            exit $ Left $ "Expecting y got: " ++ r
        else return $ Right ()

        r <- liftIO getLine
        if (r /= "z") then do
            exit $ Left $ "Expecting z got: " ++ r
        else return $ Right ()

mainCont1 :: IO ()
mainCont1 = do
    r <- runContT getSequenceCont1 return
    case r of
        Right _ -> putStrLn "Right"
        Left s  -> putStrLn s

mainCont2 :: IO ()
mainCont2 = do
    r <- runContT getSequenceCont2 return
    case r of
        Right _ -> putStrLn "Right"
        Left s  -> putStrLn s

main = mainCont2
