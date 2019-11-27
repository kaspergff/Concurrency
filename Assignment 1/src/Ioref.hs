

module IORef where

    countIORef :: Int -> [Int] -> Int -> IO ()
    countIORef threads list modulo = do
    lock <- newIORef Unlocked
    counter <- newIORef 0 
    makeForkIORef counter lock threads list modulo
    threadDelay 1000
    c <- readIORef counter
    putStrLn (show c)

    makeForkIORef :: IORef Int -> IORef Lock -> Int -> [Int] -> Int -> IO ()
    makeForkIORef _  _ 0 _ _ = return ()
    makeForkIORef c lock 1 ints modulo  = do
    _ <- forkIO $ do
        let count = countMode1 ints modulo
        interlocked lock (action c count)
    return ()
    makeForkIORef c lock n ints modulo = do
    _ <- forkIO $ do
        let count = countMode (getListPart n ints) modulo
        interlocked lock (action c count)
    makeForkIORef c lock (n-1) (ints \\ (getListPart n ints)) modulo

    action :: Num a => IORef a -> a -> IO ()
    action c count = do 
    old <- readIORef c
    writeIORef c (old + count) 