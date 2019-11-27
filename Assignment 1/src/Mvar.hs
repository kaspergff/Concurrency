

module Mvar where

    countMVar :: Int -> [Int] -> Int -> IO ()
    countMVar threads list modulo = do
    counter <- newMVar 0 
    makeForkMVar counter threads list modulo
    threadDelay 1000
    c <- takeMVar counter
    putStrLn (show c)

    makeForkMVar :: MVar Int -> Int -> [Int] -> Int -> IO ()
    makeForkMVar _ 0 _ _ = return ()
    makeForkMVar c 1 ints modulo  = do
    _ <- forkIO $ do 
        let count = countMode1 ints modulo
        old <- takeMVar c
        putMVar c (old + count)
    return ()
    makeForkMVar c n ints modulo  = do
    _ <- forkIO $ do
        let count = countMode (getListPart n ints) modulo
        old <- takeMVar c
        putMVar c (old + count)
    makeForkMVar c (n-1) (ints \\ (getListPart n ints)) modulo
    
    mVarList :: Int -> [Int] -> Int -> IO ()
    mVarList threads list modulo = do
    writelock <- newMVar 1
    mVarListFork threads list modulo writelock
    return()

    mVarListFork :: Int -> [Int] -> Int -> MVar Int -> IO ()
    mVarListFork 0 _ _ _ = return ()
    mVarListFork 1 ints modulo right = do
    _ <- forkIO $ do 
        listMode1 ints modulo right
    return ()
    mVarListFork n ints modulo right  = do
    _ <- forkIO $ do
        listMode (getListPart n ints) modulo right
    mVarListFork (n-1) (ints \\ (getListPart n ints)) modulo right