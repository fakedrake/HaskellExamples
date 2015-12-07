import Control.Concurrent

writeForever :: String -> IO ()
writeForever ln = do
  putStrLn ln
  threadDelay 1000000
  writeForever ln

main = do
  forkIO $ writeForever "hello"
  forkIO $ writeForever "oops"
  writeForever "bye"
