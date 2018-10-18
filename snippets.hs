bree = forever $ do
  putStr "dev \"US-122 MKII MIDI\" on 60 127"
  putStrLn ""
  threadDelay (100*1000) -- value in microseconds

geee a = do
  putStr $ "dev \"US-122 MKII MIDI\" on " ++ (show a) ++ " 127"
  putStrLn ""
  threadDelay (500*1000) -- value in microseconds
  geee $ a + 1
