main = do {
            sum <- readInput 0;
            putStrLn $ "The final sum is: " ++ show sum
          }
readInput :: Int -> IO Int;
readInput currentSum = do {
                            inp <- getLine;
                            if inp == "zero" then
                              readInput currentSum;
                            else if inp == "one" then
                              readInput (currentSum + 1);
                            else if inp == "two" then
                              readInput (currentSum + 2);
                            else if inp == "three" then
                              readInput (currentSum + 3);
                            else if inp == "four" then
                              readInput (currentSum + 4);
                            else if inp == "five" then
                              readInput (currentSum + 5);
                            else if inp == "six" then
                              readInput (currentSum + 6);
                            else if inp == "seven" then
                              readInput (currentSum + 7);
                            else if inp == "eight" then
                              readInput (currentSum + 8);
                            else if inp == "nine" then
                              readInput (currentSum + 9);
                            else if inp == "sum" then do
                              putStrLn $ "The current sum is: " ++ show currentSum
                              readInput currentSum;
                            else return currentSum
                          }