-- author: Monika Michaluk
-- nr indeksu: 395135
module PPrint where
import Data.List

showNewLine :: ShowS
showNewLine = showChar '\n'

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS showNewLine
pprH = intercalateS (showChar ' ')

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep = foldl (.) id . intersperse sep

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith s l = intercalateS showNewLine (map s l)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($ "")
