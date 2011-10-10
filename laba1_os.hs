

import Data.Either
import Data.Maybe
import Data.List
import Data.Char

data Failure = Failure Int Int deriving (Show)                       
type Failable = Either Failure
--type State22 = (Char,Int) -> (Failable Action, State22)
data State = State ((Char,Int) -> (Failable Action, State))

data Action = Save Char
            | NewSave Char
            | End
            | Nil



parser :: State -> [(Char,Int)] -> Failable [String]
parser s0 cs = result
    where result = case (failures results) of
                        ((Failure e n):_)  -> failure e n
                        _      -> successed $ successings results
          (as,ss) = unzip $ zipWith aut cs (s0:ss)
          results = catMaybes maybeLiterals
          maybeLiterals = snd $ mapAccumL act "" as
            

act :: String -> Failable Action -> (String, Maybe (Failable String))
act buff = either f s
    where f (Failure e n) = (buff, Just (failure e n))
          s (Save c) = (buff ++ [c],Nothing)
          s (NewSave c) = ([c],Just $ successed buff)
          s End = (buff,Just $ successed buff)
          s Nil = (buff,Nothing)

aut :: (Char,Int) -> State -> (Failable Action, State)
aut cn (State s) = s cn

s0 (c,n) | isAlpha c = ((successed (Save c)),State s1)
         | otherwise = (failure 0 n, State mfail)
s1 (c,n) | c == '@' = ((successed Nil),State s2)
         | isAlpha c = ((successed (Save c)),State s1)
         | otherwise = (failure 1 n, State mfail)
s3 (c,n) | c == '\n' = ((successed End), State end)
         | isSpace c = ((successed Nil),State s4)
         | isAlpha c = ((successed (Save c)),State s3)
         | otherwise = (failure 3 n,State mfail)
s2 (c,n) | isAlpha c = ((successed (NewSave c)),State s3)
         | otherwise = (failure 2 n,State mfail)
s4 (c,n) | c == '\n' = ((successed End), State end)
         | isSpace c = ((successed Nil), State s4)
         | otherwise = (failure 4 n,State mfail)
mfail (c,n) = (failure 4 n,State mfail)
end = mfail


email = "name@domain"
cs = indexIt email
indexIt :: [Char] -> [(Char,Int)]
indexIt cs = zip cs [1..]

successed x = Right x
failure c i = (Left (Failure c i))
successings = rights
failures = lefts

main = do
    s <- readFile "in.txt"
    putStrLn $ show $ parser (State s0) (indexIt s)
