import Data.Either
import Data.Maybe
import Data.List
import Data.Char

data Failure = Failure Int Int deriving (Show)                       
type Failable = Either Failure
type Automat = State -> State 
data State = State ((Char,Int) -> (Failable Action, State))
           

data Action = Save Char
            | NewSave Char
            | SaveEnd Char
            | End
            | Nil

save c = successed $ Save c
newSave c = successed $ NewSave c
saveEnd c = successed $ SaveEnd c
end = successed $ End
nil = successed $ Nil

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
          s (SaveEnd c) = ([], Just $ successed (buff++[c]))
          s End | null buff = ("", Nothing)
                | otherwise = ("",Just $ successed buff)
          s Nil = (buff,Nothing)



aut :: (Char,Int) -> State -> (Failable Action, State)
aut cn (State s) = s cn
-------------------------------------------------------------------------------
getSpacesAutomat :: Automat
getSpacesAutomat (State sEnd) = State s0
    where
        s0 (c,n) | isSpace c = (nil, State s0)
                 | otherwise = sEnd (c,n)
                 
getEOFAutomat :: Automat
getEOFAutomat (State sEnd) = State s0
    where
        s0 (c,n) | isEOF c   = (end, nullState)
                 | otherwise = sEnd (c,n)

-------------------------------------------------------------------------------
getSomethingAutomat :: Automat
getSomethingAutomat (State sEnd) = spaces_comment_or_eof
    where
        s0 (c,n) | isSpace c = (end, spaces_comment_or_eof)
                 | otherwise = sEnd (c,n)
        spaces_comment_or_eof = getSpacesAutomat $ getEOFAutomat $ getLineCommentAutomat $  getCCommentAutomat $ State s0
-------------------------------------------------------------------------------

getCCommentAutomat :: Automat
getCCommentAutomat (State sEnd) = State s0
    where
	s0 (c,n) | c == '/' = (save c, State s1)
                 | otherwise = sEnd (c,n)
        s1 (c,n) | c == '*' = (save c, State s2)
                 | otherwise = sFail 1 n
        s2 (c,n) | c == '*' = (save c, State s3)
                 | isEOF c = sFail 2 n
                 | otherwise = (save c, State s2) 
        s3 (c,n) | c == '*' = (save c, State s3)
                 | c == '/' = (save c, State s0)
                 | otherwise = (save c, State s2)

getLineCommentAutomat :: Automat
getLineCommentAutomat (State sEnd) = State s0 
    where 
        s0 (c,n) | c == '#' = (save c, State s1)
                 | otherwise = sEnd (c,n)
        s1 (c,n) | c == '\n' = (end, State s0)
                 | isEOF c = sFail 1 n
                 | otherwise = (save c, State s1)
------------------------------------------------------------------------------
getStringAutomat :: Automat
getStringAutomat (State sEnd) = State s0
    where
        s0 (c,n) | c == '"'  = (save c, State s1)
                 | otherwise = sEnd (c,n)
        s1 (c,n) | c == '\\' = (save c, State s2)
                 | c == '"'  = (saveEnd c, sEnd)
                 | otherwise = (save c, State s1)
        s2 (c,n) | c == 't'
                 || c == 'n'
                 || c == '\\'
                 || c == '"' = (save c, State s1)
                 | otherwise = sFail 2 n
-------------------------------------------------------------------------------        
getMailsAutomat :: Automat
getMailsAutomat (State sEnd) = spaces_mail_or_eof
    where
        s0 (c,n) | isSpace c = (end, spaces_mail_or_eof)
                 | otherwise = sEnd (c,n)

        spaces_mail_or_eof = getSpacesAutomat $ getEOFAutomat $ getMailAutomat $ State s0

getMailAutomat :: Automat
getMailAutomat (State sEnd) = name
    where
        name = getNameAutomat (State s0)
        domain = getDomainAutomat (State sEnd)
        s0 (c,n) | c == '@' = (save c, domain)
                 | otherwise = sFail 3 n
        
getNameAutomat :: Automat
getNameAutomat (State sEnd) = (State s0)
    where
        s0 (c,n) |  isAlpha c 
                 || isDigit c
                 || c == '_' = (save c,State s1)
                 | otherwise = sFail 0 n
        s1 (c,n) |  isAlpha c 
                 || isDigit c
                 || c == '_' = (save c,State s1)
                 | otherwise = sEnd (c,n)
             
getDomainAutomat :: Automat             
getDomainAutomat (State sEnd) = (State s0)        
    where
        s0 (c,n) |  isAlpha c 
                 || isDigit c = (save c, State s1)
                 | otherwise = sFail 1 n
        s1 (c,n) |  isAlpha c
                 || isDigit c = (save c, State s1)
                 | c == '-' = (save c, State s2)
                 | c == '.' = (save c, State s0)
                 | otherwise = sEnd (c,n)
        s2 (c,n) | c == '-' = (save c, State s2)
                 |  isAlpha c
                 || isDigit c = (save c, State s1)
                 | otherwise = sFail 1 n
--------------------------------------------------------------------------------        
getEndAutomat :: Automat
getEndAutomat f = getSpacesAutomat $ getEOFAutomat f
   
sFail s n = (failure s n, State (\ (c,n) -> sFail s n))

nullState = (State (\_-> sFail (-1) (-1)))

email = "name@domain"
cs = indexIt email
indexIt :: [Char] -> [(Char,Int)]
indexIt cs = zip cs [1..]

successed x = Right x
failure c i = (Left (Failure c i))
successings = rights
failures = lefts

isEOF c = c == '\SUB'

main = do
    s <- readFile "in.txt"
    putStrLn $ show $ indexIt (s++"\SUB")
    putStrLn $ show $ parser (getSomethingAutomat (getEndAutomat nullState)) (indexIt (s++"\SUB"))
