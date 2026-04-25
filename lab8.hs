import Control.Monad.Reader

data Config = Config 
    { nTeachers :: Int
    , mStudents :: Int 
    }

getStudent :: Int -> String
getStudent k = ["Ivanov", "Sidorov", "Kuznetsov"] !! k

getTeacher :: Int -> String
getTeacher k = ["Petrov", "Smirnov"] !! k

getPhone :: Int -> String
getPhone i = "8900" ++ show (1110000 + i)

getInfo :: Int -> Reader Config String
getInfo i = do
    config <- ask
    let n = nTeachers config
    
    let phone = getPhone i
    
    if i < n
        then do
            let name = getTeacher i
            return $ show i ++ ". Teacher: " ++ name ++ ", phone: " ++ phone
        else do
            let name = getStudent (i - n)
            return $ show i ++ ". Student: " ++ name ++ ", phone: " ++ phone

main :: IO ()
main = do
    let cfg = Config { nTeachers = 2, mStudents = 3 }
    
    let info1 = runReader (getInfo 1) cfg
    putStrLn info1 -- Выведет: Teacher: Smirnov, phone: 89001110001
    
    let info4 = runReader (getInfo 4) cfg
    putStrLn info4 -- Выведет: Student: Kuznetsov, phone: 89001110004