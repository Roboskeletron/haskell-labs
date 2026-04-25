data Exam = Exam {
  name :: String,
  score :: Int
} deriving (Show)

data EduForm = Contract | Budget deriving (Show)

data Student = Student {
  fullName :: String,
  course :: Int,
  group :: Int,
  eduForm :: EduForm,
  scores :: [Exam]
} deriving (Show)


avgScore :: String -> [Student] -> Double
avgScore _ [] = 0.0
avgScore examName students = totalScore / studentCount where
  exams = concatMap scores students
  filteredExams = filter (\e -> name e == examName) exams
  studentScores = map (fromIntegral . score) filteredExams
  (totalScore, studentCount) = foldl (\(total, count) score -> (total + score, count + 1)) (0, 0) studentScores

bestScore :: Int -> [Student] -> (String, Double)
bestScore g students = bestAvgScore where
  studentsInGroup = filter (\s -> group s == g) students
  scoreNames = map name (scores $ head studentsInGroup)
  avgScores = map (\n -> (n, avgScore n studentsInGroup)) scoreNames
  maxScore (n1, s1) (n2, s2) = if s1 >= s2 then (n1, s1) else (n2, s2)
  bestAvgScore = foldl maxScore ("", 0) avgScores
  

main = do
  let student1Scores = [Exam "Math" 3, Exam "English" 4]
  let student2Scores = [Exam "Math" 5, Exam "English" 5]
  let student1 = Student "Ivanov Ivan Ivanovich" 1 1 Budget student1Scores
  let student2 = Student "Petrov Peter Petrovich" 1 1 Budget student2Scores

  print student1
  print $ avgScore "Math" [student1, student2]
  print $ avgScore "English" [student1, student2]
  print $ bestScore 1 [student1, student2]