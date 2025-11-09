-- Main.hs
import Data.Char (isAlpha, isDigit, isSpace)

-- Типи
type Symbol = Char
type Word = String
type Sentence = String
type Punctuation = Char

main :: IO ()
main = do
    putStrLn "Введіть назву файлу:"
    fileName <- getLine
    content <- readFile fileName

    let cleaned = normalizeSpaces content
        sentences = splitToSentences cleaned
        processed = map swapFirstLast sentences
        result = concat processed

    let outFile = takeWhile (/= '.') fileName ++ "_output.txt"
    writeFile outFile result
    putStrLn "Обробку завершено. Результат записано у файл *_output.txt"

-- Замінює табуляції й кілька пробілів одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map (\c -> if isSpace c then ' ' else c)

-- Розділяє текст на речення, зберігаючи розділові знаки
splitToSentences :: String -> [Sentence]
splitToSentences "" = []
splitToSentences xs =
    let (sent, rest) = break (`elem` ".!?") xs
    in case rest of
        []     -> [sent]
        (p:rs) -> (sent ++ [p]) : splitToSentences rs

-- Міняє місцями перше й останнє слово в реченні (якщо більше одного слова)
swapFirstLast :: Sentence -> Sentence
swapFirstLast sentence =
    case (findFirstWord sentence, findLastWord sentence) of
        (Just (fStart, fLen), Just (lStart, lLen))
            | fStart == lStart -> sentence
            | otherwise ->
                let prefix = take fStart sentence
                    restAfterFirst = drop fStart sentence
                    firstWord = take fLen restAfterFirst
                    afterFirst = drop fLen restAfterFirst
                    middleLen = lStart - (fStart + fLen)
                    (middle, restAfterMiddle) = splitAt middleLen afterFirst
                    lastWord = take lLen restAfterMiddle
                    suffix = drop lLen restAfterMiddle
                in prefix ++ lastWord ++ middle ++ firstWord ++ suffix
        _ -> sentence

findFirstWord :: String -> Maybe (Int, Int)
findFirstWord = go 0
  where
    go _ [] = Nothing
    go idx (c:cs)
        | isWordChar c =
            let wordTail = takeWhile isWordChar cs
            in Just (idx, 1 + length wordTail)
        | otherwise = go (idx + 1) cs

findLastWord :: String -> Maybe (Int, Int)
findLastWord = go Nothing 0
  where
    go lastSeen _ [] = lastSeen
    go lastSeen idx (c:cs)
        | isWordChar c =
            let wordTail = takeWhile isWordChar cs
                len = 1 + length wordTail
                rest = dropWhile isWordChar cs
            in go (Just (idx, len)) (idx + len) rest
        | otherwise = go lastSeen (idx + 1) cs

isWordChar :: Char -> Bool
isWordChar c = isAlpha c || isDigit c || c `elem` "'-’"