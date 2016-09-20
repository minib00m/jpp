module Main where
import           Evaluator
import           Parser
import           System.Environment
import           Text.ParserCombinators.Parsec
import           TypeChecker

main :: IO ()
main = do
  filename <- getArgs
  arg <- case filename of
    [] -> getContents
    fn:_  -> readFile fn
  case parse program "pascal-like-interpreter" arg of
    (Left err) -> print err
    (Right res) -> do
        tt <- goTC res
        case fst tt of
          (Left e) -> putStrLn e
          (Right _) -> do
            co <- checkOut res
            case fst co of
              (Left e) -> putStrLn e
              (Right _) -> return ()
