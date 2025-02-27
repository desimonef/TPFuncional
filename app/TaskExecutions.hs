module TaskExecutions (execute) where

import System.Process (readProcess, readCreateProcess, proc)
import System.FilePath (takeExtension)
import System.Directory (doesFileExist)
import Control.Exception (catch, SomeException)

executeScript :: FilePath -> IO (Either String String)
executeScript path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left "El archivo no existe"
        else case takeExtension path of
            ".py"  -> run "python3" path
            ".js"  -> run "node" path
            ".sh"  -> run "bash" path
            ".bat" -> run "cmd.exe" ("/c " ++ path) -- Soporte para Windows
            _      -> return $ Left "Formato de archivo no soportado"

run :: String -> String -> IO (Either String String)
run interpreter script = catch
    (Right <$> readProcess interpreter [script] "")
    (\e -> return $ Left $ "Error ejecutando " ++ script ++ ": " ++ show (e :: SomeException))

execute :: String -> IO ()
execute script = do
    result <- executeScript script -- Prueba con un script Python
    case result of
        Left err  -> putStrLn $ "Error: " ++ err
        Right out -> putStrLn $ "Salida: " ++ out
