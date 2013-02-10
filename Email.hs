--import Data.Text (Text)
import GHC.IO.Handle
import System.Exit
import System.Process

sendmail :: String -> IO ()
sendmail message = do
    (Just hIn, _, _, pHandle) <- createProcess $ (proc
        "/usr/sbin/sendmail" ["-t"]) { std_in = CreatePipe }
    hPutStr hIn message
    hClose hIn
    exitCode <- waitForProcess pHandle
    case exitCode of
        ExitSuccess -> return ()
        _ -> error $ "sendmail exited with error code " ++ show exitCode
