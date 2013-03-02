module Email where

import Data.Text
import Network.Mail.Mime

-- Note to self:
-- Setup AmazonSES SPF and DKIM
mySendmail :: Mail -> IO ()
mySendmail message = do
    let Address mName email = mailFrom message
        flags = ["-tf", unpack email] ++
            case mName of
                Just name -> ["-F", unpack name]
                _ -> []
    putStrLn $ show flags
    renderSendMailCustom "/usr/sbin/sendmail" flags message
