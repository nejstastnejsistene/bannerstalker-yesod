module Email where

import Prelude
import Data.Text
import Network.Mail.Mime

adminAddr, noreplyAddr :: Address
adminAddr = Address (Just "Bannerstalker") "admin@bannerstalker.com"
noreplyAddr = Address (Just "Bannerstalker") "noreply@bannerstalker.com"

mySendmail :: Mail -> IO ()
mySendmail message = do
    let Address mName email = mailFrom message
        flags = ["-tf", unpack email] ++
            case mName of
                Just name -> ["-F", unpack name]
                _ -> []
    renderSendMailCustom "/usr/sbin/sendmail" flags message
