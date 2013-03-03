module Email where

import Prelude
import Data.Text
import Network.Mail.Mime

adminAddr, infoAddr :: Address
adminAddr = Address (Just "Bannerstalker") "admin@bannerstalker.com"
infoAddr = Address (Just "Bannerstalker") "info@bannerstalker.com"

mySendmail :: Mail -> IO ()
mySendmail message = do
    let Address mName email = mailFrom message
        flags = ["-tf", unpack email] ++
            case mName of
                Just name -> ["-F", unpack name]
                _ -> []
    renderSendMailCustom "/usr/sbin/sendmail" flags message
