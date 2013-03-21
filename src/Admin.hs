module Admin where

import Import

deleteUser :: UserId -> Handler ()
deleteUser userId = runDB $ do
    -- Set their userId to Nothing in notification logs.
    updateWhere [NotificationLogUserId ==. Just userId]
                [NotificationLogUserId =. Nothing]
    -- Get list of requests.
    reqIds <- fmap (map entityKey) $
        selectList [SectionRequestUserId ==. userId] []
    -- Delete requests.
    mapM_ delete reqIds
    -- Delete email verifications.
    deleteWhere [EmailVerificationUserId ==. userId]
    -- Delete the user.
    delete userId
