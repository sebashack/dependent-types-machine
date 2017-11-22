module Interpreter where

import Servant
import Control.Monad.IO.Class
import Data.Text (Text)
import API
import Operations
import DoorTypes (evalDoor)


ringBellR :: Text -> Handler Text
ringBellR nm = liftIO $ evalDoor $ ringBell nm

ringBellTwiceR :: Text -> Text -> Handler Text
ringBellTwiceR nm1 nm2 = liftIO $ evalDoor $ ringBellTwice nm1 nm2

askSecretR :: Text -> Handler (Maybe Int)
askSecretR nm = liftIO $ evalDoor $ askSecret nm

handlers :: Server DoorAPI
handlers = ringBellR
      :<|> ringBellTwiceR
      :<|> askSecretR

apiProxy :: Proxy DoorAPI
apiProxy = Proxy
