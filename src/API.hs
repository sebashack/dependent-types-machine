{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Text (Text)
import Servant

type DoorAPI = "doorMachine" :>
  (
       "ringBell" :> Capture "person" Text :> Post '[JSON] Text

  :<|> "ringBellTwice" :> Capture "person1" Text :> Capture "person2" Text :> Post '[JSON] Text

  :<|> "askSecret" :> Capture "name" Text :> Get '[JSON] (Maybe Int)
  )
