{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}

module Operations where

import DoorTypes
import Data.Text (Text)

ringBell :: Text -> DoorCmd 'DoorClosed 'DoorClosed Text
ringBell = RingBell

ringBellTwice :: Text -> Text -> DoorCmd 'DoorClosed 'DoorClosed Text
ringBellTwice nm1 nm2 = RingBell nm1 |> RingBell nm2

askSecret :: Text -> DoorCmd 'DoorClosed 'DoorClosed (Maybe Int)
askSecret nm =
      Check nm
  |>> Open
  |>> Close
