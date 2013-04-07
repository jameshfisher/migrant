module Database.Migrant.Frontend.Mock
  ( frontendMock
  ) where

import Control.Monad.State (State)

import Database.Migrant.Types.Message (Message(..))
import Database.Migrant.Types.Mock (Action (..), MockConnection)
import Database.Migrant.Backend.Mock (lg)

frontendMock :: Message -> State MockConnection ()
frontendMock = lg . ActionMessage
