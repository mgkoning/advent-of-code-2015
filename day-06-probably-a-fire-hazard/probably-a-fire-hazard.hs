import Data.Function
import Data.List
import qualified Data.Map.Strict as Map

data LightState = On | Off deriving Show

toggle :: LightState -> LightState
toggle On = Off
toggle Off = On

makeInitialState :: Map.Map (Int, Int) LightState
makeInitialState = Map.fromList [((x, y), Off) | x <- [0..999], y <- [0..999]]

readInstruction ("turn":state:from:"through":through:[]) = 0
readInstruction ("toggle":from:"through":through:[]) = 0