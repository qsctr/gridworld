{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.ViewState
import Graphics.Gloss.Interface.Pure.Game

import System.Directory
import System.Process

import Text.Read (readMaybe)

type SensorInput              = [Bool]
type FeatureVector            = [Bool]
type Location                 = Point
type Action                   = Point
type Locations                = S.Set Location
type LocationsEditingFunction = Location -> Locations -> Locations
type ErrorMessage             = String

data Mode = Viewing | EditingWalls | EditingAgents | EditingTreasures deriving (Enum, Bounded)

instance Show Mode where
    show Viewing          = "viewing mode"
    show EditingWalls     = "editing walls mode"
    show EditingAgents    = "editing agents mode"
    show EditingTreasures = "editing treasures mode"

data State = State
    { walls       :: Locations
    , agents      :: Locations
    , treasures   :: Locations
    , playing     :: Bool
    , viewState   :: ViewState
    , windowW     :: Int
    , windowH     :: Int
    , mode        :: Mode
    , leftButton  :: Bool
    , rightButton :: Bool }

main :: IO ()
main = do
    mapM_ putStrLn
        [ "=== GRIDWORLD ==="
        , ""
        , "Controls:"
        , intercalate "\n" $ map (\m -> show (fromEnum m) ++ ": switch to " ++ show m)
            [minBound :: Mode .. maxBound :: Mode]
        , show Viewing ++ ":"
        , "    Pan around: drag mouse with left click, arrow keys"
        , "    Zoom in: drag mouse with right click, scroll up, page up key, '=' key"
        , "    Zoom out: drag mouse with right click, scroll down, page down key, '-' key"
        , intercalate "\n" $ map (\m -> show m ++ ":\n    Add: left click or drag\n\
            \    Remove: right click or drag") [EditingWalls, EditingAgents, EditingTreasures]
        , ""
        , "Map instructions:"
        , "A text file can be parsed as a map with the following characters:"
        , "    Wall: " ++ show wallChar
        , "    Agent: " ++ show agentChar
        , "    Treasure: " ++ show treasureChar
        , "    Empty square: " ++ show emptyChar
        , "The lines do not have to be the same length."
        , "" ]
    let getState = do
            putStrLn "Enter map file to read. If no map file is provided an empty map will be used."
            mapFile <- getLine
            if all isSpace mapFile then return initialState else do
                exists <- doesFileExist mapFile
                if exists
                    then do
                        result <- parseMap <$> readFile mapFile
                        case result of
                            Left err -> do
                                mapM_ putStrLn
                                    [ "Parsing " ++ mapFile ++ " found the following errors:"
                                    , err
                                    , "Edit the file, then try again." ]
                                getState
                            Right state -> return $ fitWindow state
                    else do
                        putStrLn $ "File " ++ mapFile ++ " does not exist. Try again."
                        getState
    state <- getState
    play (InWindow "GridWorld" (windowW state, windowH state) (0, 0)) black 2 state draw handleEvent update

initialState :: State
initialState = State
    { walls       = S.empty
    , agents      = S.empty
    , treasures   = S.empty
    , playing     = False
    , viewState   = (viewStateInitWithConfig initialViewStateConfig)
        { viewStateViewPort = viewPortInit { viewPortScale = 15 } }
    , windowW     = 800
    , windowH     = 600
    , mode        = Viewing
    , leftButton  = False
    , rightButton = False }

initialViewStateConfig :: CommandConfig
initialViewStateConfig =
    [ (CTranslate,
        [ (MouseButton LeftButton,  Nothing) ])
    , (CScale,
        [ (MouseButton RightButton, Nothing) ])
    , (CBumpZoomOut,
        [ (MouseButton WheelUp,     Nothing)
        , (SpecialKey KeyPageUp,    Nothing)
        , (Char '=',                Nothing) ])
    , (CBumpZoomIn,
        [ (MouseButton WheelDown,   Nothing)
        , (SpecialKey KeyPageDown,  Nothing)
        , (Char '-',                Nothing) ])
    , (CBumpLeft,
        [ (SpecialKey KeyRight,     Nothing) ])
    , (CBumpRight,
        [ (SpecialKey KeyLeft,      Nothing) ])
    , (CBumpUp,
        [ (SpecialKey KeyDown,      Nothing) ])
    , (CBumpDown,
        [ (SpecialKey KeyUp,        Nothing) ]) ]

updateLocationsForMode :: Mode -> (Locations -> Locations) -> State -> Maybe State
updateLocationsForMode Viewing          _ _     = Nothing
updateLocationsForMode EditingWalls     f state = Just (state { walls = f $ walls state })
updateLocationsForMode EditingAgents    f state = Just (state { agents = f $ agents state })
updateLocationsForMode EditingTreasures f state = Just (state { treasures = f $ treasures state })

wallChar, agentChar, treasureChar, emptyChar :: Char
wallChar     = 'x'
agentChar    = 'o'
treasureChar = '$'
emptyChar    = ' '

parseMap :: String -> Either ErrorMessage State
parseMap = fmap toState . combine . map (foldr add start . pair) . zip [0, -1 ..] . lines
    where
        start = Right (S.empty, S.empty, S.empty)
        pair (y, xs) = map (\x -> ((fromIntegral x, y), xs !! x)) [0 .. length xs - 1]
        add ((x, y), c) (Right (wls, ags, trs))
            | toLower c == wallChar     = Right (S.insert (x, y) wls, ags, trs)
            | toLower c == agentChar    = Right (wls, S.insert (x, y) ags, trs)
            | toLower c == treasureChar = Right (wls, ags, S.insert (x, y) trs)
            | toLower c == emptyChar    = Right (wls, ags, trs)
            | otherwise                 = Left $ "Illegal character '" ++ [c]
                ++ "' at line " ++ show (round (-y + 1)) ++ ", col " ++ show (round (x + 1))
        add a (Left msg)
            | Left newMsg <- add a start = Left $ msg ++ "\n" ++ newMsg
            | otherwise                  = Left msg
        combine xs
            | any isLeft xs = Left $ intercalate "\n" $ lefts xs
            | otherwise     = fmap unzip3 $ sequence xs
        toState (wls, ags, trs) = initialState
            { walls     = S.unions wls
            , agents    = S.unions ags
            , treasures = S.unions trs }

fitWindow :: State -> State
fitWindow state@(State { walls, agents, treasures, viewState, windowW, windowH }) = state
    { viewState = viewState
        { viewStateViewPort = (viewStateViewPort viewState)
            { viewPortScale     = min (fromIntegral windowW / mapWidth) (fromIntegral windowH / mapHeight)
            , viewPortTranslate = ((-mapWidth + 1) / 2 , (mapHeight - 1) / 2) } } }
    where
        everything = S.unions [walls, agents, treasures]
        xs         = S.map fst everything
        ys         = S.map snd everything
        mapWidth   = S.findMax xs - S.findMin xs + 1
        mapHeight  = S.findMax ys - S.findMin ys + 1

sense :: SensorInput -> FeatureVector
sense = map or . chunksOf 2

decide :: FeatureVector -> Action
decide [True, True, True, True] = (0, 0)  -- don't move
decide [True, False, _, _]      = (1, 0)  -- east
decide [_, True, False, _]      = (0, -1) -- south
decide [_, _, True, False]      = (-1, 0) -- west
decide _                        = (0, 1)  -- north

act :: Location -> Action -> Location
act (x, y) (a, b) = (x + a, y + b)

update :: Float -> State -> State
update _ state
    | not (playing state) = state
    | otherwise           = state { agents = S.map updateAgent $ agents state }
    where
        updateAgent (x, y)
            | S.member (x, y) (treasures state) = (x, y)
            | otherwise = act (x, y) $ decide $ sense $ map (`S.member` (walls state))
                [ (x, y + 1)
                , (x + 1, y + 1)
                , (x + 1, y)
                , (x + 1, y - 1)
                , (x, y - 1)
                , (x - 1, y - 1)
                , (x - 1, y)
                , (x - 1, y + 1) ]

draw :: State -> Picture
draw state = applyViewPortToPicture (viewStateViewPort $ viewState state) $ pictures $
    map drawTreasure (S.toList $ treasures state) ++
    map drawAgent (S.toList $ agents state) ++
    map drawWall (S.toList $ walls state)
    where
        drawWall (x, y)     = color white  $ translate x y $ rectangleSolid 1 1
        drawAgent (x, y)    = color blue   $ translate x y $ circleSolid 0.5
        drawTreasure (x, y) = color orange $ translate x y $ rectangleSolid 1 1

handleEvent :: Event -> State -> State

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state@(State { playing }) =
    state { playing = not playing }

handleEvent (EventKey (Char 'f') Down _ _) state = fitWindow state

handleEvent (EventKey (Char c) Down _ _) state
    | Just n <- readMaybe [c]
    , n >= fromEnum (minBound :: Mode)
    , n <= fromEnum (maxBound :: Mode) = state { mode = toEnum n }

handleEvent event@(EventKey key _ _ _) state@(State { viewState, mode = Viewing })
    | key `elem` (map fst $ concat $ map snd $ initialViewStateConfig) =
        state { viewState = updateViewStateWithEvent event viewState }

handleEvent event@(EventMotion _) state@(State { viewState =
    viewState@(ViewState { viewStateTranslateMark, viewStateScaleMark }), mode = Viewing })
        | isJust viewStateTranslateMark || isJust viewStateScaleMark =
            state { viewState = updateViewStateWithEvent event viewState }

handleEvent (EventKey (MouseButton LeftButton) Up _ _) state = state { leftButton = False }

handleEvent (EventKey (MouseButton RightButton) Up _ _) state = state { rightButton = False }

handleEvent (EventKey (MouseButton button) Down _ pos) state
    | Just function <- functionForButton button =
        let state' = fromJust $ editLocations function state pos
        in  case button of
                LeftButton  -> state' { leftButton = True }
                RightButton -> state' { rightButton = True }
                _           -> state'

handleEvent (EventMotion pos) state@(State { leftButton, rightButton })
    | leftButton && not rightButton = fromJust $
        editLocations (fromJust $ functionForButton LeftButton) state pos
    | rightButton && not leftButton = fromJust $
        editLocations (fromJust $ functionForButton RightButton) state pos
    | otherwise                     = state

handleEvent (EventResize (w, h)) state = state { windowW = w, windowH = h }

handleEvent _ state = state

editLocations :: LocationsEditingFunction -> State -> Point -> Maybe State
editLocations function state@(State { mode, viewState }) pos = updateLocationsForMode mode
    (function (fromIntegral $ round x, fromIntegral $ round y)) state
    where
        (x, y) = invertViewPort (viewStateViewPort viewState) pos

functionForButton :: MouseButton -> Maybe LocationsEditingFunction
functionForButton LeftButton  = Just S.insert
functionForButton RightButton = Just S.delete
functionForButton _           = Nothing