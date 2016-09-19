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
import Graphics.Gloss.Interface.IO.Game

import System.Directory
import System.Process

import Text.Read (readMaybe)

type SensorInput   = [Bool]
type FeatureVector = [Bool]
type Location      = (Int, Int)
type Action        = (Int, Int)
type Locations     = S.Set Location
type ErrorMessage  = String

data Mode = Viewing | EditingWalls | EditingAgents | EditingTreasures deriving (Eq, Enum, Bounded)

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
    putStrLn controlsInstructions
    putStrLn "These instructions will be visible while the simulation is running."
    putStrLn "Press enter to continue."
    _ <- getLine
    putStrLn mapInstructions
    let getState = do
            putStrLn "Enter map file to read. If no map file is provided an empty map will be used."
            mapFile <- getLine
            if all isSpace mapFile
                then do
                    putStrLn "Using empty map."
                    return initialState
                else do
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
                                Right state -> do
                                    putStrLn $ "using map from file " ++ mapFile
                                    return $ fitWindow state
                        else do
                            putStrLn $ "File " ++ mapFile ++ " does not exist. Try again."
                            getState
        getFps = do
            putStrLn "Enter frames per second"
            fps <- readMaybe <$> getLine
            let doAgain = do
                    putStrLn "Frames per second must be between 0 (exclusive) and 60 (inclusive)"
                    getFps
            case fps of
                Just n -> if n > 0 && n <= 60
                    then return n
                    else doAgain
                Nothing -> doAgain
    state <- getState
    fps <- getFps
    _ <- updateConsole state
    playIO (InWindow "GridWorld" (windowW state, windowH state) (500, 0)) black fps state draw handleEvent update

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

controlsInstructions, mapInstructions :: String
controlsInstructions = unlines
    [ "Controls:"
    , "Space bar: play"
    , "'f': fit world to window"
    , intercalate "\n" $ map (\ m -> show (fromEnum m) ++ ": switch to " ++ show m)
        [minBound :: Mode .. maxBound :: Mode]
    , "all modes:"
    , "    Pan around: arrow keys"
    , "    Zoom in/out: scroll up/down, page up/down key, '='/'-' key"
    , show Viewing ++ ":"
    , "    Pan around: drag mouse with left click, arrow keys"
    , "    Zoom in/out: drag mouse with right click, scroll up/down, page up/down key, '='/'-' key"
    , "editing modes:"
    , "    Add: left click or drag"
    , "    Remove: right click or drag" ]
mapInstructions = unlines
    [ "Map instructions:"
    , "A text file can be parsed as a map with the following characters:"
    , "    Wall: " ++ show wallChar
    , "    Agent: " ++ show agentChar
    , "    Treasure: " ++ show treasureChar
    , "    Empty square: " ++ show emptyChar
    , "The lines do not have to be the same length." ]

wallChar, agentChar, treasureChar, emptyChar :: Char
wallChar     = 'x'
agentChar    = 'o'
treasureChar = '$'
emptyChar    = ' '

parseMap :: String -> Either ErrorMessage State
parseMap = fmap toState . combine . map (foldr add start . pair) . zip [0, -1 ..] . lines
    where
        start = Right (S.empty, S.empty, S.empty)
        pair (y, xs) = map (\ x -> ((x, y), xs !! x)) [0 .. length xs - 1]
        add ((x, y), c) (Right (wls, ags, trs))
            | toLower c == wallChar     = Right (S.insert (x, y) wls, ags, trs)
            | toLower c == agentChar    = Right (wls, S.insert (x, y) ags, trs)
            | toLower c == treasureChar = Right (wls, ags, S.insert (x, y) trs)
            | toLower c == emptyChar    = Right (wls, ags, trs)
            | otherwise                 = Left $
                "Illegal character '" ++ [c] ++ "' at line " ++ show (-y + 1) ++ ", col " ++ show (x + 1)
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

convertToText :: State -> String
convertToText state@(State { walls, agents, treasures }) = unlines $ S.foldr (add treasureChar)
    (S.foldr (add agentChar) (S.foldr (add wallChar) emptyGrid walls) agents) treasures
    where
        add c = \ (x, y) acc ->
            let accx = acc !! (yMax - y)
            in  take (yMax - y) acc ++ [take (x - xMin) accx ++ [c] ++ drop (x - xMin + 1) accx]
                ++ drop (yMax - y + 1) acc
        emptyGrid = replicate (yMax - yMin + 1) $ replicate (xMax - xMin + 1) emptyChar
        ((xMax, xMin), (yMax, yMin)) = maxMins state

fitWindow :: State -> State
fitWindow state@(State { viewState, windowW, windowH }) = state
    { viewState = viewState
        { viewStateViewPort = (viewStateViewPort viewState)
            { viewPortScale = min (fromIntegral windowW / fromIntegral (xMax - xMin + 1))
                (fromIntegral windowH / fromIntegral (yMax - yMin + 1))
            , viewPortTranslate = (fromIntegral (-(xMax + xMin)) / 2 , fromIntegral (-(yMax + yMin)) / 2) } } }
    where
        ((xMax, xMin), (yMax, yMin)) = maxMins state

maxMins :: State -> ((Int, Int), (Int, Int))
maxMins (State { walls, agents, treasures }) = ((S.findMax xs, S.findMin xs), (S.findMax ys, S.findMin ys))
    where
        everything = S.unions [walls, agents, treasures]
        xs         = S.map fst $ everything
        ys         = S.map snd $ everything

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

update :: Float -> State -> IO State
update _ state
    | not (playing state) = return state
    | otherwise           = return (state { agents = S.map updateAgent $ agents state })
    where
        updateAgent (x, y)
            | S.member (x, y) (treasures state) = (x, y)
            | otherwise = act (x, y) $ decide $ sense $ map (`S.member` (walls state))
                [ (x, y + 1)       -- N
                , (x + 1, y + 1)   -- NE
                , (x + 1, y)       -- E
                , (x + 1, y - 1)   -- SE
                , (x, y - 1)       -- S
                , (x - 1, y - 1)   -- SW
                , (x - 1, y)       -- W
                , (x - 1, y + 1) ] -- NW

draw :: State -> IO Picture
draw state = return $ applyViewPortToPicture (viewStateViewPort $ viewState state) $ pictures $
    map drawTreasure (S.toList $ treasures state) ++
    map drawAgent (S.toList $ agents state) ++
    map drawWall (S.toList $ walls state)
    where
        drawWall     loc = color white  $ translate' loc $ rectangleSolid 1 1
        drawAgent    loc = color blue   $ translate' loc $ circleSolid 0.5
        drawTreasure loc = color orange $ translate' loc $ rectangleSolid 1 1
        translate' (x, y) = translate (fromIntegral x) (fromIntegral y)

handleEvent :: Event -> State -> IO State
handleEvent event@(EventKey key _ _ _) state
    | isInViewStateConfig key && (not $ isMouseButton key) = return $ updateViewStateWithEvent' event state
handleEvent (EventKey key Down _ _) state@(State { playing })
    | key == SpecialKey KeySpace = updateConsole (state { playing = not playing })
    | key == Char 'f'            = return $ fitWindow state
    | key == Char '\DC3'         = saveToFile state
    | Char c <- key
    , Just n <- readMaybe [c]
    , n >= fromEnum (minBound :: Mode)
    , n <= fromEnum (maxBound :: Mode) = updateConsole (state { mode = toEnum n })
handleEvent event@(EventKey key@(MouseButton _) _ _ _) state@(State { mode = Viewing })
    | isInViewStateConfig key = return $ updateViewStateWithEvent' event state
handleEvent event@(EventMotion _) state@(State { viewState =
    ViewState { viewStateTranslateMark, viewStateScaleMark }, mode = Viewing })
        | isJust viewStateTranslateMark || isJust viewStateScaleMark =
            return $ updateViewStateWithEvent' event state
handleEvent (EventKey (MouseButton button) Up _ _) state
    | button == LeftButton = return (state { leftButton = False })
    | button == RightButton = return (state { rightButton = False })
handleEvent (EventKey (MouseButton button) Down _ pos) state
    | button == LeftButton = return (state' { leftButton = True })
    | button == RightButton = return (state' { rightButton = True })
    where
        state' = editLocations button pos state
handleEvent (EventMotion pos) state@(State { leftButton, rightButton })
    | leftButton && not rightButton = return $ editLocations LeftButton pos state
    | rightButton && not leftButton = return $ editLocations RightButton pos state
handleEvent (EventResize (w, h)) state = return (state { windowW = w, windowH = h })
handleEvent _ state = return state

isInViewStateConfig :: Key -> Bool
isInViewStateConfig = (`elem` (map fst $ concat $ map snd $ initialViewStateConfig))

isMouseButton :: Key -> Bool
isMouseButton (MouseButton _) = True
isMouseButton _               = False

updateViewStateWithEvent' :: Event -> State -> State
updateViewStateWithEvent' event state@(State { viewState }) =
    state { viewState = updateViewStateWithEvent event viewState }

editLocations :: MouseButton -> Point -> State -> State
editLocations button pos state@(State { viewState }) =
    editLocations' button (round x, round y) state
    where
        (x, y) = invertViewPort (viewStateViewPort viewState) pos

editLocations' :: MouseButton -> Location -> State -> State
editLocations' LeftButton loc state@(State { mode, walls, agents, treasures })
    | mode == EditingWalls, S.notMember loc agents, S.notMember loc treasures =
        state { walls = S.insert loc walls }
    | mode == EditingAgents, S.notMember loc walls =
        state { agents = S.insert loc agents }
    | mode == EditingTreasures, S.notMember loc walls =
        state { treasures = S.insert loc treasures }
editLocations' RightButton loc state@(State { mode, walls, agents, treasures })
    | mode == EditingWalls     = state { walls = S.delete loc walls }
    | mode == EditingAgents    = state { agents = S.delete loc agents }
    | mode == EditingTreasures = state { treasures = S.delete loc treasures }
editLocations' _ _ state = state

updateConsole :: State -> IO State
updateConsole state@(State { playing, mode }) = do
    callCommand "cls" -- only works on Windows
    putStrLn controlsInstructions
    if playing
        then putStrLn "Playing"
        else putStrLn "Paused"
    putStrLn $ "Mode: " ++ show mode
    return state

saveToFile :: State -> IO State
saveToFile state = do
    putStrLn ""
    putStrLn "Enter the file to save to:"
    file <- getLine
    putStrLn "Saving to file..."
    putStrLn "This might take a long time if an agent has gone far away from the rest of the map"
    writeFile file $ convertToText state
    _ <- updateConsole state
    putStrLn ""
    putStrLn "Done saving to file."
    return state