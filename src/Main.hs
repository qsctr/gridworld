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
type ErrorMessage             = String

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
    mapM_ putStrLn
        [ "Controls:"
        , intercalate "\n" $ map (\m -> show (fromEnum m) ++ ": switch to " ++ show m)
            [minBound :: Mode .. maxBound :: Mode]
        , show Viewing ++ ":"
        , "    Pan around: drag mouse with left click, arrow keys"
        , "    Zoom in: drag mouse with right click, scroll up, page up key, '=' key"
        , "    Zoom out: drag mouse with right click, scroll down, page down key, '-' key"
        , "Editing modes:"
        , "    Add: left click or drag"
        , "    Remove: right click or drag"
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
            if all isSpace mapFile
                then return initialState
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
                                Right state -> return $ fitWindow state
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
    play (InWindow "GridWorld" (windowW state, windowH state) (0, 0)) black fps state draw handleEvent update

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
            { viewPortScale = min (fromIntegral windowW / (xMax - xMin + 1))
                (fromIntegral windowH / (yMax - yMin + 1))
            , viewPortTranslate = (- (xMax + xMin) / 2 , - (yMax + yMin) / 2) } } }
    where
        everything = S.unions [walls, agents, treasures]
        xs         = S.map fst everything
        ys         = S.map snd everything
        xMax       = S.findMax xs
        xMin       = S.findMin xs
        yMax       = S.findMax ys
        yMin       = S.findMin ys

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
handleEvent (EventKey key Down _ _) state@(State { playing })
    | key == SpecialKey KeySpace = state { playing = not playing }
    | key == Char 'f'            = fitWindow state
    | Char c <- key
    , Just n <- readMaybe [c]
    , n >= fromEnum (minBound :: Mode)
    , n <= fromEnum (maxBound :: Mode) = state { mode = toEnum n }
handleEvent event@(EventKey key _ _ _) state@(State { viewState, mode = Viewing })
    | key `elem` (map fst $ concat $ map snd $ initialViewStateConfig) =
        state { viewState = updateViewStateWithEvent event viewState }
handleEvent event@(EventMotion _) state@(State { viewState =
    viewState@(ViewState { viewStateTranslateMark, viewStateScaleMark }), mode = Viewing })
        | isJust viewStateTranslateMark || isJust viewStateScaleMark =
            state { viewState = updateViewStateWithEvent event viewState }
handleEvent (EventKey (MouseButton button) Up _ _) state
    | button == LeftButton = state { leftButton = False }
    | button == RightButton = state { rightButton = False }
handleEvent (EventKey (MouseButton button) Down _ pos) state
    | button == LeftButton = state' { leftButton = True }
    | button == RightButton = state' { rightButton = True }
    where
        state' = editLocations button pos state
handleEvent (EventMotion pos) state@(State { leftButton, rightButton })
    | leftButton && not rightButton = editLocations LeftButton pos state
    | rightButton && not leftButton = editLocations RightButton pos state
handleEvent (EventResize (w, h)) state = state { windowW = w, windowH = h }
handleEvent _ state = state

editLocations :: MouseButton -> Point -> State -> State
editLocations button pos state@(State { viewState }) =
    editLocations' button (fromIntegral $ round x, fromIntegral $ round y) state
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