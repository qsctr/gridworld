module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import qualified Data.Set as S
import qualified Data.Map as M

data State = State {
    walls :: S.Set (Int, Int),
    agents :: M.Map Char (Int, Int),
    playing :: Bool,
    viewPort :: ViewPort
}

main :: IO ()
main = do
    initialState <- parseMap <$> readFile "grid.txt"
    return undefined

parseMap :: String -> State
parseMap = toState . unzip . map (foldr add (S.empty, M.empty) . pair) . zip [0..] . lines
    where
        pair (r, xs) = map (\c -> ((r, c), xs !! c)) [0 .. length xs - 1]
        add (loc, 'x') (wls, ags) = (S.insert loc wls, ags)
        add (_, ' ') x = x
        add (loc, a) (wls, ags) = (wls, M.insertWithKey existsError a loc ags)
        existsError name new old = error $ "Duplicate agents called " ++ [name] ++
            ": the first is at " ++ toLineNum new ++
            ", the second one is at " ++ toLineNum old
        toLineNum (x, y) = show (x + 1) ++ ":" ++ show (y + 1)
        toState (wlss, agss) = State {
            walls = S.unions wlss,
            agents = foldr (M.unionWithKey existsError) M.empty agss,
            playing = False,
            viewPort = viewPortInit
        }