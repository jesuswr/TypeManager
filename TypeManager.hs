module TypeManager where


import qualified Data.Map as Map
import qualified Data.List as List


data Type = Atomic {size :: Int, align :: Int}
          | Struct {subtypes :: [String]}
          | Union  {subtypes :: [String]}
          deriving (Show)
type Str2Type = Map.Map String Type


mapHasSubtypes :: Str2Type -> [String] -> Bool
mapHasSubtypes typeMap subtypes =
    foldl (\tof typeName -> tof && (Map.member typeName typeMap)) True subtypes


getEnd :: Str2Type -> String -> Int -> Int
getEnd typeMap name pos = 
    case typeMap Map.! name of
        Atomic sz al -> getNextMult pos al + sz
        Struct subts -> 
            foldl (\p typeName -> getEnd typeMap typeName p) pos subts
        Union subts  -> 
            foldl (\p typeName -> max p (getEnd typeMap typeName pos)) 0 subts


getNextMult :: Int -> Int -> Int
getNextMult pos al = div (pos + al - 1) al * al


getAlignment :: Str2Type -> String -> Int
getAlignment typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> al
        Struct subts -> getAlignment typeMap (head subts)
        Union subts  -> 
            foldl (\p typeName -> lcm p (getAlignment typeMap typeName)) 1 subts


getSize :: Str2Type -> String -> Int
getSize typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> sz
        Struct subts -> 
            foldl (\sm typeName -> sm + (getSize typeMap typeName)) 0 subts
        Union subts  -> 
            foldl (\mx typeName -> max mx (getSize typeMap typeName)) 0 subts


getBestEnd :: Str2Type -> String -> Int -> Int
getBestEnd typeMap name pos = 
    case typeMap Map.! name of
        Atomic sz al -> getNextMult pos al + sz
        Struct subts -> do
            let perms = List.permutations subts
            foldl (\mn typeNames -> min mn (getBestEndHelper typeMap typeNames pos)) 42424242 perms
        Union subts  -> 
            foldl (\p typeName -> max p (getBestEnd typeMap typeName pos)) 0 subts


getBestEndHelper :: Str2Type -> [String] -> Int -> Int
getBestEndHelper typeMap [] pos = pos
getBestEndHelper typeMap (x:xs) pos = do
    case typeMap Map.! x of
        Atomic sz al -> do
            let nextPos = getNextMult pos al + sz
            getBestEndHelper typeMap xs nextPos
        Struct subts -> do
            let nextPos = getBestEnd typeMap x pos
            getBestEndHelper typeMap xs nextPos
        Union subts -> do
            let nextPos = getBestEnd typeMap x pos
            getBestEndHelper typeMap xs nextPos


getBestAlign :: Str2Type -> String -> Int
getBestAlign typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> al
        Struct subts -> do
            let perms = List.permutations subts
            let res   = map (\names -> getBestEndHelper typeMap names 0) perms
            let pairs = zip perms res
            let (perm, mn) = foldl (\(p,m) (p2,m2) -> if m <= m2 then (p,m) else (p2,m2)) ([], 42424242) pairs
            getBestAlign typeMap (head perm)
        Union subts -> do
            let alins = map (getBestAlign typeMap) subts
            foldl (\p q -> lcm p q) 1 alins
