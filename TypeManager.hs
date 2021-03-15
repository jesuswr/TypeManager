module TypeManager where


import qualified Data.Map as Map
import qualified Data.List as List


-- data type for each posible type
data Type = Atomic {size :: Int, align :: Int}
          | Struct {subtypes :: [String]}
          | Union  {subtypes :: [String]}
          deriving (Show)

-- type to have fast acces to the current types
type Str2Type = Map.Map String Type


-- function that checks if all the given types exist in the Map
mapHasSubtypes :: Str2Type -> [String] -> Bool
mapHasSubtypes typeMap subtypes =
    foldl (\tof typeName -> tof && (Map.member typeName typeMap)) True subtypes


-- function that calculates the ending position of a data type given the
-- current free position
getEnd :: Str2Type -> String -> Int -> Int
getEnd typeMap name pos = 
    case typeMap Map.! name of
        Atomic sz al -> getNextMult pos al + sz
        Struct subts -> 
            -- in this case you calculate the ending position of each subtype
            -- beginning where the previous one ended
            foldl (\currPos typeName -> getEnd typeMap typeName currPos) pos subts
        Union subts  -> 
            -- here you just have to take the maximum ending of all subtypes
            foldl (\maxPos typeName -> max maxPos (getEnd typeMap typeName pos)) 0 subts


-- simple function to get the smallest number greater or equal than 'pos' such
-- that it is a multiple of 'al'
getNextMult :: Int -> Int -> Int
getNextMult pos al = div (pos + al - 1) al * al


-- function to get the aligment of a type
getAlignment :: Str2Type -> String -> Int
getAlignment typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> al
        -- the alignment of a struct is the alignment of its first subtype
        Struct subts -> getAlignment typeMap (head subts)
        -- the alignment of a union is the lcm of the alignment of each
        -- of its subtypes
        Union subts  -> 
            foldl (\lcmm typeName -> lcm lcmm (getAlignment typeMap typeName)) 1 subts


-- function to get the used size of a type
getSize :: Str2Type -> String -> Int
getSize typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> sz
        Struct subts -> 
            -- the used size of a struct is the sum of the used size of its
            -- subtypes
            foldl (\sm typeName -> sm + (getSize typeMap typeName)) 0 subts
        Union subts  -> 
            -- in the case of union, we take the maximum used size of its
            -- subtypes
            foldl (\mx typeName -> max mx (getSize typeMap typeName)) 0 subts


-- this is like getEnd but here we minimize the space lost by trying all
-- permutations of the structs subtypes
getBestEnd :: Str2Type -> String -> Int -> Int
getBestEnd typeMap name pos = 
    case typeMap Map.! name of
        Atomic sz al -> getNextMult pos al + sz
        Struct subts -> do
            let perms = List.permutations subts
            -- here we try all permutations and return the minimum result
            foldl (\mn typeNames -> min mn (getBestEndHelper typeMap typeNames pos)) 42424242 perms
        Union subts  -> 
            -- here we have to take the maximum value of the best results of
            -- its subtypes
            foldl (\mx typeName -> max mx (getBestEnd typeMap typeName pos)) 0 subts


-- function to help calculate the best possible permutation for a struct
getBestEndHelper :: Str2Type -> [String] -> Int -> Int
getBestEndHelper typeMap [] pos = pos
getBestEndHelper typeMap (name:rest) pos = do
    case typeMap Map.! name of
        Atomic sz al -> do
            -- get next free position and continue with the struct
            let nextPos = getNextMult pos al + sz
            getBestEndHelper typeMap rest nextPos
        Struct subts -> do
            -- solve new struct recursevely and continue with the current one
            -- after that
            let nextPos = getBestEnd typeMap name pos
            getBestEndHelper typeMap rest nextPos
        Union subts -> do
            -- get the best possible size for a union and continue with the 
            -- current struct after that
            let nextPos = getBestEnd typeMap name pos
            getBestEndHelper typeMap rest nextPos


-- function to get the alignment of the best order of a type
getBestAlign :: Str2Type -> String -> Int
getBestAlign typeMap name =
    case typeMap Map.! name of
        Atomic sz al -> al
        Struct subts -> do
            -- try all permutations and return the alignment of the best one
            let perms = List.permutations subts
            let res   = map (\names -> getBestEndHelper typeMap names 0) perms
            let pairs = zip perms res
            let (perm, mn) = foldl (\(p,m) (p2,m2) -> if m <= m2 then (p,m) else (p2,m2)) ([], 42424242) pairs
            getBestAlign typeMap (head perm)
        Union subts -> do
            -- here is like in getAlingment but we have to take the lcm of 
            -- the best alignment of the subtypes
            let alins = map (getBestAlign typeMap) subts
            foldl (\p q -> lcm p q) 1 alins
