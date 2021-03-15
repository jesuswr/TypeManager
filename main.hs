module Main where


import System.Directory
import Data.Char
import qualified Data.Map as Map
import qualified Data.List as List
import TypeManager


main :: IO()
main = do
    runMainLoop Map.empty


runMainLoop :: Str2Type -> IO()
runMainLoop typeMap = do 
    putStrLn "\nQUE DESEA HACER?"
    aux <- getLine
    let inp = words aux
    case inp of
        "ATOMICO":xs   -> addAtomic typeMap (xs!!0) (read (xs!!1)) (read (xs!!2))
        "STRUCT":xs    -> addStruct typeMap (head xs) (tail xs)
        "UNION":xs     -> addUnion typeMap (head xs) (tail xs)
        "DESCRIBIR":xs -> do
            describe typeMap (head xs)
            runMainLoop typeMap
        "SALIR":xs     -> return ()
        _              -> do
            putStrLn "COMANDO NO CONOCIDO"
            runMainLoop typeMap


-- simple function to add an atomic type
addAtomic :: Str2Type -> String -> Int -> Int -> IO()
addAtomic typeMap name size align = do
    if Map.member name typeMap then do
        putStrLn "ERROR: EL NOMBRE DADO YA HA SIDO USADO"
        runMainLoop typeMap
    else do
        runMainLoop $ Map.insert name (Atomic size align) typeMap


-- simple function to add a struct
addStruct :: Str2Type -> String -> [String] -> IO()
addStruct typeMap name subtypes = do
    if Map.member name typeMap then do
        putStrLn "ERROR: EL NOMBRE DADO YA HA SIDO USADO"
        runMainLoop typeMap
    else do
        if mapHasSubtypes typeMap subtypes then do
            runMainLoop $ Map.insert name (Struct subtypes) typeMap
        else do
            putStrLn "ERROR: UNO DE LOS NOMBRES EN LA LISTA YA HA SIDO USADO"
            runMainLoop typeMap


-- simple function to add an union
addUnion :: Str2Type -> String -> [String] -> IO()
addUnion typeMap name subtypes = do
    if Map.member name typeMap then do
        putStrLn "ERROR: EL NOMBRE DADO YA HA SIDO USADO"
        runMainLoop typeMap
    else do
        if mapHasSubtypes typeMap subtypes then do
            runMainLoop $ Map.insert name (Union subtypes) typeMap
        else do
            putStrLn "ERROR: UNO DE LOS NOMBRES EN LA LISTA YA HA SIDO USADO"
            runMainLoop typeMap


-- function that calls smaller funcions to describe each of the 
-- 3 cases: normal, packed and ordered
describe :: Str2Type -> String -> IO()
describe typeMap name = do
    if not (Map.member name typeMap) then do
        putStrLn "ERROR: EL NOMBRE DADO NO EXISTE"
        runMainLoop typeMap
    else do 
        describeNormal typeMap name
        describePacked typeMap name
        describeOrder typeMap name


-- function that prints the information of the type, respecting
-- the normal order and alignments. Wasted space is size - used space.
-- In the case of union, the wasted space is the minimum possible, this is
-- size - maximum space used by some subtype
describeNormal :: Str2Type -> String -> IO()
describeNormal typeMap name = do
    let structSize = getEnd typeMap name 0
    let alignment = getAlignment typeMap name
    let usedSpace = getSize typeMap name
    putStrLn "SIN EMPAQUETAR:"
    putStrLn $ "\tTAMANO USADO: " ++ show structSize
    putStrLn $ "\tALINEACION: " ++ show alignment
    putStrLn $ "\tESPACIO PERDIDO: " ++ show (structSize - usedSpace)


-- function that prints the information of the type respecting
-- the normal order but ignoring the alignments. Wasted space
-- is always 0 because there are no spaces between types
describePacked :: Str2Type -> String -> IO()
describePacked typeMap name = do
    let alignment = getAlignment typeMap name
    let usedSpace = getSize typeMap name
    putStrLn "EMPAQUETADO:"
    putStrLn $ "\tTAMANO USADO: " ++ show usedSpace
    putStrLn $ "\tALINEACION: " ++ show alignment
    putStrLn $ "\tESPACIO PERDIDO: 0"


-- function that prints the information of the type in the best
-- possible order, respecting alignments
describeOrder :: Str2Type -> String -> IO()
describeOrder typeMap name = do
    let structSize = getBestEnd typeMap name 0
    let usedSpace = getSize typeMap name
    let align = getBestAlign typeMap name
    putStrLn "REORDENANDO:"
    putStrLn $ "\tTAMANO USADO: " ++ show structSize
    putStrLn $ "\tALINEACION: " ++ show align
    putStrLn $ "\tESPACIO PERDIDO: " ++ show (structSize - usedSpace)