module Main where

import Test.HUnit
import TypeManager
import qualified Data.Map as Map
import qualified Data.List as List

main = do runTestTT tests

tests = TestList [
            test1,
            test2,
            test3,
            test4,
            test5,
            test6,
            test7,
            test8,
            test9,
            test10,
            test11,
            test12,
            test13,
            test14,
            test15,
            test16
        ]


-- map for tests:
-- atomic bool 1 2
-- atomic char 2 2
-- atomic double 8 8 
-- atomic int 4 4
-- struct meta [int, char, int, double, bool]
-- union uni [int, double, meta]
typeMap :: Str2Type
typeMap = Map.fromList [("BOOL",Atomic {size = 1, align = 2}),("CHAR",Atomic {size = 2, align = 2}),("DOUBLE",Atomic {size = 8, align = 8}),("INT",Atomic {size = 4, align = 4}),("META",Struct {subtypes = ["INT","CHAR","INT","DOUBLE","BOOL"]}),("UNI",Union {subtypes = ["INT","DOUBLE","META"]})]


-- basic cases
-- size of int should be 4
test1 = TestCase (assertEqual "ERROR TEST 1" 4 (getEnd typeMap "INT" 0))

-- alignment of char should be 2
test2 = TestCase (assertEqual "ERROR TEST 2" 2 (getAlignment typeMap "CHAR"))

-- used size of bool should be 1
test3 = TestCase (assertEqual "ERROR TEST 3" 1 (getSize typeMap "BOOL"))

-- check if subtypes of meta exist in the map
test4 = TestCase (assertEqual "ERROR TEST 4" True (mapHasSubtypes typeMap ["INT","CHAR","INT","DOUBLE","BOOL"]))


-- some tests in the example that was given in class
-- size of meta should be 25
test5 = TestCase (assertEqual "ERROR TEST 5" 25 (getEnd typeMap "META" 0))

-- used space should be 19
test6 = TestCase (assertEqual "ERROR TEST 6" 19 (getSize typeMap "META"))

-- used space with the best order should be equal to size
test7 = TestCase (assertEqual "ERROR TEST 7" 19 (getBestEnd typeMap "META" 0))

-- alignment in best case should be 8
test8 = TestCase (assertEqual "ERROR TEST 8" 8 (getBestAlign typeMap "META"))

-- alignment normally should be 4
test9 = TestCase (assertEqual "ERROR TEST 9" 4 (getAlignment typeMap "META"))


-- some tests in union structure
-- size should be equal to meta size
test10 = TestCase (assertEqual "ERROR TEST 10" 25 (getEnd typeMap "UNI" 0))

-- alignment should be 8 normally
test11 = TestCase (assertEqual "ERROR TEST 11" 8 (getAlignment typeMap "UNI"))

-- size in best case should be equal to meta best size
test12 = TestCase (assertEqual "ERROR TEST 12" 19 (getBestEnd typeMap "UNI" 0))

-- allignment in best case should also be 8
test13 = TestCase (assertEqual "ERROR TEST 13" 8 (getBestAlign typeMap "UNI"))

-- space used should be equal to space used by meta
test14 = TestCase (assertEqual "ERROR TEST 14" 19 (getSize typeMap "UNI"))


-- a simple map for recursive test
-- atomic int 4 4
-- atomic char 1 2
-- struct a [int, char]
-- union b [int, char]
-- struct c [a, b]
typeMap2 :: Str2Type
typeMap2 = Map.fromList [("A",Struct {subtypes = ["INT","CHAR"]}),("B",Union {subtypes = ["INT","CHAR"]}),("C",Struct {subtypes = ["A","B"]}),("CHAR",Atomic {size = 1, align = 2}),("INT",Atomic {size = 4, align = 4})]

-- size of c normally should be 12
test15 = TestCase (assertEqual "ERROR TEST 15" 12 (getEnd typeMap2 "C" 0))

-- size of c in the best case is 9
test16 = TestCase (assertEqual "ERROR TEST 16" 9 (getBestEnd typeMap2 "C" 0))