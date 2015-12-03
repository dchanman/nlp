module Main where

import Test.HUnit

test1 = TestCase (assertEqual "test number uno" 5 (2+3))
test2 = TestCase (assertEqual "test number dos" 10 (1+2+3+4))
test3 = TestCase (assertBool "should fail because false" False)

tests = TestList [
	TestLabel "sample test" test1,
	TestLabel "another sample" test2,
	TestLabel "failing test" test3
	]			

main = runTestTT tests

