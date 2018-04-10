import Class1
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
main :: IO ()
main = defaultMain allUnitTests

hasDupsTests = testGroup "Unit tests for hasdups"
  [ testCase "empty list" $ hasDups "" @?= False
  , testCase "list with one element" $ hasDups "a" @?= False
  , testCase "list with duplicates" $ hasDups "aab" @?= True

  ]

removeDupsTests = testGroup "Unit tests for removeDups"
  [ testCase "empty list" $ removeDups "" @?= ""
  , testCase "list with one element" $ removeDups "a" @?= "a"
  , testCase "list with duplicates" $ removeDups "aab" @?= "ab"
  , testCase "list with duplicates" $ removeDups "abb" @?= "ab"
  ]
isPermutationTests = testGroup "Unit tests for isPermutation"
  [ testCase "empty lists" $ isPermutation "" "" @?= True
  , testCase "not a permutation" $ isPermutation [1,2,1] [1, 2, 2] @?= False
  , testCase "is a permutation" $ isPermutation [1,2,1] [1, 1, 2] @?= True
  ]

shortestAndLongestTests = testGroup "Unit tests for shortestAndLongest"
  [ testCase "empty list" $ shortestAndLongest [""] @?= ("", "")
  , testCase "list with one element" $ shortestAndLongest ["a"] @?= ("a", "a")
  , testCase "list with more elements" $ shortestAndLongest ["This", "sentence", "is","ridiculous"] @?= ("is", "ridiculous")
  ]

propertyTestsRemoveDups = testGroup "propertyTests tests for removeDups"
  [ testProperty "No duplicates after removing" noDupsAfterRemove
  , testProperty "empty list" $
    \list -> not (null list) ==> sameFirstELement list
  ]

propertyTests = testGroup "propertyTests tests for removeDups"
  [ testProperty "No duplicates after removing" noDupsAfterRemove
  , testProperty "empty list" $
    \list -> not (null list) ==> sameFirstELement list
  ]

allUnitTests = testGroup "All"
  [ hasDupsTests
  , removeDupsTests
  , shortestAndLongestTests
  , isPermutationTests
  , propertyTestsRemoveDups
  ]

-- checkLength :: [Int] -> Bool
sameFirstELement :: [Int] -> Bool
sameFirstELement list = head list == head (removeDups list)
noDupsAfterRemove :: [Int] -> Bool
noDupsAfterRemove list = not $ hasDups $ removeDups $list
