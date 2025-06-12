import Test.Tasty
import Test.Tasty.HUnit (assertFailure, Assertion, testCase, assertBool, (@?=))
import Language.C
import Language.C.System.GCC  
import Data.List (isInfixOf)

import Parser (mainParser)
import Analyzer (analyzeBufferOverflow, analyzeUninitializedVars, printBufferOverflowRisks, printUninitWarnings)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Uninitialized Variable Analyzer"
  [ testCase "Variable used without initialization" test_uninitVar
  , testCase "Variable initialized" test_initVar
  , testCase "Conditional with init in both branches" test_ifInit
  , testCase "Variable uninitialized only in else branch" test_ifInitPartial
  , testCase "Variable initialized inside loop" test_loopInit
  , testCase "Variable used after loop without init" test_loopNoInit
  , testCase "Multiple declarations with one uninitialized" test_multiDecl
  , testCase "Function parameter used (should be considered initialized)" test_funcParam
  , testCase "Uninitialized global variable used" test_globalVar
  , testCase "Static variable used (initialized by default)" test_staticVar
  , testCase "Variable initialized with complex expression" test_complexInit
  , testCase "Variable assigned inside called function (likely not detected)" test_initInFuncCall
  , testCase "Variable conditionally initialized (only one branch)" test_conditionalUnsafe
  , testCase "Variable inside struct used without initialization" test_structVar
  , testCase "Variable declared and initialized in for loop" test_forInit
  ]

-- 1. Variable used without initialization (already done)
test_uninitVar :: Assertion
test_uninitVar = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x;"
    , "  int y = x + 1;"
    , "}"
    ]
  assertBool "Should detect x as uninitialized" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 2. Variable initialized (already done)
test_initVar :: Assertion
test_initVar = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x = 5;"
    , "  int y = x + 1;"
    , "}"
    ]
  result @?= []

-- 3. Conditional with init in both branches (already done)
test_ifInit :: Assertion
test_ifInit = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x;"
    , "  if (1) { x = 1; } else { x = 2; }"
    , "  int y = x + 1;"
    , "}"
    ]
  result @?= []

-- 4. Variable initialized only in if branch (no else) -> should warn
test_ifInitPartial :: Assertion
test_ifInitPartial = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x;"
    , "  if (1) { x = 1; }"
    , "  int y = x + 1;"
    , "}"
    ]
  assertBool "Should detect x as uninitialized (no init in else)" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 5. Variable initialized inside a loop
test_loopInit :: Assertion
test_loopInit = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x = 0;"
    , "  for (int i=0; i<10; i++) { x = i; }"
    , "  int y = x + 1;"
    , "}"
    ]
  result @?= []

-- 6. Variable used after loop without initialization
test_loopNoInit :: Assertion
test_loopNoInit = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x;"
    , "  for (int i=0; i<10; i++) { int y = i; }"
    , "  int z = x + 1;"
    , "}"
    ]
  assertBool "Should detect x as uninitialized" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 7. Multiple declarations with one uninitialized variable
test_multiDecl :: Assertion
test_multiDecl = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x = 5, y;"
    , "  int z = y + x;"
    , "}"
    ]
  assertBool "Should detect y as uninitialized" $
    any (\(msg, _) -> "uninitialized variable 'y'" `isInfixOf` msg) result

-- 8. Function parameter used (should be considered initialized)
test_funcParam :: Assertion
test_funcParam = do
  result <- runAnalysis $ unlines
    [ "void f(int x) {"
    , "  int y = x + 1;"
    , "}"
    ]
  -- Parameters are considered initialized by callers, so no warnings expected
  result @?= []

-- 9. Uninitialized global variable used
test_globalVar :: Assertion
test_globalVar = do
  result <- runAnalysis $ unlines
    [ "int x;"
    , "int main() {"
    , "  int y = x + 1;"
    , "}"
    ]
  assertBool "Should detect x as uninitialized (global variable)" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 10. Static variable used (initialized by default in C)
test_staticVar :: Assertion
test_staticVar = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  static int x;"
    , "  int y = x + 1;"
    , "}"
    ]
  -- Static variables are initialized to zero by default
  result @?= []

-- 11. Variable initialized with complex expression (no warning)
test_complexInit :: Assertion
test_complexInit = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x = 5;"
    , "  int y = x * (3 + 4);"
    , "}"
    ]
  result @?= []

-- 12. Variable assigned inside called function (likely not detected by local analysis)
test_initInFuncCall :: Assertion
test_initInFuncCall = do
  result <- runAnalysis $ unlines
    [ "void init(int *p) { *p = 1; }"
    , "int main() {"
    , "  int x;"
    , "  init(&x);"
    , "  int y = x + 1;"
    , "}"
    ]
  -- Likely to warn because analysis may be local and not track function side effects
  assertBool "Should detect x as uninitialized (assigned in function)" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 13. Variable conditionally initialized (only one branch) -> warning expected
test_conditionalUnsafe :: Assertion
test_conditionalUnsafe = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  int x;"
    , "  if (1) { x = 1; }"
    , "  int y = x + 1;"
    , "}"
    ]
  assertBool "Should detect x as uninitialized (partial init)" $
    any (\(msg, _) -> "uninitialized variable 'x'" `isInfixOf` msg) result

-- 14. Variable inside struct used without initialization
test_structVar :: Assertion
test_structVar = do
  result <- runAnalysis $ unlines
    [ "struct S { int a; };"
    , "int main() {"
    , "  struct S s;"
    , "  int y = s.a + 1;"
    , "}"
    ]
  assertBool "Should detect s.a as uninitialized" $
    any (\(msg, _) -> "uninitialized variable 's.a'" `isInfixOf` msg) result

-- 15. Variable declared and initialized in for loop
test_forInit :: Assertion
test_forInit = do
  result <- runAnalysis $ unlines
    [ "int main() {"
    , "  for (int x = 0; x < 10; x++) {"
    , "    int y = x + 1;"
    , "  }"
    , "}"
    ]
  result @?= []



runAnalysis :: String -> IO [(String, Position)]
runAnalysis code = do
  let filename = "test.c"
      input = inputStreamFromString code
      pos = initPos filename
  case parseC input pos of
    Left err -> assertFailure ("Parsing error: " ++ show err) >> return []
    Right (CTranslUnit decls _) -> return $ analyzeUninitializedVars decls
