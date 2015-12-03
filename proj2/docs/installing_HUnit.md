# Installing HUnit

This was a huge pain on Linux. Hopefully it won't be as bad for you guys.

1. Install `cabal`
	* Windows: https://wiki.haskell.org/Cabal-Install#Windows
	* Mac: You should already have `cabal`, if not read this link: http://stackoverflow.com/questions/15239289/how-to-install-haskell-cabal-tool-for-haskell-7-6-1-on-mac-osx
2. Run `cabal install hunit` (in terminal for Unix, from cabal.exe from Windows I think)
3. Run `cabal install haskell98libraries` (this gave me some HUnit dependencies I needed)

# Running HUnit tests
Open up `GHCi` and load the test file:

```
Prelude> :load test_crusher.hs
```

Hopefully everything will compile and you'll be okay.

```
[1 of 2] Compiling Crusher          ( Crusher.hs, interpreted )
[2 of 2] Compiling Main             ( test_crusher.hs, interpreted )
Ok, modules loaded: Main, Crusher.
```

Then run the tests:
```
*Main> main
```

You should be able to see the results:
```
Cases: 12  Tried: 12  Errors: 0  Failures: 0
Counts {cases = 12, tried = 12, errors = 0, failures = 0}
```

## Running individual tests

Use the `runTestTT` function after loading `test_crusher.hs`. Give a test function as a parameter

```
*Main> runTestTT test_countPiecesW_1
```

## Running a test suite

Use the `runTestTT` function, giving a list of test functions as a parameter
```
*Main> runTestTT tests_count_pieces
```
