{-# OPTIONS_GHC -F -pgmF tasty-discover -optF --hide-successes #-}

-- We're using tasty-discover to run and to detect the configuration.
-- http://hackage.haskell.org/package/tasty-discover

-- If you add a new module full of tests, be sure to add it to `other-modules`
-- in the test section the cabal file.

-- You can create a test using any testing library you wish (hspec, quickcheck, etc.).
-- Just be sure to prefix the test function name with an identifier that corresponds
-- to the testing library you wish to run the test with. Specifically:

--     prop_: QuickCheck properties.
--     scprop_: SmallCheck properties.
--     hprop_: Hedgehog properties.
--     unit_: HUnit test cases.
--     spec_: Hspec specifications.
--     test_: Tasty TestTrees.
