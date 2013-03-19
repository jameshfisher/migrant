module Database.Migrant.Test (tests) where

import Distribution.TestSuite

instance TestOptions String where
  name = id
  options = const []
  defaultOptions _ = return (Options [])
  check _ _ = []

instance PureTestable String where
  run s opts = Pass

tests :: [Test]
tests = [pure ("foo" :: String)]
