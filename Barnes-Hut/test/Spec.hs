import Test.Hspec

main :: IO ()
main = hspec test

test = describe "testing" $
  it "tests" $
    1 `shouldBe` 1
