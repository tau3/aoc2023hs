import Day11
import Day8
import Test.Hspec

main :: IO ()
main = hspec $ do
  testDay8
  testDay11pt1
  testDay11pt2

testDay8 :: SpecWith ()
testDay8 = describe "Day 8" $ do
  it "Example 1" $ do
    run ["RL", "", "AAA = (BBB, CCC)", "BBB = (DDD, EEE)", "CCC = (ZZZ, GGG)", "DDD = (DDD, DDD)", "EEE = (EEE, EEE)", "GGG = (GGG, GGG)", "ZZZ = (ZZZ, ZZZ)"] `shouldBe` 2
  it "Example 2" $ do
    run ["LLR", "", "AAA = (BBB, BBB)", "BBB = (AAA, ZZZ)", "ZZZ = (ZZZ, ZZZ)"] `shouldBe` 6

testDay11pt1 :: SpecWith ()
testDay11pt1 = describe "Day 11" $ do
  it "Example 1" $ do
    Day11.solvePt1 day11Input `shouldBe` 374

testDay11pt2 :: SpecWith ()
testDay11pt2 = describe "Day 11" $ do
  it "Example 1 Expansion 10" $ do
    Day11.solvePt2 day11Input 10 `shouldBe` 1030
  it "Example 1 Expansion 100" $ do
    Day11.solvePt2 day11Input 100 `shouldBe` 8410

run :: [String] -> Int
run input = Day8.solvePt1 a b where (a, b) = parseInput input

day11Input :: [String]
day11Input =
  [ "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  ]
