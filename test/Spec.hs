import Day8
import Day11
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day 8" $ do
    it "Example 1" $ do
      run ["RL", "", "AAA = (BBB, CCC)", "BBB = (DDD, EEE)", "CCC = (ZZZ, GGG)", "DDD = (DDD, DDD)", "EEE = (EEE, EEE)", "GGG = (GGG, GGG)", "ZZZ = (ZZZ, ZZZ)"] `shouldBe` 2
    it "Example 2" $ do
      run ["LLR", "", "AAA = (BBB, BBB)", "BBB = (AAA, ZZZ)", "ZZZ = (ZZZ, ZZZ)"] `shouldBe` 6
  describe "Day 11" $ do
    it "Example 1" $ do
     Day11.solvePt1 day11Input `shouldBe` 374 

run :: [String] -> Int
run input = Day8.solvePt1 a b where (a, b) = parseInput input

day11Input :: [String]
day11Input =
  [ "....#........",
    ".........#...",
    "#............",
    ".............",
    ".............",
    "........#....",
    ".#...........",
    "............#",
    ".............",
    ".............",
    ".........#...",
    "#....#......."
  ]


