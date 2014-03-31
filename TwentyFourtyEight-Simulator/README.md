# TwentyFourtyEight-Simulator

A really simple stateless (you could add state if you wanted) 2048 simulator written in Haskell

## Installation

```
cabal clean
cabal configure
cabal install
```

## Usage

The only thing that needs to be done is importing Simulator.TwentyFourtyEight and a few other things 
and then defining a simulation function

```Haskell
main :: IO ()
main = do
    (score, highestTile) <- runGameM strategy'
    scores <- runTimes strategy' 10000
    let highest = maximum $ scnds scores
    putStrLn $ "The score was: " ++ (show score)
    putStrLn $ "The highest tile was" ++ (show highest)
```
## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

I'm more than happy to hear suggestions or take pull requests