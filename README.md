# N-Body Simulations
Algorithms and visualizations of a dynamical system of bodies influenced
by gravitational force written in Haskell

* Direct Simulation (A O(n^2) algorithm where the force on each
particle is calculated by taking every other particle into account)
* Barnes-Hut (A O(log n)) algorithm based on quad/oct trees)

## How to Run
* Install dependencies
* Compile and launch, or alternatively run `cabal run`
