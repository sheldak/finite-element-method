## Problem
Drawing a chart for the approximation of the equation of acoustic vibration of the material layer:

![alt text](https://github.com/sheldak/finite-element-method/blob/master/README-equation.jpg)

where

![alt text](https://github.com/sheldak/finite-element-method/blob/master/README-equation2.jpg)

## How to use
- have GHC and Stack installed
- type `stack build` in the command line in project directory
- type `stack exec finite-element-method-exe [positive number]` where that positive number is a number of elements (e.g. `stack exec finite-element-method-exe 40`)
- check chart.svg for the result of the computation
