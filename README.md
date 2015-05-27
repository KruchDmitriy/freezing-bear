# Freezing Bear

The project is a mathematical program that allows to numerically compute integrals (2 methods: trapezy and Sympson's methods), find the polynom on a grid of values (Lagrange method) and display the plots associated with this polynoms or other functions.

## Install
You need to install glut and another libraries. You can use [Cabal](https://wiki.haskell.org/Cabal-Install) (you should install it), and then simply ```cabal install opengl```, ```cabal install glut``` (for the last one needed C glut libraries, you need to install it, in Ubuntu - ```sudo apt-get install freeglut-dev```).

## Compile
To compile the program run ```ghc -o main ./Main.hs```.

## How to use
Main.hs is the entry point to the program. Start it to get the functionality.

List of supported masthematical functions:

1. sin(x)
1. cos(x)
1. tg(x)
1. exp(x)
1. lg(x) - log with base 10

Every function `f(x)` can be written as `f x` too.

List of binary operators:

1. x + y
1. x - y
1. x * y
1. x / y
1. x ^ y - computes the y-th power of x
`x` and `y` are real values (Double type)

An example of use for Windows PowerShell:
```
.\main.exe
Input:
1. Integrate by trapezy method (steps count)
2. Integrate by trapezy method (step)
3. Integrate by Simpson's method
4. Interpolate by Lagrange method
4
Enter the function:
x ^ 2
Enter a, b range and the step by whitespace:
1 5 4
```

The result of this example is shown below:

![Result plot](/Results/xpower(2)_1_5_4.png "Result plot")

## How it works
The main modules of the program are `Algorithm`, `ExpressionParser`, and `Plot`. Each of them responces to for own stage of function analysis.
### ExpressionParser
Contains algorithms to transform list of characters (string representation of the function) to the data structure that is useful to compute the expression. The data structure namely `Function` is a recursive tree, each node of it is an operator, a value or a variable. Main functions of this module are
```haskell
create_func :: String -> Function
``` and
```haskell
evaluate_func :: Function -> Double -> Double
```.

Another approach to solve the problem of parsing the string representation of functions is to use built-in Haskell interpretator (`Language.Haskell.Interpreter` module), but it seems too hard to understand how `MonadInterpreter` works, but attemptions was made :) ([First parser](https://github.com/KruchDmitriy/freezing-bear/blob/first_blood/parse.hs))

### Algorithm
This module separates in two submodules: `Integral` and `Interpolation`.

**Integral** submodule allows to compute an integral of functions in range [a, b] with a fixed step by 2 methods: trapezy and Simpson's. The signatures of this functions are
```haskell
trap_integral_step :: Function -> Double -> Double -> Step -> Area
``` and
```haskell
simpson_integral :: Function -> Double -> Double -> Step -> Area
```.

**Interpolation** submodule contains 
```haskell
interpolate_lagrange :: [Point] -> Function
``` method that implements Lagrange polynom interpolation by set of points `(x_i, y_i = f(x_i))`.

### Plot
The module intends for plotting functions by own algorithm, based on `OpenGL` and `GLUT` library.
The drawing function is represented by `DescriptorFunc` structure:

```haskell
data DescriptorFunc = DescriptorFunc {
    f     :: Function,
    a     :: Double,
    b     :: Double,
    step  :: Double,
    color :: Color4 GLfloat
}
```

To draw some function you should put certain `DescriptorFunc` instance to the queue via
```haskell
add_func :: QueueOfFunc -> DescriptorFunc -> QueueOfFunc
``` and then send the queue to
```haskell
draw_window :: QueueOfFunc -> IO ()
```.
