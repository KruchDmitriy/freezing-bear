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
