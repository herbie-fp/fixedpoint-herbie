Herbie support for fixed-point numbers
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support fixed-point numbers

Fixed-point numbers are defined with `n` total bits and a scale factor of 2 ^ `s`.
Alternatively, use `:precision integer` for `:precision (fixed 32 0)`
  and `:precision (integer n)` for `:precision (fixed n 0)`.

This package contains:

+ Definitions of fixed-point numbers and their operators for Herbie
+ Rewrite rules for these operators

Installation:

1. Clone this repository, preferably into a folder called `fixedpoint-herbie`.
2. If the folder name is `fixedpoint-herbie`, run `raco pkg install` within the folder.
   Otherwise, run `raco pkg install -n fixedpoint-herbie`.
