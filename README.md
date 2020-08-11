Herbie support for fixed-point numbers
===

This repository contains a plugin for [Herbie](https://herbie.uwplse.org) to support fixed-point numbers

Fixed-point numbers are defined with n integer and m fractional bits. Use in Herbie by specifying `:precision (fixed n m)`.

This package contains:

+ Definitions of fixed-point numbers and their operators for Herbie
+ Rewrite rules for these operators

The best way to install this package is using the Racket package manager:

    raco pkg install fixedpoint-herbie