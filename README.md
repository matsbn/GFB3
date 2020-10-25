# GFB3

Implementation of a shallow-water model, discretised on a C-grid with generalized forward-backward time stepping and alternate u/v evaluation in the coriolis term. Height and velocity variables are staggered in time. The shallow-water equation monopole vortex of Milliff and McWilliams (1994) is integrated as a test case.

Edit or add the file \<compiler\>.make for the compiler and options of your choice. Build with:

```sh
$ make COMPILER=<compiler>
```
