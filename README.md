# NDRoot

Wolfram Language package leveraging the power of the built-in numerical differential equation solver `NDSolve` to find function roots

**Code:** [github.com/statius/ndroot](https://github.com/statius/ndroot)

## Installation

- Download the latest release and unpack it or clone the repository somewhere on the Wolfram Language `$Path` (e.g. the `Applications` folder in `$UserBaseDirectory` for *Mathematica*).

- Load *NDRoot* as

  ```mathematica
  Needs @ "NDRoot`"
  ```

## Usage

The basic syntax is very similar to that of `FindRoot`:

```mathematica
NDRoot[BesselY[0, x] BesselJ[0, x], {x, 0, 10}]
```

will find all roots in the inverval [0, 10]. 

For a more complete set of usage examples, see *usage-examples.nb*. 

## TODO

- Generalize to systems of equations.

## Project Information

### Licensing

This project is released under the MIT license.

### Contributions

This package is maintained by Andrew Miller (and primarily created for my own needs). Pull requests and suggestions are always welcomed.