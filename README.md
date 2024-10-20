# `ptmc` R Package 

## Parallel Tempering Markov Chain Monte Carlo 

This repository contains an R Package which takes an arbitrary likelihood and prior function for a set of fitted parameters and samples posterior values via a Metropolis Hastings algorithm with parallel tempering Markov chain Monte Carlo. This implementation is a generalised version of the algorithm outlined by [Miasojedow et al. 2012](https://arxiv.org/pdf/1205.1076.pdf). 

## Installation

To install the `ptmc` package, follow these steps:

### Step 1: Install R

Make sure you have R installed on your system. You can download R from [https://cran.r-project.org/](https://cran.r-project.org/).

### Step 2: Install `ptmc` from GitHub

You can install the development version of `ptmc` from GitHub using the `devtools` package. If you don't already have `devtools` installed, you can install it with:


```

install.packages("devtools")
devtools::install_github("dchodge/ptmc")

```

### Step 3: Load `ptmc` from GitHub
After installation, you can load the serojump package into your R session with:


```

library(ptmc)

```


## Background

See background/ptmc.pdf.

## Usage

We offer several examples of how to implement this package. 
- Case 1: A simple implementation with a continuous space of parameters


## Contributing

We welcome contributions and suggestions! If you'd like to contribute to the `ptmc` package or report issues, please feel free to:

- Submit a pull request on GitHub.
- Open an issue on the repository.

If you have any questions or feedback, or would like more informative vignettes, you can contact the package maintainer at:

**David Hodgson**  
Email: [david.hodgson@lshtm.ac.uk](mailto:david.hodgson@lshtm.ac.uk)

---

## Project Status

This package is actively maintained and in a stable, usable state. New features and improvements are continually being developed.