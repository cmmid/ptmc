## Parallel Tempering Markov Chain Monte Carlo

![](./man/figures/ptmc_ai.png)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/seroanalytics/serojump/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/seroanalytics/serojump/actions/workflows/R-CMD-check.yaml)

# ptmc: Parallel Tempering Markov Chain Monte Carlo for Discrete Spaces

**`ptmc`** is an R package designed to perform Bayesian inference using a **Parallel Tempering Markov Chain Monte Carlo (PT-MCMC)** algorithm. This implementation allows efficient exploration of  **discrete parameter spaces**, making it especially useful for models where standard MCMC techniques struggle with multimodal posteriors or slow convergence. This implementation is a generalised version of the algorithm outlined by [Miasojedow et al. 2012](https://arxiv.org/pdf/1205.1076.pdf).

## Features

* Implements a **Metropolis-Hastings algorithm** with parallel tempering for enhanced sampling efficiency.
* Supports  **arbitrary user-defined likelihood and prior functions** , enabling application to a wide range of models.
* Designed to effectively sample from **complex posterior distributions** with multiple modes.
* Parallelized execution across chains for improved performance.

## Why Use Parallel Tempering?

Standard MCMC techniques can become inefficient when dealing with posterior distributions that:

* Have multiple modes (e.g., distinct peaks in probability).
* Feature complex discrete spaces where local exploration struggles to reach distant areas of high probability.

Parallel tempering mitigates these challenges by running multiple chains at different "temperatures." High-temperature chains explore the parameter space more broadly, while low-temperature chains focus on precise posterior estimation. Occasional exchanges between chains help the algorithm escape local optima and improve convergence.

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

See background/ptmc.pdf.

## Getting Started

We offer several examples of how to implement this package.

- Case 1: A simple implementation with a continuous space of parameters
- Case 2: Negronis all round: simulation recovery of a discrete model 🍹

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
