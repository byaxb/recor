# recor: Rearrangement Correlation Coefficient

**Pearson's r is undoubtedly the gold measure for linear dependence. Now, it might be the gold measure also for nonlinear monotone dependence, if adjusted.**  

## Overview

*recor* is an R package that implements the **Rearrangement Correlation Coefficient (r#)**, an adjusted version of Pearson's correlation coefficient designed to accurately measure arbitrary monotone dependence relationships (both linear and nonlinear). Based on cutting-edge statistical research, this package addresses the underestimation problem of traditional correlation coefficients in nonlinear monotone scenarios. The rearrangement correlation is derived from a tighter inequality than the classical Cauchy-Schwarz inequality, providing sharper bounds and expanded capture range.

## Features 

- 🎯 **Extended Capture Range**: From linear to arbitrary monotone dependence.
- 📊 **High Precision Measurement**: More accurate strength measurement than classical coefficients.
- 🔄 **Backward Compatibility**: Reverts to Pearson's r in linear scenarios, and to Spearman's ρ when calculated on ranks. 
- 🚀 **Efficient Implementation**: Optimized computation with C++ backend.
- 📈 **Multiple Input Support**: Automatically handles various input types (vector, matrix, data.frame) consistently with ```stats::cor()```.

## Installation
### Install from GitHub (Recommended)
```r
# Install devtools (if not already installed)
install.packages("devtools")
devtools::install_github("byaxb/recor")
```

## Quick Start
### Basic Usage

```r
library(recor)

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 6, 8, 10)
recor(x, y)
#> [1] 1

# Nonlinear monotone relationship
x <- c(1, 2, 3, 4, 5)
y <- c(1, 8, 27, 65, 125) # y = x^3
recor(x, y) # Higher value than Pearson's r
#> [1] 1
cor(x, y)
#> [1] 0.944458


# Matrix example
set.seed(123)
mat <- matrix(rnorm(100), ncol = 5)
colnames(mat) <- LETTERS[1:5]
recor(mat) # 5x5 correlation matrix
#>    A           B          C          D          E
#> A  1.00000000 -0.09511994 -0.1283021  0.1243721 -0.2328551
#> B -0.09511994  1.00000000  0.1022576  0.2381745  0.3780232
#> C -0.12830211  0.10225762  1.0000000 -0.1523651 -0.3603780
#> D  0.12437205  0.23817455 -0.1523651  1.0000000 -0.1289523
#> E -0.23285513  0.37802315 -0.3603780 -0.1289523  1.0000000


# Two matrices
mat1 <- matrix(rnorm(50), ncol = 5)
mat2 <- matrix(rnorm(50), ncol = 5)
recor(mat1, mat2) # 5x5 cross-correlation matrix
#>       [,1]         [,2]        [,3]        [,4]        [,5]
#> [1,]  0.0001379295  0.019273397 -0.14776094 -0.01203410  0.14712263
#> [2,] -0.0850363746  0.135125063 -0.10799623  0.35026884  0.20233183
#> [3,] -0.2825948208 -0.020383616 -0.31990514 -0.33267352 -0.48254414
#> [4,]  0.4067584970 -0.008022853  0.08223935  0.02728547  0.37567963
#> [5,]  0.5566966868 -0.059564374  0.03296252  0.22249817 -0.03009148

# data.frame
recor(iris[, 1:4])
#>                Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.1210250    0.9156110   0.8445397
#> Sepal.Width    -0.1210250   1.0000000   -0.4628225  -0.3909946
#> Petal.Length    0.9156110  -0.4628225    1.0000000   0.9694665
#> Petal.Width     0.8445397  -0.3909946    0.9694665   1.0000000
```

## Theoretical Foundation

### Mathematical Definition
The rearrangement correlation coefficient is based on rearrangement inequality theorems that provide tighter bounds than the Cauchy-Schwarz inequality. Mathematically, for samples x and y, it is defined as:

r#(x, y) = sₓᵧ / |sₓ↑, ᵧ↕|

Where:
- `sₓᵧ` is the sample covariance between x and y
- `x↑` denotes the increasing rearrangement of x
- `y↕` denotes either:
  - `y↑` (increasing rearrangement of y) if sₓᵧ ≥ 0
  - `y↓` (decreasing rearrangement of y) if sₓᵧ < 0

### R Implementation
r# can be computed in R as follows:
```r
recor <- function(x, y = NULL) {
    recor_vector <- function(x, y) {
        numerator <- cov(x, y)
        if (numerator >= 0) {
            denominator <- abs(covar(
                sort(x, decreasing = FALSE),
                sort(y, decreasing = FALSE)
            ))
        } else {
            denominator <- abs(covar(
                sort(x, decreasing = FALSE),
                sort(y, decreasing = TRUE)
            ))
        }
        numerator / denominator
    }

    if (is.matrix(x) || is.data.frame(x)) {
        x <- as.matrix(x)
        if (is.null(y)) {
            p <- ncol(x)
            result <- matrix(1, nrow = p, ncol = p)
            rownames(result) <- colnames(result) <- colnames(x)

            for (i in 1:p) {
                for (j in 1:p) {
                    if (i != j) {
                        result[i, j] <- result[j, i] <- recor_vector(x[, i], x[, j])
                    }
                }
            }
            return(result)
        } else if (is.matrix(y) || is.data.frame(y)) {
            y <- as.matrix(y)
            if (nrow(x) != nrow(y)) {
                stop("The number of rows of x and y must be the same")
            }

            p <- ncol(x)
            q <- ncol(y)
            result <- matrix(0, nrow = p, ncol = q)
            rownames(result) <- colnames(x)
            colnames(result) <- colnames(y)

            for (i in 1:p) {
                for (j in 1:q) {
                    result[i, j] <- recor_vector(x[, i], y[, j])
                }
            }
            return(result)
        }
    }

    if (is.null(y)) {
        stop("y is needed when x is a vector")
    }

    if (length(x) != length(y)) {
        stop("x and y must have the same length")
    }

    if (length(x) < 2) {
        stop("x and y must have at least two elements")
    }

    recor_vector(x, y)
}
```
It is to be noted that the above R implementation is for illustrative purposes only. The actual *recor* package employs a highly optimized C++ backend to ensure efficient computation.

### Intuitive Example
To take a simple example, let  x = (4, 3, 2, 1)  and

- y1 = (5, 4, 3, 2)
- y2 = (5, 4, 3, 3.25) 
- y3 = (5, 4, 3, 3.50)
- y4 = (5, 4, 3, 3.75)
- y5 = (5, 4, 3, 4.50)

Obviously, y1 and x behaves exactly in the same way, with their values getting small and small step
by step. The behavior of y2, y3, y4 and y5 are becoming more and more different from that of x.
However, the rho values are all the same for y2, y3, y4. In contrast, the r# values can reveal all
these differences exactly.
```r
x <- c(4, 3, 2, 1) 

y_list <- list(y1 = c(5, 4, 3, 2.00),
               y2 = c(5, 4, 3, 3.25),
               y3 = c(5, 4, 3, 3.50),
               y4 = c(5, 4, 3, 3.75),
               y5 = c(5, 4, 3, 4.50))

# recor
lapply(y_list, recor, x)
#> $y1
#> [1] 1
#> 
#> $y2
#> [1] 0.9259259
#> 
#> $y3
#> [1] 0.8461538
#> 
#> $y4
#> [1] 0.76
#> 
#> $y5
#> [1] 0.3846154

#cor
lapply(y_list, cor, x, method = "spearman")
#> $y1
#> [1] 1
#> 
#> $y2
#> [1] 0.8
#> 
#> $y3
#> [1] 0.8
#> 
#> $y4
#> [1] 0.8
#> 
#> $y5
#> [1] 0.4
```

## References

Ai, X. (2024). Adjust Pearson's r to Measure Arbitrary Monotone Dependence. In *Advances in Neural Information Processing Systems* (Vol. 37, pp. 37385-37407).

## License

This project is licensed under GPL-3. See the [LICENSE](LICENSE) file for details.

## Support & Feedback

- 📧 **Email Support**: axb@bupt.edu.cn
- 🐛 **Issue Reporting**: [GitHub Issues](https://github.com/byaxb/recor/issues)
- 📚 **Documentation**: [Complete Documentation](https://github.com/byaxb/recor/wiki)

## Citation

If you use this package in your research, please cite our work as: <br>

```bibtex
@inproceedings{NEURIPS2024_41c38a83,
 author = {Ai, Xinbo},
 booktitle = {Advances in Neural Information Processing Systems},
 editor = {A. Globerson and L. Mackey and D. Belgrave and A. Fan and U. Paquet and J. Tomczak and C. Zhang},
 pages = {37385--37407},
 publisher = {Curran Associates, Inc.},
 title = {Adjust Pearson\textquotesingle s r to Measure Arbitrary Monotone Dependence},
 url = {https://proceedings.neurips.cc/paper_files/paper/2024/file/41c38a83bd97ba28505b4def82676ba5-Paper-Conference.pdf},
 volume = {37},
 year = {2024}
}
```

---

*recor: Making Correlation Measurement More Accurate*
