#' Get all the scenarios, both monotonic and notmonotonic
#' 
#' @param type, type of scenarios, see the details
#' @param with_expr, whether the funs should be attained by the expressions
#' @details
#' 
#' There are 9 types of scenarios:
#' 
#' \itemize{
#'   \item all, all the scenarios
#'   \item accuracy, scenarios for accuracy analysis (1~66)
#'   \item accuracy_mon, monotonic scenarios for accuracy analysis (1~50)
#'   \item accuracy_notmon, notmonotonic scenarios for accuracy analysis (51~66)
#'   \item power, scenarios for power analysis (1~107)
#'   \item power_mon, monotonic scenarios for power analysis (1~50, 67~107)
#'   \item power_notmon, the same as nonmonotonic scenarios for accuracy analysis (51~66)
#'   \item power_distinguish, 45 distinguishable scenarios for power analysis
#'   \item extreme_mon_pairs, 12 normally and extremely monotonic scenarios for power analysis
#' }
#' 
#' 
#' All scenarios are listed in the following table:
#' 
#' 
#' \tabular{rlllllll}{
#'   \strong{index} \tab \strong{funs} \tab \strong{exprs} \tab \strong{monotonic} \tab \strong{extreme_mon} \tab \strong{distinguish_mon} \tab \strong{power} \tab \strong{accuracy} \cr
#'     1 \tab y <- 2*x+1                                                                                    \tab 'Linear'                          \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     2 \tab y <- x^2                                                                                      \tab 'Quadratic'                       \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     3 \tab y <- sqrt(x)                                                                                  \tab 'Square Root'                     \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     4 \tab y <- x^3                                                                                      \tab 'Cubic'                           \tab YES \tab 1  \tab NA \tab YES \tab YES\cr
#'     5 \tab y <- 1/x                                                                                      \tab 'Reciprocal'                      \tab YES \tab NA \tab 15 \tab YES \tab YES\cr
#'     6 \tab y <- exp(x)                                                                                   \tab 'Exponential'                     \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     7 \tab y <- log(x)                                                                                   \tab 'Logarithm'                       \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     8 \tab y <- sin(pi/2*x)                                                                              \tab 'Sine'                            \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'     9 \tab y <- cos(pi/2*x)                                                                              \tab 'Cosine'                          \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    10 \tab y <- tan(pi/2*x)                                                                              \tab 'Tangent'                         \tab YES \tab NA \tab 19 \tab YES \tab YES\cr
#'    11 \tab y <- cot(pi/2*x)                                                                              \tab 'Cotangent'                       \tab YES \tab NA \tab 20 \tab YES \tab YES\cr
#'    12 \tab y <- asin(x)                                                                                  \tab 'I-Sine'                          \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    13 \tab y <- acos(x)                                                                                  \tab 'I-Cosine'                        \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    14 \tab y <- atan(x)                                                                                  \tab 'I-Tangent'                       \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    15 \tab y <- acot(x)                                                                                  \tab 'I-Cotangent'                     \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    16 \tab y <- sec(pi/2*x)                                                                              \tab 'Secant'                          \tab YES \tab NA \tab 4  \tab YES \tab YES\cr
#'    17 \tab y <- csc(pi/2*x)                                                                              \tab 'Cosecant'                        \tab YES \tab NA \tab 5  \tab YES \tab YES\cr
#'    18 \tab y <- sinh(x)                                                                                  \tab 'H-Sine'                          \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    19 \tab y <- cosh(x)                                                                                  \tab 'H-Cosine'                        \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    20 \tab y <- tanh(x)                                                                                  \tab 'H-Tangent'                       \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    21 \tab y <- coth(x)                                                                                  \tab 'H-Cotangent'                     \tab YES \tab NA \tab 21 \tab YES \tab YES\cr
#'    22 \tab y <- sech(100*x)                                                                              \tab 'H-Secant'                        \tab YES \tab NA \tab 10 \tab YES \tab YES\cr
#'    23 \tab y <- csch(100*x)                                                                              \tab 'H-Cosecant'                      \tab YES \tab NA \tab 11 \tab YES \tab YES\cr
#'    24 \tab y <- asinh(x)                                                                                 \tab 'I-H-Sine'                        \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    25 \tab y <- acosh(1+x)                                                                               \tab 'I-H-Cosine'                      \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    26 \tab y <- atanh(x)                                                                                 \tab 'I-H-Tangent'                     \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    27 \tab y <- acoth(1+x)                                                                               \tab 'I-H-Cotangent'                   \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    28 \tab y <- asech(x)                                                                                 \tab 'I-H-Secant'                      \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    29 \tab y <- acsch(x)                                                                                 \tab 'I-H-Cosecant'                    \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    30 \tab y <- x + 1/x                                                                                  \tab 'Hook'                            \tab YES \tab NA \tab 23 \tab YES \tab YES\cr
#'    31 \tab y <- (x+1)/(x-1)                                                                              \tab 'Rational Function'               \tab YES \tab NA \tab 42 \tab YES \tab YES\cr
#'    32 \tab y <- exp(x) / x                                                                               \tab 'Hoerl Function'                  \tab YES \tab NA \tab 13 \tab YES \tab YES\cr
#'    33 \tab y <- sigmoid(x-0.5)                                                                           \tab 'Sigmoid Function'                \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    34 \tab y <- logit(x)                                                                                 \tab 'Logit Function'                  \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    35 \tab y <- 1*(x>0.5)                                                                                \tab 'Step'                            \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    36 \tab y <- 0*(x<49/100) + (50*(x-0.5)+0.5) * (x >= 49/100 & x <=51/100) + (x>51/100)                \tab 'Piecewise'                       \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    37 \tab y <- 0.1*sin(10.6*(2*x-1)) + 1.1*(2*x-1)                                                      \tab 'Linear+Periodic'                 \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    38 \tab y <- sin(pi*x)/(pi*x)                                                                         \tab 'Sinc Function'                   \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    39 \tab y <- einsteinF(1,  x)                                                                         \tab 'Einstein Function1'              \tab YES \tab 5  \tab NA \tab YES \tab YES\cr
#'    40 \tab y <- expint_E1(x)                                                                             \tab 'Exponential Integral'            \tab YES \tab 7  \tab NA \tab YES \tab YES\cr
#'    41 \tab y <- Shi(x)                                                                                   \tab 'H-Sine Integral'                 \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    42 \tab y <- Chi(x)                                                                                   \tab 'H-Cosine Integral'               \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    43 \tab y <- erf(x)                                                                                   \tab 'Error Function'                  \tab YES \tab 11 \tab NA \tab YES \tab YES\cr
#'    44 \tab y <- erfinv(x)                                                                                \tab 'I-Error Function'                \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    45 \tab y <- gamma(x)                                                                                 \tab 'Gamma Function'                  \tab YES \tab NA \tab 32 \tab YES \tab YES\cr
#'    46 \tab y <- psi(0, x)                                                                                \tab 'Psi Function'                    \tab YES \tab NA \tab 33 \tab YES \tab YES\cr
#'    47 \tab y <- zeta(x+1)                                                                                \tab 'Riemann Zeta Function'           \tab YES \tab NA \tab 37 \tab YES \tab YES\cr
#'    48 \tab y <- bessel_Yn(0, x)                                                                          \tab 'Bessel Function'                 \tab YES \tab NA \tab NA \tab YES \tab YES\cr
#'    49 \tab y <- beta(1, x)                                                                               \tab 'Beta Function'                   \tab YES \tab NA \tab 40 \tab YES \tab YES\cr
#'    50 \tab y <- eta(x)                                                                                   \tab 'Dirichlet Eta Function'          \tab YES \tab 9  \tab NA \tab YES \tab YES\cr
#'    51 \tab y <- 4*(x-0.5)^2                                                                              \tab 'Quadratic [Symmetry]'            \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    52 \tab y <- 128*(x-1/3)^3 - 48*(x-1/3)^2-12*(x-1/3)                                                  \tab 'Cubic 2'                         \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    53 \tab y <- sin(16*pi*x)                                                                             \tab 'Sine [High Freq]'                \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    54 \tab y <- cos(14*pi*x)                                                                             \tab 'Cosine [High Freq]'              \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    55 \tab y <- 200*x*(x < 1/200) + (-198*x+199/100)*(x >= 1/200 & x < 1/100) + (-x/99+1/99) *(x>=1/100) \tab 'Lopsided L-shaped'               \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    56 \tab y <- sqrt(1-(2*x-1)^2)                                                                        \tab 'Circle'                          \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    57 \tab y <- sin(10*pi*x)+x                                                                           \tab 'Linear + Periodic [Medium Freq]' \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    58 \tab y <- 4*(2.4*x - 1.3)^3+(2.4*x - 1.3)^2-4*(2.4*x - 1.3)                                        \tab 'Cubic 3'                         \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    59 \tab y <- 41*(4*(2.4*x - 1.3)^3+(2.4*x - 1.3)^2-4*(2.4*x - 1.3))                                   \tab 'Cubic [Y-stretched]'             \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    60 \tab y <- sin(4*pi*x)                                                                              \tab 'Sine [Two Periods]'              \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    61 \tab y <- sin(8*pi*x)                                                                              \tab 'Sine [Low Freq]'                 \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    62 \tab y <- sin(9*pi*x)                                                                              \tab Sine, Non-Fourier Freq [Low]'     \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    63 \tab y <- cos(7*pi*x)                                                                              \tab Cosine, Non-Fourier Freq [Low]'   \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    64 \tab y <- sin(6*pi*x*(1+x))                                                                        \tab Sine, Varying Freq [Medium]'      \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    65 \tab y <- cos(5*pi*x*(1+x))                                                                        \tab Cosine, Varying Freq [Medium]'    \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    66 \tab y <- 0.2*sin(10.6*(2*x-1)) + 1.1*(2*x-1)                                                      \tab Linear + Periodic, High Freq 2'   \tab NO  \tab NA \tab NA \tab YES \tab YES\cr
#'    67 \tab y <- exp(3*x)                                                                                 \tab 'Exponential #2'                  \tab YES \tab 3  \tab NA \tab YES \tab NO \cr
#'    68 \tab y <- exp(300*x)                                                                               \tab 'Exponential #3'                  \tab YES \tab 4  \tab 18 \tab YES \tab NO \cr
#'    69 \tab y <- sinh(100*x)                                                                              \tab 'Hyperbolic Cosine #1'            \tab YES \tab NA \tab 7  \tab YES \tab NO \cr
#'    70 \tab y <- cosh(100*x)                                                                              \tab 'Hyperbolic Tangent #1'           \tab YES \tab NA \tab 8  \tab YES \tab NO \cr
#'    71 \tab y <- tanh(100*x)                                                                              \tab 'Hyperbolic Cotangent #1'         \tab YES \tab NA \tab 9  \tab YES \tab NO \cr
#'    72 \tab y <- einsteinF(1, 300*x)                                                                      \tab 'Einstein Function1'              \tab YES \tab 6  \tab NA \tab YES \tab NO \cr
#'    73 \tab y <- expint_E1(300*x)                                                                         \tab 'Exponential Integral #2'         \tab YES \tab 8  \tab 28 \tab YES \tab NO \cr
#'    74 \tab y <- Shi(300*x)                                                                               \tab 'Hyperbolic Sine Integral #2'     \tab YES \tab NA \tab 29 \tab YES \tab NO \cr
#'    75 \tab y <- Chi(300*x)                                                                               \tab 'Hyperbolic Cosine Integral #2'   \tab YES \tab NA \tab 30 \tab YES \tab NO \cr
#'    76 \tab y <- erf(300*x)                                                                               \tab 'Error Function #2'               \tab YES \tab 12 \tab 31 \tab YES \tab NO \cr
#'    77 \tab y <- psi(1, x)                                                                                \tab 'Psi Function #2'                 \tab YES \tab NA \tab 34 \tab YES \tab NO \cr
#'    78 \tab y <- psi(2, x)                                                                                \tab 'Psi Function #3'                 \tab YES \tab NA \tab 35 \tab YES \tab NO \cr
#'    79 \tab y <- psi(3, x)                                                                                \tab 'Psi Function #4'                 \tab YES \tab NA \tab 36 \tab YES \tab NO \cr
#'    80 \tab y <- bessel_Yn(1, x)                                                                          \tab 'Bessel Function #2'              \tab YES \tab NA \tab 38 \tab YES \tab NO \cr
#'    81 \tab y <- bessel_Yn(2, x)                                                                          \tab 'Bessel Function #3'              \tab YES \tab NA \tab 39 \tab YES \tab NO \cr
#'    82 \tab y <- eta(1000*x)                                                                              \tab 'Dirichlet Eta Function #2'       \tab YES \tab 10 \tab 41 \tab YES \tab NO \cr
#'    83 \tab y <- x                                                                                        \tab 'Identical'                       \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    84 \tab y <- x^0.25                                                                                   \tab 'Fourth Root'                     \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    85 \tab y <- 2^(10*x)                                                                                 \tab 'Exponential base=2'              \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    86 \tab y <- 2^(100*x)                                                                                \tab 'Exponential base=2 #2'           \tab YES \tab NA \tab 17 \tab YES \tab NO \cr
#'    87 \tab y <- 10^(10*x)                                                                                \tab 'Exponential base=10'             \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    88 \tab y <- 10^(100*x)                                                                               \tab 'Exponential base=10 #2'          \tab YES \tab NA \tab 1  \tab YES \tab NO \cr
#'    89 \tab y <- tan(pi*(x-0.5))                                                                          \tab 'Tangent [one period]'            \tab YES \tab NA \tab 2  \tab YES \tab NO \cr
#'    90 \tab y <- cot(pi*x)                                                                                \tab 'Cotangent [one period]'          \tab YES \tab NA \tab 3  \tab YES \tab NO \cr
#'    91 \tab y <- x^100                                                                                    \tab 'Power #1'                        \tab YES \tab NA \tab 6  \tab YES \tab NO \cr
#'    92 \tab y <- x^3000                                                                                   \tab 'Power #2'                        \tab YES \tab 2  \tab 22 \tab YES \tab NO \cr
#'    93 \tab y <- x^-3                                                                                     \tab 'Rational'                        \tab YES \tab NA \tab 16 \tab YES \tab NO \cr
#'    94 \tab y <- as.numeric(x/99*(x<=0.99) + (x > 0.99))                                                  \tab 'L-shaped'                        \tab YES \tab NA \tab 12 \tab YES \tab NO \cr
#'    95 \tab y <- 20*(x<1/20) + (-18*x+19/10) * (x>=1/20 & x < 1/10) + (-x/9+1/9)*(x>=1/10)                \tab 'Spike'                           \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    96 \tab y <- 0.2*sin(4*(2*x-1)) +1.1*(2*x-1)                                                          \tab Linear+Periodic, Low Freq'        \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'    97 \tab y <- exp(x) / x^2                                                                             \tab 'Exponential / Quadratic'         \tab YES \tab NA \tab 14 \tab YES \tab NO \cr
#'    98 \tab y <- log(x)/x                                                                                 \tab 'Logarithm / Linear'              \tab YES \tab NA \tab 43 \tab YES \tab NO \cr
#'    99 \tab y <- exp(x) / log(x)                                                                          \tab 'Exponential / Logarithm'         \tab YES \tab NA \tab 44 \tab YES \tab NO \cr
#'   100 \tab y <- exp(x) / cos(pi/2*x)                                                                     \tab 'Exponential / Cosine'            \tab YES \tab NA \tab 45 \tab YES \tab NO \cr
#'   101 \tab y <- (50*x)^(50*x)                                                                            \tab 'Power Exponential'               \tab YES \tab NA \tab 24 \tab YES \tab NO \cr
#'   102 \tab y <- einsteinF(2, x)                                                                          \tab 'Einstein function2 #1'           \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'   103 \tab y <- einsteinF(2, 1000*x)                                                                     \tab 'Einstein function2 #2'           \tab YES \tab NA \tab 25 \tab YES \tab NO \cr
#'   104 \tab y <- einsteinF(3, x)                                                                          \tab 'Einstein function3 #1'           \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'   105 \tab y <- einsteinF(3, 1000*x)                                                                     \tab 'Einstein function3 #2'           \tab YES \tab NA \tab 26 \tab YES \tab NO \cr
#'   106 \tab y <- einsteinF(4, x)                                                                          \tab 'Einstein function4 #1'           \tab YES \tab NA \tab NA \tab YES \tab NO \cr
#'   107 \tab y <- einsteinF(4, 1000*x)                                                                     \tab 'Einstein function4 #2'           \tab YES \tab NA \tab 27 \tab YES \tab NO 
#' }
#'  
#' 
#' 
#' @return numeric vector
#' @export
scenarios2 <- function(type = c("all",
                               "accuracy", "accuracy_mon", "accuracy_notmon",
                               "power", "power_mon", "power_notmon", "power_distinguish",
                               "extreme_mon_pairs", "Xks"),
                      with_expr = TRUE) {
    
    type <- match.arg(type)
    results <- switch (type,
                       all = scenario_tabular2,
                       accuracy = scenario_tabular2 %>%
                           filter(accuracy == "YES"),
                       accuracy_mon = scenario_tabular2 %>%
                           filter(accuracy == "YES" & monotonic == "YES"),
                       accuracy_notmon = scenario_tabular2 %>%
                           filter(accuracy == "YES" & monotonic == "NO"),
                       power = scenario_tabular2 %>%
                           filter(power == "YES"),
                       power_mon = scenario_tabular2 %>%
                           filter(power == "YES" & monotonic == "YES"),
                       power_notmon = scenario_tabular2 %>%
                           filter(power == "YES" & monotonic == "NO"),
                       power_distinguish = scenario_tabular2 %>%
                           filter(!is.na(distinguish_mon)) %>%
                           arrange(distinguish_mon),
                       extreme_mon_pairs = scenario_tabular2 %>%
                           filter(!is.na(extreme_mon)) %>%
                           arrange(extreme_mon),
                       Xks = data.frame(funs = paste0("y <- x^", seq(1, 10, by = 0.1)),
                                        exprs = paste0("y <- x^", seq(1, 10, by = 0.1)))
    )
    results %>%
        select(funs, exprs) -> results
    if(!with_expr) {
        results %>%
            pull(funs) -> results
    }
    return(results)
}
#' @export
scenario_index2 <- function(type = c("all",
                                    "accuracy", "accuracy_mon", "accuracy_notmon",
                                    "power", "power_mon", "power_notmon", "power_distinguish",
                                    "extreme_mon_pairs")) {
    type <- match.arg(type)
    index_of_A_in_B(scenarios2(type, with_expr = FALSE), scenarios2("all", with_expr = FALSE))
}

