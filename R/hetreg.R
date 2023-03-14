#'  Heteroskedastic linear regression
#'
#' `hetreg()` fits a heteroskedastic linear regression
#' where the variance is an exponential function of specified variables
#'
#' `hetreg()` is a wrapper for [nlme::gls()]. Compared
#' to [`gls()`][nlme::gls()], the function `hetreg()`
#' * is pipeable (plays nice with the tidyverse)
#' * automatically constructs the [`gls()`][nlme::gls()] argument `model` from
#' `y` and `x`, and the [`gls()`][nlme::gls()] argument `weights` from `het`
#' * only allows for an exponential variance function structure defined by
#' a combination [`varComb()`][nlme::varComb()] of [varExp()][nlme::varExp()]
#'  terms
#' * has tidy support
#'
#' The model estimated is
#' \deqn{y_i = x_i \beta + \epsilon_i \\
#'       var(\epsilon_i) = \exp(2 \theta v_i )}
#' where where \eqn{y_i}, \eqn{i = 1,\dotsc,n}, is the dependent variable;
#'  \eqn{x_i = (x_{1i},x_{2i},\dotsc,x_{ki})} are the k independent
#'  variables that model the mean function;
#'   \eqn{v_i = (v_{1i},v_{2i},\dotsc,v_{mi})}
#'   are the m variables that model the variance function (corresponding to the
#'   argument `het`); \eqn{\beta} is a column vector of unknown parameters in
#'    the mean function, and \eqn{\theta} is a column vector of unknown
#'    parameters in the variance function.
#'  `hetreg()` is similar to the command
#'   [hetregress](https://www.stata.com/manuals/rhetregress.pdf)
#'  in Stata, where
#'  ```Stata
#'  hetregress x y, het(z)
#'  ```
#'  corresponds to the model
#'  \deqn{y_i = x_i \beta + \epsilon_i \\
#'       var(\epsilon_i) = \exp(z_i \alpha )}
#' The \eqn{z_i} map to \eqn{v_i} and \eqn{\alpha} maps to \eqn{2 \theta}.
#'
#' @param data data frame containing the variables in `y`, `x` and `het`.
#' @param y string (character vector) with the name of the independent variable.
#' @param x vector of strings (character vectors) with the name of the
#' independent variable(s).
#' @param het vector of strings (character vectors) with the name of the
#'  variable(s) that enter(s) the variance function as independent variable(s).
#' @template param_unused_dots
#' @param gls_opt optional arguments passed to [`gls()`][nlme::gls()].
#'
#' @return an object of class `gls` representing the linear model fit.
#' See [`gls()`][nlme::gls()] for details.
#'
#' @export
hetreg <- function(data, y, x, het = x, ..., gls_opt = NULL) {
  call <- match.call()
  mean_eq <- paste(y, "~", paste(x, collapse = "+"))
  weights <- eval(parse(text = glue::glue(
    "nlme::varComb( \n",
    glue::glue_collapse(glue::glue("nlme::varExp(form = ~ {het})"), ",\n"),
    ")"
  )))
  frm <- stats::formula(paste(mean_eq, collapse = " "))
  h <- nlme::gls(frm, data = data, weights = weights, gls_opt)
  h$call$hetreg <- call
  return(h)
}
