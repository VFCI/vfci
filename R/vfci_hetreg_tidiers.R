#' Tidying methods for heteroskedastic linear regressions
#'
#' Tidying methods `tidy`, `glance` and `augment` for objects of class `het`
#' that are compatible with [broom][broom::broom()].
#'
#' @param x An object of class `het` created by [vfci::hetreg()].
#'
#' @template param_confint
#' @template param_unused_dots
#' @aliases het_tidiers
#' @return Various statistics and post-estimation values for [hetreg()]
#' @seealso [broom::tidy()], `nlme_tidiers()` from [`broom.mixed`](https://github.com/bbolker/broom.mixed)

#' @export
#' @describeIn tidy.het returns the same information as `nlme_tidiers()` from [`broom.mixed`](https://github.com/bbolker/broom.mixed)
#' for the mean equation, and adds coefficients, standard errors, t-statistics and p-values for the variance equation
tidy.het <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  get_attr <- \(x) attr(x,"formula")
  idx <- list("modelStruct","varStruct", \(x) purrr::map(x,get_attr))
  vareq_names <- purrr::pluck(x,!!!idx) %>% unname %>% purrr::map(deparse1) %>% stringr::str_remove_all("~") %>% stringr::str_split(" ")
  vareq_coef <- 2 * attr(x$apVar, "Pars") %>% unname
  estimate<-std.error<-statistic<-NULL
  result <- dplyr::tibble(
    term = c(vareq_names,"(Intercept)") %>% unlist,
    estimate = vareq_coef, # \alpha coefficients of vol eq
    std.error = 2 * sqrt(diag(x$apVar)) %>% unname, # std.error of \alpha
    statistic = estimate/std.error,
    p.value = 2 * stats::pnorm(-abs(statistic))
  )
  if (conf.int) {
    ci <- stats::confint(x, level = conf.level)
    result <- dplyr::left_join(result, ci, by = "term")
  }
  result
}
#' @export
#' @describeIn tidy.het same as [`broom.mixed::glance.gls()`](https://github.com/bbolker/broom.mixed)
glance.het <- function(x, ...) {
  x <- structure(x, class = c("gls"))
  broom.mixed::glance(x)
}
#' @export
#' @describeIn tidy.het returns the standard deviation of the error term
augment.het <- function(x, ...) {
  data.frame(
    "Model" = "het",
    ".vfci" = log(attr(x$residuals, "std")) # vfci
  )
}

#' @importFrom generics tidy
#' @export
#' @seealso [`broom.mixed::tidy.gls()`](https://github.com/bbolker/broom.mixed)
generics::tidy

#' @importFrom generics glance
#' @export
#' @seealso [`broom.mixed::glance.gls()`](https://github.com/bbolker/broom.mixed)
generics::glance

#' @importFrom generics augment
#' @export
#' @seealso [`broom.mixed::augment.gls()`](https://github.com/bbolker/broom.mixed)
generics::augment