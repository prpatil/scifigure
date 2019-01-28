#' Create a figure depicting reproducibility
#'
#' \code{reproduce_figure} is a wrapper around the \code{sci_figure} function
#' to illustrate reproducibility in a two-experiment setting. Options for
#' \code{sci_figure} are accepted, but this may be run as is.
#'
#' @param ... Additional arguments passed to \code{sci_figure}.
#'
#' @seealso \code{\link{sci_figure}} for additional arguments.
#'
#' @export

reproduce_figure <- function(...){
	exps <- init_experiments(2, exp_names = c("Original", "Reproduction"))
	exps["analyst", "Reproduction"] <- "different"
	sci_figure(exps, ...)
}

