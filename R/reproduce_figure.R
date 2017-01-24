#' Create a figure depicting reproducibility
#'
#' \code{reproduce_figure} is a wrapper around the \code{sci_figure} function
#' to illustrate reproducibility in a two-experiment setting.
#'
#' @export

reproduce_figure <- function(){
	exps <- init_experiments(2, names = c("Original", "Reproduction"))
	exps["analyst", "Reproduction"] <- "different"
	sci_figure(exps)
}

