#' Create a figure depicting replicability
#'
#' \code{replicate_figure} is a wrapper around the \code{sci_figure} function
#' to illustrate replicability in a two-experiment setting. Options for
#' \code{sci_figure} are accepted, but this may be run as is.
#'
#' @param ... Additional arguments passed to \code{sci_figure}.
#'
#' @seealso \code{\link{sci_figure}} for additional arguments.
#'
#' @export

replicate_figure <- function(...){
	exps <- init_experiments(2, names = c("Original", "Replication"))
	exps[c("experimenter", "data", "analyst", "code", "estimate", "claim"), "Replication"] <- "different"
	sci_figure(exps, ...)
}


