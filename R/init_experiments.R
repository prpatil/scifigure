#' Initialize a skeleton data frame to create a figure with \code{sci_figure}
#'
#' \code{init_experiments} generates a dataframe with the proper row and
#' column headers for user manipulation before calling \code{sci_figure}
#'
#' @param nexp The number of scientific experiments to be represented in the data frame, i.e. number of columns.
#' @param exp_names The names of each experiment, i.e. column names. Default: "Exp1, Exp2, ..."
#' @param stage_names The names of each step in the process, i.e. row names. Defaults match Patil et. al.
#' @export
#' @examples
#' # Generate the default data frame of three experiments
#' init_experiments()
#'
#' init_experiments(nexp = 5,
#' exp_names = c("Run_16_01", "Run_16_04", "Run_16_07",
#'		"Run_16_09", "Run_16_12"))
#' testthat::expect_error({
#' init_experiments(nexp = 2, exp_names = names)
#' })
#' @seealso \code{\link{sci_figure}}

init_experiments <- function(nexp = 3, exp_names = paste0("Exp", 1:nexp), stage_names = c("population", "question", "hypothesis", "experimental_design", "experimenter", "data", "analysis_plan", "analyst", "code", "estimate", "claim")){

	if(length(exp_names) != nexp){
		stop("Please ensure that the number of experiment names given matches the number of experiments.")
	}

	tmp <- as.data.frame(matrix("observed", length(stage_names), nexp), stringsAsFactors = FALSE)
	rownames(tmp) <- stage_names
	colnames(tmp) <- exp_names

	tmp
}
