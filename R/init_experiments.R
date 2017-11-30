#' Initialize a skeleton data frame to create a figure with \code{sci_figure}
#'
#' \code{init_experiments} generates a dataframe with the proper row and
#' column headers for user manipulation before calling \code{sci_figure}
#'
#' @param nexp The number of scientific experiments to be represented in the data frame, i.e. number of columns.
#' @param names The names of each experiment, i.e. column names.
#' @export
#' @examples
#' # Generate the default data frame of three experiments
#' init_experiments()
#'
#' init_experiments(nexp = 5, 
#' names = c("Run_16_01", "Run_16_04", "Run_16_07", 
#'		"Run_16_09", "Run_16_12")) 
#' @seealso \code{\link{sci_figure}}

init_experiments <- function(nexp = 3, names = paste0("Exp", 1:nexp)){
	
	if(length(names) != nexp){
		stop("Please ensure that the number of experiment names given matches the number of experiments.")
	}

	tmp <- as.data.frame(matrix("observed", 11, nexp), stringsAsFactors = FALSE)
	rownames(tmp) <- c("population", "question", "hypothesis", "experimental_design", "experimenter", "data", "analysis_plan", "analyst", "code", "estimate", "claim")
	colnames(tmp) <- names
	
	tmp
}
