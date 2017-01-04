#' Create a figure depicting reproducibility/replicability of a set of
#' scientific experiments
#'
#' \code{sci_figure} creates a graphical representation of changes in a
#' a set of subsequent studies or reproduction attempts as compared to an 
#' original study.
#'
#' @param experiments A data frame, which can be initialized with \code{init_experiments()},
#' whose rownames are the predefined stages of a scientifc experiments, columnnames are
#' the names of each experiment, and cell values represent the state of each stage
#' in each experiment (states discussed below).
#' @param hide_stages (optional) A character vector with the names of the stages
#' in the scientific experiment, i.e. rownames of \code{experiments}, which the user wishes
#' to suppress from the figure output. The default value of \code{hide_stages} is NULL, indicating
#' that all stages will be displayed.
#' @param names_of_stages Logical indicating whether or not the names of the
#' stages should be displayed.
#' @export
#' @note For the parameter \code{experiments}, the four values any cell may take
#' are: \code{observed}, \code{different}, \code{unobserved}, \code{incorrect}.
#' @examples
#'
#' # Initialize the default experiments data frame
#' exps <- init_experiments()
#' sci_figure(exps)
#' sci_figure(exps, hide_stages = c("population", "analyst"))
#'
#' # Do some manual manipulation to the experiments
#' 
#' exps["analyst", "Exp2"] <- "different"
#' exps["code", c("Exp2", "Exp3")] <- "unobserved"
#' sci_figure(exps)
#'
#' @seealso \code{\link{init_experiments}}


sci_figure <- function(experiments, hide_stages = NULL, names_of_stages = TRUE){

	if(!all(unlist(lapply(experiments, function(x){x %in% c("observed", "different", "unobserved", "incorrect")})))){
		stop("Invalid cell value in experiments data frame.")
	}

	idx <- !(rownames(experiments) %in% hide_stages)
	stage_names <- c("Population", "Question", "Hypothesis", "Exp. Design", "Experimenter", "Data", "Analysis Plan", "Analyst", "Code", "Estimate", "Claim")
	stage_names <- stage_names[idx]

	experiments <- experiments[idx,]

	if(grDevices::dev.cur() != 1){grDevices::dev.off()}

	gptext <- grid::gpar(fontsize = 14 - min(nrow(experiments), 7))

	yht <- seq(0.95, 0.05, length = nrow(experiments))

	if(names_of_stages){
		vp1 <- grid::viewport(x = 0.1, y = 0.5, width = 0.2, height = 0.9)
		grid::pushViewport(vp1)
		grid::grid.text(stage_names, x=0.9, y = yht, gp = gptext)
		grid::upViewport()
	}

	vp2 <- grid::viewport(x = 0.5, y = 0.5, width = 0.6, height = 0.9)
	grid::pushViewport(vp2)

	for(j in 1:ncol(experiments)){
		for(i in 1:nrow(experiments)){
			img <- png::readPNG(system.file("icons", paste0(paste(rownames(experiments)[i], experiments[i,j], sep = "_"), ".png"), package = "repfigure"))
			grid::grid.raster(img, x = j/(ncol(experiments)+1), y = yht[i], width = 0.05 - 0.001*ifelse(ncol(experiments) > 14, ncol(experiments), 0))
		}
	}

	grid::upViewport()
	vp3 <- grid::viewport(x = 0.5, y = 0.95, width = 0.6, height = 0.1)
	grid::pushViewport(vp3)
	grid::grid.text(colnames(experiments), x = (1:ncol(experiments))/(ncol(experiments) + 1), y = 0.7, gp = gptext, rot = ifelse(ncol(experiments) > 12, 90, 0))

	grid::upViewport()
	vp4 <- grid::viewport(x = 0.9, y = 0.5, width = 0.2, height = 0.6)
	grid::pushViewport(vp4)

	cols <- c("#D20000", "#007888","#CDCDCD", "black")
	grid::grid.rect(width = 0.25, height = 0.1, x = 0.3, y = c(0.2,0.4,0.6,0.8), gp = grid::gpar(fill = cols))
	grid::grid.text(c("Incorrect", "Different", "Unobserved", "Original"), x = 0.3, y = c(0.1, 0.3, 0.5, 0.7), gp = grid::gpar(fontsize = 14))	
}