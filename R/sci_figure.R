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
#' @param diff (optional) A Boolean flag to indicate whether the rendering of the figure should
#' emphasize the differences between the experiments ("difference mode"). The difference mode uses
#' a set of four symbols that are semantically close to the scenarios that they are encoding.
#' The default value is \code{FALSE}.
#' @param showlegend Do you want the legend to be shown?
#' @param cols colors to use for the specific scenarios
#' @param leg_text text for legend keys corresponding to the specific colors.
#'
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
#' sci_figure(exps, showlegend = FALSE)
#'
#' # Create the same figure using the difference mode
#'
#' sci_figure(exps, diff=TRUE)
#' too_many = init_experiments(nexp = 30)
#'
#' testthat::expect_warning({
#' sci_figure(too_many)
#' }, "showing the first")
#'
#' exp2 = exps
#' exp2[,1] = "bad"
#' testthat::expect_error({
#' sci_figure(exp2)
#' }, "Invalid cell")

#'
#' @seealso \code{\link{init_experiments}}


sci_figure <- function(experiments, hide_stages = NULL,
                       names_of_stages = TRUE, diff=FALSE,
                       showlegend = TRUE,
                       cols = c("#D20000", "#007888","#CDCDCD", "black"),
                       leg_text = c("Incorrect", "Different", "Unobserved", "Original")) {

  if(!all(unlist(lapply(experiments, function(x){
    x %in% c("observed", "different", "unobserved", "incorrect")
  })))){
    stop("Invalid cell value in experiments data frame.")
  }

  if(ncol(experiments) > 20){
    experiments <- experiments[,1:20]
    warning("Only showing the first 20 experiments for ease of plotting.")
  }

  idx <- !(rownames(experiments) %in% hide_stages)
  stage_names <- c("Population", "Question", "Hypothesis", "Exp. Design", "Experimenter", "Data", "Analysis Plan", "Analyst", "Code", "Estimate", "Claim")
  stage_names <- stage_names[idx]

  experiments <- experiments[idx,,drop=FALSE]

  grid::grid.newpage()

  gptext <- grid::gpar(fontsize = 16 - min(nrow(experiments), 7))

  yht <- seq(0.95, 0.05, length = nrow(experiments))

  if (names_of_stages){
    vp1 <- grid::viewport(x = 0.1, y = 0.5, width = 0.2, height = 0.9)
    grid::pushViewport(vp1)
    grid::grid.text(stage_names, x=0.9, y = yht, gp = gptext)
    grid::upViewport()
  }

  if ( diff == FALSE ) {
    icons <- scifigure::icons
  }
  else {
    icons <- scifigure::icons_diff
  }


  vp2 <- grid::viewport(x = 0.5, y = 0.5, width = 0.6, height = 0.9)
  grid::pushViewport(vp2)

  for(j in 1:ncol(experiments)){
    for(i in 1:nrow(experiments)){
      grid::grid.raster(icons[[paste(rownames(experiments)[i], experiments[i,j], sep = "_")]], x = j/(ncol(experiments)+1), y = yht[i], height = 0.08 - 0.03*(ncol(experiments) > 4), width = grid::unit(max(0.05, min(.1, 1/((ncol(experiments)*3)))), "snpc"))
    }
  }

  grid::upViewport()
  vp3 <- grid::viewport(x = 0.5, y = 0.95, width = 0.6, height = 0.1)
  grid::pushViewport(vp3)
  grid::grid.text(colnames(experiments), x = (1:ncol(experiments))/(ncol(experiments) + 1), y = 0.7, gp = gptext, rot = ifelse(ncol(experiments) > 12, 90, 0))

  if (showlegend) {
    grid::upViewport()
    vp4 <- grid::viewport(x = 0.9, y = 0.5, width = 0.2, height = 0.6)
    grid::pushViewport(vp4)


    if ( diff == FALSE ) {
      grid::grid.rect(width = 0.25, height = 0.1, x = 0.3, y = c(0.2,0.4,0.6,0.8), gp = grid::gpar(fill = cols))
    }
    else {
      grid::grid.raster(icons[[3]], x=0.3, y=0.8, height=grid::unit(0.18, "snpc"), width=grid::unit(0.18, "snpc"))
      grid::grid.raster(icons[[4]], x=0.3, y=0.6, height=grid::unit(0.18, "snpc"), width=grid::unit(0.18, "snpc"))
      grid::grid.raster(icons[[1]], x=0.3, y=0.4, height=grid::unit(0.18, "snpc"), width=grid::unit(0.18, "snpc"))
      grid::grid.raster(icons[[2]], x=0.3, y=0.2, height=grid::unit(0.18, "snpc"), width=grid::unit(0.18, "snpc"))
    }


    grid::grid.text(leg_text, x = 0.3, y = c(0.1, 0.3, 0.5, 0.7), gp = grid::gpar(fontsize = 14))

    grid::grid.rect(width = 0.25, height = 0.1, x = 0.3, y = c(0.2,0.4,0.6,0.8), gp = grid::gpar(fill = cols))
    grid::grid.text(leg_text, x = 0.3, y = c(0.1, 0.3, 0.5, 0.7), gp = grid::gpar(fontsize = 14))
  }
}
