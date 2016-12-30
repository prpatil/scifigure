#' Create a figure depicting reproducibility/replicability
#'
#' \code{repfigure} creates a graphical representation of changes in a
#' second study or reproduction attempt as compared to an original study.
#'
#' @param type Either "repl" for replication or "repr" for reproduction
#' @export
#' @note Work in progress...going to add user input
#' @examples
#' repfigure("repl")
#' repfigure("repr")
#' 


repfigure <- function(type="repl"){

	xmin <- ymin <- 0
	xmax <- ymax <- 10
	offset <- 0.95

	img_pref <- c("pop", "ques", "hyp", "ed", "exp", "dat", "ap", "ana", "code", "est", "cla")
	img_suff <- rep("b", 11)

	img_names_l <- paste(img_pref, img_suff, sep = "_")
	img_names_l <- paste(img_names_l, "png", sep = ".")

	if(type == "repr"){
		img_suff[8] <- "c"
	}

	if(type == "repl"){
		img_suff[c(5,6,8:11)] <- "c"
	}

	img_names_r <- paste(img_pref, img_suff, sep = "_")
	img_names_r <- paste(img_names_r, "png", sep = ".")

	dev.new(width=10, height = 14)
	par(mai=c(0.25,0.5,0.25,0.5))
	plot(-1, xlim = c(xmin,xmax), ylim = c(ymin,ymax), xaxt="n", yaxt="n", xlab="", ylab="",bty="n")

	for(i in 0:10){
		img <- readPNG(system.file("inst", img_names_l[i+1], package = "repfigure"))
		rasterImage(img, xmin + 1, ymax - 0.6 - offset*i, xmin + 2.25, ymax - offset*i)
	}

	for(i in 0:10){
		img <- readPNG(system.file("inst", img_names_r[i+1], package = "repfigure"))
		rasterImage(img, xmin + 4, ymax - 0.6 - offset*i, xmin + 5.25, ymax - offset*i)
	}

}