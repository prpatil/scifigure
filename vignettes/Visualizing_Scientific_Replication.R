## ---- fig.width = 9, fig.height = 7--------------------------------------
library(scifigure)
exps <- init_experiments(2)
sci_figure(exps)	

## ---- fig.width = 9, fig.height = 7--------------------------------------
exps <- init_experiments(2, c("Brady et. al.", "Irving et. al."))
exps["analysis_plan", 1] <- "unobserved"
exps[c("experimenter", "analyst", "estimate"), 2] <- "different"
sci_figure(exps, hide_stages = c("population", "hypothesis"))

## ---- fig.width = 9, fig.height = 7--------------------------------------
sci_figure(exps, hide_stages = c("population", "hypothesis"), diff = TRUE)

## ---- fig.width = 9, fig.height = 7--------------------------------------
reproduce_figure()

## ---- fig.width = 9, fig.height = 7--------------------------------------
replicate_figure()

## ---- fig.width = 14, fig.height = 7-------------------------------------
exps <- init_experiments(9, c("Original", "Reproducible", "Orignal", "Replicable", "Begley", "Payne et. al.", "Vianello (OSF)", "Potti", "Baggerly & Coombs"))
exps["analyst", 2] <- "different" # Reproducible
exps[c("experimenter", "data", "analyst", "code", "estimate", "claim"), 4] <- "different" # Replicable
exps[c("population", "hypothesis", "experimental_design", "experimenter", "data", "analysis_plan", "analyst", "code", "estimate"), 5] <- "unobserved" # Begley
exps[c("population", "experimenter", "data", "analyst", "code", "estimate", "claim"), 7] <- "different" # Vianello (OSF)
exps[c("data", "code"), 8] <- "incorrect" # Potti
exps[c("data", "code"), 9] <- "different" # Baggerly & Coombes

sci_figure(exps)

## ---- fig.width = 14, fig.height = 7-------------------------------------
sci_figure(exps, diff = TRUE)

## ---- fig.width = 14, fig.height = 7-------------------------------------
library(png)

questionnaire_observed <- readPNG(system.file("extdata", "questionnaire_observed.PNG", package = "scifigure"), native = T)
questionnaire_different <- readPNG(system.file("extdata", "questionnaire_different.PNG", package = "scifigure"), native = T)
questionnaire_incorrect <- readPNG(system.file("extdata", "questionnaire_incorrect.PNG", package = "scifigure"), native = T)
questionnaire_unobserved <- readPNG(system.file("extdata", "questionnaire_unobserved.PNG", package = "scifigure"), native = T)

measurement_observed <- readPNG(system.file("extdata", "measurement_observed.PNG", package = "scifigure"), native = T)
measurement_different <- readPNG(system.file("extdata", "measurement_different.PNG", package = "scifigure"), native = T)
measurement_incorrect <- readPNG(system.file("extdata", "measurement_incorrect.PNG", package = "scifigure"), native = T)
measurement_unobserved <- readPNG(system.file("extdata", "measurement_unobserved.PNG", package = "scifigure"), native = T)

analysis_observed <- readPNG(system.file("extdata", "analysis_observed.PNG", package = "scifigure"), native = T)
analysis_different <- readPNG(system.file("extdata", "analysis_different.PNG", package = "scifigure"), native = T)
analysis_incorrect <- readPNG(system.file("extdata", "analysis_incorrect.PNG", package = "scifigure"), native = T)
analysis_unobserved <- readPNG(system.file("extdata", "analysis_unobserved.PNG", package = "scifigure"), native = T)

result_observed <- readPNG(system.file("extdata", "result_observed.PNG", package = "scifigure"), native = T)
result_different <- readPNG(system.file("extdata", "result_different.PNG", package = "scifigure"), native = T)
result_incorrect <- readPNG(system.file("extdata", "result_incorrect.PNG", package = "scifigure"), native = T)
result_unobserved <- readPNG(system.file("extdata", "result_unobserved.PNG", package = "scifigure"), native = T)

icon_list <- list("questionnaire_observed"=questionnaire_observed, "questionnaire_different"=questionnaire_different,
		      "questionnaire_incorrect"=questionnaire_incorrect, "questionnaire_unobserved"=questionnaire_unobserved,
			"measurement_observed"=measurement_observed, "measurement_different"=measurement_different,
			"measurement_incorrect"=measurement_incorrect, "measurement_unobserved"=measurement_unobserved,
		      "analysis_observed"=analysis_observed, "analysis_different"=analysis_different,
			"analysis_incorrect"=analysis_incorrect, "analysis_unobserved"=analysis_unobserved,
		      "result_observed"=result_observed, "result_different"=result_different,
			"result_incorrect"=result_incorrect, "result_unobserved"=result_unobserved)

stage_names <- c("questionnaire", "measurement", "analysis", "result")
stage_names2 <- c("Questionnaire", "Measurement", "Analysis", "Result")

exps <- init_experiments(nexp = 3, stage_names = stage_names)
sci_figure(exps, custom_icons = icon_list, stage_names = stage_names2)

exps["analysis", "Exp1"] <- "different"
exps["questionnaire", "Exp2"] <- "incorrect"
exps["result", c("Exp2", "Exp3")] <- "unobserved"
sci_figure(exps, custom_icons = icon_list, stage_names = stage_names2)

