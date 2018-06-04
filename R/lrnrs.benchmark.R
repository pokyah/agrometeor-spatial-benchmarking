#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'  md_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true    
#'title: "Collection of R Scripts of the Agromet project"
#'date: \ 20-04-2018\
#'---


lrnrs.benchmark <- function(records.df, target.chr, prediction_grid.df, ){
  
  require(mlr)
  
  
  meuse.df <- as.data.frame(meuse)
  meuse.df<- na.omit(meuse.df)
  meuse.df <- meuse.df %>% dplyr::select(one_of("zinc", "x", "y"))
  meuse.grid.df <- as.data.frame(meuse.grid)[c("x","y")]
  
  
  
  # learners <- list("regr.lm", "regr.elmNN", "regr.kknn", "regr.km")
  # map(learners, makeLearner)
  
  # making short the df is not a tible but a strict df
  # records.df <- data.frame(records.df)
  
  # defining the learners who will be compared
  lrns.l <- list(
    makeLearner(cl = "regr.lm", id="linear regression"),
    makeLearner(cl = "regr.elmNN", id="single layer neural net"),
    makeLearner(cl ="regr.kknn", id="nearest neighbours"),
    makeLearner(cl = "regr.km", id="kriging")
  )
  
  # defining the spatialization task (dataset + target var)
  spatialization.task = mlr::makeRegrTask(
    id = "spatialization",
    data = meuse.df,
    target = "zinc",
    coordinates = meuse.df[c("x", "y")]
  )
  
  # defining the validation strategy
  valid.l = mlr::makeResampleDesc(
    method = "LOO",
    predict = "test"
  )
  
  # conducting the benchmark experiment
  benchmark.l <- benchmark(learners = lrns.l, tasks = spatialization.task, resamplings = valid.l)
  
  # getting the predictions from the model
  stations.pred.l <- getBMRPredictions(benchmark.l)
  
  # predicting on the grid 
  meuse.grid.pred <- predict(
    train(lrns.l[[1]], spatialization.task),
    newdata = meuse.grid.df
  )
  
  meuse.grid.pred.data <- dplyr::bind_cols(meuse.grid.df, meuse.grid.pred$data )
  coordinates(meuse.grid.pred.data) <- ~x+y
  class(meuse.grid.pred.data)
  
  spplot(meuse.grid.pred.data)
  
  spplot(meuse$zinc)
  
  
  # Group in a spatial sf
  #pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(resp.task.pred), as.data.frame(se.task.pred))
  pred_data.grid.df <- dplyr::bind_cols(prediction_grid.df, as.data.frame(se.task.pred))
  pred_data.grid.sf <- tsa.model.sf <- st_as_sf(x = pred_data.grid.df, 
                                                coords = c("longitude", "latitude"),
                                                crs = 4326)
  
  plot <- plot(pred_data.grid.sf)
  
  # Inspect the difference between the true, predicted and SE values
  print(head(getPredictionResponse(resp.task.pred)))
  
  # Return the predicted data and the error
  return(plot)
  
}

#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  
