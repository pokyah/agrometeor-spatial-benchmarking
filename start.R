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
#'title: "R script to retrieve Agromet API data"
#'date: \ 16-04-2018\
#'---

#+ ---------------------------------
#' ## Script preparation
#' 
#+ preparation, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Automagically set the wd and the root of all projects 
if (!require("here")) install.packages("here")
library(here)
wd.chr <- here::here()

# loading the library to manage all the other libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
requiredPackages <- read.csv("./settings/requiredPackages.csv", quote = "", sep = ",", header=TRUE, stringsAsFactors=FALSE)
p_load(char=requiredPackages$packageName, character.only=TRUE )
p_loaded()

# Dynamic Sourcing of all the required functions
source(paste0("../../pokyah/R-utilities/R-utilities.R"))
source(paste0("../agrometeor-global-utilities/source_files_recursively.fun.R"))
source_files_recursively.fun("./functions")
source_files_recursively.fun("../agrometeor-public")


#+ ---------------------------------
#' ## Data acquisition
#' 
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### Dependent variables
tsa_last_year.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "all",
    sensors.chr = "tsa",
    dfrom.chr = as.character(Sys.Date()-365),
    dto.chr = as.character(Sys.Date())
  )
)

# Filtering records
tsa_last_year.df <- tsa_last_year.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa))

# selecting features
tsa.model.df <- tsa_last_year.df %>% select(one_of(c("longitude", "latitude", "altitude", "tsa")))

#' ### Independent variable

# Get the topo rasters stack
topo.ras.stack <- build_topo_rasters.fun()

# Create the 10km² resolution virtual stations network
vn_10.grid.sf <- build_wal_grid.fun(res.num=10,geom.chr = "centers")

# "Core" the stack at the locations of the virtual stations
topo.extract.mat <- raster::extract(topo.ras.stack, st_coordinates(virtual_network_10.grid.sf))
vn_10.grid.sf$ele <- data.frame(topo.extract.mat[,1])
vn_10.grid.sf$slo <- data.frame(topo.extract.mat[,2])
vn_10.grid.sf$asp <- data.frame(topo.extract.mat[,3])

# check we have what we need - https://gis.stackexchange.com/questions/224915/extracting-data-frame-from-simple-features-object-in-r
head(data.frame(vn_10.grid.sf))

library(sp)
data(meuse)
coordinates(meuse) = ~x+y
m.sf = st_as_sf(meuse)
opar = par(mar=rep(0,4))
plot(m.sf)

st_coordinates(vn_10.grid.sf)


# load the mlr lib
library(mlr)

# creating the regression task
regr.task = mlr::makeRegrTask(
  id = "tsa.1year.pos.regr",
  data = tsa.model.df,
  target = "tsa"
)

# Get some information about the task
getTaskDesc(regr.task)

# create the response learner
resp.regr.lrn = mlr::makeLearner(
  cl = "regr.lm",
  id = "tsa.1year.pos.regr",
  predict.type = "response" #could also be "se"
)

# train the resp learner to create the regr model on our dataset
resp.regr.mod = train(resp.regr.lrn, regr.task)

# Get infos about the model
resp.regr.mod$learner
print(resp.regr.mod$learner.model)
print(summary(resp.regr.mod$learner.model))
print(resp.regr.mod$features)
print(resp.regr.mod$task.desc$size)

# Compute the model prediction for tsa_diff using ensPameseb and vvtPameseb for each hourly records
resp.task.pred = predict(
  object = resp.regr.mod,
  task = regr.task
)

# Inspect the difference between the true, predicted and SE values
print(signif(head(getPredictionTruth(resp.task.pred)),2))
print(head(getPredictionResponse(resp.task.pred)))


# Visualising the prediction of tsa_diff according to ens and vvt
resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, task = regr.task)
resp.pred.plot

# Visualising the prediction of tsa_diff according to ens only
ens.resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, features= "altitude", task = regr.task)
ens.resp.pred.plot

# Visualising the prediction of tsa_diff according to vvt only
vvt.resp.pred.plot <- plotLearnerPrediction(resp.regr.lrn, features= "longitude", task = regr.task)
vvt.resp.pred.plot

graph_reso <- 0.5
axis_x <- seq(min(records.reshaped.df$ens_61), max(records.reshaped.df$ens_61), by = graph_reso)
axis_y <- seq(min(records.reshaped.df$vvt_61), max(records.reshaped.df$vvt_61), by = graph_reso)
tsadiff_lm_surface <- expand.grid(ens_61 = axis_x, vvt_61 = axis_y,KEEP.OUT.ATTRS = F)


#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  


