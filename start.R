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


# https://www.digitalocean.com/community/tutorials/how-to-install-java-with-apt-get-on-ubuntu-16-04

#+ ---------------------------------
#' ## Script preparation
#' 
#+ preparation, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

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
source_files_recursively.fun("./R")
source_files_recursively.fun("../agrometeor_utilities_public/R/")

# Loading the data
#load(paste0(wd.chr,"/data-output/apicall.RData"))

#+ ---------------------------------
#' ## Data acquisition
#' 
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

#' ### Dependent variables

# Retrieving from API
tsa.records.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    stations_ids.chr = "all",
    sensors.chr = "tsa",
    dfrom.chr = as.character(Sys.Date()-60),
    dto.chr = as.character(Sys.Date()-59),
    api_v.chr = "v2"
  )
)

# Filtering records to keep only the useful ones
tsa.records.df <- tsa.records.df %>%
  filter(network_name == "pameseb") %>%
  filter(type_name != "Sencrop") %>%
  filter(!is.na(to)) %>%
  filter(state == "Ok") %>%
  filter(!is.na(tsa))

# Selecting only the useful features
tsa.records.df <- tsa.records.df %>%
  dplyr::select(one_of(c("mtime", "longitude", "latitude", "altitude", "tsa")))

#' ### Independent variables

# get the interpolation grid
grid.sp <- build_wal_grid.sp.fun(1000)

# core the topo rasters stack at the positions of the grid
topo.df <- data.frame(
  raster::extract(
    projectRaster(
      build_topo_rasters.fun(),
      crs=crs(grid.sp)),
    grid.sp,
    weights=TRUE,
    fun=max
    ))

# group the topo info with the lon and lat info
explanatory.df <- bind_cols(data.frame(grid.sp), topo.df)
colnames(explanatory.df) <- c("longitude", "latitude", "altitude", "pente", "orientation")

#+ ---------------------------------
#' ## Sptial prediction for one hour
#' 
#+ one_h_spatial_pred, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# predicting one hour
spatialized.df <- tsa.records.df %>%
  filter(mtime == tsa.records.df$mtime[13] ) %>%
  spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = explanatory.df
  )

# making it a spatial object 
# https://www.rdocumentation.org/packages/sp/versions/1.2-7/topics/SpatialGridDataFrame-class
#https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates#29736844
gridded <- spatialized.df
coordinates(gridded) <- c("longitude", "latitude")
crs(gridded) <- crs(grid.sp)

gridded.3812.sp <- spTransform(gridded, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))
gridded.3812.sp  <- as(gridded.3812.sp , "SpatialPixelsDataFrame")
gridded.3812.sp <- as(gridded.3812.sp, "SpatialGridDataFrame")

#+ ---------------------------------
#' ## mapping the predicted grid using tmap
#' 
#+ mapping, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

be.sp <- getData('GADM', country = 'BE', level = 1, download = TRUE)
be.sp$NAME_1
wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]

# check the CRS to know which map units are used  
proj4string(wallonie.sp)

# set CRS to "lambert 2008" which EPSG is 3812
wallonie.3812.sp <- spTransform(wallonie.sp, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))


library(tmap)
tm_shape(gridded.3812.sp, projection="3812") +
  tm_raster("response",  
            palette = "OrRd",
            title="Temperature",
            auto.palette.mapping=FALSE,
            breaks = seq(min(gridded.3812.sp@data$response, na.rm = TRUE),
                         max(gridded.3812.sp@data$response, na.rm = TRUE),
                         length = 11)) +
  tm_layout(legend.position = c(0.01,0.2),
            legend.text.size = 0.7) +
  tm_compass(position = c(0.2,0.2), color.light = "grey20") +
  tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",
               color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.2,0.01),
               just = NA) +
  tm_shape(wallonie.3812.sp) +
  tm_borders("grey20", lwd = 1.5) +
  tm_credits("© CRA-W", position = c(.85, 0))


#+ ---------------------------------
#' ## Wrangling data to make it mlr compliant
#' 
#+ data-acquisition, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Building a nested data frame, where for each hourly observation we have a 30 stations dataset of 1h temperature record.
library(purrr)
tsa.nested.df <- tsa.records.df %>%
  group_by(mtime) %>%
  nest()

# Passing to benchmarking function
tsa.bmr.l <- benchmark.hourly_sets(tsa.nested.df)

# selecting features useful for modelization
tsa.model.df <- tsa.records.df %>% select(one_of(c("longitude", "latitude", "altitude", "tsa")))

# converting to sf
tsa.model.sf <- sf::st_as_sf(x = tsa.model.df, 
                             coords = c("longitude", "latitude"),
                             crs = 4326)

#+ ---------------------------------
#' ## Spatialization
#' 
#+ spatialization, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

make_sf <- function(row){
  st_as_sf(
    x = row, 
    coords = c("longitude", "latitude"),
    crs = 4326)
}

#https://stackoverflow.com/questions/35558766/purrr-map-a-t-test-onto-a-split-df
#http://stat545.com/block024_group-nest-split-map.html
#https://purrr.tidyverse.org/reference/map.html
#https://stackoverflow.com/questions/47415072/pass-multiple-functions-to-purrrmap
#https://stackoverflow.com/questions/42518156/use-purrrmap-to-apply-multiple-arguments-to-a-function#42518473
#https://stackoverflow.com/questions/49724457/how-to-pass-second-parameter-to-function-while-using-the-map-function-of-purrr-p
#https://gis.stackexchange.com/questions/222978/lon-lat-to-simple-features-sfg-and-sfc-in-r




#One model for each hour
mod.by_mtime <- tsa.records.df %>%
  group_by(mtime) %>%
  by_row(spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = vn_1_data.grid.df
  ))




mod.by_mtime.sf <-mod.by_mtime %>%
  do(make_sf(
    row =.
  ))

bmr.by_mtime <- tsa.records.df %>%
  group_by(mtime) %>%
  do(lrns.benchmark(
    records.df = .,
    task.id.chr = "t",
    target.chr = "tsa",
    prediction_grid.df = vn_1_data.grid.df
  ))

# 
se <- spatialize(
  records.df = tsa.records.df,
  task.id.chr = "t",
  learner.id.chr = "l",
  learner.cl.chr = "regr.lm",
  target.chr = "tsa",
  prediction_grid.df = vn_1_data.grid.df
)



# http://stat545.com/block023_dplyr-do.html
# https://gis.stackexchange.com/questions/224915/extracting-data-frame-from-simple-features-object-in-r
# http://mlr-org.github.io/mlr/articles/tutorial/devel/handling_of_spatial_data.html
# http://www.bisolutions.us/A-Brief-Introduction-to-Spatial-Interpolation.php



#+ ---------------------------------
#' ## Terms of service 
#' To use the [AGROMET API](https://app.pameseb.be/fr/pages/api_call_test/) you need to provide your own user token.  
#' The present script is available under the [GNU-GPL V3](https://www.gnu.org/licenses/gpl-3.0.en.html) license and comes with ABSOLUTELY NO WARRANTY.
#' 
#' Copyright : Thomas Goossens - t.goossens@cra.wallonie.be 2018.  
#' 
#' *(This document was generated using [R software](https://www.r-project.org/) with the [knitr library](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/))*.  
#+ TOS,echo=TRUE,warning=FALSE,message=FALSE,error=FALSE  


