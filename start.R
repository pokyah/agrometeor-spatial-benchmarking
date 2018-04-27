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
source_files_recursively.fun("../agrometeor-public/R/")

# Loading the data
load(paste0(wd.chr,"/.RData"))

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
    dfrom.chr = as.character(Sys.Date()-60),
    dto.chr = as.character(Sys.Date()-55)
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

# renaming the columns to same names as prediction grid
#colnames(tsa.model.df) <- c("X", "Y", "ele", "tsa")

# converting to sf
tsa.model.sf <- sf::st_as_sf(x = tsa.model.df, 
                        coords = c("longitude", "latitude"),
                        crs = 4326)

#' ### Independent variable

# Get the topo rasters stack
topo.ras.stack <- build_topo_rasters.fun()

# Create the 10km² resolution virtual stations network
vn_10.grid.sfc <- build_wal_grid.fun(res.num=10,geom.chr = "centers")
vn_10.grid.sf <- st_sf(geom=vn_10.grid.sfc)

# Create the 1km² resolution virtual stations network
vn_1.grid.sfc <- build_wal_grid.fun(res.num=1,geom.chr = "centers")
vn_1.grid.sf <- st_sf(geom=vn_1.grid.sfc)

# "Core" the topo rasters stack at the locations of the 10 km² virtual stations
topo.num <- raster::extract(topo.ras.stack, vn_10.grid.sf, weights=FALSE, fun=max)
vn_10_data.grid.sf <- st_sf(altitude=topo.num[,1], slo=topo.num[,2], asp=topo.num[,3], geom=vn_10.grid.sfc)

# "Core" the topo rasters stack at the locations of the 1 km² virtual stations
topo.num <- raster::extract(topo.ras.stack, vn_1.grid.sf, weights=FALSE, fun=max)
vn_1_data.grid.sf <- st_sf(ele=topo.num[,1], slo=topo.num[,2], asp=topo.num[,3], geom=vn_1.grid.sfc)


# Reconsrtruct a normal 10 km² df to submit as prediction grid to mlr library
vn_10_data.grid.sf <- vn_10_data.grid.sf %>% filter(!is.na(ele)) %>% filter(!is.na(slo)) %>% filter(!is.na(asp))
coor.vn_10_data.grid.df <- data.frame(st_coordinates(x = vn_10_data.grid.sf))
topo.vn_10_data.grid.df <- (as.data.frame(vn_10_data.grid.sf))[-length(vn_10_data.grid.sf)]
vn_10_data.grid.df <- dplyr::bind_cols(coor.vn_10_data.grid.df, topo.vn_10_data.grid.df)

# Reconsrtruct a normal 1 km² df to submit as prediction grid to mlr library
vn_1_data.grid.sf <- vn_1_data.grid.sf %>% filter(!is.na(ele)) %>% filter(!is.na(slo)) %>% filter(!is.na(asp))
coor.vn_1_data.grid.df <- data.frame(st_coordinates(x = vn_1_data.grid.sf))
topo.vn_1_data.grid.df <- (as.data.frame(vn_1_data.grid.sf))[-length(vn_1_data.grid.sf)]
vn_1_data.grid.df <- dplyr::bind_cols(coor.vn_1_data.grid.df, topo.vn_1_data.grid.df)
colnames(vn_1_data.grid.df)[1:3] <- c("longitude", "latitude", "altitude")

#+ ---------------------------------
#' ## Spatialization
#' 
#+ spatialization, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# One model for each hour
by_mtime <- tsa_last_year.df %>%
  group_by(mtime) %>%
  do(spatialize(
    records.df = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = vn_1_data.grid.df
  ))

# 
se <- spatialize(
    records.df = tsa_last_year.df,
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


