library(purrr)

mod.by_mtime.nested <- tsa_last_year.df %>%
  group_by(mtime) %>%
  nest()

# mod.by_mtime.nested[[1, "data"]]
# 
# spatialized.df <- spatialize(
#   records.df  = mod.by_mtime.nested[[1, "data"]],
#   task.id.chr = "t",
#   learner.id.chr = "l",
#   learner.cl.chr = "regr.lm",
#   target.chr = "tsa",
#   prediction_grid.df = vn_1_data.grid.df
# )
# 
# test_2h.l <- purrr::map(.x = mod.by_mtime.nested$data[1:2],
#                       .f = spatialize,
#                         task.id.chr = "t",
#                         learner.id.chr = "l",
#                         learner.cl.chr = "regr.lm",
#                         target.chr = "tsa",
#                         prediction_grid.df = vn_1_data.grid.df
#                       ) 
# 
# mod.by_mtime.nested <- tsa_last_year.df %>%
#   group_by(mtime) %>%
#   nest()


test_all.df <- mod.by_mtime.nested %>%
  mutate(spatialized.df = purrr::map(.x = data, .f = spatialize,
                                  task.id.chr = "t",
                                  learner.id.chr = "l",
                                  learner.cl.chr = "regr.lm",
                                  target.chr = "tsa",
                                  prediction_grid.df = vn_1_data.grid.df ))


test_all.sf.df <- test_all.df %>%
  mutate(spatialized.sf  = purrr::map(.x = spatialized, .f = st_as_sf,
                                     coords = c("longitude", "latitude"),
                                     crs = 4326))


colnames(test_all.l)




test <- mod.by_mtime.nested %>% 
  mutate(test = map(data, spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = vn_1_data.grid.df
  )))


  by_row(spatialize(
    records.df  = .,
    task.id.chr = "t",
    learner.id.chr = "l",
    learner.cl.chr = "regr.lm",
    target.chr = "tsa",
    prediction_grid.df = vn_1_data.grid.df
  ))


library(gapminder)

(gap_nested <- gapminder %>% 
    group_by(continent, country) %>% 
    nest())

gap_nested[[1, "data"]]
(fit <- lm(lifeExp ~ I(year - 1950), data = gap_nested[[1, "data"]]))
le_vs_yr <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}
le_vs_yr(gap_nested[[1, "data"]])

fits <- map(gap_nested$data[1:2], le_vs_yr)
fits