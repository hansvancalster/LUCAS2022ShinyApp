library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

samp <- NULL
load("lucas_augmented.RData")
colnames(samp)[names(samp)=="PI"] <- "OBS_TYPE"
samp <- samp[order(paste0(samp$NUTS0,samp$NUTS2)),]
samp_sf <- st_as_sf(
  samp,
  coords = c("X_LAEA", "Y_LAEA"),
  crs = " +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
samp_sf$LC <- as.factor(samp_sf$LC)
levels(samp_sf$LC) <- LETTERS[1:8]


load("modules_samples.RData")

glimpse(samp_sf)


samp_fl <- samp_sf |> 
  filter(NUTS2 %in% paste0("BE", 21:25))

# FI = field, PI = photo interpretation
samp_fl |>
  st_drop_geometry() |>
  count(OBS_TYPE)

# PI = dual: if selected in second phase, the observation type will also be PI if
# reachability is low; not selected in second phase is always PI

# 2 by 2 km MASTER SAMPLE
mapview::mapview(samp_fl, zcol = "OBS_TYPE")

# landscape features (column LF)
# eligibity criteria used to create LF:
# (Land Use = U11 OR  STR18 = 1,2,3 OR CLC18_R = 2) AND Observation Type = Field
# source: 

samp_fl_lf <- samp_fl |>
  filter(!is.na(LF)) |> 
  inner_join(
    LF |>
      select(
        WGT_LUCAS_LF = WGT_LUCAS,
        ID, flag, CLC18_R, STR18))

# looks like the weights in samp and LF are the same
all.equal(samp_fl_lf$WGT_LUCAS, samp_fl_lf$WGT_LUCAS_LF)


mapview::mapview(samp_fl_lf, zcol = "OBS_TYPE")



# microdata downloaded from https://ec.europa.eu/eurostat/web/lucas/database/2022
md2022 <- read_csv("data/BE_LUCAS_2022.csv")
problems(md2022)

glimpse(md2022)

kle_score <- md2022 |>
  janitor::clean_names() |>
  select(
    point_id, contains("feature")
  ) |>
  select(
    -survey_feature_width
  ) |>
  pivot_longer(
    cols = contains("feature")
  ) |>
  filter(!is.na(value)) |> 
  separate_wider_delim(
    cols = name,
    delim = "_",
    names = c("obs_type", "dropme", "lf_type", "subpointid")
  ) |>
  select(-dropme) |>
  filter(value != "No LF",
         obs_type == "field") |>
  group_by(point_id, lf_type) |>
  summarise(
    n_kle = n(),
    kle_score = n() / 41
  )

md2022_fl <- samp_fl_lf |>
  janitor::clean_names() |>
  inner_join(
   kle_score,
    by = join_by(id == point_id)
  )


md2022_fl |>
  ggplot(
    aes(x = nuts2, y = n_kle)
  ) +
  geom_violin(alpha = 0.5) +
  ggforce::geom_sina(alpha = 0.2) +
  stat_summary(
    fun.data = mean_cl_boot,
    colour = "red"
  )


md2022_fl |>
  ggplot() +
  geom_sf(aes(colour = n_kle)) +
  scale_color_gradient2(midpoint = 5)

