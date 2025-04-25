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

# PI = photo interpretation (step 1)
# FI = field (step 2),
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


p1 <- md2022_fl |>
  mutate(
    provincie = factor(
      nuts2,
      levels = paste0("BE", 21:25),
      labels = c("Antwerpen", "Limburg",
                 "Oost-Vlaanderen" ,"Vlaams-Brabant", "West-Vlaanderen")
    )
  ) |>
  ggplot(
    aes(x = provincie, y = kle_score)
  ) +
  geom_violin(alpha = 0.5) +
  ggforce::geom_sina(alpha = 0.2) +
  stat_summary(
    fun.data = mean_cl_boot,
    colour = "red"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = "Score kleine landschapselementen (KLE)",
    title = "LUCAS submodule landscape features (2022)",
    x = "Provincie"
  )

p2 <- md2022_fl |>
  mutate(
    provincie = factor(
      nuts2,
      levels = paste0("BE", 21:25),
      labels = c("Antwerpen", "Limburg",
                 "Oost-Vlaanderen" ,"Vlaams-Brabant", "West-Vlaanderen")
    ),
  ) |>
  group_by(provincie) |>
  summarise(
    n_min_10perc = sum(kle_score < 0.1),
    n = n()
  ) |>
  ggplot() +
  geom_col(
    aes(y = paste0(provincie, " (n = ", n, ")"),
        x = n_min_10perc / n),
    alpha = 0.7) +
  scale_x_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Percentage LUCAS meetpunten met KLE score lager dan 10%",
    y = "Provincie")

fs::dir_create("media")
ggsave(
  filename = "media/lucas_lf_klescore_per_provincie.png",
  plot = p1)  
ggsave(
  filename = "media/lucas_lf_n_meetpunten_scorekld10_per_provincie.png",
  plot = p2)  


md2022_fl |>
  ggplot() +
  geom_sf(aes(colour = n_kle)) +
  scale_color_gradient2(midpoint = 5)

md2022_fl


# extra vragen
# voor de 5 types landscape features na te gaan wat de % zijn (klokdiagram) per provincie en voor gans Vlaanderen
# de data (incl coordinaten) te bezorgen
# a simple typology:
#    7 LF types
#    Overlap possible e.g. a tree (primary LF) over a ditch (secondary LF)
# a two-step approach:
#    office-based work (photo-interpretation, phase 1)
#    field survey (phase 2) 
# values

values <- c(
  W = "woody vegetation",
  G = "permanent grass/herbaceous",
  `T` = "temporary herbaceous",
  D = "ditches and streams",
  P = "small ponds and small wetlands",
  S = "stone walls, cairns and terraces",
  C = "cultural features",
  `No LF` = "no landscape feature"
)
# S and C do not occur in the dataset


lf_md_2022 <- md2022 |>
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
  filter(obs_type == "field")

# enkel Vlaanderen en sf van maken
lf_md_2022_sf <- samp_fl_lf |>
  janitor::clean_names() |>
  inner_join(
    lf_md_2022,
    by = join_by(id == point_id)
  )

lf_md_2022_distinct <- lf_md_2022_sf |>
  st_drop_geometry() |>
  distinct(id, subpointid, lf_type, value) |>
  as_tibble() %>%
  mutate(
    value = factor(value, levels = names(values), labels = values)
  )

# what is lf_type??

#length(unique(lf_md_2022_sf$id)) * 42 * 2

lf_md_2022_distinct_lf_type_wide <- lf_md_2022_distinct %>%
  pivot_wider(names_from = lf_type, values_from = value) %>%
  filter(`1` != `2`)

# note, a LF R package is mentioned in technical report about LF

# technisch rapport: https://op.europa.eu/en/publication-detail/-/publication/4991a3cc-7566-11ef-a8ba-01aa75ed71a1/language-en


#Nadenken over volgende vragen
#- waar liggen LUCAS-meetpunten? Liggen ze binnen de perceelsregistratie of binnen HAG, binnen gele en geelgroene bestemmingen

lf_punten <- lf_md_2022_sf |>
  distinct(id, nuts2, geometry) |>
  st_transform(crs = 31370)

#perceelsregistratie (landbouwgebruikspercelen)
lbg2022 <- read_sf("data/landbouwgebruikspercelen_2022/Landbouwgebruikspercelen_2022_-_Definitief_(extractie_26-06-2023).shp") %>%
  select(GWSNAM_H)

#HAG
mercator <- "https://www.mercator.vlaanderen.be/raadpleegdienstenmercatorpubliek/wfs"

#st_layers(paste0("WFS:", mercator))

haglayer <- "\"lu:lu_hag\""
hag <- read_sf(paste0("WFS:", mercator),
               query = paste0("SELECT * FROM ", haglayer))
# st_cast wil niet werken, daarom omweg via qgis
hag <- hag %>%
  select(regio) %>%
  st_transform(crs = 31370) %>%
  qgisprocess::qgis_run_algorithm_p("native:buffer", DISTANCE = 0) %>%
  st_as_sf()


#gewestplan gele en geelgroene bestemmingen

gwplayer <- "\"lu:lu_gwp_gv\""
gwp <- read_sf(paste0("WFS:", mercator),
               query = paste0("SELECT * FROM ", gwplayer))
gwp <-  gwp %>%
  select(svnaam) %>%
  filter(
    svnaam %in%
      c(
        "agrarische gebieden",
        "landschappelijk waardevolle agrarische gebieden"
      )
  ) %>%
  st_transform(crs = 31370) %>%
  qgisprocess::qgis_run_algorithm_p("native:buffer", DISTANCE = 0) %>%
  st_as_sf()



#intersecties
not_empty <- function(x) purrr::map_lgl(x, \(x) !rlang::is_empty(x))
is_in_lbg2022 <- st_intersects(lf_punten, lbg2022, sparse = TRUE) |>
  not_empty()
is_in_hag <- st_intersects(lf_punten, hag, sparse = TRUE) |>
  not_empty()
is_in_gewestplan_landbouw <- st_intersects(lf_punten, gwp, sparse = TRUE) |>
  not_empty()

lf_punten <- lf_punten %>%
  mutate(
    is_in_lbg2022 = is_in_lbg2022,
    is_in_hag = is_in_hag,
    is_in_gewestplan_landbouw = is_in_gewestplan_landbouw
  )

lf_punten %>%
  st_drop_geometry() %>%
  count(is_in_lbg2022, is_in_hag, is_in_gewestplan_landbouw) %>%
#  write_excel_csv2(here::here("data", "overlap_lf_lbg_hag_gp.csv"))
  DT::datatable()


#- is het aantal meetpunten voldoende om een verandering van 1% op 1 jaar te meten (dit is een tijdsintensieve stap en hiervoor hebben we ook data nodig)

official_stats_lf2022 <- readxl::read_excel(
  "data/lucas_kle_area_shares_lf2022.xlsx",
  sheet = "lucas_kle_tidy")

#“share of agricultural land covered with landscape features”.
#a procedure for computing this proportion based on observations from LUCAS core and from the LUCAS Landscape Feature module.

nuts_samplesizes <- lf_punten %>%
  st_drop_geometry() %>%
  count(nuts2, name = "samplesize")


# deff calculation is probably wrong
# sd_binom should not be used as SRS case
# mean is share of agricultural area that is LF and is calculated as
# ratio of estimator for the area of LF divided by area of AL (agric land)
# moreover mean and sd are already based on SRS case, so deff should be 1
official_stats_lf2022 <- official_stats_lf2022 %>%
  inner_join(nuts_samplesizes, by = join_by(nuts2)) %>%
  mutate(
    mean = mean / 100,
    sd = sd / 100, # = standard error of the mean?
    var = (sd * sqrt(samplesize)) ^ 2, # variance 
    sd_binom = sqrt(mean * (1 - mean)),
    deff = var / sd_binom^2
  )

# minimum detectable effect 1% per jaar - ongeveer 6% na 6 jaar (1.01^6)
p1 <- mean(
  official_stats_lf2022$mean[official_stats_lf2022$kle_type == "alle"]
)
#The design effect of an estimator is defined as the ratio between the variance of the estimator under the actual sampling design and the variance that would be obtained for an 'equivalent' estimator under a hypothetical simple random sampling without replacement of the same size. 
deff <- mean(
  official_stats_lf2022$deff[official_stats_lf2022$kle_type == "alle"]
)
mde <- 0.06 # abs(P2 - P1)
power <- 0.80
tails <- "one-tailed"
rr <- 1
hhsize <- 1 # 
alpha <- 0.05
beta <- 0.2

test1 <- ReGenesees::n.comp2prop(
  P1 = p1,
  MDE = mde,
  K1 = 1/2,
  alpha = 0.05,
  beta = 0.2,
  sides = tails,
  pooled.variance = TRUE,
  DEFF = deff,
  RR = rr,
  F = 1,
  hhSize = hhsize,
  old.clus.size = NULL, new.clus.size = NULL,
  verbose = TRUE
)
test1
test2 <- ReGenesees::n.comp2prop(
  P1 = p1,
  P2 = p1 * 1.01^6,
  K1 = 1/2,
  alpha = 0.05,
  beta = 0.2,
  sides = tails,
  pooled.variance = TRUE,
  DEFF = deff,
  RR = rr,
  F = 1,
  hhSize = hhsize,
  old.clus.size = NULL, new.clus.size = NULL,
  verbose = TRUE
)



?ReGenesees::deff()

official_stats_lf2022_nreq <- official_stats_lf2022 %>%
  crossing(
    cyclus = c(1, 3, 6, 12, 24),
    perc_per_jaar = 0.01
  ) %>%
  rowwise() %>%
  mutate(
    n_comp2prop_abs1perc = list(
      ReGenesees::n.comp2prop(
        P1 = mean,
        MDE = 0.01 * cyclus, 
        K1 = 1/2,
        alpha = 0.05,
        beta = 0.2,
        sides = "two-tailed",
        pooled.variance = TRUE,
        DEFF = deff,
        RR = 1,
        F = 1,
        hhSize = 1,
        old.clus.size = NULL, new.clus.size = NULL,
        verbose = FALSE
      )
    ),
    n_comp2prop_rel1perc = list(
      ReGenesees::n.comp2prop(
        P1 = mean,
        P2 = mean * 1.01^cyclus, 
        K1 = 1/2,
        alpha = 0.05,
        beta = 0.2,
        sides = "two-tailed",
        pooled.variance = TRUE,
        DEFF = deff,
        RR = 1,
        F = 1,
        hhSize = 1,
        old.clus.size = NULL, new.clus.size = NULL,
        verbose = FALSE
      )
    )
  )

official_stats_lf2022_nreq <- official_stats_lf2022_nreq %>%
  pivot_longer(
    cols = starts_with("n_comp"),
    names_to = "change",
    names_prefix = "n_comp2prop_",
    values_to = "reqn"
  ) %>%
  unnest_wider(reqn)

official_stats_lf2022_nreq %>%
  ggplot() +
  geom_point(
    aes(
      x = factor(cyclus),
      y = n1,
      colour = kle_type
    ),
    position = position_dodge(width = 0.5)
  ) +
  facet_grid(
    rows = vars(nuts2),
    cols = vars(change)
  ) +
  geom_hline(aes(yintercept = samplesize)) +
  geom_text(
    aes(x = 3, y = samplesize, label = samplesize),
    alpha = 0.2, nudge_y = 1
  ) +
  scale_y_continuous(
    trans = "log",
    breaks = c(10, 100, 1000, 10000)
  ) +
  labs(
    y = "Vereist aantal LUCAS locaties",
    x = "Aantal jaren dat trend zich doorzet"
  )


#- kan de BWK-methodiek instaan voor de kwaliteitsinschatting van de KLE's