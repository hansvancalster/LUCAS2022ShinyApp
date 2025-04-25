library(ggplot2)
library(dplyr)
library(tidyr)
library(LF)
library(readr)
library(sf)

# get the data from the LF internal database
data(lfall)
data(lucasCore)
data(LFmaster)
data(lfdat_I) # With Weights used for I21
lfdat <- lfdat_I
rm(lfdat_I)

# restrict to Belgium
lfdat <- lfdat %>%
  filter(NUTS0_16 == "BE")
lucasCore <- lucasCore %>%
  filter(NUTS0_16 == "BE") %>%
  as_tibble()
LFmaster <- LFmaster %>%
  filter(NUTS0_16 == "BE") %>%
  as_tibble()

# what data do we have for Belgium?
dim(lfdat)
names(lfdat)# This is most of the microdata from https://ec.europa.eu/eurostat/web/lucas/database/2022
plot(lfdat$X_LAEA, lfdat$Y_LAEA)

# compare with
# microdata downloaded from https://ec.europa.eu/eurostat/web/lucas/database/2022
md2022 <- read_csv("data/BE_LUCAS_2022.csv")
problems(md2022)
glimpse(md2022)

dim(LFmaster)
names(LFmaster)
plot(LFmaster$X_LAEA, LFmaster$Y_LAEA)

# same set of locations
setdiff(LFmaster$POINT_ID, lfdat$POINT_ID)

dim(lucasCore) # 2x2km grid
names(lucasCore)
plot(lucasCore$POINT_LONG, lucasCore$POINT_LAT)
lucas_core_sf <- lucasCore %>%
  st_as_sf(
    coords = c("X_LAEA", "Y_LAEA"),
    crs = 3035
  )

lucas_core_sf %>%
  ggplot() +
  geom_sf(
    aes(colour = OBS_TYPE)
  )

# first phase: PI on all points of the 2x2km grid
# second phase: stratified spatially balanced sample from the grid 
# described in European Commission. Statistical Office of the European Union. (2022). New LUCAS 2022 sample and subsamples design: criticalities and solutions : 2022 edition. Publications Office, LU. https://data.europa.eu/doi/10.2785/957524.
# for Belgium planned size 7402
# As in 2018, the optimization of the sample design was carried out using the R package SamplingStrata

#Therefore, we have 12 target estimates (8 LC + 4 LU) for each of the 27 Member States, for 324 (=12*27) total precision constraints.
#Each constraint corresponds to the maximum expected value for the coefficient of variation of the
#target estimate, i.e., the ratio between its standard deviation and the mean. Each of them was set to
#0.025, which means that we expect a maximum value of the coefficient of variation of 2.5 % in each
#country and for each target variable.
# so at country level 2.5% CV is targetted for each LC and LU


#As mentioned above, for this new survey round, it was decided to define the stratification of the Master
#frame a priori and determine the best allocation in the strata on this basis, instead of letting the
#optimisation algorithm determine the best stratification starting from an initial ‘atomic’ stratification, as
#was the case in 2018.

# For sample selection SamplingBigData::lpm2_kdtree was used.
