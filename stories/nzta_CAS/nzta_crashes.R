library(tidyverse)

crash <- read_csv("~/data/data/CAS_crash_statistics/Crash_Analysis_System__CAS__Data.csv") %>%
  rename(easting = X, northing = Y) %>% select(-OBJECTID)

crash %>% count(crashYear) %>% as.data.frame()
crash %>% filter(crashYear > 2012) %>%
  mutate(totalMotorVehicles = bus + carStationWagon + moped + motorcycle + otherVehicleType + schoolBus + suv + taxi + truck + unknownVehicleType + vanOrUtility) -> crash2012
save(crash2012, file=here::here("CAS.Rdata"))

# What can we do with these data??
load(here::here("stories/nzta_CAS/CAS.Rdata"))
# 1. Look at crashes through time by severity:

plot_me <- crash %>%
  filter(crashYear < 2020) %>%
  filter(region == "Manawatu-Wanganui Region") %>%
  filter(bicycle > 0 | pedestrian > 0) %>%
  count(crashYear, crashSeverity)

  group_by(crashYear) %>% summarise(Fatal = sum(fatalCount, na.rm=TRUE),
                                    Serious = sum(seriousInjuryCount, na.rm=TRUE),
                                    Minor = sum(minorInjuryCount, na.rm=TRUE))

ggplot(plot_me) +
  geom_col(aes(x=crashYear, y=n, fill=crashSeverity))

# by region
crash2012 %>% group_by(crashYear, region) %>% count(crashSeverity) %>% pivot_wider(names_from = crashSeverity, values_from = n)

# Hmm, adjust by population count?

# those involving a cyclist
crash2012 %>% count(roadworks)

# by weather or light?

# holidays

# flat vs hill

# speed limit
crash2012 %>% group_by(speedLimit) %>% count(crashSeverity) %>% pivot_wider(names_from = crashSeverity, values_from = n)

# 72 involving bikes/peds only
crash2012 %>% filter(bicycle > 0 | pedestrian > 0, totalMotorVehicles == 0, crashSeverity == "Fatal Crash") %>% as.data.frame()

crash2012 %>% filter(bicycle > 0 | pedestrian > 0, totalMotorVehicles > 0) %>% count(crashSeverity)

crash2012 %>% mutate(totalMotorVehicles = bus + carStationWagon + moped + motorcycle + otherVehicleType + schoolBus + suv + taxi + truck + unknownVehicleType + vanOrUtility) %>%
  count(totalMotorVehicles)

crash2012 %>% count(vanOrUtility)
