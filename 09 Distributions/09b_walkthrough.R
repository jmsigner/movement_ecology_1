#######################################################X
#------------- Movement Ecology with R 1 --------------X
#------------- Module 09 -- Distributions -------------X
#----------------Last updated 2026-02-11---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

library(amt)
library(tidyverse)

dat <- read_csv(here::here("data/moll_et_al2021.csv"))


dat$t <- mdy_hm(dat$t, tz = "America/Chicago")
dat <- filter(dat, t > ymd("2017-11-04"), t < ymd("2017-11-24"))
range(dat$t)


trk <- make_track(dat, x, y, t, crs = 4326)


summarize_sampling_rate(trk)

# Resample to 1.5h
trk1 <- trk |> track_resample(rate = minutes(90), tolerance = minutes(10))

table(trk$burst_)

# Find UTM zone
lon <- -93
zone <- floor((lon + 180) / 6) + 1
zone

# Northern Hemisphere: 326 + zone
# here: 32515
trk1 <- trk1 |> transform_coords(32515)

steps <- trk1 |> steps_by_burst() |> # Steps
  time_of_day(where = "both") # Add time of day

# Look at distribution of step lengths
steps |>
  ggplot(aes(sl_, fill = tod_start_)) +
  geom_density(alpha = 0.5)

steps |>
  ggplot(aes(ta_, fill = tod_start_)) +
  geom_density(alpha = 0.5)

