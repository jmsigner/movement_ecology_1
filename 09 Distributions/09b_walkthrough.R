#######################################################X
#------------- Movement Ecology with R 1 --------------X
#------------- Module 09 -- Distributions -------------X
#----------------Last updated 2026-02-11---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

# We aim to replicate the analysis of Moll et al. 2021 and in addition fit
# distributions.

library(amt)
library(tidyverse)


# Load the data
dat <- read_csv(here::here("data/moll_et_al2021.csv"))


# Parse date
dat$t <- mdy_hm(dat$t, tz = "America/Chicago") # This important!

# Filter to the time period between the 4th and 24th of November 2017,
# that is when they identified dispersal.

dat <- filter(dat, t >= ymd("2017-11-04"), t <= ymd("2017-11-24"))
range(dat$t)

# Create a track
trk <- make_track(dat, x, y, t, crs = 4326)

# ... look at sampling rate ...
summarize_sampling_rate(trk)

# ... and resample to 1.5h
trk1 <- trk |> track_resample(rate = minutes(90), tolerance = minutes(10))

table(trk1$burst_)

# We want to change the projection form geographic coordinates to UTM,
# see here: https://stackoverflow.com/questions/9186496/determining-utm-zone-to-convert-from-longitude-latitude
# Find UTM zone
lon <- -93
(floor((lon + 180)/6) %% 60) + 1

# Northern Hemisphere: 326 + zone
# here: 32515
trk1 <- trk1 |> transform_coords(32615)

# Create steps and annotate with time of day
steps <- trk1 |> steps_by_burst() |> # Steps
  time_of_day(where = "both") # Add time of day
head(steps)

table(steps$tod_end_)
table(steps$tod_start_)

steps

# Look at distribution of step lengths
steps |>
  ggplot(aes(sl_, fill = tod_start_)) +
  geom_density(alpha = 0.5)

# an turn angles
steps |>
  ggplot(aes(ta_, fill = tod_start_)) +
  geom_density(alpha = 0.5)


# Fitting statistical distributions ----

# Next we can fit a gamma distribution to the steps
# Day
day <- fit_distr(
  steps |> filter(tod_start_ == "day") |> pull(sl_),
  "gamma"
)

day

night <- fit_distr(
  steps |> filter(tod_start_ == "night") |> pull(sl_),
  "gamma"
)

night

day$params
night$params

res <- tibble(
  when = rep(c("day", "night"), each = 2),
  what = rep(c("scale", "shape"), 2),
  est = c(day$params$scale, day$params$shape,
          night$params$scale, night$params$shape),
  se = sqrt(
    c(day$vcov["scale", "scale"], day$vcov["shape", "shape"],
      night$vcov["scale", "scale"], night$vcov["shape", "shape"]))
)

res

td <- res |> nest(data = -when) |>
  mutate(gamma = map(data, ~ {
    tibble(
      x = seq(1, 5000, len = 250),
      y = dgamma(x, shape = .x$est[.x$what == "shape"], scale = .x$est[.x$what == "scale"])
    )
  })) |>
  select(tod_start_ = when, gamma) |>
  unnest(cols = gamma)

# Plotting
steps |>
  ggplot(aes(x = sl_)) +
  geom_density() +
  geom_line(aes(x = x, y = y), data = td, col = "red") +
  facet_wrap(~ tod_start_, scale = "free")

# ... Derived quantities ----
# Expected displacement
# day
res |> filter(when == "day") |> pull(est) |> prod()
res |> filter(when == "night") |> pull(est) |> prod()


displacement <- function(x) {
  # Propagate uncertainty with delta method
  est <- unlist(x$params)
  mu_hat <- prod(est)
  se_mu <- as.vector(sqrt(t(est) %*% x$vcov %*% est))

  tibble(est = mu_hat,
         lci = mu_hat - 1.96 * se_mu,
         uci = mu_hat + 1.96 * se_mu)
}

res2 <- bind_rows(
  displacement(day) |> mutate(when = "day"),
  displacement(night) |> mutate(when = "night")
)

res2 |>
  ggplot(aes(x = when, y = est, ymin = lci, ymax = uci)) +
  geom_pointrange()

