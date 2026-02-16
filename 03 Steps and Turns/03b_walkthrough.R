#######################################################X
#------------- Movement Ecology with R 1 --------------X
#------------ Module 03 -- Steps and Turns ------------X
#----------------Last updated 2026-02-11---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

# Load the data ----

library(tidyverse)
library(amt)
library(lubridate) # to deal with date and time

dat1 <- read_csv("data/fisher.csv")

# Make a track
tr <- make_track(dat1, x_, y_, t_, name = name, crs = 5070)

# Only keep one animal
leroy <- tr |> filter(name == "Leroy")

# Resample to 15 mins
leroy2 <- track_resample(leroy, rate = minutes(15), tolerance = seconds(60))

# Only keep bursts with at least three observations
leroy2 <- filter_min_n_burst(leroy2, min_n = 3)


# Steps ----
# Next we want to change representations from individual
# locations to steps. A step consists of a start and end coordinate, a step
# length and a turn angle. The time difference between the start and the end
# point is constant.

leroy2 |> steps()

# We get a warning, because the function `steps()` be default ignores bursts.
# This is problematic if there is a large time gap between to consecutive
# points. To overcome this, we can use `steps_by_burst()`.

s2 <- leroy2 |> steps_by_burst()
print(s2, n = Inf)

# The resulting tibble has 11 columns by default:
# - `burst_`: the burst number.
# - `x1_` and `y1_`: the start coordinates of the step.
# - `x2_` and `y2_`: the end coordinates of the step.
# - `sl_`: the step length
# - `direction_p`: the direction of the step (relative to?)
# - `ta_`: the turn angle
# - `t1_` and `t2_`: the start and end time of a step.
# - `dt_`: te duration of a step.

s2 |> print(n = Inf)


# Extracting covariates --------
library(terra)

# Some covariates are shipped with the amt package
dem <- get_amt_fisher_covars()$elevation

leroy2 |> extract_covariates(dem)

leroy2 |> steps_by_burst() |> extract_covariates(dem, where = "start")
leroy2 |> steps_by_burst() |> extract_covariates(dem, where = "end")
leroy2 |> steps_by_burst() |> extract_covariates(dem, where = "both")

# Get a second covariates
pop <- get_amt_fisher_covars()$pop

# Note the different resolutions
res(dem)
res(pop)

# For any GIS related analyses this would be a problem, but for extracting the covariate
# values this is not problem.
# We can chain the extract covariates function and call it twice.

leroy2 |> steps_by_burst() |>
  extract_covariates(dem) |>
  extract_covariates(pop, where = "both")


# Time of day ----

leroy2 |> time_of_day() |> count(tod_)
leroy2 |> steps_by_burst() |> time_of_day(where = "start")
leroy2 |> steps_by_burst() |> time_of_day(where = "end")
leroy2 |> steps_by_burst() |> time_of_day(where = "both")

# Adding dawn and dusk
leroy2 |> steps_by_burst() |> time_of_day(where = "end", include.crepuscule = TRUE)
leroy2 |> steps_by_burst() |> time_of_day(where = "end", include.crepuscule = TRUE) |>
  count(tod_end_)

