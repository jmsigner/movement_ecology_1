#######################################################X
#----Analysis of Animal Movement Data in R Workshop----X
#--------------Module 04 -- Random Walks---------------X
#----------------Last updated 2026-02-15---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

# In this code walkthrough, we'll develop some intuition for
# various discrete time random walk models by simulating
# data under the model.

# Load packages ----
library(tidyverse)
library(amt)
library(circular)

# Pure random walk ----
# We'll start with a pure random walk. We can easily simulate
# a pure random walk with either a point or a step representation.

# ... point representation ----
# This is the easiest way to simulate a random walk!

# Let's say we're going to simulate our RW for 100 timesteps.
T <- 100

# First, we'll create a data.frame to hold our data.
rw1 <- data.frame(x = rep(NA, T), y = rep(NA, T))

# Now specify our starting location.
rw1[1, ] <- c(0, 0)

# We will sample locations from a normal distribution centered on
# the previous location, so our only parameter left to choose is
# the standard deviation.
sd_rw <- 30

# Note that we could also specify a different SD for the x-coordinate
# and the y-coordinate. We could also use a multivariate normal
# distribution that has covariance between x- and y-coordinates.

# For reproducibility, let's set the random seed.
set.seed(20260216)

# Now we're ready to simulate! We can use a for-loop to iterate over
# the timesteps. We'll start at t = 2.

for (t in 2:T){
  # Change in x-coordinates
  dx <- rnorm(n = 1, mean = 0, sd = sd_rw)
  # Change in y-coordinates
  dy <- rnorm(n = 1, mean = 0, sd = sd_rw)

  # New coordinates
  rw1$x[t] <- rw1$x[t-1] + dx
  rw1$y[t] <- rw1$y[t-1] + dy
}

# ... ... plot ----
# We can easily plot the simulated points
plot(rw1$x, rw1$y,
     # Set the aspect ratio to 1:1
     asp = 1,
     # Change the point shape to a solid circle
     pch = 16,
     # Change axis labels and title
     xlab = "X-coordinate",
     ylab = "Y-coordinate",
     main = "Pure Random Walk")
# And draw the steps
lines(rw1$x, rw1$y)

# ... ... ggplot ----

# We can also use ggplot2 to make a figure. It's easier if we convert to
# steps first.
rw1_steps <- rw1 %>%
  make_track(x, y) %>%
  steps()

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = rw1_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Add the start point
  geom_point(data = rw1[1,], aes(x = x, y = y),
             size = 5, color = "green") +
  # Add the end point
  geom_point(data = rw1[T,], aes(x = x, y = y),
             size = 5, color = "red", shape = 15) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Pure Random Walk") +
  theme_minimal()

# ... step representation ----

# We can also simulate a pure random walk from a step perspective.
# Step lengths can come from any reasonable distribution.
# Turn angles should be uniform.

# We'll start from 0, 0 with a 25-m step to the north.
# This time, we'll also keep track of the heading.
rw2 <- data.frame(x = rep(NA, T), y = rep(NA, T), heading = rep(NA, T))
# The first point has no heading
rw2[1, ] <- c(0, 0, NA)
# The second point implies a step heading north (0 degrees)
rw2[2, ] <- c(0, 25, 0)

# We'll sample step lengths from a half-normal distribution
# by drawing a random number from a normal distribution with
# mean 0 and taking the absolute value. So we'll need an SD.
#    Just re-use the previous one.
sd_rw

# For reproducibility, let's set the random seed.
set.seed(20260216 + 1)

# Now we're ready to simulate! We can use a for-loop to iterate over
# the timesteps. We'll start at t = 3 since we already have the first step.

for (t in 3:T){
  # Draw step length
  sl <- abs(rnorm(n = 1, mean = 0, sd = sd_rw))
  # Draw turn angle
  ta <- runif(n = 1, min = -pi, max = pi)

  # Calculate the new heading
  rw2$heading[t] <- (rw2$heading[t-1] + ta) %% (2*pi)

  # Change in x-coordinates
  dx <- sl * sin(rw2$heading[t])
  # Change in y-coordinates
  dy <- sl * cos(rw2$heading[t])

  # New coordinates
  rw2$x[t] <- rw2$x[t-1] + dx
  rw2$y[t] <- rw2$y[t-1] + dy
}

# ... ... ggplot ----

# Convert to steps
rw2_steps <- rw2 %>%
  make_track(x, y) %>%
  steps()

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = rw2_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Add the start point
  geom_point(data = rw2[1,], aes(x = x, y = y),
             size = 5, color = "green") +
  # Add the end point
  geom_point(data = rw2[T,], aes(x = x, y = y),
             size = 5, color = "red", shape = 15) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Pure Random Walk") +
  theme_minimal()

# Check for autocorrelation in heading
acf(rw2$heading[2:100])
# No correlation after a lag of 1!

# Correlated random walk ----
# For a correlated random walk, it's much easier to use the step
# perspective.

# The only difference from our RW is that the CRW will have headings
# that are correlated with the previous step.

# We can accomplish this with turn angles that are concentrated around 0,
# rather than uniform around the circle.

# We'll setup our data the same way we setup the RW
crw <- data.frame(x = rep(NA, T), y = rep(NA, T), heading = rep(NA, T))
# The first point has no heading
crw[1, ] <- c(0, 0, NA)
# The second point implies a step heading north (0 degrees)
crw[2, ] <- c(0, 25, 0)

# We'll sample step lengths from the same half normal distribution
sd_rw

# We'll sample turn angles from a von Mises distribution, which is
# analogous to a normal distribution wrapped around the circle.
# The von Mises has two parameters: mean and concentration
vm_mean <- 0
vm_conc <- 3

# For reproducibility, let's set the random seed.
set.seed(20260216 + 1)

# Now simulate!

for (t in 3:T){
  # Draw step length
  sl <- abs(rnorm(n = 1, mean = 0, sd = sd_rw))
  # Draw turn angle
  suppressWarnings({ # because this function always prints out messages
    ta <- rvonmises(n = 1, mu = vm_mean, kappa = vm_conc)
  })

  # Calculate the new heading
  crw$heading[t] <- (crw$heading[t-1] + ta) %% (2*pi)

  # Change in x-coordinates
  dx <- sl * sin(crw$heading[t])
  # Change in y-coordinates
  dy <- sl * cos(crw$heading[t])

  # New coordinates
  crw$x[t] <- crw$x[t-1] + dx
  crw$y[t] <- crw$y[t-1] + dy
}

# ... plot ----

# Convert to steps
crw_steps <- crw %>%
  make_track(x, y) %>%
  steps()

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = crw_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Add the start point
  geom_point(data = crw[1,], aes(x = x, y = y),
             size = 5, color = "green") +
  # Add the end point
  geom_point(data = crw[T,], aes(x = x, y = y),
             size = 5, color = "red", shape = 15) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Correlated Random Walk") +
  theme_minimal()

# Check for autocorrelation in heading
acf(crw$heading[2:100])
# Autocorrelation remains significant for about 3 timesteps

# BRW ----
# The most common use of bias in a BRW for animal
# movement is probably to induce habitat selection.
# However, that is beyond the scope of what we're doing right now.

# If you're interested in a general formulation of BRW that simply
# includes attraction to a centroid, I wrote a general BCRW function
# that you can have a look at.
source("fun/bcrw.R")

# Print the code
# (go to the folder 'fun/' and look at the script; it's easier to read!)
bcrw

# In that function, the parameter `beta` gives the strength of bias
# toward a centroid (by taking a weighted average of a random
# heading and the heading to the attractor).

# Choose a starting location
start <- data.frame(x = 0, y = 0)

# Choose a centroid location (the attractor)
cent <- data.frame(x = 200, y = 200)

# For reproducibility, let's set the random seed.
set.seed(20260216 + 2)

# Simulate
bcrw_sim <- bcrw(start_loc = c(start$x, start$y),
                 centroid = c(cent$x, cent$y),
                 n_steps = 100,
                 sl_distr = c(shape = 4, scale = 20),
                 # Low correlation, so just a BRW
                 rho = 0.2,
                 # Strength of bias
                 beta = 0.5)

# ... plot ----

# Convert to steps
bcrw_sim_steps <- bcrw_sim %>%
  make_track(x, y) %>%
  steps()

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = bcrw_sim_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Add the start point
  geom_point(data = bcrw_sim[1,], aes(x = x, y = y),
             size = 5, color = "green") +
  # Add the end point
  geom_point(data = bcrw_sim[T,], aes(x = x, y = y),
             size = 5, color = "red", shape = 15) +
  # Add the attractor point
  geom_point(data = cent, aes(x = x, y = y),
             size = 5, color = "gold", shape = 8) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Biased Random Walk") +
  theme_minimal()

# Check for autocorrelation in heading
acf(bcrw_sim_steps$direction_p)
# No significant autocorrelation!
