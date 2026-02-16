#######################################################X
#----Analysis of Animal Movement Data in R Workshop----X
#--------------Module 04 -- Random Walks---------------X
#----------------Last updated 2026-02-15---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X


# Load packages ----
library(tidyverse)
library(amt)
library(circular)

# 1. Replicate the random walk ----
go_walk <- function(i) {
  rw2 <- data.frame(x = rep(NA, T), y = rep(NA, T), heading = rep(NA, T))
  # The first point has no heading
  rw2[1, ] <- c(0, 0, NA)
  # The second point implies a step heading north (0 degrees)
  rw2[2, ] <- c(0, 25, 0)

  # SD for step length
  sd_rw <- 30

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

  # Label the iteration
  rw2$i <- i

  return(rw2)
}


# For reproducibility, let's set the random seed.
set.seed(20260216 + 1)

RW_list <- lapply(1:15, go_walk)
RW <- do.call(rbind, RW_list)

# ... ... ggplot ----

# Convert to steps
RW_steps <- RW %>%
  make_track(x, y, i = i) %>%
  steps(keep_cols = "end")

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = RW_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = factor(i)),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Pure Random Walk") +
  theme_minimal()


# 2. Correlated random walk ----
corr_walk <- function(vm_conc) {
  crw <- data.frame(x = rep(NA, T), y = rep(NA, T), heading = rep(NA, T))
  # The first point has no heading
  crw[1, ] <- c(0, 0, NA)
  # The second point implies a step heading north (0 degrees)
  crw[2, ] <- c(0, 25, 0)

  # We'll sample step lengths from the same half normal distribution
  sd_rw <- 30

  # We'll sample turn angles from a von Mises distribution
  vm_mean <- 0

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

  crw$vm_conc <- vm_conc
  return(crw)
}


# For reproducibility, let's set the random seed.
set.seed(20260216 + 1)

CRW_list <- lapply(seq(0.1, 10, length.out = 5), corr_walk)
CRW <- do.call(rbind, CRW_list)

# ... plot ----

# Convert to steps
CRW_steps <- CRW %>%
  make_track(x, y, vm_conc = vm_conc) %>%
  steps(keep_cols = "end")

# Setup the ggplot
ggplot() +
  # Draw the line segments with arrows
  geom_segment(data = CRW_steps, aes(x = x1_, y = y1_, xend = x2_, yend = y2_, color = factor(vm_conc)),
               arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  # Set the aspect ratio to 1:1
  coord_equal() +
  # Update labels and title
  xlab("X-coordinate") +
  ylab("Y-coordinate") +
  ggtitle("Correlated Random Walk") +
  theme_minimal()

# What does this do to the ACF function?
lapply(CRW_list, function(x){
  acf(na.omit(x$heading), main = x$vm_conc[1])
})
