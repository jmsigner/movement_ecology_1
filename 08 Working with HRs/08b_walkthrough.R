#######################################################X
#------------- Movement Ecology with R 1 --------------X
#------------ Module 07 -- Working with HRs -----------X
#----------------Last updated 2026-02-11---------------X
#-------------------Code Walkthrough-------------------X
#######################################################X

library(amt)
library(ggplot2)
library(tidygraph)
library(ggraph)

leroy <- amt_fisher |> filter(name == "Leroy")
lupe <- amt_fisher |> filter(name == "Lupe")

# Create a template raster for the KDE

trast <- make_trast(amt_fisher |> filter(name %in% c("Leroy", "Lupe")), res = 50)

hr_leroy <- hr_kde(leroy, trast = trast, levels = c(0.5, 0.9))
hr_lupe <- hr_kde(lupe, trast = trast, levels = c(0.5, 0.9))

# `hr` and `phr` are directional, this means the order matters. For all other overlap measures the order does not matter.

hr_overlap(hr_leroy, hr_lupe, type = "hr")
hr_overlap(hr_lupe, hr_leroy, type = "hr")

# ADD plot

# By default `conditional = TRUE` and the full UD is used.

hr_overlap(hr_leroy, hr_lupe, type = "phr", conditional = FALSE)
hr_overlap(hr_lupe, hr_leroy, type = "phr", conditional = FALSE)

# If we set `conditional = TRUE`, the overlap is measured at home-range levels that were specified during estimation.

hr_overlap(hr_leroy, hr_lupe, type = "phr", conditional = TRUE)
hr_overlap(hr_lupe, hr_leroy, type = "phr", conditional = TRUE)


# Note, for the remaining overlap measures the order does not matter. Below
# we show this for the volumnic intersection (`type = "vi"`) as an example.

hr_overlap(hr_lupe, hr_leroy, type = "vi", conditional = TRUE)
hr_overlap(hr_leroy, hr_lupe, type = "vi", conditional = FALSE)

### $> 2$ instances

# Lets calculate daily ranges for Lupe and then and then see how different
# ranges overlap with each other.

# We have to use the same template raster in order to make ranges comparable.

trast <- make_trast(lupe, res = 50)

# Then we add a new column with day and calculate for each day a `KDE` home range.

dat <- lupe |>
  mutate(week = lubridate::floor_date(t_, "week")) |>
  nest(data = -week) |>
  mutate(kde = map(data, hr_kde, trast = trast, levels = c(0.5, 0.95, 0.99)))

# Now we can use the list column with the home-range estimates to calculate
# overlap between the different home-ranges. By default `which = "consecutive"`, this means for each list entry (= home-range estimate) the overlap to the next entry will be calculated.

hr_overlap(dat$kde, type = "vi")

# Sometimes it can be useful to provide meaningful labels. We can do this with
# the `labels` argument.

hr_overlap(dat$kde, type = "vi", labels = dat$week)

# Different options exist for the argument `which`. For example, `which = "one_to_all"` calculates the overlap between the first and all other home ranges.

hr_overlap(dat$kde, type = "vi", labels = dat$week, which = "all")
hr_overlap(dat$kde, type = "vi", labels = dat$week, which = "all", conditional = TRUE)

hr_overlap(dat$kde, type = "vi", labels = dat$week, which = "one_to_all")
hr_overlap(dat$kde, type = "vi", labels = dat$week, which = "one_to_all", conditional = TRUE)

# Several animals
trast <- make_trast(amt_fisher, res = 100)
dat1 <- amt_fisher |> nest(data = -name) |>
  mutate(kde = map(data, ~ hr_kde(., trast = trast, level = c(0.5, 0.9, 0.99))))


# Now we can calculate the overlaps between animals:
ov2 <- hr_overlap(dat1$kde, type = "hr", labels = dat1$name, which = "all",
                  conditional = TRUE)

graph <- as_tbl_graph(ov2) |>
  mutate(Popularity = centrality_degree(mode = 'in'))

ggraph(graph, layout = 'stress') +
  #geom_edge_fan(aes(col = overlap), show.legend = TRUE, arrow = arrow()) +
  geom_edge_arc(aes(col = overlap), arrow = arrow(length = unit(4, 'mm'), type = "closed"),
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) +
  geom_node_point(size = 4) +
  geom_node_label(aes(label = name), repel = TRUE, alpha = 0.7) +
  facet_edges(~ levels, ncol = 2) +
  theme_light() +
  scale_edge_color_gradient(low = "blue", high = "red")


# Most do not overlap, so we could be a bit more restrictive
ov2 <- hr_overlap(dat1$kde, type = "hr", labels = dat1$name, which = "all",
                  conditional = TRUE) |>
  filter(overlap > 0.1)
graph <- as_tbl_graph(ov2) |>
  mutate(Popularity = centrality_degree(mode = 'in'))

ggraph(graph, layout = 'stress') +
  #geom_edge_fan(aes(col = overlap), show.legend = TRUE, arrow = arrow()) +
  geom_edge_arc(aes(col = overlap), arrow = arrow(length = unit(4, 'mm'), type = "closed"),
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) +
  geom_node_point(size = 4) +
  geom_node_label(aes(label = name), repel = TRUE, alpha = 0.7) +
  facet_edges(~ levels, ncol = 2) +
  theme_light() +
  scale_edge_color_gradient(low = "blue", high = "red")


# Overlap between a home range and a simple feature

# The function `hr_overlap_feature` allows to calculate percentage overlap ($HR$ index) between a home. To illustrate this feature, we will use again the data from `lupe` and calculate the intersection with an arbitrary polygon.

poly <- amt::bbox(lupe, buffer = -500, sf = TRUE)
poly1 <- amt::bbox(lupe, sf = TRUE)
hr <- hr_mcp(lupe)
ggplot() + geom_sf(data = hr_isopleths(hr)) +
  geom_sf(data = poly, fill = NA, col = "red") +
  geom_sf(data = poly1, fill = NA, col = "blue")

hr_overlap_feature(hr, poly, direction = "hr_with_feature")
hr_overlap_feature(hr, poly1, direction = "hr_with_feature")

hr_overlap_feature(hr, poly, direction = "feature_with_hr")
hr_overlap_feature(hr, poly1, direction = "feature_with_hr")


# The same work with several home-range levels:
hr <- hr_mcp(lupe, levels = c(0.5, 0.9, 0.95))
hr_overlap_feature(hr, poly, direction = "hr_with_feature")

# Read and write shape files
# sf::st_write()
# np <- sf::st_read("")


