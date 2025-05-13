install.packages('spdep')
library(spdep)
install.packages('gstat')
library(gstat) 
install.packages('sp')
library(sp)
n <- 10 
coords <- expand.grid(x = 1:n, y = 1:n)
dist_matrix <- as.matrix(dist(coords)) 
dist_matrix <- ifelse(dist_matrix == 0, 0, 1 / dist_matrix)
install.packages('gstat')
library(gstat)


# Load required libraries
library(sp)
library(gstat)

# Create coordinates
n <- 10
coords <- expand.grid(x = 1:n, y = 1:n)

# Convert to spatial points
coords_sp <- SpatialPoints(coords)

# Convert spatial points to data frame (needed for prediction)
coords_df <- as.data.frame(coords_sp)

# Create gstat object for simulation
g <- gstat(
  formula = z ~ 1,
  locations = ~ x + y,
  dummy = TRUE,
  beta = 10,
  model = vgm(psill = 2, model = "Exp", range = 1),
  nmax = 20
)

# Simulate spatial data
simulated_spatial_data <- predict(g, newdata = coords_df, nsim = 1)

# Assign simulation values to a new data frame
sim1_coords <- coords_df
sim1_coords$z <- simulated_spatial_data$sim1  # Extract the simulated values

# Plot the result
plot(
  sim1_coords$x, sim1_coords$y,
  col = terrain.colors(n)[findInterval(sim1_coords$z, 
                                       seq(min(sim1_coords$z), max(sim1_coords$z), length.out = n))],
  pch = 20, cex = 2, main = "Simulated Spatial Data"
)

install.packages("ggplot2")
library(ggplot2) 
ggplot(sim1_coords, aes(x = sim1_coords$x, y =
                                      sim1_coords$y, fill = z)) + geom_tile() +
  scale_fill_gradient() +
  labs(title = "Spatially Simulated Data", x = "X Coordinate", y = "Y Coordinate", fill = "Value")
+
  theme_minimal()
