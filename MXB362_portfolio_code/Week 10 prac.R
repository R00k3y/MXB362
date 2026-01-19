install.packages("pracma")
library(pracma)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggquiver")
library(ggquiver)

## TASK 1 

# step 1: create a grid of values in 2D coordinate space
sp = 0.1 # grid spacing 
x_points = seq(-2, 2, by = sp)
y_points = seq(-1.5, 1.5, by =sp)

X = matrix(x_points, nrow = length(y_points), ncol = length(x_points), byrow = TRUE)
Y = matrix(y_points, nrow = length(y_points), ncol = length(x_points), byrow = FALSE)
xy_pairs = unique(expand.grid(x = as.vector(X), y = as.vector(Y))) # get unique pairs of gridpoints 

# step 2: at each point in (x,y) space, generate a third dimension
z = function(x){ x[1] * exp(-x[1]^2 - x[2]^2) }
Z = matrix(apply(data.frame(xy_pairs),1,FUN=z),nrow = length(y_points), ncol = length(x_points),byrow = TRUE)

# step 3: create a vector field from the gradients of this landscape in the x and y directions 
Vx = gradient(Z,h1 = sp, h2 = sp)$X
Vy = gradient(Z,h1 = sp, h2 = sp)$Y

# step 4: visualize the vector field using ggplot and geom_quiver 

# convert matrices into a data frame for ggplot
df <- data.frame(x = as.vector(X), y = as.vector(Y), Vx = as.vector(Vx), Vy = as.vector(Vy))

# create the vector field plot using ggplot and geom_quiver
ggplot(df, aes(x = x, y = y)) +
  geom_quiver(aes(u = Vx, v = Vy)) +
  theme_minimal() +
  labs(title = "Vector Field", x = "X", y = "Y")

## TASK 2

# step 1: load RDS files. They contain the x and Y velocities for a 2D vector field of size 400x400

x_vec = readRDS("C:/Users/colin/OneDrive/Documents/QUT/SEM 4/MXB362/Vector_visualisation_files/Vx.RDS")
y_vec = readRDS("C:/Users/colin/OneDrive/Documents/QUT/SEM 4/MXB362/Vector_visualisation_files/Vy.RDS")

# step 2: visualize the field using streamlines with regularly spaced start positions
# define grid 
# Define the grid spacing and points for the 400x400 grid

#sp2 = (4 / 399)  # spacing is set to ensure 400 points between -2 and 2, and -1.5 and 1.5
x_points2 = seq(-2, 2, length.out = 400)   # 400 x-coordinates
y_points2 = seq(-1.5, 1.5, length.out = 400)  # 400 y-coordinates

# Create the grid 
X = matrix(x_points2, nrow = length(y_points2), ncol = length(x_points2), byrow = TRUE)
Y = matrix(y_points2, nrow = length(y_points2), ncol = length(x_points2), byrow = FALSE)


# Step 1: Scale velocity by a small factor h for smoothness
h <- 0.1  # Adjust this factor to control the smoothness
Vx_scaled <- h * x_vec  # Scale the x-component of the velocity field
Vy_scaled <- h * y_vec  # Scale the y-component of the velocity field

# Step 2: Define seed points (adjust spacing if necessary)
seed_points <- expand.grid(
  x = seq(-2, 2, by = 0.2),   # Adjust spacing for seed points 
  y = seq(-1.5, 1.5, by = 0.35)
)

# Step 3: Update streamline generation function with velocity scaling
generate_streamline <- function(x_start, y_start, Vx, Vy, steps = 150, step_size = 0.05, direction = 1) {
  # Initialize the starting points of the streamline
  x <- numeric(steps)
  y <- numeric(steps)
  x[1] <- x_start
  y[1] <- y_start
  
  for (i in 2:steps) {
    # Find the closest grid point
    x_index <- which.min(abs(x[i-1] - x_points2))
    y_index <- which.min(abs(y[i-1] - y_points2))
    
    # Get the velocity at the current point (scaled)
    vx <- Vx[y_index, x_index]  # y first
    vy <- Vy[y_index, x_index]
    
    # Move to the next point
    x[i] <- x[i-1] + direction * step_size * vx
    y[i] <- y[i-1] + direction * step_size * vy
    
    # Stop if out of bounds
    if (x[i] < -2 | x[i] > 2 | y[i] < -1.5 | y[i] > 1.5) break
  }
  
  return(data.frame(x = x, y = y))
}

# Step 4: Generate bidirectional streamlines with updated settings
streamlines <- do.call(rbind, lapply(1:nrow(seed_points), function(i) {
  streamline <- generate_streamline(seed_points$x[i], seed_points$y[i], Vx_scaled, Vy_scaled, steps = 800, step_size = 0.07)
  streamline$seed_id <- i  # Assign a seed point ID for color coding
  return(streamline)
}))

# Step 5: Plot streamlines
ggplot() +
  geom_path(data = streamlines, aes(x = x, y = y, group = seed_id), color = "blue") +
  theme_minimal() +
  labs(title = "Smoothed Streamlines of the Vector Field", x = "X", y = "Y") +
  coord_fixed()


## TASK 3 

# create a grid
x_points <- seq(1, 400, by = 1)
y_points <- seq(1, 400, by = 1)
X <- matrix(x_points, ncol = length(y_points), nrow = length(x_points), byrow = FALSE)
Y <- matrix(y_points, ncol = length(y_points), nrow = length(x_points), byrow = TRUE)

# construct velocity field 
Vx <- X * (-Y + 200 - X)
Vy <- Y * (X + 200 - Y)

# random texture field (same dimensions as the grid)
RandomTexture <- matrix(runif(nrow(X) * ncol(X)), nrow = nrow(X), ncol = ncol(X))

# initialize the LIC matrix
LIC <- matrix(0, nrow = nrow(X), ncol = ncol(X))

# streamline accumulation function for LIC with increased steps and smaller step size
accumulate_LIC <- function(x_start, y_start, Vx, Vy, RandomTexture, LIC, steps = 500, step_size = 0.05, direction = 1) {
  x <- numeric(steps)
  y <- numeric(steps)
  x[1] <- x_start
  y[1] <- y_start
  
  for (i in 2:steps) {
    # Find the closest grid point
    x_index <- which.min(abs(x[i-1] - x_points))
    y_index <- which.min(abs(y[i-1] - y_points))
    
    # Clamp indices to prevent out-of-bounds errors
    x_index <- pmax(1, pmin(ncol(X), x_index))
    y_index <- pmax(1, pmin(nrow(Y), y_index))
    
    # Get velocity at the current point
    vx <- Vx[y_index, x_index]
    vy <- Vy[y_index, x_index]
    
    # Update positions using velocity (scaled by step size and direction)
    x[i] <- x[i-1] + direction * step_size * vx
    y[i] <- y[i-1] + direction * step_size * vy
    
    # Stop if out of bounds
    if (x[i] < 1 | x[i] > max(x_points) | y[i] < 1 | y[i] > max(y_points)) break
    
    # Accumulate random values into LIC
    xi <- which.min(abs(x[i] - x_points))  # Closest x grid point to current position
    yi <- which.min(abs(y[i] - y_points))  # Closest y grid point to current position
    
    # Accumulate the random value from RandomTexture into the LIC matrix
    LIC[yi, xi] <- LIC[yi, xi] + RandomTexture[y_index, x_index]
  }
  
  return(LIC)
}

# iterate over all grid points to compute LIC for both directions
for (i in 1:length(x_points)) {
  for (j in 1:length(y_points)) {
    # Accumulate forward and backward LIC
    LIC <- accumulate_LIC(x_points[i], y_points[j], Vx, Vy, RandomTexture, LIC, direction = 1)  # Forward direction
    LIC <- accumulate_LIC(x_points[i], y_points[j], Vx, Vy, RandomTexture, LIC, direction = -1)  # Backward direction
  }
}

# visualize the resulting LIC field with better contrast
image(t(LIC), col = gray.colors(256), main = "Line Integral Convolution (LIC)", axes = FALSE)



# step 5: Visualise the LIC using a function such as heatmap or image in R, e.g.:
heatmap(as.matrix(LIC),Rowv=NA, Colv=NA,col=gray.colors(256))
image(x_points, y_points,t(LIC), col = gray.colors(256))

