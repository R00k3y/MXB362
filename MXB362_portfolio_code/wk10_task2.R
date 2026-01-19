## TASK 2

# step 1: load RDS files. They contain the x and Y velocities for a 2D vector field of size 400x400

Vx = readRDS("C:/Users/colin/OneDrive/Documents/QUT/SEM 4/MXB362/Vector_visualisation_files/Vx.RDS")
Vy = readRDS("C:/Users/colin/OneDrive/Documents/QUT/SEM 4/MXB362/Vector_visualisation_files/Vy.RDS")

# define grid 
sp2 = (4 / 399)  # spacing is set to ensure 400 points between -2 and 2, and -1.5 and 1.5
x_points2 = seq(-2, 2, length.out = 400)   # 400 x-coordinates
y_points2 = seq(-1.5, 1.5, length.out = 400)  # 400 y-coordinates

# create the grid 
X = matrix(x_points2, nrow = length(y_points2), ncol = length(x_points2), byrow = TRUE)
Y = matrix(y_points2, nrow = length(y_points2), ncol = length(x_points2), byrow = FALSE)

# Define the spacing between seed points (adjust based on your needs)
seed_spacing <- 20

# Subsample seed points from the grid
seed_x_indices <- seq(1, ncol(X), by = seed_spacing)
seed_y_indices <- seq(1, nrow(Y), by = seed_spacing)

# Extract the seed points from X and Y matrices
seed_x <- X[1, seed_x_indices]  # Get x-coordinates from the X matrix
seed_y <- Y[seed_y_indices, 1]  # Get y-coordinates from the Y matrix

# Create a grid of seed points
seed_points <- expand.grid(seed_x, seed_y)

# Set parameters
h <- 0.1  # scaling factor for smooth steps, you can adjust this
n_steps <- 100  # number of steps along the streamline, adjust as needed

# Create a function to trace the streamline
trace_streamline <- function(x0, y0, Vx, Vy, X, Y, h, n_steps) {
  streamline <- data.frame(x = numeric(n_steps), y = numeric(n_steps))  # Initialize storage
  
  x <- x0  # Start with the seed x position
  y <- y0  # Start with the seed y position
  
  for (i in 1:n_steps) {
    # Find nearest grid indices for the current x, y
    ix <- which.min(abs(X[1, ] - x))
    iy <- which.min(abs(Y[, 1] - y))
    
    # Get velocity at the current position
    vx <- Vx[iy, ix]
    vy <- Vy[iy, ix]
    
    # Update the position using scaled velocity
    dx <- h * vx
    dy <- h * vy
    x <- x + dx
    y <- y + dy
    
    # Store the position in the streamline
    streamline$x[i] <- x
    streamline$y[i] <- y
  }
  
  return(streamline)
}

# Initialize a list to store all streamlines
streamlines <- list()

# Loop over all seed points to generate streamlines
for (i in 1:nrow(seed_points)) {
  x0 <- seed_points[i, 1]
  y0 <- seed_points[i, 2]
  
  # Trace the streamline from each seed point
  streamline <- trace_streamline(x0, y0, Vx, Vy, X, Y, h, n_steps)
  
  # Store the streamline
  streamlines[[i]] <- streamline
}

# Plotting the streamlines
library(ggplot2)

ggplot() +
  geom_path(data = do.call(rbind, streamlines), aes(x = x, y = y), color = 'blue') +
  geom_point(data = seed_points, aes(Var1, Var2), color = 'red') +
  theme_minimal() +
  labs(title = "Streamlines", x = "X", y = "Y")

