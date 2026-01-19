install.packages("R.matlab")
library(R.matlab)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("rgl")
library(rgl)

data <- readMat("C:/Users/colin/OneDrive/Documents/MATLAB/Wk6 prac/Volume_Visualisation_files/DiCubic.mat")

DI <- data$Di

# Initialize the I matrix with the same dimensions as x and y of Di
I <- matrix(0, nrow = 256, ncol = 256)

# Sum over the z-dimension for each x, y coordinate
for (x in 1:256) {
  for (y in 1:256) {
    I[x, y] <- sum(DI[x, y, ])
  }
}

# Display the matrix I using a grayscale color map
image(I, col = gray((0:255)/255), main = "Summed Slices of MRI Data")

# maximum intensity projection 
# find the maximum intensity for each x, y coordinate over the z-dimension
for (x in 1:256) {
  for (y in 1:256) {
    I[x, y] <- max(DI[x, y, ])
  }
}

# display new matrix from MIP
image(I, col = gray((0:255)/255), main = "Maximum Intensity Projection of MRI Data")

## read aneurism data 
aneurism_data <- readMat("C:/Users/colin/OneDrive/Documents/MATLAB/Wk6 prac/Volume_Visualisation_files/aneurism.mat")

aneurism <- aneurism_data$A

# convert from 1-dimensional data to 3D array 
array2 <- array(aneurism, dim = c(256,256,dim(aneurism)[1]/(256*256)))  # z variable is a calculation based on the 1-dimensional array

# initialize a matrix with same dimensions 
J <- matrix(0, nrow = 256, ncol = 256)

# matrix matrix for simple volume renderer
# sum over the z-dimension for each x, y coordinate
for (x in 1:256) {
  for (y in 1:256) {
    J[x, y] <- max(array2[x, y, ])
  }
}

# display image for aneurism
# Display the matrix J using a grayscale color map
image(J, col = gray((0:255)/255), main = "Summed Slices of aneurism data")

# display matrix J with a different colour map
image(J, col = heat.colors(255), main = "Summed Slices of aneurism data")

image(J, col = brewer.pal(255,"Accent"), main = "Summed Slices of aneurism data")

## compositing, ray casting volume 
# determine the dimensions and maximum value in the volume
maxDi <- max(DI)

# initialize the output image
nx <- dim(DI)[1]
ny <- dim(DI)[2]
image <- matrix(0, nrow = nx, ncol = ny)
opacity_accum <- matrix(0, nrow = nx, ncol = ny)

# front-to-back compositing
for (z in 1:dim(DI)[3]) {
  for (x in 1:nx) {
    for (y in 1:ny) {
      # Calculate the alpha (opacity) at this voxel
      alpha <- DI[x, y, z] / maxDi
      
      # Calculate the color at this voxel (grayscale intensity)
      C <- DI[x, y, z]
      
      # Composite the color and opacity into the final image
      image[x, y] <- image[x, y] + (1 - opacity_accum[x, y]) * alpha * C
      opacity_accum[x, y] <- opacity_accum[x, y] + (1 - opacity_accum[x, y]) * alpha
    }
  }
}

# Normalize the image to improve visibility
image <- image / max(image)

# Display the composited image
image(image, col = gray.colors(256), main = "Front-to-Back Composited MRI Volume")


## task 1

# define the size of the volume
nx <- dim(DI)[1]
ny <- dim(DI)[2]
nz <- dim(DI)[3]

# define the center of the cylinder in the xy-plane
center_x <- nx / 2
center_y <- ny / 2
radius <- 50

# Create a 3D mask for the half-cylinder
cylinder_mask <- array(0, dim = c(nx, ny, nz))

# Fill the mask with a half-cylinder (for positive y-values)
for (x in 1:nx) {
  for (y in 1:ny) {
    if (y > center_y && sqrt((x - center_x)^2 + (y - center_y)^2) <= radius) {
      cylinder_mask[x, y, ] <- 1  # Inside the half-cylinder
    }
  }
}

# apply the mask to the MRI volume to "cut" the half-cylinder
DI_masked <- DI * cylinder_mask

# Initialize the output image and opacity accumulation
image_top <- matrix(0, nrow = nx, ncol = ny)
opacity_accum <- matrix(0, nrow = nx, ncol = ny)

# front-to-back compositing
for (z in 1:dim(DI)[3]) {
  for (x in 1:nx) {
    for (y in 1:ny) {
      # Calculate the alpha (opacity) at this voxel
      alpha <- DI_masked[x, y, z] / maxDi
      
      # Calculate the color at this voxel (grayscale intensity)
      C <- DI_masked[x, y, z]
      
      # Composite the color and opacity into the final image
      image[x, y] <- image[x, y] + (1 - opacity_accum[x, y]) * alpha * C
      opacity_accum[x, y] <- opacity_accum[x, y] + (1 - opacity_accum[x, y]) * alpha
    }
  }
}

# Front-to-back compositing for the top view
for (z in 1:nz) {
  for (x in 1:nx) {
    for (y in 1:ny) {
      # Calculate the alpha (opacity) at this voxel
      alpha <- DI_masked[x, y, z] / maxDi
      
      # Calculate the color at this voxel (grayscale intensity)
      C <- DI_masked[x, y, z]
      
      ## potential change 
      for (x in 1:nx){
        for(y in y:ny){
          cin = 0
          ain = 0
        }
      }
      for (z in 1:nz){
        cout = cin + (1-ain)*(alpha*C)
        aout = ain + (1-ain)*alpha
      
        # Composite the color and opacity into the final image
        image_top[x, y] <- image_top[x, y] + (1 - opacity_accum[x, y]) * alpha * C
        opacity_accum[x, y] <- opacity_accum[x, y] + (1 - opacity_accum[x, y]) * alpha
      }
    }
  }
}

# Normalize the image to improve visibility
image_top <- image_top / max(image_top)

# Display the top view
image(image_top, col = gray.colors(256), main = "Top View of Half-Cylinder MRI Volume")


###
# Define the size of the volume
nx <- dim(DI)[1]
ny <- dim(DI)[2]
nz <- dim(DI)[3]

# Define the center of the cylinder in the xy-plane
center_x <- nx / 2
center_y <- ny / 2
radius <- 50

# Create a 3D mask for the half-cylinder
cylinder_mask <- array(1, dim = c(nx, ny, nz))  # Initialize mask with 1 (no cut)

# Fill the mask with a half-cylinder (for positive y-values)
for (x in 1:nx) {
  for (y in 1:ny) {
    for (z in 1:nz) {
      # Check if the voxel is inside the half-cylinder
      if (y > center_y && sqrt((x - center_x)^2 + (y - center_y)^2) <= radius) {
        cylinder_mask[x, y, z] <- 0  # Zero out the region inside the half-cylinder
      }
    }
  }
}

# Apply the mask to the MRI volume to "cut" the half-cylinder
DI_masked <- DI * cylinder_mask  # Only zero out the cut region

# Initialize the output image and opacity accumulation
image_top <- matrix(0, nrow = nx, ncol = ny)
opacity_accum <- matrix(0, nrow = nx, ncol = ny)

# Front-to-back compositing for the top view
for (z in 1:nz) {
  for (x in 1:nx) {
    for (y in 1:ny) {
      # Calculate the alpha (opacity) at this voxel
      alpha <- DI_masked[x, y, z] / max(DI)  # Normalized opacity
      
      # Calculate the color at this voxel (grayscale intensity)
      C <- DI_masked[x, y, z]
      
      # Composite the color and opacity into the final image
      image_top[x, y] <- image_top[x, y] + (1 - opacity_accum[x, y]) * alpha * C
      opacity_accum[x, y] <- opacity_accum[x, y] + (1 - opacity_accum[x, y]) * alpha
    }
  }
}

# Normalize the image to improve visibility
image_top <- image_top / max(image_top)

# Display the top view
image(image_top, col = gray.colors(256), main = "Top View of Half-Cylinder MRI Volume")






## task 2 

# load matlab data file 
toothData <- readMat("C:/Users/colin/OneDrive/Documents/MATLAB/Wk6 prac/Volume_Visualisation_files/ctTooth128x128x256.mat")
tooth<- toothData$ctTooth128x128x256

slice <- tooth[64,,]
rotated_slice <- t(apply(slice,2,rev))
image(rotated_slice, col = gray.colors(256))

## task 3
# Dimensions of the MRI data
dim_x <- dim(tooth)[1]
dim_y <- dim(tooth)[2]
dim_z <- dim(tooth)[3]

# Set up the opacity transfer function
L <- 256  # Length of the opacity array
opacity_array <- rep(0, L)  # Initialize opacity array to 0

# generate a smooth linear ramp for the opacity transfer function
# Create the first sharp peak at around index 50
opacity_array[10:40] <- seq(0, 1, length.out = 31)  # Rising edge
opacity_array[41:60] <- seq(1, 0, length.out = 20)  # Falling edge

# create the second smaller peak at around index 150
opacity_array[150:165] <- seq(0, 0.1, length.out = 16)  # Rising edge
opacity_array[166:180] <- seq(0.1, 0, length.out = 15)  # Falling edge

# create the third small peak at around index 250
opacity_array[230:240] <- seq(0, 0.3, length.out = 11)  # Rising edge
opacity_array[241:250] <- seq(0.7, 0, length.out = 10)  # Falling edge

# set up parameters for mapping volume intensities to opacity
minMRI <- min(tooth)
maxMRI <- max(tooth)
lenMRI <- maxMRI - minMRI

# initialize the alpha (opacity) volume to store opacity values for each voxel
alpha <- array(0, dim = c(dim_x, dim_y, dim_z))

# map each voxel's intensity value to an opacity using the transfer function
for (x in 1:dim_x) {
  for (y in 1:dim_y) {
    for (z in 1:dim_z) {
      volVal <- tooth[x, y, z]  # The intensity value at (x, y, z)
      # Map the volume value to an index in the opacity array
      opacityIndex <- trunc((volVal - minMRI) / lenMRI * (L - 1)) + 1
      # Assign the opacity value from the transfer function
      alpha[x, y, z] <- opacity_array[opacityIndex]
    }
  }
}

# initialize the composited image array for rendering
I <- array(0, dim = c(dim_x, dim_y))

# Step 6: Compositing loop over the x, y, and z dimensions
for (x in 1:dim_x) {
  for (y in 1:dim_y) {
    Cin <- 0  # Initial color/intensity value
    Ain <- 0  # Initial accumulated opacity
    for (z in 1:dim_z) {
      volVal <- tooth[x, y, z]  # The intensity value at (x, y, z)
      opacityIndex <- trunc((volVal - minMRI) / lenMRI * (L - 1)) + 1  # Map intensity to opacity index
      alpha_val <- opacity_array[opacityIndex]  # Get opacity value from the transfer function
      
      # Perform front-to-back compositing
      Cout <- Cin + (1 - Ain) * (alpha_val * volVal)  # Composite the color/intensity
      Aout <- Ain + (1 - Ain) * alpha_val  # Accumulate opacity
      
      Cin <- Cout  # Update color
      Ain <- Aout  # Update opacity
    }
    I[x, y] <- Cout  # Final composited value for this pixel
  }
}

# display the composited image after applying the transfer function
image(I, col = gray.colors(256), main = "Rendered Volume with Opacity Transfer Function")



# task 4
stagData <- file("C:/Users/colin/OneDrive/Documents/MATLAB/Wk6 prac/Volume_Visualisation_files/stag_beetle_832x832x494_uint16.raw",'rb')

beetle_data <- readBin(stagData, 'integer',n = 832*832*494, size = 2, signed = FALSE)
close(stagData)

beetle_data <- array(beetle_data, dim = c(832, 832, 494))

# Initialize the I matrix with the same dimensions as x and y of Di
K <- matrix(0, nrow = 832, ncol = 832)

# Sum over the z-dimension for each x, y coordinate
for (x in 1:832) {
  for (y in 1:832) {
    K[x, y] <- sum(beetle_data[x, y, ])
  }
}

# Display the matrix I using a grayscale color map
image(K, col = gray.colors(256), main = "Summed Slices of beetle image")
