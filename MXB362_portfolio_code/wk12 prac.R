install.packages("GGally")
library(GGally)
library(ggplot2)

## principal component analysis ##

#load iris dataset 
data(iris)


# create a scatter plot matrix using pairs()
pairs(iris[1:4], 
      col = iris$Species,
      pch = 19)  

# step 3: center the data 
centered_data <- as.data.frame(scale(iris[,1:4], center = TRUE, scale = FALSE))
head(centred_data)
 
# step 4: convert the centered data to a matrix
X <- as.matrix(centered_data)

# Calculate the covariance matrix manually: C = (X^T * X) / (n - 1)
n <- nrow(X)  # number of observations
C <- t(X) %*% X / (n - 1)

# step 5: calculate eigenvalue decomposition of covariance matrix
# calculate the eigenvalue decomposition of the covariance matrix
eig_decomp <- eigen(C)

# eigenvalues
eigenvalues <- eig_decomp$values

# eigenvectors (these are the principal components)
eigenvectors <- eig_decomp$vectors

# step 6: 
# project the original data onto the new principal components
projected_data <- as.matrix(centered_data) %*% eigenvectors

# take the first two principal components
pc1 <- projected_data[, 1]
pc2 <- projected_data[, 2]

# Plot the projected data (for the first two principal components)
plot(pc1, pc2, col = iris$Species, pch = 19,
     xlab = "Principal Component 1",
     ylab = "Principal Component 2",
     main = "Projection onto First Two Principal Components")

# add legend to the plot  
legend("topright", legend = levels(iris$Species),
       col = 1:3, pch = 19)
  
## Linear discriminant analysis ##

# step 1: extract columns 1 and 3 from iris dataset and standardize 

# extract columns 1 and 3 (Sepal.Length and Petal.Length)
iris_selected <- iris[, c(1, 3)]
iris_standardized <- as.data.frame(scale(iris_selected))
  
# step 2: create a numerical species ID vector 
species_id <- as.numeric(iris$Species)

# step 3
# create 2x1 vector W
W <- c(0.0716, 0.9974)

# normalize W
W_normalized <- W / sqrt(sum(W^2))
print(W_normalized)

# step 4: project reduced iris dataset onto projection vector 
projected_data <- as.matrix(iris_standardized) %*% W_normalized

# step 5: 
# Load necessary library
library(MASS)

# compute class means for each species
# Split the standardized data by species
iris_split <- split(as.matrix(iris_standardized), species_id)

# correct the process of computing column means for each class
mean_class <- lapply(iris_split, function(x) colMeans(matrix(x, ncol=2)))


# compute the overall mean across all samples
overall_mean <- colMeans(as.matrix(iris_standardized))

# compute the between-class scatter matrix Sb
Sb <- matrix(0, nrow=2, ncol=2)  # Initialize a 2x2 matrix for Sb
for (i in 1:length(unique(species_id))) {
  n_i <- sum(species_id == i)  # Number of samples in class i
  diff <- mean_class[[i]] - overall_mean  # Difference between class mean and overall mean
  Sb <- Sb + n_i * (diff %*% t(diff))  # Update Sb (outer product of diff with itself)
}

# Compute the within-class scatter matrix Sw
Sw <- matrix(0, nrow=2, ncol=2)  # Initialize a 2x2 matrix for Sw
for (i in 1:length(unique(species_id))) {
  class_data <- as.matrix(iris_standardized[species_id == i, ])  # Data for class i
  mean_vec <- matrix(mean_class[[i]], nrow=nrow(class_data), ncol=2, byrow=TRUE)
  Sw <- Sw + t(class_data - mean_vec) %*% (class_data - mean_vec)  # Update Sw
}

# Calculate Fisher's discriminant J(W)
J_W <- (t(W_normalized) %*% Sb %*% W_normalized) / (t(W_normalized) %*% Sw %*% W_normalized)

# Print Fisher's linear discriminant J(W)
print(J_W)


# visualize using histogram
projected_data <- as.matrix(iris_standardized) %*% W_normalized  # Projection


# create separate histograms for each species
hist(projected_data[species_id == 1], breaks=10, col=rgb(1,0,0,0.5), 
     xlim=c(min(projected_data), max(projected_data)),
     main="Projection onto W: Histogram by Species", 
     xlab="Projected Values", 
     ylab="Frequency")
hist(projected_data[species_id == 2], breaks=10, col=rgb(0,1,0,0.5), add=TRUE)  # Overlay species 2
hist(projected_data[species_id == 3], breaks=10, col=rgb(0,0,1,0.5), add=TRUE)  # Overlay species 3

# add a legend to differentiate species
legend("topright", legend=c("setosa", "versicolor", "virginica"), 
       fill=c(rgb(1,0,0,0.5), rgb(0,1,0,0.5), rgb(0,0,1,0.5)))


## attempt to use PCA to maximise J(W) ##

pca_result <- prcomp(iris_standardized)

# Use all principal components
pca_data_all <- pca_result$x  # Select all principal components

# Perform LDA on all principal components
lda_model <- lda(Species ~ ., data = data.frame(pca_data_all, Species = iris$Species))

# Predict the values for LDA projection
lda_pred <- predict(lda_model)

# Extract the first discriminant (LD1) and the species labels
lda_values <- lda_pred$x[, 1]  # First linear discriminant (LD1)
species_labels <- iris$Species  # Species labels

# Create separate histograms for each species
hist(lda_values[species_labels == "setosa"], 
     breaks = 10, col = rgb(1, 0, 0, 0.5), 
     xlim = range(lda_values),
     main = "LDA Projection (LD1) Histogram by Species", 
     xlab = "LD1 Projected Values", 
     ylab = "Frequency")

hist(lda_values[species_labels == "versicolor"], 
     breaks = 10, col = rgb(0, 1, 0, 0.5), add = TRUE)  # Overlay for versicolor

hist(lda_values[species_labels == "virginica"], 
     breaks = 10, col = rgb(0, 0, 1, 0.5), add = TRUE)  # Overlay for virginica

# Add a legend to differentiate species
legend("topright", legend = c("setosa", "versicolor", "virginica"), 
       fill = c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5)))

