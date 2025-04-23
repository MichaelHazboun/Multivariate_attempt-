library(MASS)

# Set parameters
n <- 200  # number of observations
mu <- c(0, 0)  # mean of both variables
Sigma <- matrix(c(1, 0.7, 0.7, 1), nrow = 2)  # covariance matrix with correlation 0.7

# Generate data
set.seed(121)
sim_data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

# Convert to a data frame
df <- data.frame(x = sim_data[,1], y = sim_data[,2])

# Check correlation
cor(df$x, df$y)
tau(df$x, df$y)
