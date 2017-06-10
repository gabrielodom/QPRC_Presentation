# 3-D Scatterplots for State Projection Matrices

library(magrittr)
library(tidyverse)
library(scatterplot3d)


######  Process State 1  ######
t <- seq(0, 3, length.out = 1000)
Error3 <- matrix(rnorm(3 * 1000, sd = 0.15), nrow = 1000)

Process_df <- data.frame(t = t)
Process_df %<>% mutate(x = t,
                       y = t ^ 2 - 3 * t,
                       z = - t ^ 3 + 3 * t ^ 2)

Process_df[,2:4] <- Process_df[,2:4] + Error3

scatterplot3d(Process_df[,2:4], box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10)

######  Process State 2  ######
P2 <- matrix(c(0, 0.5, -sqrt(3) / 2,
               0, sqrt(3) / 2,  0.5,
               1,   0,            0),
             nrow = 3, ncol = 3, byrow = TRUE)
Lam2 <- diag(c(1, 0.5, 2))

# Rotate
Process2_mat <- as.matrix(Process_df[,2:4]) %*% P2
colnames(Process2_mat) <- c("x", "y", "z")
scatterplot3d(Process2_mat, box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10)

# Scale
Process2scale_mat <- Process2_mat %*% Lam2
colnames(Process2scale_mat) <- c("x", "y", "z")
scatterplot3d(Process2scale_mat, box = FALSE, highlight.3d = TRUE,
              angle = 10)

######  Process State 3  ######
P3 <- matrix(c( 0, sqrt(3) / 2,  -0.5,
               -1,    0,            0,
                0,   0.5, sqrt(3) / 2),
             nrow = 3, ncol = 3, byrow = TRUE)
Lam3 <- diag(c(0.25, 0.1, 0.75))

# Rotate
Process3_mat <- as.matrix(Process_df[,2:4]) %*% P3
colnames(Process3_mat) <- c("x", "y", "z")
scatterplot3d(Process3_mat, box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10)

# Scale
Process3scale_mat <- Process3_mat %*% Lam3
colnames(Process3scale_mat) <- c("x", "y", "z")
scatterplot3d(Process3scale_mat, box = FALSE, highlight.3d = TRUE,
              angle = 10)

######  Combine Plots  ######
par(mfrow = c(2, 3))

# Top Left
scatterplot3d(Process2_mat, box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10, cex.symbols = 1.2)

# Top Centre
scatterplot3d(Process_df[,2:4], box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10, cex.symbols = 1.2)

# Top Right
scatterplot3d(Process3_mat, box = FALSE, highlight.3d = TRUE,
              xlim = c(-1, 5), ylim = c(-3, 5), zlim = c(-5, 5),
              angle = 10, cex.symbols = 1.2)

# Bottom Left
scatterplot3d(Process2scale_mat, box = FALSE, highlight.3d = TRUE,
              angle = 10, cex.symbols = 1.2)

# Bottom Centre
plot("")

# Bottom Right 
scatterplot3d(Process3scale_mat, box = FALSE, highlight.3d = TRUE,
              angle = 10, cex.symbols = 1.2)

par(mfrow=c(1,1))
