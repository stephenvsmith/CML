library(bnlearn)

#Need to convert to integer
asia_mat <- data.matrix(asia)

asia_d_cml <- cml(asia_mat, targets = 6, test = "gSquare")