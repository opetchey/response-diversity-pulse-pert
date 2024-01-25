x <- seq(0.01, 5, by = 0.01)

# Compute the PDF values for the lognormal distribution
i <- 3
pdf_values <- dlnorm(x, meanlog = alpha_ij_mean_treatment[i], sdlog = alpha_ij_sd_treatment[1])
mean(log(rlnorm(10000000, meanlog = alpha_ij_mean_treatment[i], sdlog = alpha_ij_sd_treatment[1])))
hist(log(rlnorm(10000000, meanlog = alpha_ij_mean_treatment[i], sdlog = alpha_ij_sd_treatment[1])))
hist(rlnorm(100, meanlog = alpha_ij_mean_treatment[i], sdlog = alpha_ij_sd_treatment[1]))

# Create a plot using the base R plot function
plot(x, pdf_values, type = "l", main = "Lognormal Distribution PDF",
     xlab = "x", ylab = "Density")

plot(log(x), pdf_values, type = "l", main = "Lognormal Distribution PDF",
     xlab = "x", ylab = "Density")
