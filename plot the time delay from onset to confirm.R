library(ggplot2) 
library(viridis)
library(cowplot)
library(lubridate)
library(DataExplorer)
library(fitdistrplus)
rm(list = ls())


# Estimating distribution from onset of symptoms to confirm 0-24
mers1 <- read.csv("mers1_25.csv")
mers1
mers1 -> mers1c
mers1c

# Calculate the difference between date_confirmed and date_onset
mers1c <- ymd(mers1c$date_confirmed) - ymd(mers1c$date_onset)
mers1c

# Convert to numeric and omit NAs
mers1c <- as.numeric(na.omit(mers1c))

# Replace any zero values with 1
mers1c <- replace(mers1c, mers1c == 0, 1)
mers1c

fitt_mers1c <- fitdist(mers1c, "gamma")
fitt_mers2c <- fitdist(mers1c, "weibull")
fitt_mers3c <- fitdist(mers1c, "lnorm")
fitt_mers1c
fitt_mers2c
fitt_mers3c

################################################

ests <- bootdist(fitt_mers1c, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_mers2c, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_mers3c, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_mers1c, fitt_mers2c, fitt_mers3c))


# Define the parameters for the distributions
gamma_shape <- c(1.31, 1.81, 1.95,1.71)
gamma_rate <- c(0.20, 0.31, 0.28, 0.20)
weibull_shape <- c(1.13, 1.43, 1.46, 1.38)
weibull_scale <- c(6.80, 6.37, 7.56,9.03)
lognormal_mean <- c(1.44, 1.45, 1.64, 1.78)
lognormal_mean_sd <- c( 0.95,  0.83, 0.79, 0.86)

# Create histograms for the data within subfigure_1
hist_data_subfigure_11 <- data.frame(x = mers1c)
histogram_subfigure_11 <- ggplot(hist_data_subfigure_11, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "white") +
  theme_minimal()
histogram_subfigure_11


# Create the plot
density_plot_11 <- ggplot(data = hist_data_subfigure_11, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[1], rate = gamma_rate[1]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[1], scale = weibull_scale[1]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[1], sdlog = lognormal_mean_sd[1]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 0-24")+
  xlim(0, 30)  # Set x-axis range from 0 to 30


# Center the title in the middle of the graph
density_plot_11 <- density_plot_11 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_11)





################################################################################
library(fitdistrplus)
# Estimating distribution from onset of symptoms to confirm

# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to confirm
mers25c <- read.csv("mers_25_50.csv")
print(mers25c)

# Calculate the difference between date_confirmed and date_onset
mers25c <- ymd(mers25c$date_confirmed) - ymd(mers25c$date_onset)
print(mers25c)

# Convert to numeric and omit NAs
mers25c <- as.numeric(na.omit(mers25c))

# Replace any zero values with 1
mers25c <- replace(mers25c, mers25c == 0, 1)

# Add 0.1 to the values
mers25c

fitt_cg_mers25 <- fitdist(mers25c, "gamma") #(cg,cw,cl mean confirm cases fitted by gamma, wuibul, lognormal )
fitt_cw_mers25 <- fitdist(mers25c, "weibull")
fitt_cl_mers25 <- fitdist(mers25c, "lnorm")
fitt_cg_mers25
fitt_cw_mers25
fitt_cl_mers25

################################################

ests <- bootdist(fitt_cg_mers25, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_cw_mers25, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_cl_mers25, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_cg_mers25, fitt_cw_mers25, fitt_cl_mers25))



# Create histograms for the data within subfigure_1
hist_data_subfigure_22 <- data.frame(x = mers25c)
histogram_subfigure_22 <- ggplot(hist_data_subfigure_22, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_22 <- ggplot(data = hist_data_subfigure_22, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[2], rate = gamma_rate[2]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[2], scale = weibull_scale[2]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[2], sdlog = lognormal_mean_sd[2]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 25-49")+
  xlim(0, 30)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_22 <- density_plot_22 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_22)
#################################################################################
# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to confirm
mers50c <- read.csv("mers_50_75.csv")
print(mers50c)

# Calculate the difference between date_confirmed and date_onset
mers50c <- ymd(mers50c$date_confirmed) - ymd(mers50c$date_onset)
print(mers50c)

# Convert to numeric and omit NAs
mers50c <- as.numeric(na.omit(mers50c))

# Replace any zero values with 1
mers50c <- replace(mers50c, mers50c == 0, 1)

# Print modified data
print(mers50c)


fitt_cg_mers50 <- fitdist(mers50c, "gamma")
fitt_cw_mers50 <- fitdist(mers50c, "weibull")
fitt_cl_mers50 <- fitdist(mers50c, "lnorm")
fitt_cg_mers50
fitt_cw_mers50
fitt_cl_mers50

################################################

ests <- bootdist(fitt_cg_mers50, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_cw_mers50, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_cl_mers50, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_cg_mers50, fitt_cw_mers50, fitt_cl_mers50))




# Create histograms for the data within subfigure_1
hist_data_subfigure_33 <- data.frame(x = mers50c)
histogram_subfigure_33 <- ggplot(hist_data_subfigure_33, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_33 <- ggplot(data = hist_data_subfigure_33, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 50-74")+
  xlim(0, 30)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_33 <- density_plot_33 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_33)


##########################################################################
# Yehya Althobaity

# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to confirm
mers75c <- read.csv("mers_75_100.csv")
print(mers75c)

# Calculate the difference between date_confirmed and date_onset
mers75c <- ymd(mers75c$date_confirmed) - ymd(mers75c$date_onset)
print(mers75c)

# Convert to numeric and omit NAs
mers75c <- as.numeric(na.omit(mers75c))

# Replace any zero values with 1
mers75c <- replace(mers75c, mers75c == 0, 1)

# Print modified data
print(mers75c)

fit_cg_mers75 <- fitdist(mers75c, "gamma")
fit_cw_mers75 <- fitdist(mers75c, "weibull")
fit_cl_mers75 <- fitdist(mers75c, "lnorm")
fit_cg_mers75
fit_cw_mers75
fit_cl_mers75
###################

################################################

ests <- bootdist(fit_cg_mers75, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_cw_mers75, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_cl_mers75, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_cg_mers75, fit_cw_mers75, fit_cl_mers75))





# Create histograms for the data within subfigure_1
hist_data_subfigure_44 <- data.frame(x = mers75c)
histogram_subfigure_44 <- ggplot(hist_data_subfigure_44, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_44 <- ggplot(data = hist_data_subfigure_44, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 75-100")+
  xlim(0, 30)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_44 <- density_plot_44 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_44)

##########################################################

# combine all plots
library(cowplot)

# Assuming you have four density_plot objects named density_plot, density_plot_2, density_plot_3, and density_plot_4

# Arrange the density plots in a grid under a common title
combined_plot <- plot_grid(
  density_plot_11, density_plot_22, density_plot_33, density_plot_44,
  nrow = 2,  # Specify the number of rows in the grid
  labels = c("A", "B", "C", "D"),  # Add labels (optional)
  align = "v",  # Align plots vertically
  rel_heights = c(1, 1)  # Adjust relative heights of plots (optional)
)

# Add a common title to the combined plot using ggtitle()
combined_plot <- combined_plot +
  ggtitle("Time from onset to confirmation for different age groups") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center the title and customize its appearance

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PDF file
ggsave("~/Desktop/mers_delays/figures/combined_plot_confirm.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot/combined_plot_confirm.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot1/combined_plot_confirm.pdf", combined_plot, width = 10, height = 8)

# Replace "~/Desktop/mers_delays/figures/" with the desired path where you want to save the PDF file.
# Adjust the width and height parameters according to your preference.


