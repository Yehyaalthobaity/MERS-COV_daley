library(ggplot2)
library(viridis)
library(cowplot)
library(lubridate)
library(DataExplorer)
library(fitdistrplus)
 
# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)
mers1a <- read.csv("mers1_25.csv")
mers1a

# Calculate the difference between date of admission and date of onset
mers1a <- ymd(mers1a$date_admission) - ymd(mers1a$date_onset)
mers1a

# Convert to numeric and remove NA values
mers1a <- as.numeric(na.omit(mers1a))

# Replace all zero values with 1
mers1a[mers1a == 0] <- 1

# Display the modified data
mers1a


fitt_mers1a <- fitdist(mers1a, "gamma")
fitt_mers2a <- fitdist(mers1a, "weibull")
fitt_mers3a <- fitdist(mers1a, "lnorm")
fitt_mers1a
fitt_mers2a
fitt_mers3a

################################################

ests <- bootdist(fitt_mers1a, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_mers2a, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_mers3a, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_mers1a, fitt_mers2a, fitt_mers3a))

# Define the parameters for the distributions
gamma_shape <- c(1.30, 1.21, 1.56, 1.33)
gamma_rate <- c(0.31, 0.35, 0.36, 0.28)
weibull_shape <- c(1.09, 1.21, 1.25, 1.13)
weibull_scale <- c(4.35, 4.35, 4.69, 4.93)
lognormal_mean <- c(1.00, 1.02, 1.11, 1.12)
lognormal_mean_sd <- c(0.90,  0.89, 0.85, 0.92)

# Create histograms for the data within subfigure_1
hist_data_subfigure_1 <- data.frame(x = mers1a)  # Assuming mers1+1 contains your data
histogram_subfigure_1 <- ggplot(hist_data_subfigure_1, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()
histogram_subfigure_1


# Create the plot
density_plot <- ggplot(data = hist_data_subfigure_1, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "gray") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[1], rate = gamma_rate[1]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[1], scale = weibull_scale[1]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[1], sdlog = lognormal_mean_sd[1]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 0-24")+
  xlim(0, 25)  # Set x-axis range from 0 to 30


# Center the title in the middle of the graph
density_plot <- density_plot + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot)





################################################################################
library(fitdistrplus)
library(lubridate)

# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)
mers25 <- read.csv("mers_25_50.csv")

# Calculate the difference between date of admission and date of onset
mers25 <- ymd(mers25$date_admission) - ymd(mers25$date_onset)

# Convert to numeric and remove NA values
mers25 <- as.numeric(na.omit(mers25))

# Replace all zero values with 1
mers25[mers25 == 0] <- 1

# Display the modified data
mers25




fit_mers25g <- fitdist(mers25, "gamma")
fit_mers25w <- fitdist(mers25, "weibull")
fit_mers25l <- fitdist(mers25, "lnorm")
fit_mers25g
fit_mers25w
fit_mers25l


ests <- bootdist(fit_mers25g, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_mers25w, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_mers25l, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_mers25g, fit_mers25w, fit_mers25l))

# Create histograms for the data within subfigure_1
hist_data_subfigure_2 <- data.frame(x = mers25)  # Assuming mers1+1 contains your data
histogram_subfigure_2 <- ggplot(hist_data_subfigure_2, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_2 <- ggplot(data = hist_data_subfigure_2, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "gray") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[2], rate = gamma_rate[2]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[2], scale = weibull_scale[2]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[2], sdlog = lognormal_mean_sd[2]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 25-49")+
  xlim(0, 25)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_2 <- density_plot_2 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_2)
#################################################################################
# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)
mers50 <- read.csv("mers_50_75.csv")
mers50

# Calculate the difference between date of admission and date of onset
mers50 <- ymd(mers50$date_admission) - ymd(mers50$date_onset)
mers50

# Convert to numeric and remove NA values
mers50 <- as.numeric(na.omit(mers50))

# Replace all zero values with 1
mers50[mers50 == 0] <- 1

# Display the modified data
mers50

fit_ag_mers50 <- fitdist(mers50, "gamma")
fit_aw_mers50 <- fitdist(mers50, "weibull")
fit_al_mers50 <- fitdist(mers50, "lnorm")
fit_ag_mers50
fit_aw_mers50
fit_al_mers50


ests <- bootdist(fit_ag_mers50, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_aw_mers50, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_al_mers50, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_ag_mers50, fit_aw_mers50, fit_al_mers50))


# Create histograms for the data within subfigure_1
hist_data_subfigure_3 <- data.frame(x = mers50)  # Assuming mers1+1 contains your data
histogram_subfigure_3 <- ggplot(hist_data_subfigure_3, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_3 <- ggplot(data = hist_data_subfigure_3, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "gray") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 50-74")+
  xlim(0, 25)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_3 <- density_plot_3 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_3)


##########################################################################



#Yehya Althobaity

# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to death
# Linton et al. (https://doi.org/10.3390/jcm9020538)
mers75 <- read.csv("mers_75_100.csv")
mers75

# Calculate the difference between date of admission and date of onset
mers75 <- ymd(mers75$date_admission) - ymd(mers75$date_onset)
mers75

# Convert to numeric and remove NA values
mers75 <- as.numeric(na.omit(mers75))

# Replace all zero values with 1
mers75[mers75 == 0] <- 1

# Display the modified data
mers75

fit_ag_mers75 <- fitdist(mers75, "gamma")
fit_aw_mers75 <- fitdist(mers75, "weibull")
fit_al_mers75 <- fitdist(mers75, "lnorm")
fit_ag_mers75
fit_aw_mers75
fit_al_mers75



ests <- bootdist(fit_ag_mers75, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_aw_mers75, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_al_mers75, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_ag_mers75, fit_aw_mers75, fit_al_mers75))



# Create histograms for the data within subfigure_1
hist_data_subfigure_4 <- data.frame(x = mers75)  # Assuming mers1+1 contains your data
histogram_subfigure_4 <- ggplot(hist_data_subfigure_4, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_4 <- ggplot(data = hist_data_subfigure_4, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "gray") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 75-100")+
  xlim(0, 25)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_4 <- density_plot_4 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_4)

##########################################################

# combine all plots
library(cowplot)

# Assuming you have four density_plot objects named density_plot, density_plot_2, density_plot_3, and density_plot_4

# Arrange the density plots in a grid under a common title
combined_plot <- plot_grid(
  density_plot, density_plot_2, density_plot_3, density_plot_4,
  nrow = 2,  # Specify the number of rows in the grid
  labels = c("A", "B", "C", "D"),  # Add labels (optional)
  align = "v",  # Align plots vertically
  rel_heights = c(1, 1)  # Adjust relative heights of plots (optional)
)

# Add a common title to the combined plot using ggtitle()
combined_plot <- combined_plot +
  ggtitle("Time from onset to admission for different age groups") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center the title and customize its appearance

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PDF file
ggsave("~/Desktop/mers_delays/figures/combine_plot1/combined_plot_admission.pdf", combined_plot, width = 10, height = 8)

# Replace "~/Desktop/mers_delays/figures/" with the desired path where you want to save the PDF file.
# Adjust the width and height parameters according to your preference.


###############################################################################
print(combined_plot)

