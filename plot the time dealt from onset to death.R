
rm(list= ls())
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
mers_25_death <- read.csv("mers_death_0_25.csv")
mers_25_death
mers_25_death <- ymd(mers_25_death$death) - ymd(mers_25_death$onset)
mers_25_death[mers_25_death < 0] <- NA       # Replace negative values by NA
mers_25_death[mers_25_death > 45] <- NA       # Replace negative values by NA
mers_25_death <- as.numeric(na.omit(mers_25_death))
mers_25_death
mers_25_death <- as.numeric(na.omit(mers_25_death))
mers_25_death
fit_mers_25_deathg <- fitdist(mers_25_death, "gamma")
fit_mers_25_deathw <- fitdist(mers_25_death, "weibull")
fit_mers_25_deathl <- fitdist(mers_25_death, "lnorm")
fit_mers_25_deathg
fit_mers_25_deathw
fit_mers_25_deathl

################################################

ests <- bootdist(fit_mers_25_deathg, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_mers_25_deathw, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_mers_25_deathl, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_mers_25_deathg, fit_mers_25_deathw, fit_mers_25_deathl))
# Define the parameters for the distributions
#gamma_shape <- c(5.42, 2.38, 1.85, 2.43)
#gamma_rate <- c(0.56, 0.14, 0.11, 0.14)
#weibull_shape <- c(2.38, 1.61, 1.36, 1.60)
#weibull_scale <- c(10.9, 18.5, 18.5, 18.7)
#lognormal_mean <- c(2.17, 2.57, 2.53, 2.59)
#lognormal_mean_sd <- c(0.44, 0.70, 0.79, 0.69)
# Define the parameters for the distributions
gamma_shape <- c(2.66, 2.16, 2.20, 2.74)
gamma_rate <- c(0.21, 0.14, 0.14, 0.17)
weibull_shape <- c(1.56, 1.57, 1.57, 1.76)
weibull_scale <- c(14.2, 16.4, 16.8, 17.5)
lognormal_mean <- c(2.33, 2.44, 2.46, 2.55)
lognormal_mean_sd <- c(0.60, 0.76, 0.74, 0.65)



# Create histograms for the data within subfigure_1
hist_data_subfigure_1111 <- data.frame(x = mers_25_death)  # Assuming mers1+1 contains your data
histogram_subfigure_1111 <- ggplot(hist_data_subfigure_1111, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()
histogram_subfigure_1111


# Create the plot
density_plot_1111 <- ggplot(data = hist_data_subfigure_1111, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[1], rate = gamma_rate[1]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[1], scale = weibull_scale[1]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[1], sdlog = lognormal_mean_sd[1]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 0-24")+
  xlim(0, 40)  # Set x-axis range from 0 to 30


# Center the title in the middle of the graph
density_plot_1111 <- density_plot_1111 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_1111)





################################################################################
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
mers_50_death <- read.csv("mers_death_25_50.csv")
print(mers_50_death)

# Calculate the difference between death and onset
mers_50_death <- ymd(mers_50_death$death) - ymd(mers_50_death$onset)
print(mers_50_death)

# Replace negative values and values greater than 45 by NA
mers_50_death[mers_50_death < 0] <- NA
mers_50_death[mers_50_death > 45] <- NA

# Convert to numeric and omit NAs
mers_50_death <- as.numeric(na.omit(mers_50_death))

# Replace any zero values with 1
mers_50_death <- replace(mers_50_death, mers_50_death == 0, 1)

# Print modified data
print(mers_50_death)

fitt_mers_50_death <- fitdist(mers_50_death, "gamma")
fitt_mers2_50_death <- fitdist(mers_50_death, "weibull")
fitt_mers3_50_death <- fitdist(mers_50_death, "lnorm")
fitt_mers_50_death
fitt_mers2_50_death
fitt_mers3_50_death
#####################
################################################

ests <- bootdist(fitt_mers_50_death, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_mers2_50_death, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_mers3_50_death, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_mers_50_death, fitt_mers2_50_death, fitt_mers3_50_death))
# Create histograms for the data within subfigure_1
hist_data_subfigure_2222 <- data.frame(x = mers_50_death)  # Assuming mers1+1 contains your data
histogram_subfigure_2222 <- ggplot(hist_data_subfigure_2222, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()
histogram_subfigure_2222


# Create the plot
density_plot_2222 <- ggplot(data = hist_data_subfigure_2222, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[2], rate = gamma_rate[2]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[2], scale = weibull_scale[2]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[2], sdlog = lognormal_mean_sd[2]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 25-49")+
  xlim(0, 60)  # Set x-axis range from 0 to 30


# Center the title in the middle of the graph
density_plot_2222 <- density_plot_2222 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_2222)




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
mers_75_death <- read.csv("mers_death_50_75.csv")
mers_75_death
mers_75_death <- ymd(mers_75_death$death) - ymd(mers_75_death$onset)
mers_75_death
mers_75_death[mers_75_death < 0] <- NA       # Replace negative values by NA
mers_75_death[mers_75_death > 45] <- NA       # Replace negative values by NA

mers_75_death <- as.numeric(na.omit(mers_75_death))
mers_75_death
fittt_mers_75_death <- fitdist(mers_75_death, "gamma")
fittt_mers2_75_death <- fitdist(mers_75_death, "weibull")
fittt_mers3_75_death <- fitdist(mers_75_death, "lnorm")
fittt_mers_75_death
fittt_mers2_75_death
fittt_mers3_75_death
##########################################
################################################

ests <- bootdist(fittt_mers_75_death, niter = 1e3)
summary(ests)
ests1 <- bootdist(fittt_mers2_75_death, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fittt_mers3_75_death, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fittt_mers_75_death, fittt_mers2_75_death, fittt_mers3_75_death))

# Create histograms for the data within subfigure_1
hist_data_subfigure_3333 <- data.frame(x = mers_75_death)  # Assuming mers1+1 contains your data
histogram_subfigure_3333 <- ggplot(hist_data_subfigure_3333, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_3333 <- ggplot(data = hist_data_subfigure_3333, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 50-74")+
  xlim(0, 60)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_3333 <- density_plot_3333 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_3333)


##########################################################################


#Yehya Althobaity

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
mers_100_death <- read.csv("mers_death_75_100.csv")
mers_100_death
mers_100_death <- ymd(mers_100_death$death) - ymd(mers_100_death$onset)
mers_100_death
mers_100_death[mers_100_death < 0] <- NA       # Replace negative values by NA
mers_100_death[mers_100_death > 45] <- NA       # Replace negative values by NA

mers_100_death <- as.numeric(na.omit(mers_100_death))
mers_100_death
fittt_mers_100_death <- fitdist(mers_100_death, "gamma")
fittt_mers2_100_death <- fitdist(mers_100_death, "weibull")
fittt_mers3_100_death <- fitdist(mers_100_death, "lnorm")
fittt_mers_100_death
fittt_mers2_100_death
fittt_mers3_100_death
###################
##########################################
################################################

ests <- bootdist(fittt_mers_100_death, niter = 1e3)
summary(ests)
ests1 <- bootdist(fittt_mers2_100_death, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fittt_mers3_100_death, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fittt_mers_100_death, fittt_mers2_100_death, fittt_mers3_100_death))


# Create histograms for the data within subfigure_1
hist_data_subfigure_4444 <- data.frame(x = mers_100_death)  # Assuming mers1+1 contains your data
histogram_subfigure_4444 <- ggplot(hist_data_subfigure_4444, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_4444 <- ggplot(data = hist_data_subfigure_4444, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[4], rate = gamma_rate[4]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[4], scale = weibull_scale[4]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[4], sdlog = lognormal_mean_sd[4]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 75-100")+
  xlim(0, 60)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_4444 <- density_plot_4444 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_4444)

##########################################################

# combine all plots
library(cowplot)

# Assuming you have four density_plot objects named density_plot, density_plot_2, density_plot_3, and density_plot_4

# Arrange the density plots in a grid under a common title
combined_plot <- plot_grid(
  density_plot_1111, density_plot_2222, density_plot_3333, density_plot_4444,
  nrow = 2,  # Specify the number of rows in the grid
  labels = c("A", "B", "C", "D"),  # Add labels (optional)
  align = "v",  # Align plots vertically
  rel_heights = c(1, 1)  # Adjust relative heights of plots (optional)
)


# Add a common title to the combined plot using ggtitle()
combined_plot <- combined_plot +
  ggtitle("Time from onset to death for different age groups") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center the title and customize its appearance

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PDF file
ggsave("~/Desktop/mers_delays/figures/combined_plot_death.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot/combined_plot_death.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot1/combined_plot_death.pdf", combined_plot, width = 10, height = 8)

# Replace "~/Desktop/mers_delays/figures/" with the desired path where you want to save the PDF file.
# Adjust the width and height parameters according to your preference.

