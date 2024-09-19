# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")


# Estimating distribution from onset of symptoms to report
mers1r <- read.csv("mers1_25.csv")
print(mers1r)

# Calculate the difference between date_reported and date_onset
mers1r <- ymd(mers1r$date_reported) - ymd(mers1r$date_onset)
print(mers1r)

# Convert to numeric and omit NAs
mers1r <- as.numeric(na.omit(mers1r))

# Replace any zero values with 1
mers1r <- replace(mers1r, mers1r == 0, 1)

# Print modified data
print(mers1r)

fittt_mers1 <- fitdist(mers1r, "gamma")
fittt_mers2 <- fitdist(mers1r, "weibull")
fittt_mers3 <- fitdist(mers1r, "lnorm")
fittt_mers1
fittt_mers2
fittt_mers3


################################################

ests <- bootdist(fittt_mers1, niter = 1e3)
summary(ests)
ests1 <- bootdist(fittt_mers2, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fittt_mers3, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fittt_mers1, fittt_mers2, fittt_mers3))

# Define the parameters for the distributions
gamma_shape <- c(1.66, 2.08, 2.51, 2.14)
gamma_rate <- c(0.20,  0.29, 0.31, 0.22)
weibull_shape <- c(1.32, 1.56, 1.66, 1.55)
weibull_scale <- c(8.79, 7.80, 9.01, 10.6)
lognormal_mean <- c(1.75, 1.68, 1.87, 2.00)
lognormal_mean_sd <- c(0.85, 0.72, 0.69, 0.76)



# Create histograms for the data within subfigure_1
hist_data_subfigure_111 <- data.frame(x = mers1r)
histogram_subfigure_111 <- ggplot(hist_data_subfigure_111, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()
histogram_subfigure_111


# Create the plot
density_plot_111 <- ggplot(data = hist_data_subfigure_111, aes(x = x)) +
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
density_plot_111 <- density_plot_111 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_111)





################################################################################
library(fitdistrplus)
# Estimating distribution from onset of symptoms to report

# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to report
mers25r <- read.csv("mers_25_50.csv")
print(mers25r)

# Calculate the difference between date_reported and date_onset
mers25r <- ymd(mers25r$date_reported) - ymd(mers25r$date_onset)
print(mers25r)

# Replace negative values by NA
mers25r[mers25r < 0] <- NA

# Convert to numeric and omit NAs
mers25r <- as.numeric(na.omit(mers25r))

# Replace any zero values with 1
mers25r <- replace(mers25r, mers25r == 0, 1)

# Print modified data
print(mers25r)

fitt_rg_mers25 <- fitdist(mers25r, "gamma")
fitt_rw_mers25 <- fitdist(mers25r, "weibull")
fitt_rl_mers25 <- fitdist(mers25r, "lnorm")
fitt_rg_mers25
fitt_rw_mers25
fitt_rl_mers25
#####################
################################################

ests <- bootdist(fitt_rg_mers25, niter = 1e3)
summary(ests)
ests1 <- bootdist(fitt_rw_mers25, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fitt_rl_mers25, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fitt_rg_mers25, fitt_rw_mers25, fitt_rl_mers25))

# Create histograms for the data within subfigure_1
hist_data_subfigure_222 <- data.frame(x = mers25r)
histogram_subfigure_222 <- ggplot(hist_data_subfigure_222, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_222 <- ggplot(data = hist_data_subfigure_222, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[2], rate = gamma_rate[2]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[2], scale = weibull_scale[2]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[2], sdlog = lognormal_mean_sd[2]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 25-49")+
  xlim(0, 40)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_222 <- density_plot_222 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_222)
#################################################################################
# Load libraries
library(lubridate)
library(bbmle)
library(plotrix)
library(fitdistrplus)
library(RColorBrewer)

# Define colors
cols <- brewer.pal(3, "Set1")

# Estimating distribution from onset of symptoms to report
mers50r <- read.csv("mers_50_75.csv")
print(mers50r)

# Calculate the difference between date_reported and date_onset
mers50r <- ymd(mers50r$date_reported) - ymd(mers50r$date_onset)
print(mers50r)

# Replace negative values by NA
mers50r[mers50r < 0] <- NA

# Convert to numeric and omit NAs
mers50r <- as.numeric(na.omit(mers50r))

# Replace any zero values with 1
mers50r <- replace(mers50r, mers50r == 0, 1)

# Print modified data
print(mers50r)

fittt_rg_mers50 <- fitdist(mers50r, "gamma")
fittt_rw_mers50 <- fitdist(mers50r, "weibull")
fittt_rl_mers50 <- fitdist(mers50r, "lnorm")
fittt_rg_mers50
fittt_rw_mers50
fittt_rl_mers50
#####################
################################################

ests <- bootdist(fittt_rg_mers50, niter = 1e3)
summary(ests)
ests1 <- bootdist(fittt_rw_mers50, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fittt_rl_mers50, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fittt_rg_mers50, fittt_rw_mers50, fittt_rl_mers50))


# Create histograms for the data within subfigure_1
hist_data_subfigure_333 <- data.frame(x = mers50r)
histogram_subfigure_333 <- ggplot(hist_data_subfigure_333, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_333 <- ggplot(data = hist_data_subfigure_333, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[3], rate = gamma_rate[3]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[3], scale = weibull_scale[3]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[3], sdlog = lognormal_mean_sd[3]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 50-74")+
  xlim(0, 40)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_333 <- density_plot_333 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_333)


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

# Estimating distribution from onset of symptoms to report
mers75r <- read.csv("mers_75_100.csv")
print(mers75r)

# Calculate the difference between date_reported and date_onset
mers75r <- ymd(mers75r$date_reported) - ymd(mers75r$date_onset)
print(mers75r)

# Replace negative values and values greater than 28 by NA
mers75r[mers75r < 0] <- NA
mers75r[mers75r > 28] <- NA  

# Convert to numeric and omit NAs
mers75r <- as.numeric(na.omit(mers75r))

# Replace any zero values with 1
mers75r <- replace(mers75r, mers75r == 0, 1)

# Print modified data
print(mers75r)

fit_rg_mers75 <- fitdist(mers75r, "gamma")
fit_rw_mers75 <- fitdist(mers75r, "weibull")
fit_rl_mers75 <- fitdist(mers75r, "lnorm")
fit_rg_mers75
fit_rw_mers75
fit_rl_mers75
###################

ests <- bootdist(fit_rg_mers75, niter = 1e3)
summary(ests)
ests1 <- bootdist(fit_rw_mers75, niter = 1e3)
summary(ests1)

ests2 <- bootdist(fit_rl_mers75, niter = 1e3)
summary(ests2)

###########################################
library(actuar)

gofstat(list(fit_rg_mers75, fit_rw_mers75, fit_rl_mers75))


# Create histograms for the data within subfigure_1
hist_data_subfigure_444 <- data.frame(x = mers75r)
histogram_subfigure_444 <- ggplot(hist_data_subfigure_444, aes(x = x, fill = "Data")) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(y = "Frequency", x = "Days") +
  scale_fill_manual(values = "gray") +
  theme_minimal()

# Create the plot
density_plot_444 <- ggplot(data = hist_data_subfigure_444, aes(x = x)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "black", alpha = 0.6, fill = "white") +
  stat_function(fun = function(x) dgamma(x, shape = gamma_shape[4], rate = gamma_rate[4]), aes(color = "Gamma"), size = 1, linetype = "solid") +
  stat_function(fun = function(x) dweibull(x, shape = weibull_shape[4], scale = weibull_scale[4]), aes(color = "Weibull"), size = 1, linetype = "longdash") +
  stat_function(fun = function(x) dlnorm(x, meanlog = lognormal_mean[4], sdlog = lognormal_mean_sd[4]), aes(color = "Lognormal"), size = 1, linetype = "dotted") +
  labs(y = "Density", x = "Days", color = "Distribution") +
  scale_color_manual(values = c("Gamma" = "Dark Goldenrod 1", "Weibull" = "Maroon", "Lognormal" = "Purple 4")) +
  theme_minimal() +
  ggtitle("Age 75-100")+
  xlim(0, 40)  # Set x-axis range from 0 to 30

# Center the title in the middle of the graph
density_plot_444 <- density_plot_444 + theme(plot.title = element_text(hjust = 0.5))

# Print the plot
print(density_plot_444)

##########################################################

# combine all plots
library(cowplot)

# Assuming you have four density_plot objects named density_plot, density_plot_2, density_plot_3, and density_plot_4

# Arrange the density plots in a grid under a common title
combined_plot <- plot_grid(
  density_plot_111, density_plot_222, density_plot_333, density_plot_444,
  nrow = 2,  # Specify the number of rows in the grid
  labels = c("A", "B", "C", "D"),  # Add labels (optional)
  align = "v",  # Align plots vertically
  rel_heights = c(1, 1)  # Adjust relative heights of plots (optional)
)

# Add a common title to the combined plot using ggtitle()
combined_plot <- combined_plot +
  ggtitle("Time from onset to reporting for different age groups") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))  # Center the title and customize its appearance

# Print the combined plot
print(combined_plot)

# Save the combined plot as a PDF file
ggsave("~/Desktop/mers_delays/figures/combined_plot_report.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot/combined_plot_report.pdf", combined_plot, width = 10, height = 8)
ggsave("~/Desktop/mers_delays/figures/combine_plot1/combined_plot_report.pdf", combined_plot, width = 10, height = 8)

# Replace "~/Desktop/mers_delays/figures/" with the desired path where you want to save the PDF file.
# Adjust the width and height parameters according to your preference.




































