
library(psych)
library(nnet)
library(rcompanion)
library(knitr)
library(jtools)
library(jtools)
library(lmtest)
library(leaps)
library(tidyverse)
library(HSAUR2)
library(devtools)
library(MASS)
library(tidyverse)
library(modelr)
library(broom)
library(faraway)
library(glmnet)
library(caret)
library(corrplot)
library(car)
library(olsrr)
library(pastecs)
library(ggplot2)
library(readxl)
library(ggplot2)
library(dplyr)
library(RcmdrMisc)
library(knitr)
library(tidyverse)
library(lattice)
library(gvlma)
library(vcd)
library(gridExtra)
library(grid)
library(readr)
library(e1071)
library(readxl)
library(MASS)
library(rcompanion)
library(devtools)
library(qqplotr)
library(ggplot2)
library(tidyr)

attach(Book1)
str(df)
df<-data.frame(Book1)
df1<-as.numeric(df$x)



## Descriptive Statistics 
# Calculate mean

# Mean
mean <- mean(df1) ; mean
cat("The Mean  is ", mean, "\n")

## median 

median<- median(df1);median
cat("The Median  is ", median, "\n")

# Variance
variance<- var(df1); variance
cat("The variance  is ", variance, "\n")

## skewness
skew <- skewness(df1); skew
cat("The skewness  is ", skew, "\n")

## kurtosis 
kurt <- kurtosis(df1); kurt
cat("The kurtosis  is ", kurt, "\n")


## Graphical Summaries

## barplot and summary statistics 
hist(df1, freq = TRUE, breaks = -0.5:16.5, main = " Frequency Histogram", 
     xlab = "Values", ylab = "Frequency",
     col = "yellow", border = "black")

## Overlaying Distributions (Normal Distribution)
hist(df1, freq = TRUE, breaks = -0.5:16.5, main = " Frequency Histogram", 
     xlab = "Values", ylab = "Frequency",
     col = "yellow", border = "black")

curve(250*dnorm(x, mean = mean(df1), sd = sd(df1)), 
      col = "green", add = TRUE , lwd = 2)

## Overlaying Distributions (Gamma Distribution)
n=250
hist(df1, freq = TRUE, breaks = -0.5:16.5, main = " Frequency Histogram With Gamma Distribution Overlayed", 
     xlab = "Values", ylab = "Frequency",
     col = "yellow", border = "black")

rate0<- mean/((250-1)*variance/250) 
shape0<- mean*rate0

x= seq(0,16,0.1)
nf2= 250*dgamma(x,shape = shape0,rate = rate0 )
lines(x,nf2)

## overlay with curve 
curve(250*dgamma(x, shape = shape0, rate = rate0), 
      col = "blue", add = TRUE , lwd = 2)

## Overlaying Distributions (Both  Distribution)

hist(df1, freq = TRUE, breaks = -0.5:16.5, main = " Frequency Histogram With Gamma Distribution Overlayed", 
     xlab = "Values", ylab = "Frequency",
     col = "yellow", border = "black")

curve(250*dgamma(x, shape = shape0, rate = rate0), 
      col = "blue", add = TRUE , lwd = 2)

curve(250*dnorm(x, mean = mean(df1), sd = sd(df1)), 
      col = "green", add = TRUE , lwd = 2)

legend("topright", legend = c("Gamma", "Normal"),
       col = c("blue", "green"), lty = 1, lwd = 2)


test_statistics = sqrt(250)*skew/sqrt(6)
cat("The test_statistics  is ", test_statistics, "\n")



#  ESTIMATION 

## Normal Distribution 

count_data <- c(2.9518345, 3.9388473, 2.0611947, 2.5501953, 2.3085359, 3.6021186, 2.5097325, 3.3458444, 3.0430528, 10.4347281, 2.7026414, 7.5957317, 3.8394892, 4.0748238, 3.8787315, 3.2189439, 4.0542754, 3.7460869, 3.4951031, 5.0324187, 4.2518845, 7.7635849, 1.9258159, 1.3409598, 3.3215834, 2.8389649, 4.3681919, 1.5072993, 1.4334211, 4.222746, 4.3911903, 2.7596659, 2.0505925, 3.2170536, 8.6241934, 8.5471231, 1.1395587, 7.4534604, 5.1878536, 3.1787636, 10.0166548, 4.9553171, 3.1077923, 4.9067434, 4.5816825, 3.3348609, 1.5241676, 1.7360338, 5.1173866, 3.8177061, 8.8730337, 8.8916411, 6.3002191, 3.202906, 5.3223872, 2.8805777, 4.2201919, 15.532156, 3.0169468, 3.7661819, 6.9947156, 5.2999819, 6.8641697, 7.9685238, 2.0759022, 5.8306407, 5.9984629, 3.7660152, 1.6240323, 4.1434851, 2.8960964, 2.6784146, 3.1781631, 2.9641222, 2.6806397, 2.3405723, 1.8714592, 5.6135622, 6.3033591, 5.6260477, 2.181948, 8.3439134, 3.2117282, 1.4806274, 2.3164924, 5.2489198, 2.4870267, 6.618619, 5.6141725, 3.3489726, 3.6710012, 1.6087058, 2.6110378, 5.0006981, 2.7134779, 2.066166, 6.6617576, 2.4676576, 4.1298227, 6.8662709, 5.8924198, 5.2810988, 4.521661, 2.8839952, 2.6382683, 9.4221469, 2.567415, 4.78724, 3.9322943, 4.2152738, 4.0373169, 4.2787065, 2.5878252, 2.0108109, 3.2545508, 0.9091306, 5.4486562, 4.313952, 2.7907841, 9.4735996, 1.8022357, 5.611349, 2.8458236, 3.2569113, 5.8257501, 1.0718122, 5.0398585, 2.1130899, 3.9980808, 2.164855, 3.8745264, 3.0153226, 3.3968946, 5.292036, 6.7801422, 3.2649999, 2.5488462, 2.9536965, 6.2201573, 7.1046948, 2.4807249, 8.1203447, 3.63408, 2.8308505, 5.2458368, 3.7117574, 2.955315, 5.5709552, 1.2238246, 1.9011773, 5.5648233, 1.8503191, 0.8904525, 4.621527, 3.5956579, 1.9583989, 9.7676448, 1.8768076, 3.1782186, 7.7297411, 4.5189492, 3.9849149, 4.1137594, 4.24139, 4.9340905, 4.1768136, 5.4440393, 3.8201072, 4.443273, 5.1148079, 2.8588517, 4.2338794, 4.9317477, 4.3737888, 2.2869107, 9.2577784, 3.3010511, 6.3451917, 1.7382654, 4.8416376, 3.6234582, 2.9694476, 4.0276265, 1.7913988, 7.2623669, 4.2529559, 3.1140006, 6.7106612, 1.9123473, 2.7128674, 4.6545161, 4.0169165, 1.0512623, 3.6953352, 4.8657693, 4.3212, 3.8120598, 3.4547145, 6.1779445, 3.8245089, 4.2589977, 3.1525031, 4.1787425, 5.2850202, 3.1624593, 1.5299445, 1.8431052, 5.1984571, 3.3271134, 5.9822611, 8.5524772, 4.4215559, 9.7707913, 4.6400718, 10.3608386, 3.7908427, 3.8349174, 4.0443578, 3.6383255, 5.2811999, 2.5151011, 6.0024412, 4.4977425, 4.4691735, 5.0115394, 3.2613598, 0.9758006, 2.6014184, 1.6510634, 1.3329671, 6.9591673, 4.5080408, 4.6077752, 4.9633771, 1.6533104, 10.150465, 5.9882372, 2.512263, 8.0126831, 2.4067788, 1.500168, 2.8341068, 2.8068887, 6.27132, 3.8074231, 1.4574121, 5.1225551, 4.2774953, 1.6103354, 0.8167878)

mu0<- mean(count_data)
cat("The  mean is ", mean, "\n")

s<- sd(count_data)
cat("The  standard deviation is ", s, "\n")

## calculate probabilities 
# Define the mean and standard deviation of the data
# Sample count data
count_data <- c(2.9518345, 3.9388473, 2.0611947, 2.5501953, 2.3085359, 3.6021186, 2.5097325, 3.3458444, 3.0430528, 10.4347281, 2.7026414, 7.5957317, 3.8394892, 4.0748238, 3.8787315, 3.2189439, 4.0542754, 3.7460869, 3.4951031, 5.0324187, 4.2518845, 7.7635849, 1.9258159, 1.3409598, 3.3215834, 2.8389649, 4.3681919, 1.5072993, 1.4334211, 4.222746, 4.3911903, 2.7596659, 2.0505925, 3.2170536, 8.6241934, 8.5471231, 1.1395587, 7.4534604, 5.1878536, 3.1787636, 10.0166548, 4.9553171, 3.1077923, 4.9067434, 4.5816825, 3.3348609, 1.5241676, 1.7360338, 5.1173866, 3.8177061, 8.8730337, 8.8916411, 6.3002191, 3.202906, 5.3223872, 2.8805777, 4.2201919, 15.532156, 3.0169468, 3.7661819, 6.9947156, 5.2999819, 6.8641697, 7.9685238, 2.0759022, 5.8306407, 5.9984629, 3.7660152, 1.6240323, 4.1434851, 2.8960964, 2.6784146, 3.1781631, 2.9641222, 2.6806397, 2.3405723, 1.8714592, 5.6135622, 6.3033591, 5.6260477, 2.181948, 8.3439134, 3.2117282, 1.4806274, 2.3164924, 5.2489198, 2.4870267, 6.618619, 5.6141725, 3.3489726, 3.6710012, 1.6087058, 2.6110378, 5.0006981, 2.7134779, 2.066166, 6.6617576, 2.4676576, 4.1298227, 6.8662709, 5.8924198, 5.2810988, 4.521661, 2.8839952, 2.6382683, 9.4221469, 2.567415, 4.78724, 3.9322943, 4.2152738, 4.0373169, 4.2787065, 2.5878252, 2.0108109, 3.2545508, 0.9091306, 5.4486562, 4.313952, 2.7907841, 9.4735996, 1.8022357, 5.611349, 2.8458236, 3.2569113, 5.8257501, 1.0718122, 5.0398585, 2.1130899, 3.9980808, 2.164855, 3.8745264, 3.0153226, 3.3968946, 5.292036, 6.7801422, 3.2649999, 2.5488462, 2.9536965, 6.2201573, 7.1046948, 2.4807249, 8.1203447, 3.63408, 2.8308505, 5.2458368, 3.7117574, 2.955315, 5.5709552, 1.2238246, 1.9011773, 5.5648233, 1.8503191, 0.8904525, 4.621527, 3.5956579, 1.9583989, 9.7676448, 1.8768076, 3.1782186, 7.7297411, 4.5189492, 3.9849149, 4.1137594, 4.24139, 4.9340905, 4.1768136, 5.4440393, 3.8201072, 4.443273, 5.1148079, 2.8588517, 4.2338794, 4.9317477, 4.3737888, 2.2869107, 9.2577784, 3.3010511, 6.3451917, 1.7382654, 4.8416376, 3.6234582, 2.9694476, 4.0276265, 1.7913988, 7.2623669, 4.2529559, 3.1140006, 6.7106612, 1.9123473, 2.7128674, 4.6545161, 4.0169165, 1.0512623, 3.6953352, 4.8657693, 4.3212, 3.8120598, 3.4547145, 6.1779445, 3.8245089, 4.2589977, 3.1525031, 4.1787425, 5.2850202, 3.1624593, 1.5299445, 1.8431052, 5.1984571, 3.3271134, 5.9822611, 8.5524772, 4.4215559, 9.7707913, 4.6400718, 10.3608386, 3.7908427, 3.8349174, 4.0443578, 3.6383255, 5.2811999, 2.5151011, 6.0024412, 4.4977425, 4.4691735, 5.0115394, 3.2613598, 0.9758006, 2.6014184, 1.6510634, 1.3329671, 6.9591673, 4.5080408, 4.6077752, 4.9633771, 1.6533104, 10.150465, 5.9882372, 2.512263, 8.0126831, 2.4067788, 1.500168, 2.8341068, 2.8068887, 6.27132, 3.8074231, 1.4574121, 5.1225551, 4.2774953, 1.6103354, 0.8167878)

# Sort the data
sorted_data <- sort(count_data)

# Determine the boundaries for the intervals
num_intervals <- 5
boundaries <- seq(min(sorted_data), max(sorted_data), length.out = num_intervals + 1)

# Create intervals using cut()
intervals2 <- cut(sorted_data, breaks = boundaries, include.lowest = TRUE)

# Count the number of values in each interval
interval_counts <- table(intervals2)

intervals.new<- c("0.817-3.76", "3.76 - 6.7","6.7-9.65","9.65-12.6","12.6-15.5" )
freq<- c(117,102,24,6,1)
new_data<-data.frame(intervals.new,freq)

mean_data <- mean(count_data)
sd_data <- sd(count_data)

# Define the intervals and their boundaries
intervals2 <- unique(intervals2)
interval_boundaries <- cut(sort(count_data), breaks = boundaries)

# Initialize a vector to store probabilities
interval_probabilities <- numeric(length(intervals2))

# Calculate the probability for each interval
for (i in 1:length(intervals2)) {
  # Get the lower and upper bounds of the interval
  lower_bound <- boundaries[i]
  upper_bound <- boundaries[i + 1]
  
  # Calculate the cumulative probabilities at the bounds
  lower_prob <- pnorm(lower_bound, mean = mean_data, sd = sd_data)
  upper_prob <- pnorm(upper_bound, mean = mean_data, sd = sd_data)
  
  # Calculate the probability within the interval
  interval_probabilities[i] <- upper_prob - lower_prob
}

# Create a dataframe to store interval probabilities
interval_prob_df <- data.frame(Interval = as.character(intervals2), Probability = interval_probabilities)


integral5 <- pnorm(3.76, mu0, s) - pnorm(0.817, mu0, s)
integral6 <- pnorm(6.7, mu0, s) - pnorm(3.76, mu0, s)
integral7 <- pnorm(9.65, mu0, s) - pnorm(6.7, mu0, s)
integral8 <- pnorm(12.6, mu0, s) - pnorm(9.65, mu0, s)
integral9 <- pnorm(15.5, mu0, s) - pnorm(12.6, mu0, s)

## new 
integral5= integral5*250
integral6 = integral6*250
integral7 = integral7*250
integral8= integral8*250
integral9= integral9*250

## noramal estimate 
expected_normal<- c(integral5,integral6,integral7,integral8,integral9)
expected_normal<- round(expected_normal, digits = 3)

## normal estimation 
normal_est<-data.frame(intervals.new,freq,expected_normal)
kable(normal_est)



## Normal  Fit vrs Observed Frequencies 
data234<- data.frame(
  intervals = c("0.817-3.76", "3.76-6.7", "6.7-9.65", "9.65-12.6", "12.6-15.5"),
  freq = c(117, 102, 24, 6, 1),
  expected_nrom = c(90.018, 113.214, 29.896, 1.557, 0.015)
)

data_long <- pivot_longer(data234, cols = c(freq, expected_nrom), names_to = "variable", values_to = "value")

# Plot combined bar plot
ggplot(data_long, aes(x = intervals, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Interval", y = "Value", title = "Combined Bar Plot for Expected Normal Frequencies and Observed Frequencies") +
  scale_fill_manual(values = c("freq" = "green", "expected_nrom" = "blue")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##  Goodness of fit test (Kolmogorov-Smirnov)



## Hypothesis


ks.test(df1, "pnorm", mean=mean, sd= sqrt(variance))

The P-value is  0.008522. Since this p-value is less than 0.05, we reject   our null hypothesis and conclude that the observed frequency distribution and the estimated expected frequency distribution are the not same. That is, the normal distribution does not fit our data. 


##  GAMMA DISTRIBUTION
The probability density function (PDF) of the gamma distribution is given by:
  
  \[ f(x; \alpha, \beta) = \frac{\beta^\alpha x^{\alpha - 1} e^{-\beta x}}{\Gamma(\alpha)} \]

Where:
  \( f(x; \alpha, \beta) \) is the probability density function.

\( x \) is the random variable.

\( \alpha \) is the shape parameter.

\( \beta \) is the rate parameter.

\( \Gamma(\alpha) \) is the gamma function.


## Gamma distribution
gamma1 <- pgamma(3.76, shape = shape0, rate = rate0) - pgamma(0.817, shape = shape0, rate = rate0)
gamma2 <- pgamma(6.7, shape = shape0, rate = rate0) - pgamma(3.76,shape = shape0, rate = rate0)
gamma3 <- pgamma(9.65, shape = shape0, rate = rate0) - pgamma(6.7, shape = shape0, rate = rate0)
gamma4 <- pgamma(12.6, shape = shape0, rate = rate0) - pgamma(9.65, shape = shape0, rate = rate0)
gamma5 <- pgamma(15.5, shape = shape0, rate = rate0) - pgamma(12.6, shape = shape0, rate = rate0)

## new 
gamma1= gamma1*250
gamma2 = gamma2*250
gamma3 = gamma3*250
gamma4= gamma4*250
gamma5= gamma5*250

## gamma estimate 
expected_gamma<- c(gamma1,gamma2,gamma3,gamma4,gamma5)
expected_gamma<- round(expected_gamma, digits = 3)

## gamma estimation 
gamma_est<-data.frame(intervals.new,freq,expected_gamma)
kable(gamma_est)

# Create a data frame with the given information
data <- data.frame(
  intervals = c("0.817-3.76", "3.76-6.7", "6.7-9.65", "9.65-12.6", "12.6-15.5"),
  freq = c(117, 102, 24, 6, 1),
  expected_gamma = c(119.192, 96.448, 26.164, 4.661, 0.667)
)

# Reshape the data into long format for ggplot
library(tidyr)
data_long <- pivot_longer(data, cols = c(freq, expected_gamma), names_to = "variable", values_to = "value")


## Gamma  Fit vrs Observed Frequencies (Barplot)

data <- data.frame(
  intervals = c("0.817-3.76", "3.76-6.7", "6.7-9.65", "9.65-12.6", "12.6-15.5"),
  freq = c(117, 102, 24, 6, 1),
  expected_gamma = c(119.192, 96.448, 26.164, 4.661, 0.667)
)

data_long <- pivot_longer(data, cols = c(freq, expected_gamma), names_to = "variable", values_to = "value")

# Plot combined bar plot
ggplot(data_long, aes(x = intervals, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Interval", y = "Value", title = "Combined Bar Plot for Expected Gamma Frequencies and Observed Frequencies") +
  scale_fill_manual(values = c("freq" = "orange", "expected_gamma" = "purple")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

The gamma looks like a good fit for the data. 

## Goodness of Fit Test ((Kolmogorov-Smirnov))

Null Hypothesis (\(H_0\)):
  \[ H_0: F(x) = F_{\text{gamma}}(x; \alpha, \beta) \]

Alternative Hypothesis (\(H_a\)):
  \[ H_a: F(x) \neq F_{\text{gamma}}(x; \alpha, \beta) \]

ks_test_gamma <- ks.test(count_data, "pgamma", shape = shape0, rate = rate0)
ks_test_gamma

The P-value is  0.7551.  Since this p-value is greater than 0.05, we fail to reject   our null hypothesis and conclude that the observed frequency distribution and the estimated expected frequency distribution from the gamma distribution are the  same. That is, the gamma distribution  fits our data. 

# CONCLUSION 

We can conclude that the gamma distribution is a perfect fit for our data.The data is also skewed . 
