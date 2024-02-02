# IMPORTING AND READING DATASET
library(readxl)
d <- read_excel("C:\\Users\\ramri\\Desktop\\cardio_train new.xlsx")

#STRUCTURE AND SUMMARY
str(d)
summary(d)

library(ggplot2)
library(dplyr)

#BAR CHART FOR DISTRIBUTION OF CATEGORICAL VARIABLE.
# Create a table of the counts of each category of the variable of interest
variable_counts <- d %>% 
  count(active)
# Create bar chart of smoking status
ggplot(variable_counts, aes(x = active, y = n, fill = gender)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Smoking Status", y = "Count", 
       title = "Distribution of Smoking Status in the Cardiovascular Dataset")+
  theme_bw()

#FREQUENCY POLYGON
ggplot(d, aes(x=age, color=gender)) +
  geom_freqpoly(alpha = 2, color = "red") +
  labs(title="Distribution of Age by Gender in Cardiovascular Dataset", 
       x="Age", y="Frequency") +
  scale_fill_manual(values=c("#E69F00", "#0072B2")) +
  theme_dark()

#COUNTING

# Compute basic statistics of the age variable
mean_age <- mean(d$age)
sd_age <- sd(d$age)
median_age <- median(d$age)
q1_age <- quantile(d$age, 0.25)
q3_age <- quantile(d$age, 0.75)

# Print the results
cat("Mean age: ", mean_age, "\n")
cat("Standard deviation of age: ", sd_age, "\n")
cat("Median age: ", median_age, "\n")
cat("1st quartile of age: ", q1_age, "\n")
cat("3rd quartile of age: ", q3_age, "\n")

# Create a scatterplot of age vs. height with point size based on count
ggplot(d, aes(x=age, y=height)) +
  geom_count() +
  labs(title="Scatterplot of Age vs. Height", x="Age", y="Height")

# Analyze Exponential Relationship between the Weight and Age
ggplot(d, aes(x=age, y=gender)) +
  geom_point() +
  labs(title="Scatterplot of Age vs. Gender", x="Age", y="Gender")
# Fit a linear regression model
linear_model <- lm(cardio ~ age, data=d)

# Add the linear regression line to the scatterplot
ggplot(d, aes(x=age, y=gender)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Scatterplot of Age vs. Weight with Linear Regression",
       x="Age", y="Gender")

#TILE PLOT
library(ggcorrplot)
d[] <- lapply(d, as.numeric)
d <- na.omit(d)
corr_matrix <- cor(d)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = FALSE)

#HEXBIN
library(hexbin)
hexbinplot(weight ~ height | age, 
           data = d, xlab = "Height", ylab = "Weight")

#Visualizing the combination of a categorical and a continuous variable
my_colors <- c("#E69F00", "#56B4E9")
boxplot(age ~ gender, data = d, 
        xlab = "Gender Class", ylab = "Age",
        main = "Age Distribution by Cardio Class",
        col = my_colors,
        border = "gray",
        notch = TRUE,
        notchwidth = 0.5,
        boxwex = 0.6,
        whisklty = 2,
        whisklwd = 1.5,
        outcex = 0.5,
        axes = FALSE)

axis(side = 1, at = 1:2, labels = levels(d$gender))
axis(side = 2, las = 1)
legend("topright", legend = c("Male", "Female"), fill = my_colors)

#Display the number of points on same bin
boxplot_data <- d %>%
  group_by(gender, cholesterol_2) %>%
  summarize(count = n())

box_plot <- ggplot(boxplot_data, aes(x = cholesterol_2, y = count, 
                                     fill = gender)) +
  geom_boxplot() +
  geom_text(aes(label = count), color = "white", size = 6, fontface = "bold",
            position = position_dodge(width = 0.8))

# Customize the plot
box_plot +
  labs(x = "Cholesterol level", y = "Count", 
       title = "Box plot of cholesterol level by gender") +
  theme_bw() +
  theme(legend.position = "bottom")

#****STATISTICAL MODELLING****

#PREDICTION OF Smoke FROM Age
# Fit a logistic regression model
model <- glm(smoke ~ age, data = d, family = binomial)
summary(model)
plot_data <- data.frame(Age = d$age, Smoke = as.factor(d$smoke))
plot_data$predicted_Smoke <- predict(model, newdata = d, type = "response")

# Plot the data
ggplot(plot_data, aes(x = Age, y = predicted_Smoke, color = Smoke)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = FALSE, size = 1) +
  scale_color_manual(values = c("gray40", "red")) +
  labs(x = "Age", y = "Predicted probability of smoking") +
  theme_minimal()

#UNIVARIATE GRAPHICAL EDA:
#HISTOGRAMS WITH DIFFERENT BIN WIDTHS
library(tidyverse)
bin_widths <- c(2, 5, 10)
hist_list <- lapply(bin_widths, function(w) {
  ggplot(d, aes(x = age)) +
    geom_histogram(binwidth = w, fill = "steelblue", color = "white") +
    labs(title = paste0("Bin width: ", w), x = "Age", y = "Count") +
    theme_minimal()
})
gridExtra::grid.arrange(grobs = hist_list, ncol = 3)

#STEM AND LEAF
stem(d$age, scale = 1, width = 65, atom = 1e-08)

#BOX PLOT
ggplot(d, aes(x = smoke, y = age, fill = gender)) +
  geom_boxplot(fill = "black", color = "white") +
  labs(title = "Distribution of Age", y = "Age") +
  theme_minimal() +
  coord_flip()

#QQPLOT
ggplot(d, aes(sample = age)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Age", x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  theme_minimal()

#QQ DENSITY PLOT
ggplot(d, aes(sample = age)) +
  stat_qq(fill = "black", color = "grey", size = 3) +
  stat_density(aes(height), color = "yellow",size = 2 ) +
  labs(title = "Q-Q Density Plot of Age", x = "Theoretical Quantiles", 
       y = "Density") +
  theme_minimal()

#MULTIVARIATE NON GRAPHICAL EDA
#CROSS TABULATION OF CARDIO DATASET
xtabs(~smoke + gender, data = d)

#UNIVARIATE STATISTICS BY CATEGORIES
aggregate(age ~ gender, d, function(x) c(mean = mean(x), sd = sd(x)))

#MULTI VARIATE GRAPHICAL EDA
#BOXPLOT
library(ggplot2)
ggplot(d, aes(x = factor(gender), y = age, fill = factor(gender))) +
  geom_boxplot(color = "black", size = 0.5, alpha = 0.8, outlier.shape = NA) +
  labs(title = "Boxplot of Age by Gender",
       x = "Gender",
       y = "Age",
       fill = "Gender") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

#SCATTERPLOTS
ggplot(d, aes(x = age, y = ap_hi)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age", y = "Systolic Blood Pressure") +
  theme_grey()

## EDA -2 :
# EXPLORING DATASET
mean_age <- mean(d$age)
sd_age <- sd(d$age)
median_age <- median(d$age)
q1_age <- quantile(d$age, 0.25)
q3_age <- quantile(d$age, 0.75)

# Print the results
cat("Mean age: ", mean_age, "\n")
cat("Standard deviation of age: ", sd_age, "\n")
cat("Median age: ", median_age, "\n")
cat("1st quartile of age: ", q1_age, "\n")
cat("3rd quartile of age: ", q3_age, "\n")

#SCALE ESTIMATION USING BOXPLOT
ggplot(d, aes(x=1, y=age)) +
  geom_boxplot() +
  labs(x="", y="Age") +
  scale_y_continuous(limits = c(0, 110), 
                     expand = c(0, 0), breaks=seq(0, 110, 10))

#SUNFLOWERPLOT
library(graphics)
sunflowerplot(d$age, d$smoke, 
              main = "Sunflower Plot of Age vs. Smoke", 
              xlab = "Age", 
              ylab = "Smoke")

#BEANPLOT
library(beanplot)
d$gender_age <- paste0(d$gender, "-", 
                       cut(d$age, breaks = c(0, 45, 55, 65, Inf)))
beanplot(height ~ gender_age, data = d, 
         col = c("#00AFBB", "#E7B800"), border = NA, 
         side = "both", what = c(0, 1, 0, 0), ylab = "Height (cm)",
         xlab = "Gender-Age Group",
         ylim = c(140, 200), 
         main = "Distribution of Height by Gender-Age Group")

legend("topright", legend = c("Female", "Male"), 
       fill = c("#00AFBB", "#E7B800"), border = NA)

data_summary <- d %>% group_by(gender_age) %>% 
  summarize(mean_height = mean(height))
ggplot(data_summary, aes(x = gender_age, y = mean_height, group = 1)) + 
  geom_line(color = "red", size = 1.5) + 
  geom_point(color = "red", size = 3)






