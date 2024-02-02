library(tidyverse)

# Dataset retrieved from facebook.
data <- read_excel("C:\\Users\\ramri\\Desktop\\KAG_conversion_data.xlsx")

#Overview of Data.
glimpse(data)

# Unique age groups.
unique(data$age)

#Duplicate Copy of data.
dataTf <- data

dataTf$age[dataTf$age == '30-34'] <- 32
dataTf$age[dataTf$age == '35-39'] <- 37
dataTf$age[dataTf$age == '40-44'] <- 42
dataTf$age[dataTf$age == '45-49'] <- 47

#convert age variable datatype to integer.
dataTf$age <- as.integer(dataTf$age)

#check age variable.
unique(dataTf$age)
str(dataTf$age)

# Make Gender -> Male (as 0), Female (as 1).
dataTf$gender[dataTf$gender == 'M'] <- 0
dataTf$gender[dataTf$gender == 'F'] <- 1
dataTf$gender <- as.integer(dataTf$gender)

# abbreviate some variable names.
dataTf <- dataTf %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion)

# Final Preprocessed data.
glimpse(dataTf)

# Exploratory Data Analysis and Visualisation.

# Heatmap Visualisation.
library(heatmaply)

dataMatNorm <- as.matrix(normalize(dataTf, method = "standardize"))
heatmap(dataMatNorm)

dataTf <- dataTf %>%
  mutate(CTR = ((Clicks / impr) * 100), CPC = Spent / Clicks)

dataTf$CTR <- round(dataTf$CTR, 4)
dataTf$CPC <- round(dataTf$CPC, 2)

glimpse(dataTf)

# Trim the dataset.
dataTfTrim <- dataTf %>%
  select(CTR, CPC, appConv, conv, impr, Spent,Clicks)
#Heatmap.
heatmap(cor(normalize(na.omit(dataTfTrim))))

data_id1178 <- data %>%
  rename(xyzCampId = xyz_campaign_id, fbCampId = fb_campaign_id, impr = Impressions,
         conv = Total_Conversion, appConv = Approved_Conversion) %>%
  filter(xyzCampId == 1178)

glimpse(data_id1178)

#Data Explorer
library(DataExplorer)
plot_missing(data_id1178)

# Bar Plot.
options(repr.plot.width = 4, repr.plot.height = 4)
plot_bar(data_id1178)

# Histogram.
options(repr.plot.width = 8, repr.plot.height = 4)
plot_histogram(data_id1178)

# Correlation map.
plot_correlation(data_id1178)

# Feature Engineering (introduce totConv, conVal, appConval)
data_id1178 <- data_id1178 %>%
  mutate(totConv = conv + appConv,
         conVal = conv * 5,
         appConVal = appConv * 100) %>%
  mutate(totConVal = conVal + appConVal) %>%
  mutate(costPerCon = round(Spent / totConv, 2),
         ROAS = round(totConVal / Spent, 2))

data_id1178 <- data_id1178 %>%
  mutate(CPM = round((Spent / impr) * 1000, 2))

# Added new variables
head(data_id1178)

# Smooth and point graph.
options(repr.plot.width=6, repr.plot.height=3)
ggplot(data_id1178, aes(Spent, totConv)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total number of conersions")
ggplot(data_id1178, aes(Spent, totConVal)) + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Total value of conversions")

# Box Plot.
options(repr.plot.height = 3, repr.plot.width = 4)
ggplot(data_id1178, aes(gender, ROAS)) + geom_boxplot() + scale_y_log10()

# Wilcox formula.
wilcox.test(ROAS ~ gender, data = data_id1178)