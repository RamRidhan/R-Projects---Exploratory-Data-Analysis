library(readxl)
#HISTOGRAM
pd <- read_excel("C:/Users/ramri/Downloads/tech_layoffs.xlsx")
hist(pd$total_layoffs, main = "HISTOGRAM LAYOFFS ANALYSIS",
     sub = "Source : Tech Company Layoffs in 2023- Google",
     xlab = "LAYOFF COUNT",
     ylab = "FREQUENCY",
     col = "darkblue",
     border = "white",
     breaks = 35,
     xlim = c(0, 1000),
     ylim = c(0, 100),
     las = 1)
abline(v = median(pd$total_layoffs), col = "red", lwd = 2)
text(median(pd$total_layoffs), 90, "Median", col = "red", pos = 4)
legend("topright", legend = c("2022", "2023"), 
       title = "YEAR", 
       fill = c("blue", "red"), bg = "white")

#BARPLOT
library(dplyr)
lc <- pd %>%
  group_by(company) %>%
  summarize(total_layoffs = sum(as.numeric(as.character(total_layoffs)), na.rm = TRUE))
lc <- lc[order(-lc$total_layoffs),]
top_10_layoffs <- head(lc, 10)
print(top_10_layoffs)

barplot(table(top_10_layoffs$company),
        as.numeric(top_10_layoffs$total_layoffs),
        names.arg = top_10_layoffs$company,
        col = "lightblue",
        border = "black",
        xlab = "Company",
        ylab = "Layoffs",
        ylim = c(0, 2),
        main = "TECH LAYOFFS",
        sub = "Souce : Forbes Tech Layoffs Analysis ")

legend("topright",
       legend = "Layoffs",
       fill = "lightblue",
       border = "black",
       bty = "n")

#boxplot
boxplot(pd$total_layoffs, 
        xlab = "Companies", 
        ylab = "Number of Layoffs", 
        col = "lightblue", 
        border = "black", 
        horizontal = TRUE, 
        notch = TRUE, 
        notchwidth = 0.5, 
        medcol = "red", 
        whisklty = 2, 
        staplelty = 1, 
        outcol = "blue", 
        outpch = 19, 
        ylim = c(0, 300))

legend("topright", 
       legend = c("Data"), 
       pch = 19, 
       col = "blue") 

text(150, 0, "Source: Forbes Research", pos = 1)

abline(h = seq(0, 350, by = 50), col = "gray", lty = 2) 
abline(v = seq(0, 300, by = 100), col = "gray", lty = 2) 

title(main = "Boxplot of Tech Layoffs", 
       col.main = "red")

#LINE GRAPH
plot(pd$total_layoffs, 
     type = "o", 
     lwd = 2, 
     pch = 19, 
     col = "blue", 
     xlab = "Time",
     ylab = "Number of Layoffs", 
     xlim = c(1, 12), 
     ylim = c(0, 400), 
     main = "Tech Layoffs in 2022", 
     sub = "Sample Data", 
     cex.main = 1.2, 
     cex.lab = 1.2,
     cex.sub = 1, 
     col.main = "red") 

legend("topright",
       legend = c("Data"), 
       pch = 19, 
       col = "blue") 

text(11, 350, "Source: Forbes Research", pos = 4) 

abline(h = seq(0, 400, by = 50), col = "gray", lty = 2) 
abline(v = seq(1, 12), col = "gray", lty = 2)

# Density Plot
plot(density(pd$total_layoffs), 
     main = "Density Plot of Tech Layoffs in 2022", 
     xlab = "Number of Layoffs", 
     ylab = "Density", 
     col = "blue",
     lwd = 2, 
     ylim = c(0, 0.015))

legend("topright", 
       legend = "Tech Layoffs", 
       lwd = 2, 
       col = "blue") 

text(200, 0.014, "Source: Forbes Research", pos = 4) 

abline(h = seq(0, 0.015, by = 0.001), col = "gray", lty = 2) 
abline(v = seq(0, 400, by = 50), col = "gray", lty = 2)

# Scatter Plot
plot(pd$impacted_workforce_percentage, pd$total_layoffs, 
     main = "Tech Layoffs in 2022",
     xlab = "Quarter", 
     ylab = "Number of Layoffs", 
     col = "blue", 
     pch = 16, 
     ylim = c(0, 400),
     xlim = c(0, 5), 
     bty = "L") 

abline(lm(pd$total_layoffs ~ pd$impacted_workforce_percentage), col = "red", lwd = 2)

legend("topright", 
       legend = "Tech Layoffs",
       col = "blue", 
       pch = 16) 

text(4.5, 350, "Source: Forbes Research", pos = 4)

#mosaic plot
library(vcd)
df <- data.frame(
  Company = c("Tech", "Non- Tech", "Tech", "Non- Tech", "Tech", "Non- Tech"),
  Layoff_status = c("More", "Less", "Less", "More", "None", "None"),
  count = c(50, 60, 20, 30, 10, 5)
)

mosaicplot(count ~ Layoff_status + Company, data = df,
           color = c("blue", "red", "green"), 
           las = 1, 
           main = "Mosaic Plot of Company and Layoff Status",
           xlab = "Layoff Status", 
           ylab = "Company", 
           cex.axis = 0.8, 
           cex.lab = 0.8, 
           cex.main = 1.2,
           font.main = 2, 
           grid = TRUE) 
legend("topright", 
       legend = c("More", "Less", "None"), 
       fill = c("blue", "red", "green"))

text(5, 250, "Source: Forbes Research", pos = 4, cex = 0.8) 

#CORRELATION PLOT
library(fivethirtyeight)
library(corrplot)
data("airline_safety")
corr_matrix <- cor(mtcars)

corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, 
         addCoef.col = "black", addCoefasPercent = TRUE,
         col = colorRampPalette(c("#FFFFFF", "#0092C3", "#D7191C"))(50))

# PIE CHART
cyl_table <- table(pd$cyl)
pie(cyl_table, 
    main="Pie Chart of Cyclic cities", 
    col=rainbow(length(cyl_table)), 
    labels=c("Total Layoff", "Impacted Workforce", "Status"))

legend("topright",legend = c("Total Layoff", "Impacted Workforce", "Status"), 
col = c("green","yellow","blue"), 
pch = 16, 
bty = "n")

