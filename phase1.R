# ### **Importing Libraries**

install.packages("plyr")
install.packages("e1071")
install.packages("psych")
install.packages("dplyr")
install.packages("hexbin")
install.packages("ggExtra")
install.packages("GGally")
install.packages("ggcorrplot")
install.packages("scatterplot3d")


library(ggplot2)
library(plyr)
library(e1071)
library(psych)
library(dplyr)
library(hexbin)
library(ggExtra)
library(GGally)
library(ggcorrplot)
library(scatterplot3d)


# ### __Importing Data__

healthCare <- read.csv("/content/HealthCare.csv")


describe(healthCare)


summary(healthCare)


# ### __Cleaning Data__


healthCare$hypertension <- mapvalues(healthCare$hypertension,
                                     from = c(0, 1),
                                     to = c("No", "Yes"))


healthCare$heart_disease <- mapvalues(healthCare$heart_disease,
                                      from = c(0, 1),
                                      to = c("No", "Yes"))


healthCare$stroke <- mapvalues(healthCare$stroke,
                               from = c(0, 1),
                               to = c("No", "Yes"))



healthCare[(healthCare$gender=="Other"),]


healthCare <- healthCare[!(healthCare$gender=="Other"),]


# ### __Question 0__

# #### __Part B__

str(healthCare)

# #### __Part C__

colMeans(is.na(healthCare))

healthCare[c("bmi")][is.na(healthCare[c("bmi")])] <- mean(healthCare$bmi, na.rm=TRUE)

healthCare[c("health_bills")][is.na(healthCare[
  c("health_bills")])] <- median(healthCare$health_bills, na.rm=TRUE)

# #### __Part D__


summary(healthCare)


# ### __Question 1__

# #### __Part A__



bmi <- healthCare$bmi
n <- 1000
binwidth <- ceiling(log2(length(bmi)))
bmiDensHist <- ggplot(healthCare, aes(x=bmi)) 
bmiDensHist <- bmiDensHist + geom_histogram(aes(y=..count..), fill ="plum2", colour="black",
                                            binwidth = binwidth) 
bmiDensHist <- bmiDensHist + geom_line(aes(y = ..density.. * n * binwidth), color = "seagreen4", 
                                       size = 1, stat = 'density')
bmiDensHist <- bmiDensHist + xlab("BMI")
bmiDensHist <- bmiDensHist + ylab("count")
bmiDensHist <- bmiDensHist + ggtitle("Histogram of BMI")
bmiDensHist <- bmiDensHist + theme_classic()
bmiDensHist <- bmiDensHist + theme(plot.title = element_text(hjust = 0.5))
bmiDensHist


# #### __Part B__


bmiQQ <- ggplot(healthCare, aes(sample=bmi))
bmiQQ <- bmiQQ + geom_qq(col= "chocolate1")
bmiQQ <- bmiQQ + ggtitle("QQ-Plot of BMI")
bmiQQ <- bmiQQ + xlab("") + ylab("")
bmiQQ <- bmiQQ + theme_classic()
bmiQQ <- bmiQQ + theme(plot.title = element_text(hjust = 0.5))
bmiQQ <- bmiQQ + geom_qq_line(geom = "path", position = "identity")
bmiQQ


# #### __Part C__

skewness(bmi)


# #### __Part D__


str(boxplot.stats(bmi))


outliers <- c(bmi[bmi < 11.3], bmi[bmi > 46.2])
outliers


# #### __Part E__

mean(bmi)


median(bmi)


var(bmi)


sd(bmi)


# #### __Part F__


bmiStatistics <- healthCare %>% summarize(mean = mean(bmi), median = median(bmi))

bmiDensity <- ggplot(healthCare, aes(x=bmi))
bmiDensity <- bmiDensity + geom_density(color="black", fill="palegreen1")
bmiDensity <- bmiDensity + geom_vline(data = bmiStatistics, aes(xintercept = mean, color= "mean"), 
                                      linetype = "dashed")
bmiDensity <- bmiDensity + geom_vline(data = bmiStatistics, aes(xintercept = median, color= "median"), 
                                      linetype = "dashed")
bmiDensity <- bmiDensity + scale_color_manual(name = "statistics", 
                                              values = c(mean = "darkorchid", median = "darkorange2"))  
bmiDensity <- bmiDensity + xlab("BMI")
bmiDensity <- bmiDensity + ggtitle("Density Plot of BMI")
bmiDensity <- bmiDensity + theme_classic()
bmiDensity <- bmiDensity + theme(plot.title = element_text(hjust = 0.5))
bmiDensity


# #### __Part G__


bmiGroups <- c(length(bmi[bmi <= 18.5]),
               length(bmi[bmi > 18.5 & bmi < 25]),
               length(bmi[bmi >= 25 & bmi < 30]),
               length(bmi[bmi > 30]))

bmiPercents <- round(100 * bmiGroups / sum(bmiGroups), 1)
bmiLabels = c("Underweight",
              "Normal weight",
              "Overweight",
              "Obesity")

data <- data.frame(group=paste0(bmiLabels, " : ", bmiPercents, "%"), value=bmiGroups)

bmiPie <- ggplot(data, aes(x="", y=value, fill=group)) 
bmiPie <- bmiPie + scale_fill_manual(values=c("yellow2", "deeppink", "limegreen", "darkorchid2"))
bmiPie <- bmiPie + geom_bar(stat="identity", width=1)
bmiPie <- bmiPie + coord_polar("y", start=0)
bmiPie <- bmiPie + ggtitle("Pie Chart of BMI")
bmiPie <- bmiPie + theme(plot.title = element_text(hjust = 0.5))
bmiPie


# #### __Part H__

bmiBox <- ggplot(healthCare, aes(x = bmi)) 
bmiBox <- bmiBox + geom_boxplot(col="purple", fill="palegreen2")
bmiBox <- bmiBox + coord_flip()
bmiBox <- bmiBox + labs(x="BMI")
bmiBox <- bmiBox + ggtitle("Box Plot of BMI")
bmiBox <- bmiBox + theme_classic()
bmiBox <- bmiBox + theme(plot.title = element_text(hjust = 0.5))
bmiBox


str(boxplot.stats(bmi))


IQR(bmi)


# ### __Question 2__

# #### __Part A__


smokingStatus <- healthCare$smoking_status
smokingStatusTable <- table(smokingStatus)
smokingStatusTable <- data.frame(smokingStatusTable)
smokingStatusTable$Percentage <- smokingStatusTable$Freq / sum(smokingStatusTable$Freq) * 100
smokingStatusTable


# #### __Part B__


colors <- c("hotpink1", "orange", "seagreen3", "turquoise2")

smokingStatusBar <- ggplot(data=healthCare, aes(x=smoking_status))
smokingStatusBar <- smokingStatusBar + geom_bar(fill = colors)
smokingStatusBar <- smokingStatusBar + geom_text(aes(label = scales::percent(..count../sum(..count..)), y= ..count.. ), 
                                                 stat= "count", vjust = 5)
smokingStatusBar <- smokingStatusBar + ggtitle("Bar Plot of Smoking Status")
smokingStatusBar <- smokingStatusBar + theme_minimal()
smokingStatusBar <- smokingStatusBar + labs(x="Smoking Status")
smokingStatusBar <- smokingStatusBar + theme(plot.title = element_text(hjust = 0.5))
smokingStatusBar


# #### __Part C__

colors <- c("seagreen3", "hotpink1", "turquoise2", "orange")

sortedSmokingStatus <- within(healthCare,
                              smoking_status <- factor(
                                smoking_status,
                                levels=names(sort(table(smoking_status),
                                                  decreasing=FALSE))))

SmokingStatusBarH <- ggplot(data=sortedSmokingStatus, aes(smoking_status))
SmokingStatusBarH <- SmokingStatusBarH + geom_bar(fill=colors)
SmokingStatusBarH <- SmokingStatusBarH + theme_minimal()
SmokingStatusBarH <- SmokingStatusBarH + ggtitle("Horizontal Bar Plot of Smoking Status")
SmokingStatusBarH <- SmokingStatusBarH + labs(x="Smoking Status")
SmokingStatusBarH <- SmokingStatusBarH + theme(plot.title = element_text(hjust = 0.5))
SmokingStatusBarH <- SmokingStatusBarH + coord_flip()
SmokingStatusBarH


# #### __Part D__


smokingStatusViolin <- ggplot(healthCare, aes(x=smoking_status, y=health_bills))
smokingStatusViolin <- smokingStatusViolin + geom_violin(fill = "deeppink4")
smokingStatusViolin <- smokingStatusViolin + ggtitle("Violin Plot of Smoking Status")
smokingStatusViolin <- smokingStatusViolin + labs(x="Smoking Status", y="Health Bills")
smokingStatusViolin <- smokingStatusViolin + theme(plot.title = element_text(hjust = 0.5))
smokingStatusViolin


# ### __Question 3__

# #### __Part A__

# #### __Part B__


scatter <- ggplot(healthCare, aes(x=bmi, y=health_bills)) 
scatter <- scatter + geom_point(color="magenta3")
scatter <- scatter + theme_classic()
scatter <- scatter + ggtitle("Scatter Plot of BMI and Health Bills")
scatter <- scatter + labs(x="BMI", y="Health Bills")
scatter <- scatter + theme(plot.title = element_text(hjust = 0.5))
scatter

# #### __Part C__


healthBills <- healthCare$health_bills
cor(bmi, healthBills)


# #### __Part D__


# #### __Part E__

cor.test(formula = ~ bmi + health_bills, data = healthCare)


# #### __Part F__


scatter <- ggplot(healthCare, aes(x=bmi, y=health_bills, shape=smoking_status, color=smoking_status)) 
scatter <- scatter + geom_point()
scatter <- scatter + theme_classic()
scatter <- scatter + ggtitle("Scatter Plot of BMI and Health Bills")
scatter <- scatter + labs(x="BMI", y="Health Bills")
scatter <- scatter + theme(plot.title = element_text(hjust = 0.5))
scatter


# #### __Part G__


hexBin <- ggplot(healthCare, aes(bmi, healthBills))
hexBin <- hexBin + theme_classic()
hexBin <- hexBin + scale_fill_gradient(low =  "palevioletred1", high = "palevioletred4") 
hexBin <- hexBin + ggtitle("Hex Bin Plot of BMI and Health Bills")
hexBin <- hexBin + labs(x="BMI", y="Health Bills")
hexBin <- hexBin + theme(plot.title = element_text(hjust = 0.5))
hexBin <- hexBin + geom_point(col="transparent")




hexBin2 <- hexBin + geom_hex(bins=2)
hexBin2 <- hexBin2 + geom_smooth(col="purple") 
ggMarginal(hexBin2, type = "histogram")




hexBin5 <- hexBin + geom_hex(bins=6)
hexBin5 <- hexBin5 + geom_smooth(col="purple") 
ggMarginal(hexBin5, type = "histogram")




hexBin8 <- hexBin + geom_hex(bins=12)
hexBin8 <- hexBin8 + geom_smooth(col="purple") 
ggMarginal(hexBin8, type = "histogram")




hexBin8 <- hexBin + geom_hex(bins=18)
hexBin8 <- hexBin8 + geom_smooth(col="purple") 
ggMarginal(hexBin8, type = "histogram")


# #### __Part H__


density2D <- ggplot(healthCare, aes(x=bmi, y=healthBills)) 
density2D <- density2D + stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
density2D <- density2D + theme_classic()
density2D <- density2D + scale_fill_gradient(low =  "orchid1", high = "orchid4") 
density2D <- density2D + ggtitle("2D Density Plot of BMI and Health Bills")
density2D <- density2D + labs(x="BMI", y="Health Bills")
density2D <- density2D + theme(plot.title = element_text(hjust = 0.5))
density2D


# ### __Question 4__


# #### __Part A__


ggpairs(healthCare[c("age", "avg_glucose_level", "bmi", "health_bills")],
        title="Bivariate Relations between the Variables ", 
        lower=list(coreSize=10, continuous = wrap("points", color= "orangered")),
        upper=list(coreSize=10, continuous = wrap("density", color= "royalblue1")))


# #### __Part B__


numericVars <- healthCare[c("age", "avg_glucose_level", "bmi", "health_bills")]
corr <- cor(numericVars)
p.mat <- cor_pmat(numericVars)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, p.mat = p.mat, sig.level = 0.05)


p.mat


# #### __Part C__

colors <- c("hotpink1", "orange", "seagreen3", "turquoise2")
smokingFactor <- factor(healthCare$smoking_status)
colors <- colors[as.numeric(smokingFactor)]
scatter3D <- scatterplot3d(healthCare[c("bmi", "age", "health_bills")],
                           pch = 16, color=colors, type="h")
legend("right", legend = levels(smokingFactor), 
       col = c("hotpink1", "orange", "seagreen3", "turquoise2"), pch = 16)



# ### __Question 5__

# #### __Part A__


tableColors <- c("red", "red","red","red")
contingencyTable <- table(healthCare$gender, healthCare$smoking_status)
addmargins(contingencyTable)


# #### __Part B__


barData <- healthCare %>% group_by(smoking_status, gender) %>% summarise(Count=n())

groupedBar <- ggplot(barData, aes(fill=gender, y=Count, x=smoking_status)) 
groupedBar <- groupedBar + geom_bar(position="dodge", stat="identity") 
groupedBar <- groupedBar + geom_text(aes(label=Count), position = position_dodge(width = 0.9), 
                                     vjust = -0.25, size = 4)
groupedBar <- groupedBar + ggtitle("Grouped Bar Plot of Gender & Smoking") 
groupedBar <- groupedBar + xlab("Smoking Status")
groupedBar <- groupedBar + theme(plot.title = element_text(hjust = 0.5))       
groupedBar        


# #### __Part C__


barData <- healthCare %>% group_by(smoking_status, gender) %>% summarise(Count=n())

segmentedBar <- ggplot(barData, aes(fill=gender, y=Count, x=smoking_status)) 
segmentedBar <- segmentedBar + geom_bar(stat="identity") 
segmentedBar <- segmentedBar + geom_text(size = 3, aes(label=Count), 
                                         position = position_stack(vjust = 0.5))
segmentedBar <- segmentedBar + ggtitle("Segmented Bar Plot of Gender & Smoking") 
segmentedBar <- segmentedBar + xlab("Smoking Status")
segmentedBar <- segmentedBar + theme(plot.title = element_text(hjust = 0.5))       
segmentedBar 


# #### __Part D__


mosaicData <- healthCare %>%
  group_by(smoking_status, gender) %>%
  summarise(count = n()) %>%
  mutate(smoking_status.count = sum(count),
         prop = count/sum(count)) %>%
  ungroup()

mosaicPlot <- ggplot(mosaicData, aes(x = smoking_status, y = prop, 
                                     width = smoking_status.count, fill = gender))
mosaicPlot <- mosaicPlot + geom_bar(stat = "identity")
mosaicPlot <- mosaicPlot + geom_text(aes(label = scales::percent(prop)), 
                                     position = position_stack(vjust = 0.5))
mosaicPlot <- mosaicPlot + facet_grid(~smoking_status, scales = "free_x", space = "free_x")
mosaicPlot <- mosaicPlot + ggtitle("Mosaic Plot of Gender & Smoking") 
mosaicPlot <- mosaicPlot + xlab("Smoking Status")
mosaicPlot <- mosaicPlot + ylab("Proportion")
mosaicPlot <- mosaicPlot + theme(plot.title = element_text(hjust = 0.5),
                                 strip.background = element_blank(),
                                 strip.text.x = element_blank()) 

mosaicPlot


# ### __Question 6__

# #### __Part A__


billPopulation <- healthCare$health_bills
sampleSize <- 100
set.seed(2)
billSampleIndex <- sample(1:nrow(healthCare), sampleSize)
billSample <- healthCare[c("health_bills")][billSampleIndex, ]
sampleMean <- mean(billSample)
sampleMean


sampleMean <- mean(billSample)
populationSd <- sd(billPopulation)

SE <- populationSd / sqrt(sampleSize)

ME <- qnorm(0.975) * SE

low <- sampleMean - ME
up <- sampleMean + ME

print(paste("the 95% CI=(",low,"up to ",up,")"), quote = FALSE)


# #### __Part B__


# #### __Part C__


billData <- healthCare %>%
  summarize(mean = mean(health_bills),
            up = up,
            low = low)

binwidth <- 600
billHist <- ggplot(healthCare, aes(x=health_bills)) 
billHist <- billHist + geom_histogram(fill ="mistyrose", colour="black", binwidth = binwidth) 
billHist <- billHist + geom_vline(data = billData, aes(xintercept = low, color = "low"))
billHist <- billHist + geom_vline(data = billData, aes(xintercept = up, color = "up"))
billHist <- billHist + geom_vline(data = billData, aes(xintercept = mean, , color = "mean"),
                                  linetype = "dashed")
billHist <- billHist + scale_color_manual(name = "statistics", 
                                          values = c(mean = "darkorchid", low = "darkorange2", up = "mediumvioletred"))                                                           
billHist <- billHist + xlab("Health Bills")
billHist <- billHist + ggtitle("Histogram of Health Bills")
billHist <- billHist + theme_classic()
billHist <- billHist + theme(plot.title = element_text(hjust = 0.5))
billHist


# #### __Part D__


mu_0 <- 3000
#test statistics 
z <- (sampleMean - mu_0) / SE
p_value <- pnorm(z, lower.tail = FALSE)
p_value

# #### __Part E__


# #### __Part F__


actualMean <- mean(billPopulation)
alpha <- 0.05
zAlpha <- qnorm(alpha, lower.tail = FALSE)
zStatistics <- ((SE * zAlpha + mu_0) - actualMean) / SE
power <- pnorm(zStatistics, lower.tail = FALSE)
beta <- 1 - power
beta


# #### __Part G__
# 

power


# ### __Question 7__

# #### __Part A__

sampleSize = 25
set.seed(123)
index <- sample(1:nrow(healthCare), sampleSize)
pairSample = healthCare[c("bmi", "age")][index, ]


# ##### __Part a__

# ##### __Part b__


diff <- pairSample$age - pairSample$bmi
t.test(diff, mu = 0)



# #### __Part B__


sampleSize = 100
set.seed(123)
ageSampleIndex <- sample(1:nrow(healthCare), sampleSize)
bmiSampleIndex <- sample(1:nrow(healthCare), sampleSize)

ageSample <- healthCare[c("age")][ageSampleIndex, ]
bmiSample <- healthCare[c("bmi")][bmiSampleIndex, ]



t.test(ageSample, bmiSample)


# ### __Question 8__


# #### __Part A__


bmi = healthCare$bmi
CI = 0.95
sampleSize = 100
numBootSamples = 1000
set.seed(4)
boot <- replicate(numBootSamples, sample(bmi, size = sampleSize))
means <- sort(apply(X = boot, MARGIN = 2, FUN = mean))
lowIndex <- (1 - CI)/2 * numBootSamples
upIndex <- numBootSamples - (1 - CI)/2 * numBootSamples
low <- means[lowIndex]
up <- means[upIndex]
print(paste("the 95% CI=(",low,"up to ",up,")"), quote = FALSE)


# #### __Part B__


sampleSize = 20
df = numBootSamples - 1
set.seed(4)
mySample <- sample(bmi, size = sampleSize, replace = TRUE)
tStar <- qt(0.975, df)
SE <- sd(means) 
ME <- tStar * SE
low <- mean(mySample) - ME
up <- mean(mySample) + ME
print(paste("the 95% CI=(",low,"up to ",up,")"), quote = FALSE)


# #### __Part C__


meansDf <- data.frame(mean=means)
qqBMI <- ggplot(meansDf, aes(sample=mean))
qqBMI <- qqBMI + stat_qq(col="hotpink") + geom_qq_line()
qqBMI <- qqBMI + labs(x="BMI", title="QQ Plot of BMI")
qqBMI <- qqBMI + theme_classic()
qqBMI


# ### __Question 9__


private <- healthCare[healthCare$work_type == "Private",]$health_bills
selfEmployed <- healthCare[healthCare$work_type == "Self-employed",]$health_bills
govtJob <- healthCare[healthCare$work_type == "Govt_job",]$health_bills
children <- healthCare[healthCare$work_type == "children",]$health_bills
neverWorked <- healthCare[healthCare$work_type == "Never_worked",]$health_bills


y <- c(private, selfEmployed, govtJob, children, neverWorked)
n <- c(length(private), length(selfEmployed), length(govtJob), length(children), 
       length(neverWorked))
group = rep(1:5, n)
tmpfn = function(x) c(sum = sum(x), mean = mean(x), var = var(x), n = length(x))
tapply(y, group, tmpfn)
data = data.frame(y = y, group = factor(group))
fit = lm(y ~ group, data)
anova(fit)


# #### __Bonus__

colors <- c("hotpink1", "mediumorchid1", "goldenrod3", "turquoise2", "lightsalmon1")

boxPlot <- ggplot(healthCare, aes(x = work_type, y = health_bills)) 
boxPlot <- boxPlot + geom_boxplot(fill = colors)
boxPlot <- boxPlot + ggtitle("Boxplots of Health Bills")
boxPlot <- boxPlot + xlab("Work Type") 
boxPlot <- boxPlot + ylab("Health Bills") 
boxPlot <- boxPlot + theme(plot.title = element_text(hjust = 0.5))
boxPlot


t.test(private, selfEmployed, data = y)


t.test(private, govtJob, data = y)


t.test(private, children, data = y)


t.test(private, neverWorked, data = y)


t.test(selfEmployed, govtJob, data = y)



t.test(selfEmployed, children, data = y)



t.test(selfEmployed, neverWorked, data = y)


t.test(govtJob, children, data = y)



t.test(govtJob, neverWorked, data = y)



t.test(children, neverWorked, data = y)


