R

#load data
data <- read.csv("C:/data.csv")

#examine the dataset
head(data)
summary(data)
dim(data)
plot(data$x, data$y)

#linear regression
result <- lm(data$y ~ data$x)
summary(results)
hist(results$residuals, breaks = 100)
# Confidence intervals for coefficients
confint(result)
# Predict values based on regression equation
predict(result)  
# Regression diagnostics
lm.influence(result)
influence.measures(result)

#sql
library(RODBC)
conn <- odbcConnect("abc", uid="admin", pwd="sa")

data <- sqlQuery(conn, "select x, y 
						from z
						where x > 100
						")
						
class(data$x)
typeof(data$x)

#structure of a list 
v <- list(5, 10)
z <- list("x", 1, "y", 123, TRUE, v)
str(z)
str(v)

#descriptive statistics
summary(data)
x <- data$x
y <- data$y

corr(x,y)
cov(x,y)
IQR(x,y) #interquantile range
mean(x)
median(x)
range(x)
sd(x)
var(x)

install.packages("psych")
library("psych")
describe(data)

#apply sd over columns 1:3
apply(data[, c(1:3)], margin=2, fun=sd)

#visualisation
plot(data)
barplot(data) #col = heat.colors() col = rainbow()
dotchart(data)
hist(data)
curve(dnorm(x, mean = mean(data), sd = sd(data)), add = TRUE)
plot(density(data)) #continous histogram
stem(data) #stem-and-leaf plot
rug(data) #add 1-d rug plot
lines()

#barplot reorganize
data.bar <- t(data[-1])  # Removes first columns, transposes second
colnames(mean.bar) <- data[, 1]
barplot(mean.bar)

#modified scatter
plot(data)
# Linear regression line
abline(lm(data$x ~ data$x))
# locally weighted scatterplot smoothing
lines(lowess (data$x,data$y))
#or
install.packages("car")
library(car)
scatterplot(data$x ~ data$y)

# viz options
par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
par(mfrow = c(1, 3) # Num. rows/cols
legend(locator(1)) #set manually

library("RColorBrewer")
display.brewer.all()

#frequencies
data.freq <- table(data)
prop.table(data.freq) # proportions of total
margin.table(data) #marginal frequencies

#hypothesis
prop.test(145, 250) #1-sample proportions test 145/250 siginicantly > 0.5
t.test(data$x) # Default t-test (compares mean to 0)
chisq.test(data$x) #Pearson's chi-squared test, default-assume equal distribution

# Independent 2-group t-test (with defaults)
t.test(x ~ y, data = data)
# t-test with options
t.test(x ~ y,
       data = data,
       alternative = "less",  # one-tailed 
       conf.level = 0.90)  # 90% CI 

#data transformation
# z-scores
data.z <- scale(data) # M = 0, SD = 1

# Logarithmic Transformations
data.ln <- log(data)  # Natural log (base = e)
data.log10 <- log10(data)  # Common log (base = 10)
data.log2 <- log2(data)  # Binary log (base = 2)
# x.ln <- log(x + 1) # Add 1 to avoid undefined logs when X = 0

# Squaring (For negatively skewed variables, distribution may need to be recentered)
# Ranking
# Dichotomizing

#handle NAs
which(is.na(data))

#other
cbind(data$x, data$y)
rbind(data, c(1,2,3,'abc'))
data.all <- merge(data, data2, by = "Year")

#split and get stats
aggregate(data$x ~ data$y, FUN = mean) #fun applied od data$x by data$y

# Correlation matrix for data frame
cor(data)
cor.test(data$x, data$y)
# Install "Hmisc" package to get p-values for matrix
install.packages("Hmisc")
library("Hmisc")
# Need to coerce from data frame to matrix
# to get correlation matrix and p-values
rcorr(as.matrix(data))



