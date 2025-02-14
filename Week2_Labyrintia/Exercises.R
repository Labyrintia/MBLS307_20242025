#Excercise 2

#2.2
log(12.43)
log10(12.43)
log2(12.43)
sqrt(12.43)

#2.3
diameter <- 20
radius <- diameter/2
area_circle <- pi*radius^2

#2.4
(14*0.51)^(1/3)

#2.5
weight <- c(69, 62, 57, 59, 59, 64, 56, 66, 67, 66)

#2.6
first_five <- weight[1:5]
first_five

#2.7
height <- c(112, 102, 83, 84, 99, 90, 77, 112, 133, 112)
summary(height)
some_child <- height[c(2,3,9,10)]
shorter_child <- height <= 99
shorter_child = height[shorter_child]
shorter_child

#2.8
height_m <- height/100
bmi <- weight/sqrt(height)
bmi

#2.9
seq1 <- seq(0,1,0.1)

#2.10
seq2 <- rev(seq(1,10,0.5))

#2.11
rep(seq(1,3,1),3)
letters[rep(seq(1,7,2),each = 3)]
letters[rep(seq(1,7,2),3)]
rep(seq(1,3,1), each=3, times=2)
rep(seq(1,5,1), times = c(rev(seq(1,5,1))))
rep(c(7,2,8,1), times = c(4,3,1,5))    

#2.12
height_sorted <- sort(height)
height_sorted_dec <- sort(height, decreasing = TRUE)
height_sorted

#2.13
child_names <- c("Alfred", "Barbara", "James", "Jane", "John", "Judy", "Louise", "Mary", "Ronald", "William")
child_names

#2.14
height_ord <- order(height_sorted)
names_sort <- child_names[height_ord]
names_sort

#2.15
weight_ord <- order(weight)
weight_sort <- child_names[weight_ord]
weight_rev <- rev(weight_sort)

#2.16
mydata = c(2, 4, 1, 6, 8, 5, NA, 4, 7)
mean(mydata)
mean(mydata, na.rm = TRUE)

#2.17
rm(seq1)

#exercise 3
#3.5
whale <- read.csv("whaledata.csv")

#3.6
head(whale)
str(whale)
#101,8, chr

#3.7
summary(whale)

#3.8
whale.sub <- whale[1:10,1:4]
whale.num <- whale[,c(1,3,4)]

#3.9
whale[whale$depth >= 1200,]
whale[whale$gradient >= 200,]
whale[whale$water.noise == "low",]
whale[whale$month == "May" & whale$water.noise == "high",]
whale[whale$month == "October" & whale$water.noise == "low" & whale$gradient >= 132,]
whale[whale$latitude >= 60.0 & whale$latitude <= 61.0 & whale$longitude >= -6.0 & whale$longitude <= -4.0,]
whale[whale$water.noise != "medium",]

#3.10
whale[whale$month == "October" & whale$water.noise == "low" & whale$gradient >= median(whale$gradient),]

#3.11
whale[whale$depth >= 1200 & whale$number.whales == mean(whale$number.whales),]

#3.12
subset(whale, month == "May" & time.at.station <= 1000 & depth >= 1000)
subset(whale, month == "October" & latitude >= 61, select = c("month","latitude","longitude","number.whales"))

#3.13
whale.depth.sort <- whale[order(whale$depth),]
whale.depth.sort

#3.14
whale[order(xtfrm(whale$water.noise), whale$depth),]
whale[order(xtfrm(whale$water.noise), -whale$depth),]

#3.15
tapply(whale$number.whales, whale$water.noise, mean, na.rm = TRUE)
tapply(whale$number.whales, list(whale$water.noise, whale$month), mean, na.rm = TRUE)

#3.16

#3.17
table(whale$water.noise)
table(whale$water.noise, whale$month)
xtabs(~ water.noise + month, data = whale)

#3.18

#exercise 4
#4.4
squid <- read.csv("squid1.csv")
str(squid)
summary(squid)
squid.year <- as.factor(squid$year)
squid.month <- as.factor(squid$month)
squid.maturity.stage  <- as.factor(squid$maturity.stage)

#4.5
xtabs(~ squid.maturity.stage + month, data = squid)

#4.6
par(mfrow = c(2,2))
dotchart(squid$DML)
dotchart(squid$weight)
dotchart(squid$nid.length)
dotchart(squid$ovary.weight)

#4.7
which(squid$nid.length > 400)
squid$nid.length[11]
squid$nid.length[11] <- 43.2
dotchart(squid$nid.length)      

#4.8
par(mfrow = c(2,2))
hist(squid$DML)
hist(squid$weight)
hist(squid$eviscerate.weight)
hist(squid$ovary.weight)

#4.9
par(mfrow = c(1,1))
plot(x = squid$DML, y = squid$weight)

squid$weight.sqrt <- sqrt(squid$weight)
squid$weight.log <- log(squid$weight)

par(mfrow = c(1,2))
plot(x = squid$DML, y = squid$weight.sqrt)
plot(x = squid$DML, y = squid$weight.log)

#4.10
#install.packages("vioplot")
library(vioplot)

par(mfrow = c(1,1))
boxplot(DML ~ maturity.stage, data = squid, ylab = 'Dorsal mantle length (cm)', xlab = 'Stage of maturity')
vioplot(DML ~ maturity.stage, data = squid, ylab = 'Dorsal mantle length (cm)', xlab = 'Stage of maturity')

#4.11
#install.packages("lattice")
coplot(weight.sqrt ~ DML | maturity.stage, data = squid)

#4.12
pairs(~ DML+weight+ovary.weight+nid.length+nid.weight, data = squid)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr")
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(squid[,c(5,8,9,11,13)], diag.panel = panel.hist, upper.panel = panel.cor, lower.panel = panel.smooth)

#exercise ggplot2 
library(ggplot2)
library(tidyverse)

ggplot(mpg_df, aes(x = class,y = displ)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(aes(color = manufacturer)) + 
  ylab("Engine size (L)") + 
  xlab("Class of the car") + 
  ylim(c(0,10)) + 
  theme_classic()

#exercise pipes
mean(mpg$displ)
mpg %>% select(displ) %>% pull() %>% mean()

#exercise 5
prawns <- read.csv("prawnGR.csv")
str(prawns)

#5.2
shapiro.test(prawns$GRate[prawns$diet == "Artificial"])
shapiro.test(prawns$GRate[prawns$diet == "Natural"])
var.test(prawns$GRate ~ prawns$diet)

par(mfrow = c(1,2))
qqnorm(prawns$GRate[prawns$diet == "Artificial"])
qqline(prawns$GRate[prawns$diet == "Artificial"])
qqnorm(prawns$GRate[prawns$diet == "Natural"])
qqline(prawns$GRate[prawns$diet == "Natural"])

#5.3
t.test(GRate ~ diet, var.equal = TRUE, data = prawns)

#5.4
growth.lm <- lm(GRate ~ diet, data = prawns)
growth.lm

#5.5
anova(growth.lm)

#5.6
par(mfrow = c(2,2))
plot(growth.lm)

#5.7
gigartina <- read.csv("Gigartina.csv")
str(gigartina)
xtabs(~diatom.treat, data = gigartina)
par(mfrow = c(1,1))
boxplot(diameter ~ diatom.treat, data = gigartina)

#5.8
#There is no difference in diameter

#5.9
gigartina.lm <- lm(diameter ~ diatom.treat, data = gigartina)

#5.10
anova(gigartina.lm)

#5.11
par(mfrow = c(2,2))
plot(gigartina.lm)

#5.13
#install.packages("mosaic")
library(mosaic)
TukeyHSD(gigartina.lm)

#5.14
par(mfrow = c(1,1))
plot(TukeyHSD(gigartina.lm), cex.axis = 0.5, las = 2)

#5.15
temora <- read.csv("TemoraBR.csv")
str(temora)

#5.16
temora$Facclimitisation_temp <- as.factor(temora$acclimitisation_temp)
boxplot(beat_rate ~ Facclimitisation_temp, data = temora)

with(temora, plot(beat_rate ~ temp, xlab = "temperature", ylab = "beat rate", col = as.numeric(Facclimitisation_temp),
                  pch = as.numeric(Facclimitisation_temp)))
legend("topleft", legend = c("5", "10", "20"), pch = unique(as.numeric(temora$Facclimitisation_temp)), col = unique(as.numeric(temora$Facclimitisation_temp)))

#5.18
temora.lm <- lm(beat_rate ~ temp * Facclimitisation_temp, data = temora)

#5.19
anova(temora.lm)

#5.20
par(mfrow = c(2,2))
plot(temora.lm)

#6.1
circle.area <- function(d){
  pi*(d/2)^2
}
circle.area(3.4)
circle_dia <- seq(0,50,5)
circle.area(circle_dia)

#6.2
far.cent <- function(x){
  y <- (x-32)*5/9
  cat("Fahrenheit: ", round(x, digits = 3), "oF", "\n") 
  cat("Centigrade: ", round(y, digits = 3), "oC", "\n")
}
far.cent(10)

#6.3
norm.data <- rnorm(100,35,15)
summary.script <- function(dat){
  mean <- round(mean(dat),digits = 3)
  median <- round(median(dat), digits = 3)
  min <- round(min(dat), digits = 3)
  max <- round(max(dat), digits = 3)
  cat("mean:", mean, "\n")     
  cat("median:", median, "\n")    
  cat("range:", "from:", min, "to", max, "\n")
  hist(dat, freq = FALSE, col = "pink")
  lines(density(dat), col = "red")
}
summary.script(norm.data)

#6.4
median2 <- function(x){
  n = length(x)
  if (n %% 2 == 1)
    sort(x)[(n+1)/2]
  else {
    two <- sort(x)[(n/2) + 0:1]
    mean(two)
  }
}

median.dat <- sample(1:50,size=10,replace = FALSE)
median2(median.dat)
median(median.dat)

#6.5
ricker <- function(nzero, r, t, k = 100){
  n <- numeric(t + 1)
  n[1] <- nzero
  for (i in 1:t) {
    n[i+1] <- n[i]*exp(r*(1-n[i]/k))
  }
  Time <- 0:t
  plot(Time, n, type = "o", xlab = "t(s)", xlim = c(0,t), ylab = "Population size (N)", main = paste("r =",r))
}
ricker(nzero = 0.1, r = 0.1, t = 100)

#heatmap exercise
#install.packages("pheatmap")
library(pheatmap)

df <- data.frame(row.names = c("geneA", "geneB", "geneC", "geneD", "geneE", "geneF", "geneG", "geneH", "geneI", "geneJ"),
                 condition1 = runif(10, min = -3, max = 3),
                 condition2 = runif(10, min = -3, max = 3),
                 condition3 = runif(10, min = -3, max = 3),
                 condition4 = runif(10, min = -3, max = 3),
                 condition5 = runif(10, min = -3, max = 3))
pheatmap(df,
         cluster_cols = FALSE,
         border_color = NA)
pheatmap(df,
         cluster_cols = FALSE,
         border_color = NA,
         filename = "heatmap.png")
