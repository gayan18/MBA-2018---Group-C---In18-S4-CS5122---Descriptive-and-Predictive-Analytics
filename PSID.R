
psidRawData = read.csv("PSID.csv", header = TRUE)

# check whether their is row data
which(is.na(psidRawData), arr.ind = TRUE)

#To remove data with missing values in educatn column
psidData = psidRawData[!is.na(psidRawData$educatn),]

which(is.na(psidData), arr.ind = TRUE)

#To remove error in dataset
psidDataClean = psidData[psidData$kids < 97,]
nrow(psidDataClean)

#To remove error in dataset
psidDataClean = psidData[psidData$educatn < 95,]
nrow(psidDataClean)

#view summary
summary(psidData)


# average earnings - group by marital status
em = aggregate(earnings~married, psidDataClean, mean)

# variance of earnings - group by marital status
aggregate(earnings~married, psidDataClean, sd)


#Bar Chart - Average earning by marital status 
barplot(em$earnings, col=c("blue"),
        main= "Average Earning by Marital Status" ,xlab="Marital Status", ylim = c(0,20000) ,ylab="Total Avg Earning", axisnames = TRUE,cex.axis = par("cex.axis"), cex.names=par("cex.axis"))

?barplot

# average education - group by marital status
aggregate(educatn~married, psidDataClean, mean)

# variance of education - group by marital status
aggregate(educatn~married, psidDataClean, sd)


library(e1071)
skewness(psidDataClean$age)

plot(psidDataClean$age,psidDataClean$hours, col="blue", cex.axis = 0.5)

pie(table(psidDataClean$married))


hist(psidDataClean$educatn)

hist(psidDataClean$age)

# K- Means Clusering
kc = kmeans(psidDataClean[,4:8], 2)
kc

# age, educatn
plot(psidDataClean[,4:5], col=kc$cluster)
points(kc$centers[,1:2], col=1:2,pch=8,cex=2)

# educatn, earnings
plot(psidDataClean[,5:6], col=kc$cluster)
points(kc$centers[,2:3],col=1:2, pch=8,cex=2)

# age, educatn, earnings, hours, kids
plot(psidDataClean[,4:8], col=kc$cluster)
points(kc$centers[,3:4],col=1:2, pch=8,cex=2)


earningAvg = aggregate(earnings ~ age, psidDataClean, mean)
earningAvg

#bar chart
barplot(earningAvg$earnings, col=c("blue"),
        main= "Average Earning by Age" ,xlab="age", ylim = c(0,20000) ,ylab="Total Avg Earning", axisnames = TRUE,cex.axis = par("cex.axis"), cex.names=par("cex.axis"))


#line chart
x <- earningAvg$age
y <- earningAvg$earnings
plot(x,y, type = "l", col=c("red") )

#Kmeans
kc2= kmeans(psidDataClean[4],5)
kc2

plot(earningAvg, col=kc$cluster)
plot(earningAvg$age,avgEarnings$earnings)
hist(earningAvg$age)
hist(earningAvg$earnings)
earningAvg
