#Gayan
psidData1 = read.csv("PSID.csv", header = TRUE)
psidData = psidData1[!is.na(psidData1$educatn),]
summary(psidData)

kc = kmeans(psidData[,4:8], 2)
kc

plot(psidData[,4:5], col=kc$cluster)
points(kc$centers[,1:2], col=1:2,pch=8,cex=2)

plot(psidData[,5:6], col=kc$cluster)
points(kc$centers[,2:3],col=1:2, pch=8,cex=2)

plot(psidData[,4:8], col=kc$cluster)
points(kc$centers[,3:4],col=1:2, pch=8,cex=2)

#Yasura
psid = read.csv("PSID.csv",header = TRUE)

psid

dems <- psid[psid$educatn != "NA",]
dems

aggregate(earnings~married, psid, mean)

aggregate(earnings~married, psid, sd)

aggregate(educatn~married, psid, mean)

aggregate(educatn~married, psid, sd)


library(e1071)
skewness(psid$age)

plot(psid$age,psid$educatn, col="blue", cex.axis = 0.5)

pie(table(psid$married))

hist(psid$educatn)

hist(psid$age)

#Madhavi
psidData = read.csv("PSID.csv", header = TRUE)
psid =  psidData[rowSums(is.na(psidData)) ==0,]
psid

earningAvg = aggregate(earnings ~ age, psid, mean)
earningAvg

#bar chart
barplot(earningAvg$earnings, col=c("blue"),
        main= "Average Earning by Age" ,xlab="age", ylim = c(0,20000) ,ylab="Total Avg Earning", axisnames = TRUE,cex.axis = par("cex.axis"), cex.names=par("cex.axis"), )
?barplot

#line chart
x <- earningAvg$age
y <- earningAvg$earnings
plot(x,y, type = "l", col=c("red") )

#Kmeans
kc= kmeans(psid[4],5)
kc

plot(earningAvg, col=kc$cluster)
plot(earningAvg$age,avgEarnings$earnings)
hist(earningAvg$age)
hist(earningAvg$earnings)
earningAvg
#cov(psid$age, psid$earnings)


