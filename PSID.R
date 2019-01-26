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

#Madhawi
psid = read.csv("PSID.csv", header = TRUE)
psid[,4:7]
avgEarnings = aggregate(earnings ~ age, psid, mean)


kc= kmeans(psid[4],5)
kc

plot(avgEarnings, col=kc$cluster)



plot(avgEarnings$age,avgEarnings$earnings)
hist(avgEarnings$age)
hist(avgEarnings$earnings)
avgEarnings
cov(psid$age, psid$earnings)


