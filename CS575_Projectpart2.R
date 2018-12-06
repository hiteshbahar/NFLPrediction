bigdirtydata <- read.csv("bigdirtydata.csv", header = TRUE)

library(ggplot2)
ggplot(bigdirtydata, aes(x=pos, y=rushing_yds)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Rushing Yards for Positions") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(bigdirtydata, aes(x = rushing_yds, y = pos))+
  geom_bar(stat="identity")
ggplot(bigdirtydata, aes(x = reorder(bigdirtydata$team, -bigdirtydata$passing_yds), y = bigdirtydata$passing_yds))+
  geom_bar(stat="identity")+
  labs(title="7 Season Rushing Yards",x ="Team", y = "Rushing Yards")
megadata <- megadata[order(megadata[,1]),]
agg <- aggregate(megadata$fumrate, by=list(megadata$X), FUN=mean)[2]
which(agg == max(agg))
aggsack <- aggregate(megadata$dsack, by=list(megadata$X), FUN=mean)[2]
maxsack <- max(aggsack)
which(maxsack == max(aggsack))

plot(megadata[,1],megadata$points, xlab = "NFL Teams", ylab = "Points Scored per Season", cex.axis = 0.5, cex.label = 0.5)
ggplot(megadata, aes(x=X, y=points))+
  geom_boxplot()
plot(megadata[,1],megadata$dsack, xlab = "NFL Teams", ylab = "Sacks per Season", cex.axis = 0.5, cex.label = 0.5)
megadata$fumrate <- megadata$fumrec / megadata$fumtot
plot(megadata[,1],megadata$fumrate, xlab = "NFL Teams", ylab = "Fumble/Recovery Rate", cex.axis = 0.5, cex.label = 0.5)







nfl2010 <- read.csv("NFL2010.csv", header = TRUE)
nfl2011 <- read.csv("NFL2011.csv", header = TRUE)
nfl2012 <- read.csv("NFL2012.csv", header = TRUE)
nfl2013 <- read.csv("NFL2013.csv", header = TRUE)
nfl2014 <- read.csv("NFL2014.csv", header = TRUE)
nfl2015 <- read.csv("NFL2015.csv", header = TRUE)
nfl2016 <- read.csv("NFL2016.csv", header = TRUE)

minordata <- rbind(nfl2014, nfl2015)
megadata <- rbind(nfl2010, nfl2011, nfl2012, nfl2013, nfl2014, nfl2015)
megadata <- megadata[order(megadata$X),] 



fake <- megadata[1:6,]





teamfit2016 <- numeric(32)
teamnew2016 <- numeric(32)
teamfitpred <- numeric(32)
teamnewpred <- numeric(32)
for (i in 1:32) {
  teamfit <- lm(points ~ dint + dsack + fumtot + fumrec + fgperc + ydsperrush + ydsperpass, megadata[(1+(i-1)*6):6*i,])
  teamnew <- lm(points ~ dint + dsack + fumtot + fumrec + fgperc + ydsperrush + ydsperpass, minordata[(1+(i-1)*2):2*i,])
  teamfit2016[i] <- predict(teamfit, nfl2016[i,])
  teamnew2016[i] <- predict(teamnew, nfl2016[i,])
  teamfitpred[i] <- predict(teamfit)
  teamnewpred[i] <- predict(teamnew)
}
library(MASS)

fullfit <- lm(points ~ dint + dsack + fumtot + fumrec + fgperc + ydsperrush + ydsperpass, megadata)
lfit <- loess(points ~ dint + dsack + fgperc + ydsperpass, data = megadata, span = 0.25)

fullfit2016 <- predict(fullfit, nfl2016)
lfit2016 <- predict(lfit, nfl2016)

teamfit2016 <- array(teamfit2016)
fullfit2016 <- array(fullfit2016)
lfit2016 <- array(lfit2016)
teamnew2016 <- array(teamnew2016)
actual2016 <- array(nfl2016$points)

teamfiterr <- sum(abs(teamfit2016 - actual2016), na.rmn = TRUE)
fullfiterr <- sum(abs(fullfit2016 - actual2016), na.rm = TRUE)
lfiterr <- sum(abs(lfit2016 - actual2016), na.rm = TRUE)
teamnewerr <- sum(abs(teamnew2016 - actual2016), na.rm = TRUE)

teamfiterr
fullfiterr
lfiterr
teamnewerr

ffpred <- predict(fullfit)
fullfitpred <- numeric(32)
for (i in 1:32){
  fullfitpred[i] <- mean(ffpred[(1+(i-1)*6):6*i], na.rm = TRUE)
}

lfpred <- predict(lfit)
lfitpred <- numeric(32)
for (i in 1:32){
  lfitpred[i] <- mean(lfpred[(1+(i-1)*6):6*i], na.rm = TRUE)
}
fullfitpred <- as.data.frame(fullfitpred)
fullfitpred$teams <- levels(megadata[,1])

lfitpred <- as.data.frame(lfitpred)
lfitpred$teams <- levels(megadata[,1])

teamfitpred <- as.data.frame(teamfitpred)
teamfitpred$teams <- levels(megadata[,1])

teamnewpred <- as.data.frame(teamnewpred)
teamnewpred$teams <- levels(megadata[,1])

winlfitpred <- matrix(nrow = 32, ncol = 32, "NA")
colnames(winlfitpred) <- levels(megadata[,1])
rownames(winlfitpred) <- levels(megadata[,1])
for (i in 1:32){
  for (j in 1:32){
    k = max(lfitpred$lfitpred[i],lfitpred$lfitpred[j])
    l = which(lfitpred$lfitpred == k)
    winlfitpred[i,j] = lfitpred$teams[l]
   }
}

winfullfitpred <- matrix(nrow = 32, ncol = 32, "NA")
colnames(winfullfitpred) <- levels(megadata[,1])
rownames(winfullfitpred) <- levels(megadata[,1])
for (i in 1:32){
  for (j in 1:32){
    k = max(fullfitpred$fullfitpred[i],fullfitpred$fullfitpred[j])
    l = which(fullfitpred$fullfitpred == k)
    winfullfitpred[i,j] = fullfitpred$teams[l]
  }
}

winteamfitpred <- matrix(nrow = 32, ncol = 32, "NA")
colnames(winteamfitpred) <- levels(megadata[,1])
rownames(winteamfitpred) <- levels(megadata[,1])
for (i in 1:32){
  for (j in 1:32){
    k = max(teamfitpred$teamfitpred[i],teamfitpred$teamfitpred[j])
    l = which(teamfitpred$teamfitpred == k)
    winteamfitpred[i,j] = teamfitpred$teams[l]
  }
}
team1 <- teamnewpred

winteamnewpred <- matrix(nrow = 32, ncol = 32, "NA")
colnames(winteamnewpred) <- levels(minordata[,1])
rownames(winteamnewpred) <- levels(minordata[,1])
for (i in 1:32){
  for (j in 1:32){
    k = max(team1$teamnewpred[i],team1$teamnewpred[j])
    l = which(team1$teamnewpred == k)
    winteamnewpred[i,j] = team1$teams[l]
  }
}



winfullfitpred[upper.tri(winfullfitpred, diag = FALSE)] <- ""
winteamfitpred[upper.tri(winteamfitpred, diag = FALSE)] <- ""
winlfitpred[upper.tri(winlfitpred, diag = FALSE)] <- ""
winteamnewpred[upper.tri(winteamnewpred, diag = FALSE)] <- ""

write.csv(winfullfitpred, "winfullfitpred.csv")
write.csv(winteamfitpred, "winteamfitpred.csv")
write.csv(winlfitpred, "winlfitpred.csv")

#https://www.fantasyfootballnerd.com/nfl-picks/expert/9/seth-wickersham

