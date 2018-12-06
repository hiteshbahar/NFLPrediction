data <- read.csv("season2016.csv")
library(dplyr)

data <- data[order(data$team),] 

datanew <- subset(data)#, select = c(team, defense_int, defense_sk, fumbles_tot, fumbles_rcv, kicking_fga, kicking_fgm, kicking_xpa, kicking_xpmade, passing_att, passing_yds, receiving_tds, rushing_tds, rushing_att, rushing_yds))
datanew[is.na(datanew)] <- 0
teams <- data$team
nam = levels(teams)
nam <- as.matrix(nam)

try <- split(datanew, datanew$team)

head(try[1])
cake <- try[1]
  
n = 15
  
mat1 <- integer(n)
mat1[1] <- "ARI"
for (i in 2:n){
  mat1[i] <- colSums(try$ARI[i])
}

mat2 <- integer(n)
mat2[1] <- "ATL"
for (i in 2:n){
  mat2[i] <- colSums(try$ATL[i])
}

mat3 <- integer(n)
mat3[1] <- "BAL"
for (i in 2:n){
  mat3[i] <- colSums(try$BAL[i])
}

mat4 <- integer(n)
mat4[1] <- "BUF"
for (i in 2:n){
  mat4[i] <- colSums(try$BUF[i])
}

mat5 <- integer(n)
mat5[1] <- "CAR"
for (i in 2:n){
  mat5[i] <- colSums(try$CAR[i])
}

mat6 <- integer(n)
mat6[1] <- "CHI"
for (i in 2:n){
  mat6[i] <- colSums(try$CHI[i])
}

mat7 <- integer(n)
mat7[1] <- "CIN"
for (i in 2:n){
  mat7[i] <- colSums(try$CIN[i])
}

mat8 <- integer(n)
mat8[1] <- "CLE"
for (i in 2:n){
  mat8[i] <- colSums(try$CLE[i])
}

mat9 <- integer(n)
mat9[1] <- "DAL"
for (i in 2:n){
  mat9[i] <- colSums(try$DAL[i])
}

mat10 <- integer(n)
mat10[1] <- "DEN"
for (i in 2:n){
  mat10[i] <- colSums(try$DEN[i])
}

mat11 <- integer(n)
mat11[1] <- "DET"
for (i in 2:n){
  mat11[i] <- colSums(try$DET[i])
}

mat12 <- integer(n)
mat12[1] <- "GB"
for (i in 2:n){
  mat12[i] <- colSums(try$GB[i])
}

mat13 <- integer(n)
mat13[1] <- "HOU"
for (i in 2:n){
  mat13[i] <- colSums(try$HOU[i])
}

mat14 <- integer(n)
mat14[1] <- "IND"
for (i in 2:n){
  mat14[i] <- colSums(try$IND[i])
}

mat15 <- integer(n)
mat15[1] <- "JAC"
for (i in 2:n){
  mat15[i] <- colSums(try$JAC[i])
}

mat16 <- integer(n)
mat16[1] <- "KC"
for (i in 2:n){
  mat16[i] <- colSums(try$KC[i])
}

mat17 <- integer(n)
mat17[1] <- "MIA"
for (i in 2:n){
  mat17[i] <- colSums(try$MIA[i])
}

mat18 <- integer(n)
mat18[1] <- "MIN"
for (i in 2:n){
  mat18[i] <- colSums(try$MIN[i])
}

mat19 <- integer(n)
mat19[1] <- "NE"
for (i in 2:n){
  mat19[i] <- colSums(try$NE[i])
}

mat20 <- integer(n)
mat20[1] <- "NO"
for (i in 2:n){
  mat20[i] <- colSums(try$NO[i])
}

mat21 <- integer(n)
mat21[1] <- "NYG"
for (i in 2:n){
  mat21[i] <- colSums(try$NYG[i])
}

mat22 <- integer(n)
mat22[1] <- "NYJ"
for (i in 2:n){
  mat22[i] <- colSums(try$NYJ[i])
}

mat23 <- integer(n)
mat23[1] <- "OAK"
for (i in 2:n){
  mat23[i] <- colSums(try$OAK[i])
}

mat24 <- integer(n)
mat24[1] <- "PHI"
for (i in 2:n){
  mat24[i] <- colSums(try$PHI[i])
}

mat25 <- integer(n)
mat25[1] <- "PIT"
for (i in 2:n){
  mat25[i] <- colSums(try$PIT[i])
}

mat26 <- integer(n)
mat26[1] <- "SD"
for (i in 2:n){
  mat26[i] <- colSums(try$SD[i])
}

mat27 <- integer(n)
mat27[1] <- "SEA"
for (i in 2:n){
  mat27[i] <- colSums(try$SEA[i])
}

mat28 <- integer(n)
mat28[1] <- "SF"
for (i in 2:n){
  mat28[i] <- colSums(try$SF[i])
}

mat29 <- integer(n)
mat29[1] <- "STL"
for (i in 2:n){
  mat29[i] <- colSums(try$STL[i])
}

mat30 <- integer(n)
mat30[1] <- "TB"
for (i in 2:n){
  mat30[i] <- colSums(try$TB[i])
}

mat31 <- integer(n)
mat31[1] <- "TEN"
for (i in 2:n){
  mat31[i] <- colSums(try$TEN[i])
}

mat32 <- integer(n)
mat32[1] <- "WAS"
for (i in 2:n){
  mat32[i] <- colSums(try$WAS[i])
}



frame <- rbind(mat1, mat2, mat3, mat4, mat5, mat6, mat7, mat8, mat9, mat10, mat11, mat12, mat13, mat14, mat15, mat16, mat17, mat18, mat19, mat20, mat21, mat22, mat23, mat24, mat25, mat26, mat27, mat28, mat29, mat30, mat31, mat32)
colnames(frame) <- colnames(data)#c("team", "dint", "dsack", "fumtot", "fumrec", "fga", "fgm", "xpa", "xpmade", "passatt", "passyd", "rec_tds", "rush_td", "rush_att", "rush_yds" )

dataframe <- as.data.frame(frame)
colnames(dataframe) <- colnames(data)#c("team", "dint", "dsack", "fumtot", "fumrec", "fga", "fgm", "xpa", "xpmade", "passatt", "passyd", "rec_tds", "rush_td", "rush_att", "rush_yds" )

dataframe[,13] = as.numeric(as.character(dataframe[,13]))
dataframe[,12] = as.numeric(as.character(dataframe[,12]))
dataframe$tds = dataframe[,13]+dataframe[,12]
dataframe <- dataframe[, -c(12:13)]

dataframe$fgm = as.numeric(as.character(dataframe[,7]))
dataframe$xpmade = as.numeric(as.character(dataframe[,9]))
dataframe$points = 6*dataframe$tds + 3*dataframe$fgm + 1*dataframe$xpmade

dataframe[,7] = as.numeric(as.character(dataframe[,7]))
dataframe[,6] = as.numeric(as.character(dataframe[,6]))
dataframe$fgperc = dataframe[,7] / dataframe[,6]
dataframe <- dataframe[, -c(6:7)]

dataframe[,7] = as.numeric(as.character(dataframe[,7]))
dataframe[,6] = as.numeric(as.character(dataframe[,6]))
dataframe$xpperc = dataframe[,7] / dataframe[,6]
dataframe <- dataframe[, -c(6:7)]

dataframe[,7] = as.numeric(as.character(dataframe[,7]))
dataframe[,6] = as.numeric(as.character(dataframe[,6]))
dataframe$ydsperpass = dataframe[,7] / dataframe[,6]
dataframe <- dataframe[, -c(6:7)]

dataframe[,7] = as.numeric(as.character(dataframe[,7]))
dataframe[,6] = as.numeric(as.character(dataframe[,6]))
dataframe$ydsperrush = dataframe[,7] / dataframe[,6]
dataframe <- dataframe[, -c(6:7)]

rownames(dataframe) <- dataframe[,1]
dataframe <- dataframe[,-1]

write.csv(dataframe, file = "NFL2016.csv")

bigdata1 <- read.csv("dirtyseason2010.csv", header = TRUE)
bigdata2 <- read.csv("dirtyseason2011.csv", header = TRUE)
bigdata3 <- read.csv("dirtyseason2012.csv", header = TRUE)
bigdata4 <- read.csv("dirtyseason2013.csv", header = TRUE)
bigdata5 <- read.csv("dirtyseason2014.csv", header = TRUE)
bigdata6 <- read.csv("dirtyseason2015.csv", header = TRUE)
bigdata7 <- read.csv("dirtyseason2016.csv", header = TRUE)

bigdata <- rbind(bigdata1, bigdata2, bigdata3, bigdata4, bigdata5, bigdata6, bigdata7)
(bigdata == "NA")# / (dim(bigdata$name)*dim(bigdata$id))

bigdata <- bigdata[order(bigdata$team),] 

bigdata[is.na(bigdata)] <- 0

write.csv(bigdata, file = "bigdirtydata.csv")
