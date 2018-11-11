#HW1

WHO = read.csv("WHO.csv")

#1d
WHO$Country[which.min(WHO$LiteracyRate)]

#1e
WHO$Country[which.max(WHO$GNI)]

#1f
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)

#1g
sum(WHO$Population > 10000)

#1h
WHO.Americas = subset(WHO, Region == "Americas")
AMtopCM <-order(WHO.Americas$ChildMortality, na.last = TRUE, decreasing = TRUE)
AMTop5CM <- head(WHO.Americas[AMtopCM,],5)
AMTop5CM$Country


NBA = read.csv("Historical NBA Performance.csv")

#2a
NBA.Bulls = subset(NBA, Team == "Bulls")
BullsHighWP <- max(NBA.Bulls$Winning.Percentage)
NBA$Year[NBA$Winning.Percentage == BullsHighWP]

#2b
NBA$Team[NBA$Winning.Percentage == 0.5]


STAT = read.csv("Seasons_Stats.csv")

#3a
STAT.Year1980 <- subset(STAT,Year > 1979)
Ave3Par <- aggregate(STAT.Year1980$X3PAr~STAT.Year1980$Player+STAT.Year1980$Year, FUN = mean, na.rm = TRUE)
Ave3Par$`STAT.Year1980$Player`[Ave3Par$`STAT.Year1980$X3PAr`== max(Ave3Par$`STAT.Year1980$X3PAr`)]

#3b
AveFTr <- aggregate(STAT$FTr~STAT$Player+STAT$Year, FUN = mean, na.rm = TRUE)
AveFTr$`STAT$Player`[AveFTr$`STAT$FTr` == max(AveFTr$`STAT$FTr`)]

#3c
STAT.Lebron <- subset(STAT, Player == "LeBron James")
STAT.Lebron$Year[which.max(STAT.Lebron$PTS)]

#3d
STAT.MJ <- subset(STAT, Player == "Michael Jordan*")
STAT.MJ$Year[which.max(STAT.MJ$PTS)]

#3e
STAT.Kobe <- subset(STAT, Player == "Kobe Bryant")
STAT.Kobe$PER[which.min(STAT.Kobe$MP)]


UNIV = read.csv("National Universities Rankings.csv")

#4a
UndergradWithoutComma <- gsub("\\,","",UNIV$Undergrad.Enrollment)
UndergradNumeric <- as.numeric(UndergradWithoutComma)
RankUndergrad <- UNIV[order(UndergradNumeric,decreasing = TRUE, na.last = TRUE),]
TopUndergrad <- head(RankUndergrad,1)
TopUndergrad$Name

#4b
UNIV.TOP10 <- UNIV[order(UNIV$Rank),][1:10,]
TuitionWithNoDollar <- gsub("\\$|\\,","",UNIV.TOP10$Tuition.and.fees)
mean(as.numeric(TuitionWithNoDollar))
