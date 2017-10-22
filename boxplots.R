#A <- read.csv("~/Desktop/matches/topRanks.csv")
A <- read.csv("~/Desktop/matches/topRanksLong.csv")
#B <- subset(A,,select=c(b30_t10,lp_t10,wLEX_t10,wSMA_t10,X5o_t10))
#C <- subset(A,,select=c(b30_t3,lp_t3,wLEX_t3,wSMA_t3,X5o_t3))


B <- subset(A,Rank==3)
C <- subset(A,Rank==10)


meth <- factor(A$Method)
rnk <- factor(A$Rank)
per <- as.numeric(A$Percent)

model <- aov(per~meth+rnk)
scheffe.test(model, "meth", alpha=0.05, console=TRUE)



meth <- factor(B$Method)
rnk <- factor(B$Rank)
per <- as.numeric(B$Percent)

model <- aov(per~meth)
scheffe.test(model, "meth", alpha=0.05, console=TRUE)

#Groups, Treatments and means
#a 	 SMA-5o       	 23.52 
#b 	 SMA-Warm     	 17.92 
#bc 	 SMA-Lex,Warm 	 17.65 
#c 	 SMA-30c      	 17.53 
#d 	 LP-C         	 8.29 


boxplot(per~meth, ylab="Percent of Officers Recieving Top 3 Preference", xlab="Solution Method")


meth <- factor(C$Method)
rnk <- factor(C$Rank)
per <- as.numeric(C$Percent)

model <- aov(per~meth)
scheffe.test(model, "meth", alpha=0.05, console=TRUE)

#Groups, Treatments and means
#a 	 SMA-5o       	 53.16 
#b 	 SMA-Warm     	 46.45 
#c 	 SMA-30c      	 45.67 
#c 	 SMA-Lex,Warm 	 45.64 
#d 	 LP-C         	 37.6 
boxplot(per~meth, ylab="Percent of Officers Recieving Top 10 Preference", xlab="Solution Method")

