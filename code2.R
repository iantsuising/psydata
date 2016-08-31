library(corrplot)
library(ggplot2)
library(scales)
library(reshape)
library(sampling)

source("totalScorId.R")
source("multiplot.R")
source("lvl.R")
source("agelvl.R")
source("age2.R")
source("edu2.R")
source("marriage.lvl.R")


A<-read.csv("A.csv")
B<-read.csv("B.csv")
C<-read.csv("C.csv")
AH<-read.csv("AH.csv")

B<-B[,-18]
B<-B[-135,]
B<-B[-1203,]
B[61,10]<-2.67
B$Interpersonal.sensitivity<-as.numeric(as.character(B$Interpersonal.sensitivity))

A<-A[A$Age!=0,]
A<-A[A$Age!=290,]

t<-totalScorId(A$Total.score)
age.lvl<-agelvl(A$Age)
A<-cbind(A,t,age.lvl)
t<-totalScorId(B$Total.score)
age.lvl<-agelvl(B$Age)
B<-cbind(B,t,age.lvl)
t<-totalScorId(C$Total.score)
age.lvl<-agelvl(C$Age)
C<-cbind(C,t,age.lvl)
t<-totalScorId(AH$Total.score)
age.lvl<-agelvl(AH$Age)
AH<-cbind(AH,t,age.lvl)

data<-rbind(A,B,C,AH)

data$Age[data$Age==2]<-20

marr2.lvl <- marr.lvl(data$Marriage)

data <- cbind(data,marr2.lvl)

##level##
so.lvl<-lvl(data$Somatization,3,1.85)
ocd.lvl<-(lvl(data$Obsessive.compulsive,3,2.2))
is.lvl<-(lvl(data$Interpersonal.sensitivity,2.16,2.16))
dep.lvl<-(lvl(data$Depression,4,3))
anx.lvl<-(lvl(data$Anxiety,3,1.82))
ho.lvl<-(lvl(data$Hostility,3,2.04))
pa.lvl<-(lvl(data$Phobic.anxiety,1.64,1.64))
pi.lvl<-(lvl(data$Paranoid.ideation,3,2))
psy.lvl<-(lvl(data$Psychoticism,3,1.17))
ai.lvl<-(lvl(data$Additional.items,3,2))


age2.lvl <- age2(data$Age)
edu2.lvl <- edu2(data$Education)


Group<-c(rep("A",dim(A)[1]),rep("B",dim(B)[1]),rep("C",dim(C)[1]),rep("AH",dim(AH)[1]))

data.lvled<-cbind(data,age2.lvl,edu2.lvl,Group,so.lvl,ocd.lvl,is.lvl,dep.lvl,anx.lvl,ho.lvl,pa.lvl,pa.lvl,pi.lvl,psy.lvl,ai.lvl)

A<-data.lvled[1:3151,]
B<-data.lvled[3152:(3151+1233),]
C<-data.lvled[(3151+1233+1):(3151+1233+2246),]
AH<-data.lvled[(3151+1233+2246+1):(3151+1233+2246+15578),]


############################################################
                                        #Total Score Comare
############################################################

A.1.1.1.1 <- A[A$age2.lvl==1 & A$edu2.lvl==1 & A$Sex==1 & A$Marriage==1, ]
A.1.1.1.2 <- A[A$age2.lvl==1 & A$edu2.lvl==1 & A$Sex==1 & A$Marriage==2, ]
A.1.1.2.1 <- A[A$age2.lvl==1 & A$edu2.lvl==1 & A$Sex==2 & A$Marriage==1, ]
A.1.1.2.2 <- A[A$age2.lvl==1 & A$edu2.lvl==1 & A$Sex==2 & A$Marriage==2, ]
A.1.2.1.1 <- A[A$age2.lvl==1 & A$edu2.lvl==2 & A$Sex==1 & A$Marriage==1, ]
A.1.2.1.2 <- A[A$age2.lvl==1 & A$edu2.lvl==2 & A$Sex==1 & A$Marriage==2, ]
A.1.2.2.1 <- A[A$age2.lvl==1 & A$edu2.lvl==2 & A$Sex==2 & A$Marriage==1, ]
A.1.2.2.2 <- A[A$age2.lvl==1 & A$edu2.lvl==2 & A$Sex==2 & A$Marriage==2, ]
A.2.1.1.1 <- A[A$age2.lvl==2 & A$edu2.lvl==1 & A$Sex==1 & A$Marriage==1, ]
A.2.1.1.2 <- A[A$age2.lvl==2 & A$edu2.lvl==1 & A$Sex==1 & A$Marriage==2, ]
A.2.1.2.1 <- A[A$age2.lvl==2 & A$edu2.lvl==1 & A$Sex==2 & A$Marriage==1, ]
A.2.1.2.2 <- A[A$age2.lvl==2 & A$edu2.lvl==1 & A$Sex==2 & A$Marriage==2, ]
A.2.2.1.1 <- A[A$age2.lvl==2 & A$edu2.lvl==2 & A$Sex==1 & A$Marriage==1, ]
A.2.2.1.2 <- A[A$age2.lvl==2 & A$edu2.lvl==2 & A$Sex==1 & A$Marriage==2, ]
A.2.2.2.1 <- A[A$age2.lvl==2 & A$edu2.lvl==2 & A$Sex==2 & A$Marriage==1, ]
A.2.2.2.2 <- A[A$age2.lvl==2 & A$edu2.lvl==2 & A$Sex==2 & A$Marriage==2, ]
B.1.1.1.1 <- B[B$age2.lvl==1 & B$edu2.lvl==1 & B$Sex==1 & B$Marriage==1, ]
B.1.1.1.2 <- B[B$age2.lvl==1 & B$edu2.lvl==1 & B$Sex==1 & B$Marriage==2, ]
B.1.1.2.1 <- B[B$age2.lvl==1 & B$edu2.lvl==1 & B$Sex==2 & B$Marriage==1, ]
B.1.1.2.2 <- B[B$age2.lvl==1 & B$edu2.lvl==1 & B$Sex==2 & B$Marriage==2, ]
B.1.2.1.1 <- B[B$age2.lvl==1 & B$edu2.lvl==2 & B$Sex==1 & B$Marriage==1, ]
B.1.2.1.2 <- B[B$age2.lvl==1 & B$edu2.lvl==2 & B$Sex==1 & B$Marriage==2, ]
B.1.2.2.1 <- B[B$age2.lvl==1 & B$edu2.lvl==2 & B$Sex==2 & B$Marriage==1, ]
B.1.2.2.2 <- B[B$age2.lvl==1 & B$edu2.lvl==2 & B$Sex==2 & B$Marriage==2, ]
B.2.1.1.1 <- B[B$age2.lvl==2 & B$edu2.lvl==1 & B$Sex==1 & B$Marriage==1, ]
B.2.1.1.2 <- B[B$age2.lvl==2 & B$edu2.lvl==1 & B$Sex==1 & B$Marriage==2, ]
B.2.1.2.1 <- B[B$age2.lvl==2 & B$edu2.lvl==1 & B$Sex==2 & B$Marriage==1, ]
B.2.1.2.2 <- B[B$age2.lvl==2 & B$edu2.lvl==1 & B$Sex==2 & B$Marriage==2, ]
B.2.2.1.1 <- B[B$age2.lvl==2 & B$edu2.lvl==2 & B$Sex==1 & B$Marriage==1, ]
B.2.2.1.2 <- B[B$age2.lvl==2 & B$edu2.lvl==2 & B$Sex==1 & B$Marriage==2, ]
B.2.2.2.1 <- B[B$age2.lvl==2 & B$edu2.lvl==2 & B$Sex==2 & B$Marriage==1, ]
B.2.2.2.2 <- B[B$age2.lvl==2 & B$edu2.lvl==2 & B$Sex==2 & B$Marriage==2, ]
C.1.1.1.1 <- C[C$age2.lvl==1 & C$edu2.lvl==1 & C$Sex==1 & C$Marriage==1, ]
C.1.1.1.2 <- C[C$age2.lvl==1 & C$edu2.lvl==1 & C$Sex==1 & C$Marriage==2, ]
C.1.1.2.1 <- C[C$age2.lvl==1 & C$edu2.lvl==1 & C$Sex==2 & C$Marriage==1, ]
C.1.1.2.2 <- C[C$age2.lvl==1 & C$edu2.lvl==1 & C$Sex==2 & C$Marriage==2, ]
C.1.2.1.1 <- C[C$age2.lvl==1 & C$edu2.lvl==2 & C$Sex==1 & C$Marriage==1, ]
C.1.2.1.2 <- C[C$age2.lvl==1 & C$edu2.lvl==2 & C$Sex==1 & C$Marriage==2, ]
C.1.2.2.1 <- C[C$age2.lvl==1 & C$edu2.lvl==2 & C$Sex==2 & C$Marriage==1, ]
C.1.2.2.2 <- C[C$age2.lvl==1 & C$edu2.lvl==2 & C$Sex==2 & C$Marriage==2, ]
C.2.1.1.1 <- C[C$age2.lvl==2 & C$edu2.lvl==1 & C$Sex==1 & C$Marriage==1, ]
C.2.1.1.2 <- C[C$age2.lvl==2 & C$edu2.lvl==1 & C$Sex==1 & C$Marriage==2, ]
C.2.1.2.1 <- C[C$age2.lvl==2 & C$edu2.lvl==1 & C$Sex==2 & C$Marriage==1, ]
C.2.1.2.2 <- C[C$age2.lvl==2 & C$edu2.lvl==1 & C$Sex==2 & C$Marriage==2, ]
C.2.2.1.1 <- C[C$age2.lvl==2 & C$edu2.lvl==2 & C$Sex==1 & C$Marriage==1, ]
C.2.2.1.2 <- C[C$age2.lvl==2 & C$edu2.lvl==2 & C$Sex==1 & C$Marriage==2, ]
C.2.2.2.1 <- C[C$age2.lvl==2 & C$edu2.lvl==2 & C$Sex==2 & C$Marriage==1, ]
C.2.2.2.2 <- C[C$age2.lvl==2 & C$edu2.lvl==2 & C$Sex==2 & C$Marriage==2, ]

##################

c(dim(A.1.1.1.1)[1],
dim(A.1.1.1.2)[1],
dim(A.1.1.2.1)[1],
dim(A.1.1.2.2)[1],
dim(A.1.2.1.1)[1],
dim(A.1.2.1.2)[1],
dim(A.1.2.2.1)[1],
dim(A.1.2.2.2)[1],
dim(A.2.1.1.1)[1],
dim(A.2.1.1.2)[1],
dim(A.2.1.2.1)[1],
dim(A.2.1.2.2)[1],
dim(A.2.2.1.1)[1],
dim(A.2.2.1.2)[1],
dim(A.2.2.2.1)[1],
dim(A.2.2.2.2)[1],
dim(B.1.1.1.1)[1],
dim(B.1.1.1.2)[1],
dim(B.1.1.2.1)[1],
dim(B.1.1.2.2)[1],
dim(B.1.2.1.1)[1],
dim(B.1.2.1.2)[1],
dim(B.1.2.2.1)[1],
dim(B.1.2.2.2)[1],
dim(B.2.1.1.1)[1],
dim(B.2.1.1.2)[1],
dim(B.2.1.2.1)[1],
dim(B.2.1.2.2)[1],
dim(B.2.2.1.1)[1],
dim(B.2.2.1.2)[1],
dim(B.2.2.2.1)[1],
dim(B.2.2.2.2)[1],
dim(C.1.1.1.1)[1],
dim(C.1.1.1.2)[1],
dim(C.1.1.2.1)[1],
dim(C.1.1.2.2)[1],
dim(C.1.2.1.1)[1],
dim(C.1.2.1.2)[1],
dim(C.1.2.2.1)[1],
dim(C.1.2.2.2)[1],
dim(C.2.1.1.1)[1],
dim(C.2.1.1.2)[1],
dim(C.2.1.2.1)[1],
dim(C.2.1.2.2)[1],
dim(C.2.2.1.1)[1],
dim(C.2.2.1.2)[1],
dim(C.2.2.2.1)[1],
dim(C.2.2.2.2)[1])



listA <- list(
    A.1.1.1.1,
    A.1.1.1.2,
    A.1.1.2.1,
    A.1.1.2.2,
    A.1.2.1.1,
A.1.2.1.2,
A.1.2.2.1,
A.1.2.2.2,
A.2.1.1.1,
A.2.1.1.2,
A.2.1.2.1,
A.2.1.2.2,
A.2.2.1.1,
A.2.2.1.2,
A.2.2.2.1,
A.2.2.2.2)

listB <- list(B.1.1.1.1,
B.1.1.1.2,
B.1.1.2.1,
B.1.1.2.2,
B.1.2.1.1,
B.1.2.1.2,
B.1.2.2.1,
B.1.2.2.2,
B.2.1.1.1,
B.2.1.1.2,
B.2.1.2.1,
B.2.1.2.2,
B.2.2.1.1,
B.2.2.1.2,
B.2.2.2.1,
B.2.2.2.2)

listC <- list(
C.1.1.1.1,
C.1.1.1.2,
C.1.1.2.1,
C.1.1.2.2,
C.1.2.1.1,
C.1.2.1.2,
C.1.2.2.1,
C.1.2.2.2,
C.2.1.1.1,
C.2.1.1.2,
C.2.1.2.1,
C.2.1.2.2,
C.2.2.1.1,
C.2.2.1.2,
C.2.2.2.1,
C.2.2.2.2)



########### t- tests
                                        # 1.1.1.1



                                        # 1.1.1.2
# 1.1.2.1
# 1.1.2.2
# 1.2.1.1
# 1.2.1.2
# 1.2.2.1
# 1.2.2.2
# 2.1.1.1
# 2.1.1.2
# 2.1.2.1
# 2.1.2.2
# 2.2.1.1
# 2.2.1.2
# 2.2.2.1
                                        # 2.2.2.2
result <- matrix(rep(0,16*3),ncol=3)

for(i in 1:13){

    temA <- listA[[i]]
    temB <- listB[[i]]
    temC <- listC[[i]]

    (iii <- min(dim(temA)[1],dim(temB)[1],dim(temC)[1]))

 # temA <- temA[1:iii,]
  # temB <- temB[1:iii,]
  # temC <- temC[1:iii,]

    result[i,1] <- signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
    result[i,2] <- signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
    result[i,3] <- signif(t.test(temB$Total.score,temC$Total.score)[[3]],digit=2)
}


result>=0.05



P <- data.lvled[data.lvled$Group!="AH",]
AH <- data.lvled[data.lvled$Group=="AH",]

P0 <- P[P$t==0,]
P1 <- P[P$t==1,]
P2 <- P[P$t==2,]

AH0 <- AH[AH$t==0,]
AH1 <- AH[AH$t==1,]
AH2 <- AH[AH$t==2,]


c.t<-function(A){
	return(paste(round(mean(A),2),"+-",round(sd(A),2)))
}

(c(c.t(P0$Age),
   c.t(P1$Age),
   c.t(P2$Age),
   c.t(AH0$Age),
   c.t(AH1$Age),
   c.t(AH2$Age)
   ))

n.t <- function(A){
    s <- length(A[,1])+length(A[,2])+length(A[,3])
    return(paste(length(A[,1]),"(",round(length(A[,1])/s,2),") |",length(A[,2]),"(",round(length(A[,2])/s,2),") |",length(A[,3]),"(",round(length(A[,3])/s,2),") |"))

}

n.t <- function(A,s){
    return(paste(length(A)," (",round(100*length(A)/s,2),")",sep=""))
}

sink("rrr.txt")


s <- length(c(P0$Sex[P0$Sex==1],P1$Sex[P1$Sex==1],P2$Sex [P2$Sex==1]))

n.t(P0$Sex[P0$Sex==1],s)
n.t(P1$Sex[P1$Sex==1],s)
n.t(P2$Sex[P2$Sex==1],s)

s <- length(c(P0$Sex[P0$Sex==2],P1$Sex[P1$Sex==2],P2$Sex[P2$Sex==2]))

n.t(P0$Sex[P0$Sex==2],s)
n.t(P1$Sex[P1$Sex==2],s)
n.t(P2$Sex[P2$Sex==2],s)


s <- length(c(P0$age2.lvl[P0$age2.lvl==1],P1$age2.lvl[P1$age2.lvl==1],P2$age2.lvl [P2$age2.lvl==1]))

n.t(P0$age2.lvl[P0$age2.lvl==1],s)
n.t(P1$age2.lvl[P1$age2.lvl==1],s)
n.t(P2$age2.lvl[P2$age2.lvl==1],s)

s <- length(c(P0$age2.lvl[P0$age2.lvl==2],P1$age2.lvl[P1$age2.lvl==2],P2$age2.lvl[P2$age2.lvl==2]))

n.t(P0$age2.lvl[P0$age2.lvl==2],s)
n.t(P1$age2.lvl[P1$age2.lvl==2],s)
n.t(P2$age2.lvl[P2$age2.lvl==2],s)


s <- length(c(P0$edu2.lvl[P0$edu2.lvl==1],P1$edu2.lvl[P1$edu2.lvl==1],P2$edu2.lvl[P2$edu2.lvl==1]))

n.t(P0$edu2.lvl[P0$edu2.lvl==1],s)
n.t(P1$edu2.lvl[P1$edu2.lvl==1],s)
n.t(P2$edu2.lvl[P2$edu2.lvl==1],s)


s <- length(c(P0$edu2.lvl[P0$edu2.lvl==2],P1$edu2.lvl[P1$edu2.lvl==2],P2$edu2.lvl[P2$edu2.lvl==2]))

n.t(P0$edu2.lvl[P0$edu2.lvl==2],s)
n.t(P1$edu2.lvl[P1$edu2.lvl==2],s)
n.t(P2$edu2.lvl[P2$edu2.lvl==2],s)


s <- length(c(P0$Marriage[P0$Marriage==1],P1$Marriage[P1$Marriage==1],P2$Marriage[P2$Marriage==1]))

n.t(P0$Marriage[P0$Marriage==1],s)
n.t(P1$Marriage[P1$Marriage==1],s)
n.t(P2$Marriage[P2$Marriage==1],s)

s <- length(c(P0$Marriage[P0$Marriage==2],P1$Marriage[P1$Marriage==2],P2$Marriage[P2$Marriage==2]))

n.t(P0$Marriage[P0$Marriage==2],s)
n.t(P1$Marriage[P1$Marriage==2],s)
n.t(P2$Marriage[P2$Marriage==2],s)

s <- length(c(P0$Marriage[P0$Marriage==3],P1$Marriage[P1$Marriage==3],P2$Marriage[P2$Marriage==3]))

n.t(P0$Marriage[P0$Marriage==3],s)
n.t(P1$Marriage[P1$Marriage==3],s)
n.t(P2$Marriage[P2$Marriage==3],s)

s <- length(c(P0$Marriage[P0$Marriage==4],P1$Marriage[P1$Marriage==4],P2$Marriage[P2$Marriage==4]))

n.t(P0$Marriage[P0$Marriage==4],s)
n.t(P1$Marriage[P1$Marriage==4],s)
n.t(P2$Marriage[P2$Marriage==4],s)

sink()


###AH

sink("rrr.txt")

s <- length(c(AH0$Sex[AH0$Sex==1],AH1$Sex[AH1$Sex==1],AH2$Sex [AH2$Sex==1]))

n.t(AH0$Sex[AH0$Sex==1],s)
n.t(AH1$Sex[AH1$Sex==1],s)
n.t(AH2$Sex[AH2$Sex==1],s)

s <- length(c(AH0$Sex[AH0$Sex==2],AH1$Sex[AH1$Sex==2],AH2$Sex[AH2$Sex==2]))

n.t(AH0$Sex[AH0$Sex==2],s)
n.t(AH1$Sex[AH1$Sex==2],s)
n.t(AH2$Sex[AH2$Sex==2],s)


s <- length(c(AH0$age2.lvl[AH0$age2.lvl==1],AH1$age2.lvl[AH1$age2.lvl==1],AH2$age2.lvl [AH2$age2.lvl==1]))

n.t(AH0$age2.lvl[AH0$age2.lvl==1],s)
n.t(AH1$age2.lvl[AH1$age2.lvl==1],s)
n.t(AH2$age2.lvl[AH2$age2.lvl==1],s)

s <- length(c(AH0$age2.lvl[AH0$age2.lvl==2],AH1$age2.lvl[AH1$age2.lvl==2],AH2$age2.lvl[AH2$age2.lvl==2]))

n.t(AH0$age2.lvl[AH0$age2.lvl==2],s)
n.t(AH1$age2.lvl[AH1$age2.lvl==2],s)
n.t(AH2$age2.lvl[AH2$age2.lvl==2],s)


s <- length(c(AH0$edu2.lvl[AH0$edu2.lvl==1],AH1$edu2.lvl[AH1$edu2.lvl==1],AH2$edu2.lvl[AH2$edu2.lvl==1]))

n.t(AH0$edu2.lvl[AH0$edu2.lvl==1],s)
n.t(AH1$edu2.lvl[AH1$edu2.lvl==1],s)
n.t(AH2$edu2.lvl[AH2$edu2.lvl==1],s)


s <- length(c(AH0$edu2.lvl[AH0$edu2.lvl==2],AH1$edu2.lvl[AH1$edu2.lvl==2],AH2$edu2.lvl[AH2$edu2.lvl==2]))

n.t(AH0$edu2.lvl[AH0$edu2.lvl==2],s)
n.t(AH1$edu2.lvl[AH1$edu2.lvl==2],s)
n.t(AH2$edu2.lvl[AH2$edu2.lvl==2],s)


s <- length(c(AH0$Marriage[AH0$Marriage==1],AH1$Marriage[AH1$Marriage==1],AH2$Marriage[AH2$Marriage==1]))

n.t(AH0$Marriage[AH0$Marriage==1],s)
n.t(AH1$Marriage[AH1$Marriage==1],s)
n.t(AH2$Marriage[AH2$Marriage==1],s)

s <- length(c(AH0$Marriage[AH0$Marriage==2],AH1$Marriage[AH1$Marriage==2],AH2$Marriage[AH2$Marriage==2]))

n.t(AH0$Marriage[AH0$Marriage==2],s)
n.t(AH1$Marriage[AH1$Marriage==2],s)
n.t(AH2$Marriage[AH2$Marriage==2],s)

s <- length(c(AH0$Marriage[AH0$Marriage==3],AH1$Marriage[AH1$Marriage==3],AH2$Marriage[AH2$Marriage==3]))

n.t(AH0$Marriage[AH0$Marriage==3],s)
n.t(AH1$Marriage[AH1$Marriage==3],s)
n.t(AH2$Marriage[AH2$Marriage==3],s)

s <- length(c(AH0$Marriage[AH0$Marriage==4],AH1$Marriage[AH1$Marriage==4],AH2$Marriage[AH2$Marriage==4]))

n.t(AH0$Marriage[AH0$Marriage==4],s)
n.t(AH1$Marriage[AH1$Marriage==4],s)
n.t(AH2$Marriage[AH2$Marriage==4],s)

sink()
#################

res2 <- c(
    c.t(P0$Somatization),
    c.t(P0$Obsessive.compulsive),
    c.t(P0$Interpersonal.sensitivity),
    c.t(P0$Depression),
    c.t(P0$Anxiety),
    c.t(P0$Hostility),
    c.t(P0$Phobic.anxiety),
    c.t(P0$Paranoid.ideation),
    c.t(P0$Psychoticism),
    c.t(P0$Additional.items),
    c.t(P1$Somatization),
    c.t(P1$Obsessive.compulsive),
    c.t(P1$Interpersonal.sensitivity),
    c.t(P1$Depression),
    c.t(P1$Anxiety),
    c.t(P1$Hostility),
    c.t(P1$Phobic.anxiety),
    c.t(P1$Paranoid.ideation),
    c.t(P1$Psychoticism),
    c.t(P1$Additional.items),
    c.t(P2$Somatization),
    c.t(P2$Obsessive.compulsive),
    c.t(P2$Interpersonal.sensitivity),
    c.t(P2$Depression),
    c.t(P2$Anxiety),
    c.t(P2$Hostility),
    c.t(P2$Phobic.anxiety),
    c.t(P2$Paranoid.ideation),
    c.t(P2$Psychoticism),
    c.t(P2$Additional.items),
    c.t(AH0$Somatization),
    c.t(AH0$Obsessive.compulsive),
    c.t(AH0$Interpersonal.sensitivity),
    c.t(AH0$Depression),
    c.t(AH0$Anxiety),
    c.t(AH0$Hostility),
    c.t(AH0$Phobic.anxiety),
    c.t(AH0$Paranoid.ideation),
    c.t(AH0$Psychoticism),
    c.t(AH0$Additional.items),
    c.t(AH1$Somatization),
    c.t(AH1$Obsessive.compulsive),
    c.t(AH1$Interpersonal.sensitivity),
    c.t(AH1$Depression),
    c.t(AH1$Anxiety),
    c.t(AH1$Hostility),
    c.t(AH1$Phobic.anxiety),
    c.t(AH1$Paranoid.ideation),
    c.t(AH1$Psychoticism),
    c.t(AH1$Additional.items),
    c.t(AH2$Somatization),
    c.t(AH2$Obsessive.compulsive),
    c.t(AH2$Interpersonal.sensitivity),
    c.t(AH2$Depression),
    c.t(AH2$Anxiety),
    c.t(AH2$Hostility),
    c.t(AH2$Phobic.anxiety),
    c.t(AH2$Paranoid.ideation),
    c.t(AH2$Psychoticism),
    c.t(AH2$Additional.items)
    )

library(zoo)

rest2 <- matrix(res2,10,6)

write.csv(coredata(rest2),"rrr.csv")

png(file="corr1.png", bg="transparent")
corrplot(cor(P0[,8:17]),type="upper",method="number")
title("corr for P0", line = -12)
dev.off()
png(file="corr2.png", bg="transparent")
corrplot(cor(P1[,8:17]),type="upper",method="number")
title("corr for P1", line = -12)
dev.off()
png(file="corr3.png", bg="transparent")
corrplot(cor(P2[,8:17]),type="upper",method="number")
title("corr for P1", line = -12)
dev.off()
png(file="corr4.png", bg="transparent")
corrplot(cor(AH0[,8:17]),type="upper",method="number")
title("corr for P1", line = -12)
dev.off()
png(file="corr5.png", bg="transparent")
corrplot(cor(AH1[,8:17]),type="upper",method="number" )
title("corr for P1", line = -12)
dev.off()
png(file="corr6.png", bg="transparent")
corrplot(cor(AH2[,8:17]),type="upper",method="number" )
title("corr for P1", line = -12)
dev.off()

pdf("corr.plot.pdf")
corrplot(cor(P0[,8:17]),type="upper",method="number")
title("corr for P0", line = -25)
corrplot(cor(P1[,8:17]),type="upper",method="number")
title("corr for P1", line = -25)
corrplot(cor(P2[,8:17]),type="upper",method="number")
title("corr for P2", line = -25)
corrplot(cor(AH0[,8:17]),type="upper",method="number")
title("corr for AH0", line = -25)
corrplot(cor(AH1[,8:17]),type="upper",method="number" )
title("corr for AH1", line = -25)
corrplot(cor(AH2[,8:17]),type="upper",method="number" )
title("corr for AH2", line = -25)
dev.off()


##########PAC
pca1 <- prcomp(P2[,8:17], scale=T)
predict(pca1)


##########ANOVA


P0[,3] <- as.factor(P0[,3])
P0[,5] <- as.factor(P0[,5])
P1[,3] <- as.factor(P1[,3])
P1[,5] <- as.factor(P1[,5])
P2[,3] <- as.factor(P2[,3])
P2[,5] <- as.factor(P2[,5])
AH0[,3] <- as.factor(AH0[,3])
AH0[,5] <- as.factor(AH0[,5])
AH1[,3] <- as.factor(AH1[,3])
AH1[,5] <- as.factor(AH1[,5])
AH2[,3] <- as.factor(AH2[,3])
AH2[,5] <- as.factor(AH2[,5])

sink("posthoc.txt")
fit1 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Somatization~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()


sink("posthoc.ocd.txt")
fit1 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Obsessive.compulsive~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()



sink("posthoc.is.txt")
fit1 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Interpersonal.sensitivity~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()



sink("posthoc.dep.txt")
fit1 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Depression~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()


sink("posthoc.anx.txt")
fit1 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()


sink("posthoc.ho.txt")
fit1 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Hostility~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()


sink("posthoc.pa.txt")
fit1 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Phobic.anxiety~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()


sink("posthoc.pi.txt")
fit1 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Paranoid.ideation~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()

sink("posthoc.psy.txt")
fit1 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=P0)
summary(fit1)
TukeyHSD(fit1)

fit2 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=AH0)
summary(fit2)
TukeyHSD(fit2)

fit3 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=P1)
summary(fit3)
TukeyHSD(fit3)

fit4 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=AH1)
summary(fit4)
TukeyHSD(fit4)

fit5 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=P2)
summary(fit5)
TukeyHSD(fit5)

fit6 <- aov(Psychoticism~age2.lvl+Sex+edu2.lvl+Marriage,data=AH2)
summary(fit6)
TukeyHSD(fit6)
sink()



#####Reg

P0$edu2.lvl <- factor(P0$edu2.lvl)
P0$age2.lvl <- factor(P0$age2.lvl)
P0$Sex <- factor(P0$Sex)

AH0$edu2.lvl <- factor(AH0$edu2.lvl)
AH0$age2.lvl <- factor(AH0$age2.lvl)
AH0$Sex <- factor(AH0$Sex)

fitP0 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=P0,x=TRUE)

fitAH0 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=AH0,x=TRUE)

summary(fitP0)
summary(fitAH0)



P1$edu2.lvl <- factor(P1$edu2.lvl)
P1$age2.lvl <- factor(P1$age2.lvl)
P1$Sex <- factor(P1$Sex)

AH1$edu2.lvl <- factor(AH1$edu2.lvl)
AH1$age2.lvl <- factor(AH1$age2.lvl)
AH1$Sex <- factor(AH1$Sex)

fitP1 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=P1,x=TRUE)

fitAH1 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=AH1,x=TRUE)

summary(fitP1)
summary(fitAH1)


P2$edu2.lvl <- factor(P2$edu2.lvl)
P2$age2.lvl <- factor(P2$age2.lvl)
P2$Sex <- factor(P2$Sex)

AH2$edu2.lvl <- factor(AH2$edu2.lvl)
AH2$age2.lvl <- factor(AH2$age2.lvl)
AH2$Sex <- factor(AH2$Sex)

fitP2 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=P2,x=TRUE)

fitAH2 <- lm(Total.score~age2.lvl+Sex+edu2.lvl+marr2.lvl+Somatization+Interpersonal.sensitivity+Depression+Anxiety+Hostility+Phobic.anxiety+Paranoid.ideation+Psychoticism+Additional.items,data=AH2,x=TRUE)

summary(fitP2)
summary(fitAH2)


