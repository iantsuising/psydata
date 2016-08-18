########## three locations comparisons ##########

signif(t.test(A$Total.score,B$Total.score)[[3]],digit=2)
signif(t.test(A$Total.score,C$Total.score)[[3]],digit=2)
signif(t.test(C$Total.score,B$Total.score)[[3]],digit=2)

signif(t.test(A$Somatization,B$Somatization)[[3]],digit=2)
signif(t.test(A$Somatization,C$Somatization)[[3]],digit=2)
signif(t.test(C$Somatization,B$Somatization)[[3]],digit=2)

signif(t.test(A$Obsessive.compulsive,B$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(A$Obsessive.compulsive,C$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(C$Obsessive.compulsive,B$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(A$Interpersonal.sensitivity,B$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(A$Interpersonal.sensitivity,C$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(C$Interpersonal.sensitivity,B$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(A$Depression,B$Depression)[[3]],digit=2)
signif(t.test(A$Depression,C$Depression)[[3]],digit=2)
signif(t.test(C$Depression,B$Depression)[[3]],digit=2)

signif(t.test(A$Anxiety,B$Anxiety)[[3]],digit=2)
signif(t.test(A$Anxiety,C$Anxiety)[[3]],digit=2)
signif(t.test(C$Anxiety,B$Anxiety)[[3]],digit=2)

signif(t.test(A$Hostility,B$Hostility)[[3]],digit=2)
signif(t.test(A$Hostility,C$Hostility)[[3]],digit=2)
signif(t.test(C$Hostility,B$Hostility)[[3]],digit=2)

signif(t.test(A$Phobic.anxiety,B$Phobic.anxiety)[[3]],digit=2)
signif(t.test(A$Phobic.anxiety,C$Phobic.anxiety)[[3]],digit=2)
signif(t.test(C$Phobic.anxiety,B$Phobic.anxiety)[[3]],digit=2)

signif(t.test(A$Paranoid.ideation,B$Paranoid.ideation)[[3]],digit=2)
signif(t.test(A$Paranoid.ideation,C$Paranoid.ideation)[[3]],digit=2)
signif(t.test(C$Paranoid.ideation,B$Paranoid.ideation)[[3]],digit=2)

signif(t.test(A$Psychoticism,B$Psychoticism)[[3]],digit=2)
signif(t.test(A$Psychoticism,C$Psychoticism)[[3]],digit=2)
signif(t.test(C$Psychoticism,B$Psychoticism)[[3]],digit=2)

signif(t.test(A$Additional.items,B$Additional.items)[[3]],digit=2)
signif(t.test(A$Additional.items,C$Additional.items)[[3]],digit=2)
signif(t.test(C$Additional.items,B$Additional.items)[[3]],digit=2)

##########
signif(t.test(A$Age,B$Age)[[3]],digit=2)
signif(t.test(A$Age,C$Age)[[3]],digit=2)
signif(t.test(C$Age,B$Age)[[3]],digit=2)


signif(t.test(A$Sex,B$Sex)[[3]],digit=2)
signif(t.test(A$Sex,C$Sex)[[3]],digit=2)
signif(t.test(C$Sex,B$Sex)[[3]],digit=2)

signif(t.test(A$Education,B$Education)[[3]],digit=2)
signif(t.test(A$Education,C$Education)[[3]],digit=2)
signif(t.test(C$Education,B$Education)[[3]],digit=2)

signif(t.test(A$Marriage,B$Marriage)[[3]],digit=2)
signif(t.test(A$Marriage,C$Marriage)[[3]],digit=2)
signif(t.test(C$Marriage,B$Marriage)[[3]],digit=2)

########## same age ##########

#### agelvl=1 #####
temA<-(A[A$age.lvl==1,])
temB<-(B[B$age.lvl==1,])
temC<-C[C$age.lvl==1,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:361,]
temB<-temB
temC<-temC[1:361,]

signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)




#### agelvl=2 #####
temA<-(A[A$age.lvl==2,])
temB<-(B[B$age.lvl==2,])
temC<-C[C$age.lvl==2,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:444,]
temB<-temB
temC<-temC[1:444,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### agelvl=3 #####
temA<-(A[A$age.lvl==3,])
temB<-(B[B$age.lvl==3,])
temC<-C[C$age.lvl==3,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:237,]
temB<-temB
temC<-temC[1:237,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### agelvl=4 #####
temA<-(A[A$age.lvl==4,])
temB<-(B[B$age.lvl==4,])
temC<-C[C$age.lvl==4,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:169,]
temB<-temB
temC<-temC[1:169,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


#### agelvl=5 #####
temA<-(A[A$age.lvl==5,])
temB<-(B[B$age.lvl==5,])
temC<-C[C$age.lvl==5,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:22,]
temB<-temB
temC<-temC[1:22,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


########## Sex ##########
#### Sex=1 #####
temA<-(A[A$Sex==1,])
temB<-(B[B$Sex==1,])
temC<-C[C$Sex==1,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:649,]
temB<-temB
temC<-temC[1:649,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


#### Sex=2 #####
temA<-(A[A$Sex==2,])
temB<-(B[B$Sex==2,])
temC<-C[C$Sex==2,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:584,]
temB<-temB
temC<-temC[1:584,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)



########## Education ##########
#### Education=0 #####
temA<-(A[A$Education==0,])
temB<-(B[B$Education==0,])
temC<-C[C$Education==0,]
dim(temA)
dim(temB)
dim(temC)

temB<-temB
temC<-temC[1:40,]

signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### Education=1 #####
temA<-(A[A$Education==1,])
temB<-(B[B$Education==1,])
temC<-C[C$Education==1,]
dim(temA)
dim(temB)
dim(temC)
temB<-temB
temC<-temC[1:91,]
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


#### Education=2 #####
temA<-(A[A$Education==2,])
temB<-(B[B$Education==2,])
temC<-C[C$Education==2,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:529,]
temB<-temB
temC<-temC[1:529,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### Education=3 #####
temA<-(A[A$Education==3,])
temB<-(B[B$Education==3,])
temC<-C[C$Education==3,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:505,]
temB<-temB
temC<-temC[1:505,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### Education=4 #####
temA<-(A[A$Education==4,])
temB<-(B[B$Education==4,])
temC<-C[C$Education==4,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:68,]
temB<-temB
temC<-temC[1:68,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


########## Marriage ##########
#### Marriage=1 #####
temA<-(A[A$Marriage==1,])
temB<-(B[B$Marriage==1,])
temC<-C[C$Marriage==1,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:730,]
temB<-temB
temC<-temC[1:730,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### Marriage=2 #####
temA<-(A[A$Marriage==2,])
temB<-(B[B$Marriage==2,])
temC<-C[C$Marriage==2,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:469,]
temB<-temB
temC<-temC[1:469,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)

#### Marriage=4 #####
temA<-(A[A$Marriage==4,])
temB<-(B[B$Marriage==4,])
temC<-C[C$Marriage==4,]
dim(temA)
dim(temB)
dim(temC)
temA<-temA[1:17,]
temB<-temB
temC<-temC[1:17,]
signif(t.test(temA$Total.score,temB$Total.score)[[3]],digit=2)
signif(t.test(temA$Total.score,temC$Total.score)[[3]],digit=2)
signif(t.test(temC$Total.score,temB$Total.score)[[3]],digit=2)

signif(t.test(temA$Somatization,temB$Somatization)[[3]],digit=2)
signif(t.test(temA$Somatization,temC$Somatization)[[3]],digit=2)
signif(t.test(temC$Somatization,temB$Somatization)[[3]],digit=2)

signif(t.test(temA$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temA$Obsessive.compulsive,temC$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(temC$Obsessive.compulsive,temB$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(temA$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temA$Interpersonal.sensitivity,temC$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(temC$Interpersonal.sensitivity,temB$Interpersonal.sensitivity)[[3]],digit=2)


signif(t.test(temA$Depression,temB$Depression)[[3]],digit=2)
signif(t.test(temA$Depression,temC$Depression)[[3]],digit=2)
signif(t.test(temC$Depression,temB$Depression)[[3]],digit=2)

signif(t.test(temA$Anxiety,temB$Anxiety)[[3]],digit=2)
signif(t.test(temA$Anxiety,temC$Anxiety)[[3]],digit=2)
signif(t.test(temC$Anxiety,temB$Anxiety)[[3]],digit=2)

signif(t.test(temA$Hostility,temB$Hostility)[[3]],digit=2)
signif(t.test(temA$Hostility,temC$Hostility)[[3]],digit=2)
signif(t.test(temC$Hostility,temB$Hostility)[[3]],digit=2)

signif(t.test(temA$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temA$Phobic.anxiety,temC$Phobic.anxiety)[[3]],digit=2)
signif(t.test(temC$Phobic.anxiety,temB$Phobic.anxiety)[[3]],digit=2)

signif(t.test(temA$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temA$Paranoid.ideation,temC$Paranoid.ideation)[[3]],digit=2)
signif(t.test(temC$Paranoid.ideation,temB$Paranoid.ideation)[[3]],digit=2)

signif(t.test(temA$Psychoticism,temB$Psychoticism)[[3]],digit=2)
signif(t.test(temA$Psychoticism,temC$Psychoticism)[[3]],digit=2)
signif(t.test(temC$Psychoticism,temB$Psychoticism)[[3]],digit=2)

signif(t.test(temA$Additional.items,temB$Additional.items)[[3]],digit=2)
signif(t.test(temA$Additional.items,temC$Additional.items)[[3]],digit=2)
signif(t.test(temC$Additional.items,temB$Additional.items)[[3]],digit=2)


set.seed(2016)
tem<-rbind(A,B,C)
dim(tem)
tem<-tem[sample(nrow(tem),6630),]

tem1<-tem[tem$age.lvl==1,]
tem2<-tem[tem$age.lvl==2,]
tem3<-tem[tem$age.lvl==3,]
tem4<-tem[tem$age.lvl==4,]
dim(tem1)
dim(tem2)
dim(tem3)
dim(tem4)
tem1<-tem1[1:772,]
tem2<-tem2[1:772,]
tem3<-tem3[1:772,]
tem4<-tem4[1:772,]
signif(t.test(tem1$t,tem2$t)[[3]],digit=2)
signif(t.test(tem1$t,tem3$t)[[3]],digit=2)
signif(t.test(tem1$t,tem4$t)[[3]],digit=2)
signif(t.test(tem2$t,tem3$t)[[3]],digit=2)
signif(t.test(tem2$t,tem4$t)[[3]],digit=2)
signif(t.test(tem3$t,tem4$t)[[3]],digit=2)

signif(t.test(tem1$t,tem2$t)[[3]],digit=2)
signif(t.test(tem1$t,tem3$t)[[3]],digit=2)
signif(t.test(tem1$t,tem4$t)[[3]],digit=2)
signif(t.test(tem2$t,tem3$t)[[3]],digit=2)
signif(t.test(tem2$t,tem4$t)[[3]],digit=2)
signif(t.test(tem3$t,tem4$t)[[3]],digit=2)
