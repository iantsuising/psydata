temA1 <- A[A$age.lvl==1 & A$Sex==1,]
temA2 <- A[A$age.lvl==1 & A$Sex==2,]
temB1 <- B[B$age.lvl==1 & B$Sex==1,]
temB2 <- B[B$age.lvl==1 & B$Sex==2,]
temC1 <- C[C$age.lvl==1 & C$Sex==1,]
temC2 <- C[C$age.lvl==1 & C$Sex==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

############## Age level ==2
temA1 <- A[A$age.lvl==2 & A$Sex==1,]
temA2 <- A[A$age.lvl==2 & A$Sex==2,]
temB1 <- B[B$age.lvl==2 & B$Sex==1,]
temB2 <- B[B$age.lvl==2 & B$Sex==2,]
temC1 <- C[C$age.lvl==2 & C$Sex==1,]
temC2 <- C[C$age.lvl==2 & C$Sex==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

########## age level =3
temA1 <- A[A$age.lvl==3 & A$Sex==1,]
temA2 <- A[A$age.lvl==3 & A$Sex==2,]
temB1 <- B[B$age.lvl==3 & B$Sex==1,]
temB2 <- B[B$age.lvl==3 & B$Sex==2,]
temC1 <- C[C$age.lvl==3 & C$Sex==1,]
temC2 <- C[C$age.lvl==3 & C$Sex==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

##########age level ==4
temA1 <- A[A$age.lvl==4 & A$Sex==1,]
temA2 <- A[A$age.lvl==4 & A$Sex==2,]
temB1 <- B[B$age.lvl==4 & B$Sex==1,]
temB2 <- B[B$age.lvl==4 & B$Sex==2,]
temC1 <- C[C$age.lvl==4 & C$Sex==1,]
temC2 <- C[C$age.lvl==4 & C$Sex==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

###################################################################
### contral age
### age group ==1

temA2 <- A[A$age.lvl==1 & A$Education==2,]
temA3 <- A[A$age.lvl==1 & A$Education==3,]
temA4 <- A[A$age.lvl==1 & A$Education==4,]

min(dim(temA2)[1],
dim(temA3)[1],
dim(temA4)[1])->iii

iii

temA2 <- temA2[1:iii,]
temA3 <- temA3[1:iii,]
temA4 <- temA4[1:iii,]

c(signif(t.test(temA3$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA2$Additional.items)[[3]],digit=2))


c(signif(t.test(temA4$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA4$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA4$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA4$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA4$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA4$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA4$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA4$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA4$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA4$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA4$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temA3$Total.score,temA4$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA4$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA4$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA4$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA4$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA4$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA4$Additional.items)[[3]],digit=2))


############age level ==2

temA2 <- A[A$age.lvl==2 & A$Education==2,]
temA3 <- A[A$age.lvl==2 & A$Education==3,]
temA4 <- A[A$age.lvl==2 & A$Education==4,]

min(dim(temA2)[1],
dim(temA3)[1],
dim(temA4)[1])->iii

iii

temA2 <- temA2[1:iii,]
temA3 <- temA3[1:iii,]
temA4 <- temA4[1:iii,]

c(signif(t.test(temA3$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA2$Additional.items)[[3]],digit=2))


c(signif(t.test(temA4$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA4$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA4$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA4$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA4$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA4$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA4$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA4$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA4$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA4$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA4$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temA3$Total.score,temA4$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA4$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA4$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA4$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA4$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA4$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA4$Additional.items)[[3]],digit=2))


###############Age level ==3

temA2 <- A[A$age.lvl==3 & A$Education==2,]
temA3 <- A[A$age.lvl==3 & A$Education==3,]
temA4 <- A[A$age.lvl==3 & A$Education==4,]

min(dim(temA2)[1],
dim(temA3)[1],
dim(temA4)[1])->iii

iii

temA2 <- temA2[1:iii,]
temA3 <- temA3[1:iii,]
temA4 <- temA4[1:iii,]

c(signif(t.test(temA3$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA2$Additional.items)[[3]],digit=2))


c(signif(t.test(temA4$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA4$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA4$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA4$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA4$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA4$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA4$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA4$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA4$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA4$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA4$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temA3$Total.score,temA4$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA4$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA4$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA4$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA4$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA4$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA4$Additional.items)[[3]],digit=2))


#############Age level ==4

temA2 <- A[A$age.lvl==4 & A$Education==2,]
temA3 <- A[A$age.lvl==4 & A$Education==3,]
temA4 <- A[A$age.lvl==4 & A$Education==4,]

min(dim(temA2)[1],
dim(temA3)[1],
dim(temA4)[1])->iii

iii

temA2 <- temA2[1:iii,]
temA3 <- temA3[1:iii,]
temA4 <- temA4[1:iii,]

c(signif(t.test(temA3$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA2$Additional.items)[[3]],digit=2))


c(signif(t.test(temA4$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA4$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA4$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA4$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA4$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA4$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA4$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA4$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA4$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA4$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA4$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temA3$Total.score,temA4$Total.score)[[3]],digit=2),
signif(t.test(temA3$Somatization,temA4$Somatization)[[3]],digit=2),
signif(t.test(temA3$Obsessive.compulsive,temA4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA3$Interpersonal.sensitivity,temA4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA3$Depression,temA4$Depression)[[3]],digit=2),
signif(t.test(temA3$Anxiety,temA4$Anxiety)[[3]],digit=2),
signif(t.test(temA3$Hostility,temA4$Hostility)[[3]],digit=2),
signif(t.test(temA3$Phobic.anxiety,temA4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA3$Paranoid.ideation,temA4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA3$Psychoticism,temA4$Psychoticism)[[3]],digit=2),
signif(t.test(temA3$Additional.items,temA4$Additional.items)[[3]],digit=2))

########################## Group B, age controlled

######age ==1
temB0 <- B[B$age.lvl==1 & B$Education==0,]
temB1 <- B[B$age.lvl==1 & B$Education==1,]
temB2 <- B[B$age.lvl==1 & B$Education==2,]
temB3 <- B[B$age.lvl==1 & B$Education==3,]
temB4 <- B[B$age.lvl==1 & B$Education==4,]

c(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1])

min(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1]) -> iii

iii

iii <- 133

temB3 <- temB3[1:iii,]

temB0 <- temB0[1:iii,]
temB1 <- temB1[1:iii,]
temB2 <- temB2[1:iii,]
temB3 <- temB3[1:iii,]
temB4 <- temB4[1:iii,]

c(signif(t.test(temB2$Total.score,temB3$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB3$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB3$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB3$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB3$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB3$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB3$Additional.items)[[3]],digit=2))


########3age level == 2
temB0 <- B[B$age.lvl==2 & B$Education==0,]
temB1 <- B[B$age.lvl==2 & B$Education==1,]
temB2 <- B[B$age.lvl==2 & B$Education==2,]
temB3 <- B[B$age.lvl==2 & B$Education==3,]
temB4 <- B[B$age.lvl==2 & B$Education==4,]

dim(temB0)[1]
    dim(temB1)[1]
    dim(temB2)[1]
    dim(temB3)[1]
    dim(temB4)[1]

min(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4[1])) -> iii

iii

iii <- 42


temB2 <- temB2[1:iii,]
temB3 <- temB3[1:iii,]
temB4 <- temB4[1:iii,]

c(signif(t.test(temB2$Total.score,temB3$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB3$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB3$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB3$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB3$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB3$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB3$Additional.items)[[3]],digit=2))

c(signif(t.test(temB2$Total.score,temB4$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB4$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB4$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB4$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB4$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB4$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB4$Additional.items)[[3]],digit=2))

c(signif(t.test(temB4$Total.score,temB3$Total.score)[[3]],digit=2),
signif(t.test(temB4$Somatization,temB3$Somatization)[[3]],digit=2),
signif(t.test(temB4$Obsessive.compulsive,temB3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB4$Interpersonal.sensitivity,temB3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB4$Depression,temB3$Depression)[[3]],digit=2),
signif(t.test(temB4$Anxiety,temB3$Anxiety)[[3]],digit=2),
signif(t.test(temB4$Hostility,temB3$Hostility)[[3]],digit=2),
signif(t.test(temB4$Phobic.anxiety,temB3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB4$Paranoid.ideation,temB3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB4$Psychoticism,temB3$Psychoticism)[[3]],digit=2),
signif(t.test(temB4$Additional.items,temB3$Additional.items)[[3]],digit=2))


###########Age level == 3
temB0 <- B[B$age.lvl==3 & B$Education==0,]
temB1 <- B[B$age.lvl==3 & B$Education==1,]
temB2 <- B[B$age.lvl==3 & B$Education==2,]
temB3 <- B[B$age.lvl==3 & B$Education==3,]
temB4 <- B[B$age.lvl==3 & B$Education==4,]

c(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1])

min(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1]) -> iii

iii

iii <- 30


temB1 <- temB1[1:iii,]
temB2 <- temB2[1:iii,]
temB3 <- temB3[1:iii,]

c(signif(t.test(temB2$Total.score,temB1$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB1$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB1$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB1$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB1$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB1$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB1$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB1$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB1$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB1$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB1$Additional.items)[[3]],digit=2))

c(signif(t.test(temB3$Total.score,temB1$Total.score)[[3]],digit=2),
signif(t.test(temB3$Somatization,temB1$Somatization)[[3]],digit=2),
signif(t.test(temB3$Obsessive.compulsive,temB1$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB3$Interpersonal.sensitivity,temB1$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB3$Depression,temB1$Depression)[[3]],digit=2),
signif(t.test(temB3$Anxiety,temB1$Anxiety)[[3]],digit=2),
signif(t.test(temB3$Hostility,temB1$Hostility)[[3]],digit=2),
signif(t.test(temB3$Phobic.anxiety,temB1$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB3$Paranoid.ideation,temB1$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB3$Psychoticism,temB1$Psychoticism)[[3]],digit=2),
signif(t.test(temB3$Additional.items,temB1$Additional.items)[[3]],digit=2))


c(signif(t.test(temB2$Total.score,temB3$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB3$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB3$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB3$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB3$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB3$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB3$Additional.items)[[3]],digit=2))


###############age level ==4


temB0 <- B[B$age.lvl==4 & B$Education==0,]
temB1 <- B[B$age.lvl==4 & B$Education==1,]
temB2 <- B[B$age.lvl==4 & B$Education==2,]
temB3 <- B[B$age.lvl==4 & B$Education==3,]
temB4 <- B[B$age.lvl==4 & B$Education==4,]

c(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1])

min(dim(temB0)[1],
    dim(temB1)[1],
    dim(temB2)[1],
    dim(temB3)[1],
    dim(temB4)[1]) -> iii

iii

iii <- 38

temB2 <- temB2[1:iii,]
temB3 <- temB3[1:iii,]

c(signif(t.test(temB2$Total.score,temB3$Total.score)[[3]],digit=2),
signif(t.test(temB2$Somatization,temB3$Somatization)[[3]],digit=2),
signif(t.test(temB2$Obsessive.compulsive,temB3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB2$Interpersonal.sensitivity,temB3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB2$Depression,temB3$Depression)[[3]],digit=2),
signif(t.test(temB2$Anxiety,temB3$Anxiety)[[3]],digit=2),
signif(t.test(temB2$Hostility,temB3$Hostility)[[3]],digit=2),
signif(t.test(temB2$Phobic.anxiety,temB3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB2$Paranoid.ideation,temB3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB2$Psychoticism,temB3$Psychoticism)[[3]],digit=2),
signif(t.test(temB2$Additional.items,temB3$Additional.items)[[3]],digit=2))


####################Group C Age controlled

########age level ==1

temC0 <- C[C$age.lvl==1 & C$Education==0,]
temC1 <- C[C$age.lvl==1 & C$Education==1,]
temC2 <- C[C$age.lvl==1 & C$Education==2,]
temC3 <- C[C$age.lvl==1 & C$Education==3,]
temC4 <- C[C$age.lvl==1 & C$Education==4,]

c(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1])

min(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1]) -> iii

iii

iii <- 212

temC2 <- temC2[1:iii,]
temC3 <- temC3[1:iii,]

c(signif(t.test(temC2$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC3$Additional.items)[[3]],digit=2))

#######age level ==2


temC0 <- C[C$age.lvl==2 & C$Education==0,]
temC1 <- C[C$age.lvl==2 & C$Education==1,]
temC2 <- C[C$age.lvl==2 & C$Education==2,]
temC3 <- C[C$age.lvl==2 & C$Education==3,]
temC4 <- C[C$age.lvl==2 & C$Education==4,]

c(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1])

min(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1]) -> iii

iii

iii <- 114

temC2 <- temC2[1:iii,]
temC3 <- temC3[1:iii,]
temC4 <- temC4[1:iii,]

c(signif(t.test(temC2$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC3$Additional.items)[[3]],digit=2))

c(signif(t.test(temC2$Total.score,temC4$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC4$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC4$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC4$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC4$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC4$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC4$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC4$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC4$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC4$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC4$Additional.items)[[3]],digit=2))


c(signif(t.test(temC4$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC4$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC4$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC4$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC4$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC4$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC4$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC4$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC4$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC4$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC4$Additional.items,temC3$Additional.items)[[3]],digit=2))


#############age level ==3


temC0 <- C[C$age.lvl==3 & C$Education==0,]
temC1 <- C[C$age.lvl==3 & C$Education==1,]
temC2 <- C[C$age.lvl==3 & C$Education==2,]
temC3 <- C[C$age.lvl==3 & C$Education==3,]
temC4 <- C[C$age.lvl==3 & C$Education==4,]

c(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1])

min(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1]) -> iii

iii

iii <- 91

temC2 <- temC2[1:iii,]
temC3 <- temC3[1:iii,]
temC1 <- temC1[1:iii,]

c(signif(t.test(temC2$Total.score,temC1$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC1$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC1$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC1$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC1$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC1$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC1$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC1$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC1$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC1$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC1$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC3$Additional.items)[[3]],digit=2))


c(signif(t.test(temC2$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC3$Additional.items)[[3]],digit=2))


##########Age level ==4
temC0 <- C[C$age.lvl==4 & C$Education==0,]
temC1 <- C[C$age.lvl==4 & C$Education==1,]
temC2 <- C[C$age.lvl==4 & C$Education==2,]
temC3 <- C[C$age.lvl==4 & C$Education==3,]
temC4 <- C[C$age.lvl==4 & C$Education==4,]

c(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1])

min(dim(temC0)[1],
    dim(temC1)[1],
    dim(temC2)[1],
    dim(temC3)[1],
    dim(temC4)[1]) -> iii

iii

iii <- 41
temC0 <- temC0[1:iii,]
temC2 <- temC2[1:iii,]
temC3 <- temC3[1:iii,]
temC1 <- temC1[1:iii,]

c(signif(t.test(temC2$Total.score,temC1$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC1$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC1$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC1$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC1$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC1$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC1$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC1$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC1$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC1$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC1$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC3$Additional.items)[[3]],digit=2))


c(signif(t.test(temC1$Total.score,temC0$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC0$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC0$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC0$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC0$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC0$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC0$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC0$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC0$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC0$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC0$Additional.items)[[3]],digit=2))

c(signif(t.test(temC2$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC3$Additional.items)[[3]],digit=2))

c(signif(t.test(temC2$Total.score,temC0$Total.score)[[3]],digit=2),
signif(t.test(temC2$Somatization,temC0$Somatization)[[3]],digit=2),
signif(t.test(temC2$Obsessive.compulsive,temC0$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC2$Interpersonal.sensitivity,temC0$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC2$Depression,temC0$Depression)[[3]],digit=2),
signif(t.test(temC2$Anxiety,temC0$Anxiety)[[3]],digit=2),
signif(t.test(temC2$Hostility,temC0$Hostility)[[3]],digit=2),
signif(t.test(temC2$Phobic.anxiety,temC0$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC2$Paranoid.ideation,temC0$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC2$Psychoticism,temC0$Psychoticism)[[3]],digit=2),
signif(t.test(temC2$Additional.items,temC0$Additional.items)[[3]],digit=2))


c(signif(t.test(temC0$Total.score,temC3$Total.score)[[3]],digit=2),
signif(t.test(temC0$Somatization,temC3$Somatization)[[3]],digit=2),
signif(t.test(temC0$Obsessive.compulsive,temC3$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC0$Interpersonal.sensitivity,temC3$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC0$Depression,temC3$Depression)[[3]],digit=2),
signif(t.test(temC0$Anxiety,temC3$Anxiety)[[3]],digit=2),
signif(t.test(temC0$Hostility,temC3$Hostility)[[3]],digit=2),
signif(t.test(temC0$Phobic.anxiety,temC3$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC0$Paranoid.ideation,temC3$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC0$Psychoticism,temC3$Psychoticism)[[3]],digit=2),
signif(t.test(temC0$Additional.items,temC3$Additional.items)[[3]],digit=2))

#############################
###########marriges group compare

##################age level =1

temA1 <- A[A$age.lvl==1 & A$Marriage==1,]
temA2 <- A[A$age.lvl==1 & A$Marriage==2,]
temB1 <- B[B$age.lvl==1 & B$Marriage==1,]
temB2 <- B[B$age.lvl==1 & B$Marriage==2,]
temC1 <- C[C$age.lvl==1 & C$Marriage==1,]
temC2 <- C[C$age.lvl==1 & C$Marriage==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

c(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])


iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

############## Age level ==2
temA1 <- A[A$age.lvl==2 & A$Marriage==1,]
temA2 <- A[A$age.lvl==2 & A$Marriage==2,]
temB1 <- B[B$age.lvl==2 & B$Marriage==1,]
temB2 <- B[B$age.lvl==2 & B$Marriage==2,]
temC1 <- C[C$age.lvl==2 & C$Marriage==1,]
temC2 <- C[C$age.lvl==2 & C$Marriage==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

########## age level =3
temA1 <- A[A$age.lvl==3 & A$Marriage==1,]
temA2 <- A[A$age.lvl==3 & A$Marriage==2,]
temB1 <- B[B$age.lvl==3 & B$Marriage==1,]
temB2 <- B[B$age.lvl==3 & B$Marriage==2,]
temC1 <- C[C$age.lvl==3 & C$Marriage==1,]
temC2 <- C[C$age.lvl==3 & C$Marriage==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

c(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))

##########age level ==4
temA1 <- A[A$age.lvl==4 & A$Marriage==1,]
temA2 <- A[A$age.lvl==4 & A$Marriage==2,]
temB1 <- B[B$age.lvl==4 & B$Marriage==1,]
temB2 <- B[B$age.lvl==4 & B$Marriage==2,]
temC1 <- C[C$age.lvl==4 & C$Marriage==1,]
temC2 <- C[C$age.lvl==4 & C$Marriage==2,]

min(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])->iii

c(dim(temA1)[1],
dim(temA2)[1],
dim(temB1)[1],
dim(temB2)[1],
dim(temC1)[1],
dim(temC2)[1])

iii

temA1<-temA1[1:iii,]
temA2<-temA2[1:iii,]
temB1<-temB1[1:iii,]
temB2<-temB2[1:iii,]
temC1<-temC1[1:iii,]
temC2<-temC2[1:iii,]

c(signif(t.test(temA1$Total.score,temA2$Total.score)[[3]],digit=2),
signif(t.test(temA1$Somatization,temA2$Somatization)[[3]],digit=2),
signif(t.test(temA1$Obsessive.compulsive,temA2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temA1$Interpersonal.sensitivity,temA2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temA1$Depression,temA2$Depression)[[3]],digit=2),
signif(t.test(temA1$Anxiety,temA2$Anxiety)[[3]],digit=2),
signif(t.test(temA1$Hostility,temA2$Hostility)[[3]],digit=2),
signif(t.test(temA1$Phobic.anxiety,temA2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temA1$Paranoid.ideation,temA2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temA1$Psychoticism,temA2$Psychoticism)[[3]],digit=2),
signif(t.test(temA1$Additional.items,temA2$Additional.items)[[3]],digit=2))

c(signif(t.test(temB1$Total.score,temB2$Total.score)[[3]],digit=2),
signif(t.test(temB1$Somatization,temB2$Somatization)[[3]],digit=2),
signif(t.test(temB1$Obsessive.compulsive,temB2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temB1$Interpersonal.sensitivity,temB2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temB1$Depression,temB2$Depression)[[3]],digit=2),
signif(t.test(temB1$Anxiety,temB2$Anxiety)[[3]],digit=2),
signif(t.test(temB1$Hostility,temB2$Hostility)[[3]],digit=2),
signif(t.test(temB1$Phobic.anxiety,temB2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temB1$Paranoid.ideation,temB2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temB1$Psychoticism,temB2$Psychoticism)[[3]],digit=2),
signif(t.test(temB1$Additional.items,temB2$Additional.items)[[3]],digit=2))

c(signif(t.test(temC1$Total.score,temC2$Total.score)[[3]],digit=2),
signif(t.test(temC1$Somatization,temC2$Somatization)[[3]],digit=2),
signif(t.test(temC1$Obsessive.compulsive,temC2$Obsessive.compulsive)[[3]],digit=2),
signif(t.test(temC1$Interpersonal.sensitivity,temC2$Interpersonal.sensitivity)[[3]],digit=2),
signif(t.test(temC1$Depression,temC2$Depression)[[3]],digit=2),
signif(t.test(temC1$Anxiety,temC2$Anxiety)[[3]],digit=2),
signif(t.test(temC1$Hostility,temC2$Hostility)[[3]],digit=2),
signif(t.test(temC1$Phobic.anxiety,temC2$Phobic.anxiety)[[3]],digit=2),
signif(t.test(temC1$Paranoid.ideation,temC2$Paranoid.ideation)[[3]],digit=2),
signif(t.test(temC1$Psychoticism,temC2$Psychoticism)[[3]],digit=2),
signif(t.test(temC1$Additional.items,temC2$Additional.items)[[3]],digit=2))
