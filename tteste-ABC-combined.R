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

signif(t.test(tem1$Somatization,tem2$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem4$Somatization)[[3]],digit=2)
signif(t.test(tem2$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem2$Somatization,tem4$Somatization)[[3]],digit=2)
signif(t.test(tem3$Somatization,tem4$Somatization)[[3]],digit=2)

signif(t.test(tem1$Obsessive.compulsive,tem2$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem2$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem2$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem3$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(tem1$Interpersonal.sensitivity,tem2$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem2$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem2$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem3$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(tem1$Depression,tem2$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem4$Depression)[[3]],digit=2)
signif(t.test(tem2$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem2$Depression,tem4$Depression)[[3]],digit=2)
signif(t.test(tem3$Depression,tem4$Depression)[[3]],digit=2)

signif(t.test(tem1$Anxiety,tem2$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem4$Anxiety)[[3]],digit=2)
signif(t.test(tem2$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem2$Anxiety,tem4$Anxiety)[[3]],digit=2)
signif(t.test(tem3$Anxiety,tem4$Anxiety)[[3]],digit=2)

signif(t.test(tem1$Hostility,tem2$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem4$Hostility)[[3]],digit=2)
signif(t.test(tem2$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem2$Hostility,tem4$Hostility)[[3]],digit=2)
signif(t.test(tem3$Hostility,tem4$Hostility)[[3]],digit=2)

signif(t.test(tem1$Phobic.anxiety,tem2$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem2$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem2$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem3$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)

signif(t.test(tem1$Paranoid.ideation,tem2$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem2$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem2$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem3$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)

signif(t.test(tem1$Psychoticism,tem2$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem4$Psychoticism)[[3]],digit=2)
signif(t.test(tem2$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem2$Psychoticism,tem4$Psychoticism)[[3]],digit=2)
signif(t.test(tem3$Psychoticism,tem4$Psychoticism)[[3]],digit=2)

signif(t.test(tem1$Additional.items,tem2$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem4$Additional.items)[[3]],digit=2)
signif(t.test(tem2$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem2$Additional.items,tem4$Additional.items)[[3]],digit=2)
signif(t.test(tem3$Additional.items,tem4$Additional.items)[[3]],digit=2)

tem1<-tem[tem$Sex==1,]
tem2<-tem[tem$Sex==2,]
dim(tem1)
dim(tem2)
tem1<-tem1[1:2824,]
tem2<-tem2[1:2824,]

signif(t.test(tem1$t,tem2$t)[[3]],digit=2)

signif(t.test(tem1$Somatization,tem2$Somatization)[[3]],digit=2)

signif(t.test(tem1$Obsessive.compulsive,tem2$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(tem1$Interpersonal.sensitivity,tem2$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(tem1$Depression,tem2$Depression)[[3]],digit=2)

signif(t.test(tem1$Anxiety,tem2$Anxiety)[[3]],digit=2)

signif(t.test(tem1$Hostility,tem2$Hostility)[[3]],digit=2)

signif(t.test(tem1$Phobic.anxiety,tem2$Phobic.anxiety)[[3]],digit=2)

signif(t.test(tem1$Paranoid.ideation,tem2$Paranoid.ideation)[[3]],digit=2)

signif(t.test(tem1$Psychoticism,tem2$Psychoticism)[[3]],digit=2)

signif(t.test(tem1$Additional.items,tem2$Additional.items)[[3]],digit=2)

tem0<-tem[tem$Education==0,]
tem1<-tem[tem$Education==1,]
tem2<-tem[tem$Education==2,]
tem3<-tem[tem$Education==3,]
tem4<-tem[tem$Education==4,]
dim(tem0)
dim(tem1)
dim(tem2)
dim(tem3)
dim(tem4)
tem0<-tem0
tem1<-tem1[1:123,]
tem2<-tem2[1:123,]
tem3<-tem3[1:123,]
tem4<-tem4[1:123,]

signif(t.test(tem0$t,tem1$t)[[3]],digit=2)
signif(t.test(tem0$t,tem2$t)[[3]],digit=2)
signif(t.test(tem0$t,tem3$t)[[3]],digit=2)
signif(t.test(tem0$t,tem4$t)[[3]],digit=2)
signif(t.test(tem1$t,tem2$t)[[3]],digit=2)
signif(t.test(tem1$t,tem3$t)[[3]],digit=2)
signif(t.test(tem1$t,tem4$t)[[3]],digit=2)
signif(t.test(tem2$t,tem3$t)[[3]],digit=2)
signif(t.test(tem2$t,tem4$t)[[3]],digit=2)
signif(t.test(tem3$t,tem4$t)[[3]],digit=2)

signif(t.test(tem0$Somatization,tem1$Somatization)[[3]],digit=2)
signif(t.test(tem0$Somatization,tem2$Somatization)[[3]],digit=2)
signif(t.test(tem0$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem0$Somatization,tem4$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem2$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem4$Somatization)[[3]],digit=2)
signif(t.test(tem2$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem2$Somatization,tem4$Somatization)[[3]],digit=2)
signif(t.test(tem3$Somatization,tem4$Somatization)[[3]],digit=2)

signif(t.test(tem0$Obsessive.compulsive,tem1$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem0$Obsessive.compulsive,tem2$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem0$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem0$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem2$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem2$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem2$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem3$Obsessive.compulsive,tem4$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(tem0$Interpersonal.sensitivity,tem1$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem0$Interpersonal.sensitivity,tem2$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem0$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem0$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem2$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem2$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem2$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem3$Interpersonal.sensitivity,tem4$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(tem0$Depression,tem1$Depression)[[3]],digit=2)
signif(t.test(tem0$Depression,tem2$Depression)[[3]],digit=2)
signif(t.test(tem0$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem0$Depression,tem4$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem2$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem4$Depression)[[3]],digit=2)
signif(t.test(tem2$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem2$Depression,tem4$Depression)[[3]],digit=2)
signif(t.test(tem3$Depression,tem4$Depression)[[3]],digit=2)

signif(t.test(tem0$Anxiety,tem1$Anxiety)[[3]],digit=2)
signif(t.test(tem0$Anxiety,tem2$Anxiety)[[3]],digit=2)
signif(t.test(tem0$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem0$Anxiety,tem4$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem2$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem4$Anxiety)[[3]],digit=2)
signif(t.test(tem2$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem2$Anxiety,tem4$Anxiety)[[3]],digit=2)
signif(t.test(tem3$Anxiety,tem4$Anxiety)[[3]],digit=2)

signif(t.test(tem0$Hostility,tem1$Hostility)[[3]],digit=2)
signif(t.test(tem0$Hostility,tem2$Hostility)[[3]],digit=2)
signif(t.test(tem0$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem0$Hostility,tem4$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem2$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem4$Hostility)[[3]],digit=2)
signif(t.test(tem2$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem2$Hostility,tem4$Hostility)[[3]],digit=2)
signif(t.test(tem3$Hostility,tem4$Hostility)[[3]],digit=2)

signif(t.test(tem0$Phobic.anxiety,tem1$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem0$Phobic.anxiety,tem2$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem0$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem0$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem2$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem2$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem2$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem3$Phobic.anxiety,tem4$Phobic.anxiety)[[3]],digit=2)

signif(t.test(tem0$Paranoid.ideation,tem1$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem0$Paranoid.ideation,tem2$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem0$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem0$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem2$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem2$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem2$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem3$Paranoid.ideation,tem4$Paranoid.ideation)[[3]],digit=2)

signif(t.test(tem0$Psychoticism,tem1$Psychoticism)[[3]],digit=2)
signif(t.test(tem0$Psychoticism,tem2$Psychoticism)[[3]],digit=2)
signif(t.test(tem0$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem0$Psychoticism,tem4$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem2$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem4$Psychoticism)[[3]],digit=2)
signif(t.test(tem2$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem2$Psychoticism,tem4$Psychoticism)[[3]],digit=2)
signif(t.test(tem3$Psychoticism,tem4$Psychoticism)[[3]],digit=2)

signif(t.test(tem0$Additional.items,tem1$Additional.items)[[3]],digit=2)
signif(t.test(tem0$Additional.items,tem2$Additional.items)[[3]],digit=2)
signif(t.test(tem0$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem0$Additional.items,tem4$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem2$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem4$Additional.items)[[3]],digit=2)
signif(t.test(tem2$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem2$Additional.items,tem4$Additional.items)[[3]],digit=2)
signif(t.test(tem3$Additional.items,tem4$Additional.items)[[3]],digit=2)


tem1<-tem[tem$Marriage==1,]
tem2<-tem[tem$Marriage==2,]
tem3<-tem[tem$Marriage==4,]

dim(tem1)
dim(tem2)
dim(tem3)
dim(tem4)
tem1<-tem1[1:84,]
tem2<-tem2[1:84,]

signif(t.test(tem1$t,tem2$t)[[3]],digit=2)
signif(t.test(tem1$t,tem3$t)[[3]],digit=2)
signif(t.test(tem2$t,tem3$t)[[3]],digit=2)

signif(t.test(tem1$Somatization,tem2$Somatization)[[3]],digit=2)
signif(t.test(tem1$Somatization,tem3$Somatization)[[3]],digit=2)
signif(t.test(tem2$Somatization,tem3$Somatization)[[3]],digit=2)

signif(t.test(tem1$Obsessive.compulsive,tem2$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem1$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)
signif(t.test(tem2$Obsessive.compulsive,tem3$Obsessive.compulsive)[[3]],digit=2)

signif(t.test(tem1$Interpersonal.sensitivity,tem2$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem1$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)
signif(t.test(tem2$Interpersonal.sensitivity,tem3$Interpersonal.sensitivity)[[3]],digit=2)

signif(t.test(tem1$Depression,tem2$Depression)[[3]],digit=2)
signif(t.test(tem1$Depression,tem3$Depression)[[3]],digit=2)
signif(t.test(tem2$Depression,tem3$Depression)[[3]],digit=2)

signif(t.test(tem1$Anxiety,tem2$Anxiety)[[3]],digit=2)
signif(t.test(tem1$Anxiety,tem3$Anxiety)[[3]],digit=2)
signif(t.test(tem2$Anxiety,tem3$Anxiety)[[3]],digit=2)

signif(t.test(tem1$Hostility,tem2$Hostility)[[3]],digit=2)
signif(t.test(tem1$Hostility,tem3$Hostility)[[3]],digit=2)
signif(t.test(tem2$Hostility,tem3$Hostility)[[3]],digit=2)

signif(t.test(tem1$Phobic.anxiety,tem2$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem1$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)
signif(t.test(tem2$Phobic.anxiety,tem3$Phobic.anxiety)[[3]],digit=2)

signif(t.test(tem1$Paranoid.ideation,tem2$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem1$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)
signif(t.test(tem2$Paranoid.ideation,tem3$Paranoid.ideation)[[3]],digit=2)

signif(t.test(tem1$Psychoticism,tem2$Psychoticism)[[3]],digit=2)
signif(t.test(tem1$Psychoticism,tem3$Psychoticism)[[3]],digit=2)
signif(t.test(tem2$Psychoticism,tem3$Psychoticism)[[3]],digit=2)

signif(t.test(tem1$Additional.items,tem2$Additional.items)[[3]],digit=2)
signif(t.test(tem1$Additional.items,tem3$Additional.items)[[3]],digit=2)
signif(t.test(tem2$Additional.items,tem3$Additional.items)[[3]],digit=2)
