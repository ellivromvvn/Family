library(car)

m1<-lm(formula = GWA ~ Year*K_Family*Q2, data = fam_data)
a<-Anova(m1, type = 3)

summary(a)

drop1(m1,.~.,test = "F")
attach(fam_data)
detach(fam_data)

table(K_Family)
(table(Q2)/99)*100

hist.GWA<-hist(GWA,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                              2.75, 3, 3.25))

hist.GWA

#GWA when grouped according to Year
Second<-GWA[Year=="2"]
Third<-GWA[Year=="3"]
SS<-summary(Second)
ST<-summary(Third)
hist.second<-hist(Second,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5))
hist.third<-hist(Third,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                              2.75, 3, 3.25))
#GWA when grouped according to Sex
Male<-GWA[Sex=="M"]
Female<-GWA[Sex=="F"]
SM<-summary(Male)
SF<-summary(Female)
hist.male<-hist(Male,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75,
                                3))
hist.female<-hist(Female,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                                  2.75, 3, 3.25))

#GWA when grouped according to Kinds of Family
Incomplete<-GWA[K_Family=="Incomplete"]
Complete<-GWA[K_Family=="Complete"]
SI<-summary(Incomplete)
SC<-summary(Complete)
hist.incomplete<-hist(Incomplete,breaks = c(1.5, 1.75, 2, 2.25,
                                           2.5, 2.75, 3))
hist.complete<-hist(Complete,breaks = c(1.25, 1.5, 1.75, 2, 2.25,
                                        2.5, 2.75, 3, 3.25))

#GWA when grouped according to Q1
Q1Yes<-GWA[Q1=="Yes"]
Q1No<-GWA[Q1=="No"]
SQ1Yes<-summary(Q1Yes)
SQ1No<-summary(Q1No)
hist.Q1Yes<-hist(Q1Yes,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                                  2.75, 3, 3.25))
hist.Q1No<-hist(Q1No,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5))

#GWA when grouped according to Q2
Q2Yes<-GWA[Q2=="Yes"]
Q2No<-GWA[Q2=="No"]
SQ2Yes<-summary(Q2Yes)
SQ2No<-summary(Q2No)
hist.Q2Yes<-hist(Q2Yes,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                                  2.75, 3))
hist.Q2No<-hist(Q2No,breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5,
                                2.75, 3, 3.25))
