#New variables

#Contraceptives
ID_Number <- h135$DUPERSID
Hormonal_contraceptives <- c(h135$TC1S1_1, h135$TC2S1_1)
contraceptives <- data.frame(ID_Number, Hormonal_contraceptives)
contraceptives$Hormonal_contraceptives <- ifelse(contraceptives$Hormonal_contraceptives==102,1,0)

contraceptives.agg <- aggregate(Hormonal_contraceptives ~ ID_Number, contraceptives, max)


#Breast Cancer
ID_Number <- h137$DUPERSID
Breast_cancer <- h137$CCCODEX
cancer <- data.frame(ID_Number, Breast_cancer)
cancer$Breast_cancer <- ifelse(cancer$Breast_cancer=="024",1,0)

cancer.agg <- aggregate(Breast_cancer ~ ID_Number, cancer, max)

#Demographics (AGE & SEX)
ID_Number <- h138$DUPERSID
Sex <- h138$SEX
Race <- h138$RACEX
Age <- c(h138$AGE31X, h138$AGE42X, h138$AGE53X, h138$AGE10X)
Demographics <- data.frame(ID_Number, Sex, Race, Age)
Age_Included <- subset(Demographics, Demographics$Age >15 & Demographics$Age <60)
Women <- subset(Demographics, Demographics$Sex == 2)
Demographics_Included <- merge(Age_Included, Women)


#FULL MERGE
dtfinal <- merge(contraceptives.agg, cancer.agg, all= TRUE)
dtfinal_2 <- merge(dtfinal, Demographics_Included, by="ID_Number")
dtfinal_2 <- dtfinal_2$Hormonal_contraceptives %>% replace_NA(0)

library(dplyr)

df1 <- df1 %>%
  mutate(myCol1 = if_else(is.na(myCol1), 0, myCol1))
#Analysis

#Frequency
table(dtfinal_2$Hormonal_contraceptives)
easy_table<-cbind(table(dtfinal_2$Hormonal_contraceptives))
table(dtfinal_2$Breast_cancer)
easy_table<-cbind(table(dtfinal_2$Breast_cancer))

table1<-table(dtfinal_2$Hormonal_contraceptives,dtfinal_2$Breast_cancer)

prop.table(table1)
chisq.test(table1)
summary(table1)

#Graphs
library(ggplot2)
baseplot<-ggplot(dtfinal_2, aes(x=Hormonal_contraceptives, y=Breast_cancer)) + geom_point() 
baseplot+geom_smooth(method="lm")

Age_diagnosed <- subset(h137, h137$AGEDIAG> 15 & h137$AGEDIAG< 60)
