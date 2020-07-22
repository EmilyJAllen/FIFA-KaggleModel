library(MASS)
library(car)
library(corrplot)
library(mice)
library(dplyr)
library(VIM)
library(stringr)
library(ggplot2)
library(leaps)

#L O A D     D A T A 
FIFA<- read.csv("FifaTrainNew.csv")
FIFA_test<- read.csv("FifaNoY.csv")


#C L E A N    F I F A ################################################################
FIFAc<-apply(X = FIFA[,21:46],MARGIN = 2, 
             FUN = function(x)as.numeric(str_extract(string = x,pattern = "\\d+(?=\\+)")))
FIFAc$weight_n<-sapply(FIFAc$Weight,FUN = function(x)as.numeric(str_extract(string = x,pattern = "\\d+(?=l)")))
FIFAcopy[,21:46]<-FIFAc 
FIFA<-FIFAcopy

#I N I T I A L   T R A I N I N G   A N D    T E S T I N G   D A T A###################
FIFA<- read.csv("FifaTrainNew.csv")
FIFA_test<- read.csv("FifaNoY.csv")
FIFAclean<-FIFA %>% filter(is.na(Club)==FALSE)

# C R O S S   V A L I D A T I O N #####################################################
set.seed(11111)
samplesize<-floor(0.7 * nrow(FIFAclean))
train_ind <- sample(seq_len(nrow(FIFAclean)), size = samplesize) 
train<-FIFAclean[train_ind,]
test<-FIFAclean[-train_ind,]

# A L L   N E W   V A R I A B L E S######################################################
test<-test%>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))
test<-test %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
test<- test %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))
test<- test %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
test<-test %>% mutate(famSpecial= as.factor(ifelse(Special>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
test<- test %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))


train<-train%>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))
train<-train %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
train<-train %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))
train<-train %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
train<-train %>% mutate(famSpecial= as.factor(ifelse(Special>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
train<- train %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))


FIFA<- FIFA %>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))
FIFA<- FIFA %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
FIFA<- FIFA %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))
FIFA<- FIFA %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFA<- FIFA %>% mutate(famSpecial= as.factor(ifelse(Special>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFA<- FIFA %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))
FIFA<-full_join(FIFA, clubwagemeans[,c(2,4)], by= "Club")
FIFA$rank2<- as.factor(FIFA$rank2)
FIFA<- FIFA %>% mutate(richclub = as.factor(ifelse(Club %in% richclub, 1,0)))
FIFA<- FIFA %>% mutate(extraspecial = as.factor(ifelse(Special>=2000, 1, 0)))
FIFA<- FIFA %>% mutate(unspecial = as.factor(ifelse(Special<=1000, 1, 0)))

FIFA_test<- FIFA_test %>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))
FIFA_test<- FIFA_test %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
FIFA_test<- FIFA_test %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))
FIFA_test<- FIFA_test %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFA_test<- FIFA_test %>% mutate(famSpecial= as.factor(ifelse(Special>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFA_test<- FIFA_test %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))
FIFA_test<-full_join(FIFA_test, clubwagemeans[,c(2,4)], by= "Club")
FIFA_test$rank2<- as.factor(FIFA_test$rank2)
FIFA_test<- FIFA_test %>% mutate(richclub = as.factor(ifelse(Club %in% richclub, 1,0)))
FIFA_test<- FIFA_test %>% mutate(extraspecial = as.factor(ifelse(Special>=2000, 1, 0)))
FIFA_test<- FIFA_test %>% mutate(unspecial = as.factor(ifelse(Special<=1000, 1, 0)))
FIFA_test$Club<-as.character(FIFA_test$Club)
FIFA_test<-FIFA_test %>% mutate(ClubSig= ifelse(Club %in% sigclubs, Club, "other"))
FIFA_test$ClubSig<-as.factor(FIFA_test$ClubSig)

FIFAclean<-FIFA
FIFAclean<-FIFAclean %>% filter(is.na(Club)==FALSE)
FIFAclean<- FIFAclean %>% mutate(extraspecial = as.factor(ifelse(Special>=2000, 1, 0)))
FIFAclean<- FIFAclean %>% mutate(unspecial = as.factor(ifelse(Special<=1000, 1, 0)))
FIFAclean$Club<-as.character(FIFAclean$Club)
FIFAclean<-FIFAclean %>% mutate(ClubSig= ifelse(Club %in% sigclubs, Club, "other"))
FIFAclean$ClubSig<-as.factor(FIFAclean$ClubSig)
FIFAclean<- FIFAclean %>% mutate(WageNewT = log(WageNew)^1.7)
FIFAclean<- FIFAclean %>% mutate(ShotPowerT= (ShotPower)^(2))
FIFAclean<- FIFAclean %>% mutate(OverallT = (Overall)^(2.5))
FIFAclean<- FIFAclean %>% mutate(LongPassingT = (LongPassing)^(2))
FIFAclean<- FIFAclean %>% mutate(PotentialT = (Potential)^(0.75))
FIFAclean<- FIFAclean %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))

FIFAclean<- FIFAclean %>% mutate(age_outlier = as.factor(ifelse((Age>34 | Age <22), 1, 0)))
FIFAclean<- FIFAclean %>% mutate(LS_clean = ifelse(is.na(LS), mean(LS, na.rm=TRUE), LS))

FIFAclean<- FIFAclean %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
FIFAclean<- FIFAclean %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))
FIFAclean<- FIFAclean %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFAclean<- FIFAclean %>% mutate(famSpecial= as.factor(ifelse(Special>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))
FIFAclean<- FIFAclean %>% mutate(famous= as.factor(ifelse(International.Reputation==4 | International.Reputation==5, 1, 0)))
FIFAclean<- FIFAclean %>% mutate(age_outlier = as.factor(ifelse((Age>34 | Age <22), 1, 0)))
FIFAclean<- FIFAclean %>% mutate(LS_clean = ifelse(is.na(LS), mean(LS, na.rm=TRUE), LS))
avg<-rowSums(FIFAclean[,21:46])/(26)
FIFAclean$AVG<-avg

#S T R A T E G I E S   T O   R E F I N E   C L U B #########################
############################################################################
  # restructuring Clubs variable to a factor with fewer levels

#T R Y  0 identify only clubs that stand out in graph with very high and very low pay
sigclubs<- c("FC Union Berlin", "FSV Mainz 05", "Ajax", "Ahli", "Al Hilal", "Al Ittihad",
            "Al Nassr", "Arsenal", "AS Monaco", "AS Saint-<c9>tienne", 
            "Aston Villa", "Atl<e9>tico Madrid", "Atl<e9>tico Mineiro", "Bayer 04 Leverkusen",
            "Be?ikta? JK", "Blackburn Rovers", "Bologna ", "Bournemouth",
            "Brentford ", "Brighton & Hove Albion", "Bristol City", "Burnley", "Cardiff City",
            "CD Legan<e9>s", "Celtic", "Chelsea", "Chicago Fire", "Club Am<e9>rica",
            "Club Le<f3>n", "Cruz Azul", "Cruzeiro", "Crystal Palace", "DC United",
            "Deportivo Alav<e9>s", "Derby County")
FIFAclean<- FIFAclean %>% mutate(ClubSig= ifelse(as.character(Club) %in% sigclubs, Club, "other"))
FIFAclean$ClubSig<-as.factor(FIFAclean$ClubSig)



#T R Y   #1 clubs that overpay based on >2 SD away from average of wage for international rep == 1
FIFAclean%>% filter(International.Reputation==1) %>% summarize(avgWage= mean(WageNew))
overpayvec<-FIFAclean%>% filter(International.Reputation==1) %>% select(WageNew, Club) %>%
  filter(((WageNew-mean(WageNew))/sdu(WageNew))>(2))
unique(overpayvec$Club)
FIFAclean<-FIFAclean%>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))

#T R Y   #2 group clubs into 14 groups by average wage
clubwagemeans<-FIFAclean%>%group_by(Club)%>%summarize(avg=mean(WageNew))%>%arrange(desc(avg))
clubwagemeans<-cbind(index=1:651, clubwagemeans)
clubwagemeans<-clubwagemeans%>% 
  mutate(rank2= ifelse(avg<1300, 1,
                       ifelse(avg>1300&&avg<1700, 2,
                              ifelse(avg>1700&& avg <2300, 3,
                                     ifelse(avg>2300 && avg<2840, 4,
                                            ifelse(avg>2840 && avg<3400,5,
                                                   ifelse(avg>3400 && avg<4000,6,
                                                          ifelse(avg>4000 && avg<4700,7,
                                                                 ifelse(avg>4700 && avg<5500,8,
                                                                        ifelse(avg>5500 && avg<6800,9,
                                                                               ifelse(avg>6800 && avg<9400,10,
                                                                                      ifelse(avg>9400&&avg<13000,11,
                                                                                             ifelse(avg>13000 && avg<18000,12,
                                                                                                    ifelse(avg>18000 && avg<27000, 13,
                                                                                                           ifelse(avg>27000 && avg<200000,14)))))))))))))))


FIFAclean<-full_join(FIFAclean, clubwagemeans[,c(2,4)], by= "Club")
FIFAclean$rank2<- as.factor(FIFAclean$rank2)

#T R Y   #3 clubs that overpay based on 2 SD over mean wage for each value of international reputation
  #investigate value by value and manually add to vector richclub
payClub1<-FIFAc%>% filter(International.Reputation==c(1)) %>% group_by(Club) %>% summarize(mean_wage = mean(WageNew))
avgPC1<-mean(payClub1$mean_wage)
sdPC1<-sd(payClub1$mean_wage)
sds_PC1<-sapply(payClub1$mean_wage, function(x)(x-avgPC1)/sdPC1) 
which((sds_PC1)>=2)

payClub2<-FIFAc%>% filter(International.Reputation==c(2)) %>% group_by(Club) %>% summarize(mean_wage = mean(WageNew))
avgPC2<-mean(payClub2$mean_wage)
sdPC2<-sd(payClub2$mean_wage)
sds_PC2<-sapply(payClub2$mean_wage, function(x)(x-avgPC2)/sdPC2) 
sizeof(which(abs(sds_PC2)>=2))

payClub3<-FIFAc%>% filter(International.Reputation==c(3)) %>% group_by(Club) %>% summarize(mean_wage = mean(WageNew))
avgPC3<-mean(payClub3$mean_wage)
sdPC3<-sd(payClub3$mean_wage)
sds_PC3<-sapply(payClub3$mean_wage, function(x)(x-avgPC3)/sdPC3) 

payClub4<-FIFAc%>% filter(International.Reputation==c(4)) %>% group_by(Club) %>% summarize(mean_wage = mean(WageNew))
avgPC4<-mean(payClub4$mean_wage)
sdPC4<-sd(payClub4$mean_wage)
sds_PC4<-sapply(payClub4$mean_wage, function(x)(x-avgPC4)/sdPC4) 

payClub<-FIFAc%>%group_by(Club) %>% summarize(mean_wage = mean(WageNew))
avgPC<-mean(payClub$mean_wage)
sdPC<-sd(payClub$mean_wage)
sds_PC<-sapply(payClub$mean_wage, function(x)(x-avgPC)/sdPC) 
which((sds_PC)<=-2)

richclubs<-c( "FC Barcelona", "Arsenal", "Chelsea", "Everton", "FC Bayern M<fc>nchen", 
             "Juventus", "Liverpool", "Manchester City", "Manchester United", "Milan", "Paris Saint-Germain",
             "Real Madrid", "Tottenham Hotspur", "Inter", "Napoli")
FIFAclean<- FIFAclean %>% mutate(richclub = as.factor(ifelse((Club %in% richclubs)==TRUE, 1 ,0)))

#T R Y   #4 rank clubs by their ranking according to FiveThirtyEight's global rankings 
  # https://projects.fivethirtyeight.com/global-club-soccer-rankings/
  # only rank based on the top 55 teams + add previous "overpaying" teams that are missing
rankings<- global_rankings %>% filter(rank<55) %>% select(rank, name)
rankings<- rankings%>% rename(Club=name)
rankings$Club<- as.character(rankings$Club)
Club<- c(rankings$Club, "FC Barcelona", "FC Bayern M<fc>nchen", "West Ham United", "Inter",
         "Milan", "Atl<e9>tico Madrid", "Olympique Lyonnais")
Club<- c(rankings$Club, "Morecambe", "Cambridge United", "Stevenage", "Macclesfield Town",
         "Rangers FC", "Newport County", "Carlisle United", "Atlanta United")
Club<- c(rankings$Club,"AS Monaco", "Roma")
rankings<-data.frame(rank= c(1:71), Club = Club)
FIFAclean<- FIFAclean %>% mutate(ClubNew= as.factor(ifelse(Club %in% rankings$Club, Club, "other")))
FIFAclean$ClubNew<-factor(FIFAclean$ClubNew)

#T R Y   #5 only keep clubs that are significant in a SLR WageNew ~ Club
clubreg<-lm(WageNew~as.character(Club), data=FIFAclean)%>% summary()
clubcoeffs<-clubreg$coefficient
colnames(clubcoeffs)<-c("Estimate", "StdError", "tvalue", "pvalue")
clubcoeffs <- as.data.frame(clubcoeffs)
clubs<-row.names(clubcoeffs)
clubs2<-sapply(clubs,FUN = function(x)str_remove(string = x,pattern = ".*(?=\\))"))
clubs2<-sapply(clubs2,FUN = function(x)str_remove(string = x,pattern = "\\)"))
clubs2<-as.vector(clubs2)
clubs2
clubcoeffs<-cbind(clubcoeffs, clubs2)
clubcoeffs<-clubcoeffs[-1,]
sigcoeffs<-clubcoeffs %>% filter(pvalue<=0.05)
sigclubs<-sigcoeffs[,5]
sigclubs<-as.character(sigclubs)

FIFA$Club<-as.character(FIFA$Club)
FIFA<-FIFA %>% mutate(ClubSig= ifelse(Club %in% sigclubs, Club, "other"))
FIFA$ClubSig<-as.factor(FIFA$ClubSig)

