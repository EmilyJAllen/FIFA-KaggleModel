library(MASS)
library(car)
library(corrplot)
library(mice)
library(dplyr)
library(VIM)
library(stringr)
library(ggplot2)
library(leaps)

#############################################################
#### S O M E    T R A N S F O R M A T I O N S
FIFA<- FIFA %>% mutate(WageNewT = log(WageNew)^1.7)
FIFA<- FIFA %>% mutate(OverallT = (Overall)^(2.5))
FIFA<- FIFA %>% mutate(LongPassingT = (LongPassing)^(2))
FIFA<- FIFA %>% mutate(PotentialT = (Potential)^(0.75))
FIFA<- FIFA %>% mutate(ShotPowerT= (ShotPower)^(2))


FIFA_test<- FIFA_test %>% mutate(OverallT = (Overall)^(2.5))
FIFA_test<- FIFA_test %>% mutate(LongPassingT = (LongPassing)^(2))
FIFA_test<- FIFA_test %>% mutate(PotentialT = (Potential)^(0.75))
FIFA_test<- FIFA_test %>% mutate(ShotPowerT= (ShotPower)^(2))

# P O T E N T I A L   D U M M Y   V A R I A B L E S###########################
############################################################################

#####Recreating International reputation####
ggplot(data=FIFAclean, aes(International.Reputation))+geom_histogram(stat="count")


FIFAclean<-FIFAclean %>% mutate(IR_new= as.factor(ifelse(International.Reputation==5,4,International.Reputation)))
FIFAclean<-FIFAclean %>% mutate(IR_new= as.factor(ifelse(WageNew<=25000, 0, IR_new)))

ggplot(data=FIFAclean%>% filter(IR_new==2), aes(WageNew))+geom_histogram()
ggplot(data=FIFAclean, aes(Work.Rate,WageNew))+geom_point()



######position#################################
FIFAclean<- FIFAclean%>%mutate(Forward= as.factor(ifelse(Position=="LF"|Position=="RF", 1,0)))


#######overpaying clubs above 2 SDs ###########
FIFAclean%>% filter(International.Reputation==1) %>% summarize(avgWage= mean(WageNew))
overpayvec<-FIFAclean%>% filter(International.Reputation==1) %>% select(WageNew, Club) %>%
  filter(((WageNew-mean(WageNew))/sd(WageNew))>(2))
unique(overpayvec$Club)
FIFAclean<-FIFAclean%>% mutate(overpayclub_full= as.factor(ifelse(Club %in% unique(overpayvec$Club), 1, 0)))

#########unfamous+ good and famous + good######
FIFAclean<- FIFAclean %>% mutate(unfamGood= as.factor(ifelse(Overall>=80 & (International.Reputation==1 | International.Reputation==2 | International.Reputation==3),1,0)))
FIFAclean<- FIFAclean %>% mutate(famGood= as.factor(ifelse(Overall>=80 & (International.Reputation==4 | International.Reputation==5),1,0)))

#########Real.Face == True AND high rank#######
FIFAclean<- FIFAclean %>% mutate(realfaceHighrank=as.factor(ifelse(Real.Face=="Yes" & rank2==2, 1, 0)))



# M O D E L S #############################################################
###########################################################################
mod20<- lm(WageNew~ clubcat, data=FIFAc)

####################################
mod21<-lm(log(WageNew)~(avg)+I(Overall^2.5)+I(Potential^0.5),
          data=FIFAc)
summary(lm(WageNew~Real.Face,data=FIFA ))


################
mod23<-lm(log(WageNew) ~I(Overall^2.5)+ International.Reputation + I(Reactions^(2.00))+ I(Potential^(0.5)) 
          + rank2+AVG+I(LongPassing^(2))+Real.Face*unfamGood, data=FIFAclean)

mod23_red<-lm(log(WageNew) ~I(Overall^2.5) + I(Potential^(0.5)) 
          + rank2+I(LongPassing^(2))+Real.Face*unfamGood, data=FIFAclean)

###############
mod24_full<-lm(log(WageNew) ~I(Overall^2.5)+ I(Potential^(0.50))+
                I(Reactions^(2.00)) + rank2*Real.Face+ 
                 I(Vision^(1.5)) + 
                I(ShotPower^(2.0)) + I(LongPassing^(2)) + 
               Preferred.Foot+ IR_new + unfamGood,data=FIFAclean)



mod24_full<-lm(log(WageNew) ~I(Overall^2.5)+overpayclub_full+ I(Potential^(0.50))+realfaceHighrank+
                I(Vision^(1.5))  + I(LongPassing^(2)) + 
                unfamGood+ Club
              ,data=FIFAclean)
mod24_red<-lm(log(WageNew) ~I(Overall^2.5)*overpayclub_full+ I(Potential^(0.50))*realfaceHighrank+
                I(Vision^(1.5))  + I(LongPassing^(2)) + 
                Club
              ,data=FIFAclean)

mod24_red<-lm(log(WageNew) ~I(Overall^2.5)*overpayclub_full+ I(Potential^(0.50))*realfaceHighrank+
                I(Vision^(1.5))  + I(LongPassing^(2)) + 
                Club
              ,data=train)

########################
modE<-lm(log(WageNew) ~OverallT+ PotentialT+unfamGood+Real.Face+
                Skill.Moves+famGood+rank2 + richclub
              ,data=FIFAclean) # t test: 0.1497 R^2: 0.8604
modE<-lm(WageNewT ~ OverallT + Interceptions + LongPassingT +
           extraspecial+Age + Real.Face + realfaceHighrank + unfamGood+
           Club, data=FIFAclean) #R^2: .9026, 

modR <- lm(log(WageNew) ~ I(Overall^(2.5))  + Club +  I(LongPassing^(2)) + I(Vision^(1.5)) 
           + I(sqrt(Potential)) + Real.Face, data = train)

modR <- lm(log(WageNew) ~ I(Overall^(2.5))+Real.Face +  I(LongPassing^(2)) + I(Vision^(1.5)) 
           + I(sqrt(Potential)) *unfamGood+
             +famGood+realfaceHighrank+ Club, data = train)

modR <- lm(log(WageNew) ~ I(Overall^(2.5))+Real.Face +  I(LongPassing^(2)) + I(Vision^(1.5)) 
           + I(sqrt(Potential)) +unfamGood+ famSpecial+
             +realfaceHighrank+ Club, data = FIFAclean)

modR <- lm(I(log(WageNew)^1.5) ~ I(Overall^(2.5))+ Interceptions+  I(LongPassing^(2)) +
           I((Potential)^0.5) + Age+ unfamGood+ famSpecial+ Real.Face +
             realfaceHighrank, data = FIFAclean)

modR_red1<-lm(WageNewT ~ OverallT + Interceptions + LongPassingT +
                PotentialT + Age + Real.Face + realfaceHighrank + 
                Club, data=FIFA)

modR <- lm(log(WageNew) ~ I(Overall^(2.5))  + I(Vision^(1.5))  + 
             I(sqrt(Potential)) + I(LS^(2))  + Real.Face+ as.factor(Club) ,  data = FIFAclean)

modC<-lm(WageNewT ~I(Overall^2.5)+ I(Potential^(0.50))+
           Skill.Moves+unfamGood+famGood+realfaceHighrank+Club
         ,data=FIFAclean)
modC<-lm(WageNewT ~I(Overall^2.5)+ I(Potential^(0.50))+
           Skill.Moves+unfamGood+famGood+unspecial+
           Club
         ,data=FIFAclean) #R^2: .902, bad diagnostics

modC<-lm(WageNewT ~I(Overall^2.5)+ I(Potential^(0.50))+LongPassingT+
           Real.Face+ famGood+extraspecial+Club
         ,data=FIFAclean)


modF<-lm(WageNewT ~ OverallT*unspecial + PotentialT*Real.Face  + 
           LongPassingT +ShotPowerT + 
           as.factor(Skill.Moves)+ (famGood) + (unfamGood) +
           as.factor(ClubSig), data=FIFA) #better linearity and random errors but poor normality


modF<-lm(WageNewT ~ OverallT*unspecial + PotentialT+Real.Face  + 
           LongPassingT + as.factor(Skill.Moves) + 
           ClubNew, data=FIFAclean) 


modF<-lm(WageNewT ~ OverallT*unspecial + PotentialT+Real.Face  + ShotPowerT+ 
           as.factor(Skill.Moves)+ as.factor(Club),
         weights = (Potential*Overall*ShotPower)^2 , 
         data=FIFAclean) 

modF<-lm(WageNewT ~ OverallT*unspecial + PotentialT+Real.Face  + 
           ShotPowerT+
           as.factor(Club),weights = zscore_weight , data=FIFAclean) 


mean_Overall<- mean(FIFAclean$OverallT, na.rm = TRUE)
sd_Overall<- sd(FIFAclean$OverallT, na.rm = TRUE)
zscore_weight<- sapply(FIFAclean$OverallT, function(x)abs((x-mean_Overall)/sd_Overall)) 




#shouldnt use Reactions because of multicol. 
#dont include extraspecial with all the others in modF
#back BIC removes fam and unfamgood
