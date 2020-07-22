#CURRENT MODEL
current<-modF

#checking correlations ####
cor(FIFAclean[,c("WageNew",
            "International.Reputation", "Overall", "Special", "AVG")], 
    use = "pairwise.complete.obs")
corrplot.mixed(cor(FIFAclean[,c("WageNew",
                           as.numeric(International.Reputation), "Overall", "Special", "AVG", "AVG2", 
                                                    "Reactions", "Skill.Moves")], use="pairwise.complete.obs"))

#Single variable regressions ######
lm(WageNew~rank.y, data=FIFAclean)%>% summary()
lm(WageNew~IR_new, data=FIFAclean)%>% summary()
lm(WageNewT~Position, data=FIFAclean)%>% summary()
lm(WageNew~unspecial, data=FIFAclean)%>% summary()
lm(WageNew~Age, data=FIFAclean)%>% summary()

#Categorical tests################
ggplot(data=FIFAclean, aes(x=OverallT, y=WageNew, color= as.factor(International.Reputation)))+
  geom_point()+ theme_classic()+ ggtitle("WageNew by Overall^2 and International Reputation")+
  scale_color_discrete(name="Intl Rep")
ggplot(data=FIFAclean, aes(x=OverallT, y=WageNew))+
  geom_point(color="deepskyblue")+ theme_classic()+ggtitle("Wage By Overall^2.5")
ggplot(data=FIFAclean, aes(x=PotentialT, y=WageNew))+
  geom_point(color="orange")+ theme_classic()+ggtitle("Wage By Potential^.75")

ggplot(data=FIFAc, aes(x=unfamGood, y=WageNew))+
  geom_boxplot(size=1)+ ggtitle("Wage by Goodness and Unfamousness")+ theme_classic()
    
as.character(clubcoeffs[,0])
order(unique(as.character(FIFA$Club)))
club_coefs<-cbind(clubcoeffs[-1,0], clubcoeffs[-1,4])
club_coefs<-as.data.frame(club_coefs)
clubcoeffsvec<-club_coefs %>% filter(V1<=0.05) %>%rownames()

#t tests ############################
t.test(WageNew~Real.Face, data = FIFAc) #relationship is significant
t.test(WageNew~(unspecial), data=FIFAclean)

#interaction plots###################
interaction.plot(x.factor = FIFA$Potential, response = FIFA$WageNew,
                 trace.factor = FIFA$unspecial) #interaction?

interaction.plot(x.factor = FIFAclean$Overall, response = FIFAclean$WageNew,
                 trace.factor = as.factor(FIFAclean$International.Reputation),
                 col= c("red", "orange", "limegreen", "blue", "purple"),
                 trace.label = "Intl Rep" ) #interaction?
interaction.plot(x.factor = FIFAc$Potential, response = FIFAc$WageNew,
                 trace.factor = FIFAc$famGood )
interaction.plot(x.factor = FIFAclean$OverallT, response = FIFAclean$WageNewT,
                 trace.factor = as.factor(FIFAclean$unspecial),col = c("deepskyblue", "orange"),
                 trace.label = "Unspecial")

ggplot(aes(x=OverallT, y=WageNewT, color=unspecial), data=FIFAclean)+geom_point()+
  
ggplot(aes(x=Potential, y=WageNew, color=unfamGood), data=FIFA)+geom_point()

#AIF/BIF tests ########################
current_inc<-lm(WageNewT ~ OverallT*unspecial+ PotentialT  +
                Real.Face  + ShotPowerT + I(LS^2)+
                as.factor(Skill.Moves),
                data=FIFAclean) 
current_inc<-lm(WageNewT ~ OverallT+ PotentialT  +
                  Real.Face+ I(LS^2),
                data=FIFAclean) 

backBIC<- step(current_inc, direction = "backward", data=FIFA, k=log(12745))
backAIC<-step(current_inc, direction = "backward", data=FIFA)

mint<-lm((log(WageNew)^1.7)~1, data=FIFA)
forwardBIC <- step(mint, scope=list(lower = ~1, upper=  ~ OverallT + PotentialT  +
                                      Real.Face  + LongPassingT +ShotPowerT + 
                                      Skill.Moves+ extraspecial+ unspecial, 
                                    direction="forward",
                                    data=FIFAclean), k=log(nrow(FIFAclean)))

X<- cbind(FIFAclean$OverallT,FIFAclean$PotentialT,
          (FIFAclean$ShotPower)^2, (FIFAclean$LongPassing)^2,FIFAclean$Real.Face,
          FIFAclean$Skill.Moves, FIFAclean$famGood, FIFAclean$unfamGood, FIFAclean$unspecial)
b<-regsubsets(as.matrix(X),log(FIFAclean$WageNew))
rs<-summary(b)
plot(rs$adjr2)

#check assumptions#########################
vif(current)

mmps(current_inc)
avPlots(current_inc)

#Diagnostics###############################
summary(current)

anova(current)
aov(current) #if applicable

par(mfrow=c(2,3))
par(mfrow=c(1,1))
plot(current,1:6)

#Transformations #########################
inverseResponsePlot(current)

summary(powerTransform(cbind(WageNew,Overall,Potential, LongPassing,
                             ShotPower)~1,data=FIFA))

#remove leverage##########################
badlev<-which((hatvalues(current))>=((2*11874)/12577) && abs(rstandard(current))>2)
which(hatvalues(current)>=(2*11874)/12577)

#predict##################################
curPredict<- predict(current, FIFA_test)
curPredict<-exp(curPredict^(1/1.7))
qplot(curPredict, test$WageNew)+geom_smooth(method = "lm")
summary(lm(test$WageNew~curPredict))
summary(curPredict)
summary(FIFAclean$WageNew)

t.test(curPredict, train$WageNew)

#check NAs################################
which(is.na(curPredict))
curPredict<-ifelse(is.na(curPredict), mean(FIFAclean$WageNew), curPredict)
which(is.na(curPredict))


#write model to new file##################
Predict_table<- cbind("Ob"= c(1:5462), "WageNew"= curPredict)

write.csv(Predict_table, "prediction_Lec1P", row.names = FALSE)
write.csv(FIFAclean, "FIFAclean.csv")
