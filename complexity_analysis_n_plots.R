

library(cjoint)
library(foreign)
library(texreg)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(gridExtra)
library(coefplot)
#library(readstata13)
library(plyr)
library(FindIt)


setwd("C:/Users/André Laroze/Dropbox/CESS-Santiago/Denise/Complexity")

rm(list=ls())
v<-"_06June2017"





#load("conjoint_data.RData")

#df<-read.dta13("conjoint_data_merged.dta")

load("all_data.RData")


###################
### Data Management
###################

names(cjoint.data)

cjoint.data.r<-cjoint.data
cjoint.data.r<-cbind(cjoint.data.r, df[, c("partisan", "crimedum1", "crimedum3", "jobsdum1", "jobsdum3", "garbagedum1", "garbagedum3")])



### Renaming variables for simplicity in data analysis and coding
names(cjoint.data.r) <- c("ID", "contest_number", "garbage", "crime", "jobs", "education", "ocupation", "gender"
                        , "party_aff", "race", "age", "chosen_cand", "cand_eval", "time_spent", "time_spend", 
                          "partisan", "crimedum1", "crimedum3", "jobsdum1", "jobsdum3", "garbagedum1", "garbagedum3")

unique(cjoint.data.r$crime)

cjoint.data.r$garbage<-gsub(" ", "_", cjoint.data.r$garbage, fixed = TRUE)
cjoint.data.r$crime<-gsub(" ", "_", cjoint.data.r$crime, fixed = TRUE)
cjoint.data.r$jobs<-gsub(" ", "_", cjoint.data.r$jobs, fixed = TRUE)
cjoint.data.r$education<-gsub(" ", "_", cjoint.data.r$education, fixed = TRUE)
cjoint.data.r$ocupation<-gsub(" ", "_", cjoint.data.r$ocupation, fixed = TRUE)
cjoint.data.r$race<-gsub(" ", "_", cjoint.data.r$race, fixed = TRUE)

### Eliminating useless variables (all missing values)
cjoint.data.r$time_spent<-NULL

### Set as factor for level change
names<-c("garbage", "crime", "jobs", "education", "party_aff", "race", "gender")
cjoint.data.r[,names] <- lapply(cjoint.data.r[,names] , factor)


# numeric code for variables, for implicity in recoding
cjoint.data.r$crime_f<-as.numeric(cjoint.data.r$crime) # 1 no change, 2 decrease, 3 increase
cjoint.data.r$jobs_f<-as.numeric(cjoint.data.r$jobs) # 1 no change, 2 decrease, 3 increase
cjoint.data.r$garbage_f<-as.numeric(cjoint.data.r$garbage) # 1 no change, 2 decrease, 3 increase

## high crime, low jobs
cjoint.data.r$cj_neg<-0
cjoint.data.r$cj_neg[cjoint.data.r$crime_f==3 & cjoint.data.r$jobs_f==2 ]<-1

## low crime, high jobs
cjoint.data.r$cj_pos<-0
cjoint.data.r$cj_pos[cjoint.data.r$crime_f==2 & cjoint.data.r$jobs_f==3 ]<-1

## Equal crime and garbage
cjoint.data.r$eq_cg<-0
cjoint.data.r$eq_cg[cjoint.data.r$crime_f==3 & cjoint.data.r$garbage_f==2| cjoint.data.r$crime_f==2 & cjoint.data.r$garbage_f==3]<-1
cjoint.data.r$eq_cg[cjoint.data.r$crime_f==1 & cjoint.data.r$garbage_f==1]<-1

# Equal jobs and garbage
cjoint.data.r$eq_jg<-0
cjoint.data.r$eq_jg[cjoint.data.r$jobs_f==cjoint.data.r$garbage_f]<-1



### Setting baseline categories
cjoint.data.r <- within(cjoint.data.r, garbage <- relevel(garbage, ref = "Declined_considerably"))
cjoint.data.r <- within(cjoint.data.r, crime <- relevel(crime, ref = "Considerable_increase_in_murder_rate"))
cjoint.data.r <- within(cjoint.data.r, jobs <- relevel(jobs, ref = "Significant_decline_in_private_sector_jobs"))
cjoint.data.r <- within(cjoint.data.r, education <- relevel(education, ref = "Graduated_from_high_school"))
cjoint.data.r <- within(cjoint.data.r, party_aff <- relevel(party_aff, ref = "Independent"))
cjoint.data.r <- within(cjoint.data.r, race <- relevel(race, ref = "White"))
cjoint.data.r <- within(cjoint.data.r, gender <- relevel(gender, ref = "Male"))

#####
#####################
### Coeficients plots
#####################
#####

##########################
#####  Candidate Selection
##########################

####################
###### Causal Anova
#####################

### ordering factors
cjoint.data.r$jobs <- factor(cjoint.data.r$jobs,ordered=TRUE,
                             levels=c("Significant_increase_in_new_private_sector_jobs","No_change_in_private_sector_jobs",
                                      "Significant_decline_in_private_sector_jobs"))

cjoint.data.r$crime <- factor(cjoint.data.r$crime,ordered=TRUE,
                             levels=c("Considerable_decrease_in_murder_rate","Did_not_change",
                                      "Considerable_increase_in_murder_rate"))

cjoint.data.r$garbage <- factor(cjoint.data.r$garbage,ordered=TRUE,
                             levels=c("Improved_considerably","Did_not_change",
                                      "Declined_considerably"))


cv.fit <- cv.CausalANOVA(chosen_cand~  crime + jobs + garbage,
                         data=cjoint.data.r,
                         pair.id=cjoint.data.r$contest_number,
                         diff=T, nway=3, seed=236665)  ### Different seeds suggest 0.7 or 1 as the cost

cv.fit
plot(cv.fit)

fit <- CausalANOVA(chosen_cand~  crime + jobs + garbage,
                   data=cjoint.data.r,
                   pair.id=cjoint.data.r$contest_number,diff=TRUE,
                   nway=3,
                   cost=0.85, ### Different seeds suggest 0.7 or 1 as the cost, so I've gone 0.85 as the midpoint
                   select.prob=F,boot=500, seed=2696333)

summary(fit)


plot(fit,fac.name=c("crime","jobs"))


### Alternative LM model

lm1<-lm(chosen_cand~ garbage*jobs*crime , data=cjoint.data.r)
summary(lm1)



########################
#### Subseting approach
lm1<-lm(chosen_cand~ education + race + gender + party_aff + garbage  , data=subset(cjoint.data.r, cj_neg==1 ))
summary(lm1)

lm2<-lm(chosen_cand~ education + race + gender + party_aff + garbage  , data=subset(cjoint.data.r, cj_pos==1 ))
summary(lm2)

lm3<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crime + garbage  , data=subset(cjoint.data.r, eq_cg==1))
summary(lm3)

lm3.d<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crime + garbage  , data=subset(cjoint.data.r, eq_cg==0))
summary(lm3.d)

lm4<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crime + garbage  , data=subset(cjoint.data.r, eq_jg==1))
summary(lm4)

lm4.d<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crime + garbage  , data=subset(cjoint.data.r, eq_jg==0))
summary(lm4.d)


texreg(list(lm1,lm2,lm3, lm3.d, lm4, lm4.d),  custom.model.names = c("Crime and Jobs, Neg", "Crime and Jobs, Pos", 
                                                       "Crime and Garb, equal",   "Crime and Garb, diff",
                                                       "Job and Garb, equal" ,   "Job and Garb, diff" )
       , file="coef_subsets.tex")


### Interaction approach
lm1<-lm(chosen_cand~ education + race + gender + party_aff + garbage*cj_neg  , data=cjoint.data.r)
summary(lm1)

lm2<-lm(chosen_cand~ education + race + gender + party_aff + garbage*cj_pos  , data=cjoint.data.r)
summary(lm2)

lm3<-lm(chosen_cand~ education + race + gender + party_aff + jobs*eq_cg  , data=cjoint.data.r)
summary(lm3)

lm4<-lm(chosen_cand~ education + race + gender + party_aff + crime*eq_jg  , data=cjoint.data.r)
summary(lm4)

texreg(list(lm1,lm2,lm3, lm4), file="interaction.tex")


###### Main Model
#names<-c("chosen_cand", "crimedum1",  "crimedum3", "jobsdum1" + jobsdum3 + garbagedum1 + garbagedum3 + college + race1 + race2 + race3 + female + PID1 + PID3)
#cjoint.data.r[,names] <- lapply(cjoint.data.r[,names] , factor)



lm1<-lm(chosen_cand~ education + race + gender + party_aff + garbage + jobs  + crime , data=cjoint.data.r)
coefplot(lm1, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Chosen Candidate", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       
       

ggsave(paste0("Plots/Chosen_candidate_m1", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)


#### Partisan = Democrat
lm2<-lm(chosen_cand~ education + race + gender + party_aff + garbage + jobs  + crime , 
        data=subset(cjoint.data.r, partisan=="Democrat"))
coefplot(lm2, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Chosen Candidate - Democrats", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       
      

ggsave(paste0("Plots/Chosen_candidate_m2", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)

#### Partisan = Republican
lm3<-lm(chosen_cand~ education + race + gender + party_aff + garbage + jobs  + crime , 
        data=subset(cjoint.data.r, partisan=="Republican"))
coefplot(lm3, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Chosen Candidate - Republicans", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       
     

ggsave(paste0("Plots/Chosen_candidate_m3", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)


##########################
#####  Rating modes
##########################

##### Main model

lm4<-lm(cand_eval~ education + race + gender + party_aff + garbage + jobs  + crime , 
        data=cjoint.data.r)
coefplot(lm4, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Candidate Rating", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       

ggsave(paste0("Plots/Candidate_rating_m4", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)


#### Partisan = Democrat
lm5<-lm(cand_eval~ education + race + gender + party_aff + garbage + jobs  + crime , 
        data=subset(cjoint.data.r, partisan=="Democrat"))
coefplot(lm5, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Candidate Rating - Democrats", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       


ggsave(paste0("Plots/Candidate_rating_m5", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)





#### Partisan = Republican
lm6<-lm(cand_eval~ education + race + gender + party_aff + garbage + jobs  + crime , 
        data=subset(cjoint.data.r, partisan=="Republican"))
coefplot(lm6, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"=" Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", "(Intercept)"= "Intercept")
         , title ="Candidate Rating - Republicans", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       


ggsave(paste0("Plots/Candidate_rating_m6", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)



###########################
### Interaction Models
###########################

### Crime*Jobs
lm7<-lm(chosen_cand~ education + race + gender + party_aff + garbage + crimedum3*jobsdum1 + jobsdum3*crimedum1 , 
        data=cjoint.data.r)
coefplot(lm7, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                        "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                        "educationGraduated_from_college"= "College", 
                                        "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                        "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"= "Jobs - Declined",
                                        "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", 
                                        "(Intercept)"= "Intercept", "crimedum3"="Crime - Decreased", "crimedum1"= "Crime - Increased", 
                                        "jobsdum1" ="Jobs - Declined", "jobsdum3" ="Jobs - Increased", "crimedum3:jobsdum1" = " Crime Decreased * Jobs Declined", "jobsdum3:crimedum1" = " Jobs Increased * Crime Increased"
                                        )
         , title ="Chosen Candidate", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       
ggsave(paste0("Plots/Chosen_cand_interaction1_m7_1", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)

lm7.2<-lm(chosen_cand~ education + race + gender + party_aff + garbage + crimedum3:jobsdum1 + jobsdum3:crimedum1 , 
        data=cjoint.data.r)
coefplot(lm7.2, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"= "Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", 
                                                    "(Intercept)"= "Intercept", "crimedum3"="Crime - Decreased", "crimedum1"= "Crime - Increased", 
                                                    "jobsdum1" ="Jobs - Declined", "jobsdum3" ="Jobs - Increased", "crimedum3:jobsdum1" = " Crime Decreased * Jobs Declined", "jobsdum3:crimedum1" = " Jobs Increased * Crime Increased"
)
, title ="Chosen Candidate", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       


ggsave(paste0("Plots/Chosen_cand_interaction1_m7_2", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)



lm8.1<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crimedum3*garbagedum1 + garbagedum3*crimedum1, 
          data=cjoint.data.r)
coefplot(lm8.1, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                      "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                      "educationGraduated_from_college"= "College", 
                                                      "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                      "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"= "Jobs - Declined",
                                                      "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", 
                                                      "(Intercept)"= "Intercept", "crimedum3"="Crime - Decreased", "crimedum1"= "Crime - Increased", 
                                                      "jobsdum1" ="Jobs - Declined", "jobsdum3" ="Jobs - Increased", "crimedum3:jobsdum1" = "Crime Decreased * Jobs Declined", "jobsdum3:crimedum1" = "Jobs Increased * Crime Increased",
                                                      "garbagedum1" = "Garbage - Declined", "garbagedum3" = "Garbage - Improved", "crimedum3:garbagedum1" = "Crime Decreased * Garbage Declined", "garbagedum3:crimedum1" ="Garbage Increased * Crime Increased"
)
         , title ="Chosen Candidate", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       


ggsave(paste0("Plots/Chosen_cand_interaction2_m8_1", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)






lm8.2<-lm(chosen_cand~ education + race + gender + party_aff + jobs + crimedum3:garbagedum1 + garbagedum3:crimedum1, 
        data=cjoint.data.r)
coefplot(lm8.2, outerCI = 2, innerCI = 1, newNames =c("party_affRepublican"="Republican", "party_affDemocratic"="Democrat", "genderFemale"="Female",
                                                    "raceAsian_American"="Asian American", "raceHispanic"="Hispanic", "raceBlack"="Black", 
                                                    "educationGraduated_from_college"= "College", 
                                                    "garbageImproved_considerably"="Garbage - Improved", "garbageDeclined_considerably"="Garbage - Declined", 
                                                    "jobsSignificant_increase_in_new_private_sector_jobs"="Jobs - Increased", "jobsSignificant_decline_in_private_sector_jobs"= "Jobs - Declined",
                                                    "crimeConsiderable_decrease_in_murder_rate"="Crime - Decreased", "crimeConsiderable_increase_in_murder_rate"="Crime - Increased", 
                                                    "(Intercept)"= "Intercept", "crimedum3"="Crime - Decreased", "crimedum1"= "Crime - Increased", 
                                                    "jobsdum1" ="Jobs - Declined", "jobsdum3" ="Jobs - Increased", "crimedum3:jobsdum1" = "Crime Decreased * Jobs Declined", "jobsdum3:crimedum1" = "Jobs Increased * Crime Increased",
                                                    "garbagedum1" = "Garbage - Declined", "garbagedum3" = "Garbage - Improved", "crimedum3:garbagedum1" = "Crime Decreased * Garbage Declined", "garbagedum3:crimedum1" ="Garbage Increased * Crime Increased"
)
, title ="Chosen Candidate", ylab=NULL, xlab=NULL, intercept = F, interceptName = "Intercept"
)       


ggsave(paste0("Plots/Chosen_cand_interaction2_m8_2", v, ".png" ,""), scale = 1, units = c("cm"), dpi = 300)




############
### boxplots
############

boxplot(cand_eval ~  crime ,data=cjoint.data.r)

, main="", 
        xlab="Crime", ylab="Predicted prob of being chosen president",
        names = c("No Change", "Decrease", "Increase")
) 


lm.out <- lm(chosen_cand ~  crime + jobs + garbage  + education
             + gender + party_aff + race , data=cjoint.data.r)

summary(lm.out)

cjoint.data.r$yhat<-predict(lm.out, interval = "none")


png(filename=paste0("Plots/crime", v, ".png" ,""))
boxplot(yhat~crime,data=cjoint.data.r, main="", 
        xlab="Crime", ylab="Predicted prob of being chosen president",
        names = c("No Change", "Decrease", "Increase")
) 
dev.off()    

boxplot(yhat~crime*party_aff,data=cjoint.data.r, main="", 
        xlab="Crime", ylab="Predicted prob of being chosen president",
        notch=TRUE, 
        col=(c("gold","darkgreen"))
        #names = c("No Change", "Decrease", "Increase")
) 


#Graphics
pd <- position_dodge(0.2)

###########
##### Crime
###########

#Alternative measure
fitted_crime <- summarySE(cjoint.data.r, measurevar="yhat", groupvars=c("crime", "party_aff"))
real_crime <- summarySE(cjoint.data.r, measurevar="chosen_cand", groupvars=c("crime", "party_aff"))

crime<-ggplot(real_crime, aes(x=crime, y=chosen_cand, colour=party_aff, group=party_aff)) +
  geom_errorbar(aes(ymin=chosen_cand-ci, ymax=chosen_cand+ci), colour="black", width=.1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + 
  labs(title = "Crime")+
  scale_colour_hue(name="Party Affiliation",l=40) +
  scale_y_continuous(name="Share of respondents who chose candidate", limits=c(0, 1))+
  theme_bw() + scale_x_discrete(name="", 
                                labels=c("Did_not_change" = "No change", 
                                         "Considerable_decrease_in_murder_rate" = "Declined",                                        
                                         "Considerable_increase_in_murder_rate" = "Increased"))

png(filename=paste0("Plots/crime", v, ".png" ,""))  
crime
dev.off() 

### Jobs
real_jobs <- summarySE(cjoint.data.r, measurevar="chosen_cand", groupvars=c("jobs", "party_aff"))
jobs<-ggplot(real_jobs, aes(x=jobs, y=chosen_cand, colour=party_aff, group=party_aff)) +
  geom_errorbar(aes(ymin=chosen_cand-ci, ymax=chosen_cand+ci), colour="black", width=.1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + 
  labs(title = "Jobs")+
  scale_colour_hue(name="Party Affiliation",l=40) +
  scale_y_continuous(name="Share of respondents who chose candidate", limits=c(0, 1))+
  theme_bw() + scale_x_discrete(name="", 
                                labels=c("No_change_in_private_sector_jobs" = "No change", 
                                         "Significant_decline_in_private_sector_jobs" = "Declined", 
                                         "Significant_increase_in_new_private_sector_jobs" = "Increased"
                                ))

png(filename=paste0("Plots/jobs", v, ".png" ,""))
jobs
dev.off() 

### Garbage
real_garbage <- summarySE(cjoint.data.r, measurevar="chosen_cand", groupvars=c("garbage", "party_aff"))

garbage<-ggplot(real_garbage, aes(x=garbage, y=chosen_cand, colour=party_aff, group=party_aff)) +
  geom_errorbar(aes(ymin=chosen_cand-ci, ymax=chosen_cand+ci), colour="black", width=.1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + 
  labs(title = "Garbage")+
  scale_colour_hue(name="Party Affiliation",l=40) +
  scale_y_continuous(name="Share of respondents who chose candidate", limits=c(0, 1))+
  theme_bw() + scale_x_discrete(name="", 
                                labels=c("Did_not_change" = "No change", 
                                         "Declined_considerably" = "Declined",
                                         "Improved_considerably" = "Improved"))

png(filename=paste0("Plots/garbage", v, ".png" ,""))
garbage
dev.off() 

wd<-60
ht<-20

png(filename=paste0("Plots/all", v, ".png" ,""), width=wd, height=ht, units = 'cm', res = 300)
grid.arrange(crime, jobs, garbage, ncol=3)

dev.off() 