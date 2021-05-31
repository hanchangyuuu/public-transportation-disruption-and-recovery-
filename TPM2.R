library(tidyverse)
library(dplyr)
library(car)
library(pscl)
list_origin <- read.csv("COVID-19_Disruption_on_Travel_Patterns_data.csv")
dim(list_origin)

##select
list1 <- list_origin %>% select(gender,age,hh_income,edu,current_job,essential_industry,drive_alone_bc,drive_alone_dr,
                                mode_work_school_bc,wfh_bc,wfh_dr,commute_miles,transit_physical_distance,online_groc_pref,restaurant_fun,
                                coworker_interact,transit_bc,transit_dr, wfh_before_covid_opt,wfh_change_reason,
                                commute_miles,business_shut_down,transit_health,essential_industry)



###charactor to factor
list1$wfh_change_reason <- as.factor(list1$wfh_change_reason)
list1$gender <- as.factor(list1$gender)   
list1$edu<- as.factor(list1$edu)     
list1$hh_income <- as.factor(list1$hh_income)    
list1$current_job <- as.factor(list1$current_job)  
list1$drive_alone_bc <- as.factor(list1$drive_alone_bc)
list1$drive_alone_dr <- as.factor(list1$drive_alone_dr)
list1$transit_bc <- as.factor(list1$transit_bc)
list1$transit_dr <- as.factor(list1$transit_dr)
list1$mode_work_school_bc <- as.factor(list1$mode_work_school_bc)
list1$wfh_bc <- as.factor(list1$wfh_bc)
list1$wfh_dr <- as.factor(list1$wfh_dr)
list1$transit_physical_distance <- as.factor(list1$transit_physical_distance)
list1$wfh_before_covid_opt <- as.factor(list1$wfh_before_covid_opt)
list1$business_shut_down <- as.factor(list1$business_shut_down)
list1$essential_industry<- as.factor(list1$essential_industry)
summary(list1$transit_health)
###check



###current job
orderingjob <- c("Covid lay off", "Disabled from a stroke","Laid off until phase 3","Layoff","Unemployed",
                 "Home maker","Homemaker","Homeworker","Stay at home mom","Stay at home mom.","Volunteer ",
                 "Retired",
                 
                 "Student","Student and unemployed ","Student taking a break ",
                 
                 "Army", "Currently employed but notified i will be laid off in 30 days","Employed FT but on furlough till 7/20",
                 "Employed full time - furlough","Employed, Full time","Employed, Part time","Full time but on FMLA parental leave",
                 "Fur lowed ","Furloughed","Furloughed ", "Furloughed full time","Furloughed- will have job back in few months. ",
                 "On leave from a full time job","Retired, working p/t ","School employee, off for summer","Teacher on summer break ",
                 "Homemaker /self employed","Independent insurance broker ","self employed","Self employed ","Self employed/Voluntarily temporarily closed","Self employeed ","self-employed","Self-employed",
                 "Sell employed/freelancer","Semi retired/self employed","Sub contractor")

list1$current_job <- as.numeric(factor(list1$current_job, levels = orderingjob))
list1$current_job <- ifelse((list1$current_job>0&list1$current_job< 13) ,-1,ifelse((list1$current_job>12&list1$current_job< 16) ,0,1 ))
list1 <- list1 %>% filter(current_job != "-1") %>% droplevels()
list1$current_job <- as.factor(list1$current_job)
summary(list1$current_job)






###gender constrain to 2 // female
list1$gender <- as.numeric(factor(list1$gender))
list1 <- list1 %>% filter(gender==2 | gender==4) %>% droplevels()
list1$gender <- ifelse(list1$gender==2, 0, 1)
list1$gender <- as.factor(list1$gender)
summary(list1$gender)
dim(list1)


list1$wfh_change_reason <- as.numeric(factor(list1$wfh_change_reason))
list1$wfh_change_reason <- ifelse(list1$wfh_change_reason==8, 0, 1)
list1$wfh_change_reason <- as.factor(list1$wfh_change_reason)
summary(list1$wfh_change_reason)
dim(list1)



###factor to numeric
ordering <- c("Never", "Once a month or less", "A few times a month", "1-2 days a week", "3-4 days a week", "Everyday")
list1$transit_dr <- as.numeric(factor(list1$transit_dr, levels = ordering))
list1$transit_bc <- as.numeric(factor(list1$transit_bc, levels = ordering))
### public transit less?
list1$transit_less <- ifelse(list1$transit_dr < list1$transit_bc, 1, 0)
list1$transit_less <- as.factor(list1$transit_less)
summary(list1$transit_less)



###I would travel on a bus or light rail if physical distancing measures are enforced 
summary(list1$transit_physical_distance)
ordering1 <- c("Strongly disagree", "Disagree", "Somewhat disagree", "Somewhat agree", "Agree", "Strongly agree")
list1$transit_physical_distance <- as.numeric(factor(list1$transit_physical_distance, levels = ordering1))
list1$transit_physical_distance <- ifelse(list1$transit_physical_distance > 3, 1, 0)
list1$transit_physical_distance <- as.factor(list1$transit_physical_distance)
summary(list1$transit_physical_distance)
dim(list1)





summary(list1$mode_work_school_bc)
list1$mode_work_school_bc <- as.numeric(factor(list1$mode_work_school_bc))
list1$mode_pt_bc <- ifelse(list1$mode_work_school_bc == 6, 1, 0)
list1$mode_pt_bc <- as.factor(list1$mode_pt_bc)
summary(list1$mode_pt_bc)

###edu
dim(list1)
###too small to be representative
list1 <- list1%>%filter(edu != "" &edu != "Less than high school") %>%droplevels()
dim(list1)
orderingedu <- c("High school graduate or GED","Some college/technical school training",
                 "2-year college degree (Associate)","4-year college degree (e.g. BA, BS)",
                 "Graduate degree (e.g. MS, PhD, MBA)","Professional degree (e.g. MD, JD)")
list1$edu <- as.numeric(factor(list1$edu, levels = orderingedu))
list1$edu<- ifelse((list1$edu == 1|list1$edu == 2),0, ifelse((list1$edu == 3|list1$edu == 4), 0,1 ))
list1$edu <- as.factor(list1$edu)
summary(list1$edu)

###income combine
ordering0 <- c("Prefer not to answer", 
               "Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999",
               "$75,000 to $99,999","$100,000 to $149,999","$150,000 to $199,999",
               "$200,000 or more")
list1$hh_income <- as.numeric(factor(list1$hh_income, levels = ordering0))
list1$hh_income <- ifelse(list1$hh_income == 1,99, ifelse((list1$hh_income>1&list1$hh_income< 9), 0, ifelse((list1$hh_income>8 &list1$hh_income < 12), 1, 2)))
list1$hh_income <- as.factor(list1$hh_income)

list1 <- list1 %>% filter(hh_income != "99") %>% droplevels()
summary(list1$hh_income)
dim(list1)

########################################################################################################################


# Media is exaggerating the spread of the coronavirus. 
list1$business_shut_down <- as.numeric(factor(list1$business_shut_down, levels = ordering1))
list1$business_shut_down<- ifelse(list1$business_shut_down > 3, 1, 0)
list1$business_shut_down <- as.factor(list1$business_shut_down)
summary(list1$business_shut_down)
dim(list1)


list1$transit_health <- as.numeric(factor(list1$transit_health, levels = ordering1))
list1$transit_health<- ifelse(list1$transit_health > 3, 1, 0)
list1$transit_health <- as.factor(list1$transit_health)
summary(list1$transit_health)
dim(list1)



###age
#list1$age<- ifelse(list1$age<65,1, 0)
#list1$age <- as.factor(list1$age)
summary(list1$age)
sd(list1$age)











###factor to numeric
ordering <- c("Never", "Once a month or less", "A few times a month", "1-2 days a week", "3-4 days a week", "Everyday")
list1$drive_alone_dr <- as.numeric(factor(list1$drive_alone_dr, levels = ordering))
list1$drive_alone_bc <- as.numeric(factor(list1$drive_alone_bc, levels = ordering))
###car less?
list1$use_car_less <- ifelse(list1$drive_alone_dr < list1$drive_alone_bc, 1, 0)
list1$use_car_less <- as.factor(list1$use_car_less)
summary(list1$use_car_less)


###factor to numeric
summary(list1$wfh_bc)
list1$wfh_bc <- as.numeric(factor(list1$wfh_bc, levels = ordering))
list1$wfh_dr <- as.numeric(factor(list1$wfh_dr, levels = ordering))
###wfh more?
list1$wfh_more <- ifelse(list1$wfh_bc < list1$wfh_dr, 1, 0)
list1$wfh_more <- as.factor(list1$wfh_more)
summary(list1$wfh_more)





###commute by car before
#summary(list1$mode_work_school_bc)
#list1$mode_work_school_bc <- as.numeric(factor(list1$mode_work_school_bc))
#list1 <- list1 %>% filter(mode_work_school_bc != 5)
#list1$mode_car_bc <- ifelse(list1$mode_work_school_bc == 6, 1, 0)
#list1$mode_car_bc <- as.factor(list1$mode_car_bc)
#summary(list1$mode_car_bc)






########################################################################################################################


# gender,age,race,hh_income,edu,current_job,essential_industry,drive_alone_bc,drive_alone_dr,
#                                mode_work_school_bc,wfh_bc,wfh_dr,commute_miles,transit_physical_distance,,online_groc_pref,restaurant_fun,
#                                coworker_interact
# gender+edu+business_shut_down+mode_pt_bc
logit1 <- glm(transit_less ~ mode_pt_bc+current_job+hh_income+edu+gender, family="binomial", data=list1, x=T)
summary(logit1)
pR2(logit1)
exp(logit1$coefficients)
exp(confint(logit1))
logit2 <- glm(transit_less ~ mode_pt_bc+current_job+hh_income+edu+gender+wfh_change_reason, family="binomial", data=list1, x=T)
summary(logit2)
pR2(logit2)
exp(logit2$coefficients)
exp(confint(logit2))
logit3 <- glm(transit_less ~ mode_pt_bc+current_job+hh_income+edu+gender+wfh_change_reason+transit_physical_distance, family="binomial", data=list1, x=T)
summary(logit3)
pR2(logit3)
exp(logit3$coefficients)
exp(confint(logit3))


vif(logit3)
list1$logage <- log(list1$age)*list1$age
logit4 <- glm(transit_less ~ logage+age+current_job+hh_income+edu+gender+wfh_change_reason+transit_physical_distance, family="binomial", data=list1, x=T)
summary(logit4)

logitagree <-glm(transit_less ~ mode_pt_bc+current_job+hh_income+edu+gender+wfh_change_reason, family="binomial", data=list2, x=T)
summary(logitagree)
pR2(logitagree)
exp(logitagree$coefficients)
exp(confint(logitagree))

logitdisagree <-glm(transit_less ~ mode_pt_bc+current_job+hh_income+edu+gender+wfh_change_reason, family="binomial", data=list3, x=T)
summary(logitdisagree)
pR2(logitdisagree)
exp(logitdisagree$coefficients)
exp(confint(logitdisagree))
########################################################################################################################
list2 <- list1 %>% filter(transit_physical_distance=="1")
dim(list2)
summary(list2$transit_less)
summary(list2$gender)
summary(list2$edu)
summary(list2$current_job)
summary(list2$mode_pt_bc)
summary(list2$hh_income)
summary(list2$wfh_change_reason)
dim(list2)
list3 <- list1 %>% filter(transit_physical_distance=="0")
summary(list3$transit_less)
summary(list3$gender)
summary(list3$edu)
summary(list3$current_job)
summary(list3$hh_income)
summary(list3$wfh_change_reason)
dim(list3)


listbar2 <- list2 %>% select(wfh_more, transit_less)
library(dplyr)
listbar2 <- listbar2 %>% group_by(wfh_more,transit_less) %>% dplyr::summarize(count = n())
listbar2 <- mutate(listbar2,wfh_more = factor(wfh_more,
                                                 level = c(0,1),
                                                 labels = c("Not affected", "Affected")))
listbar2 <- mutate(listbar2,Public_transit_frequency = factor(transit_less,
                                              level = c(0,1),
                                              labels = c("No change or more", "Less")))

listbar3 <- list3 %>% select(wfh_more, transit_less)
library(dplyr)
listbar3 <- listbar3 %>% group_by(wfh_more,transit_less) %>% dplyr::summarize(count = n())
listbar3 <- mutate(listbar3,wfh_more = factor(wfh_more,
                                              level = c(0,1),
                                              labels = c("Not affected", "Affected")))
listbar3 <- mutate(listbar3,Public_transit_frequency = factor(transit_less,
                                                              level = c(0,1),
                                                              labels = c("No change or more", "Less")))

p1 <- ggplot(data=listbar2, aes(x=wfh_more, y=count, fill=transit_less)) +
  geom_bar(stat="identity",position = "dodge")+
  scale_fill_brewer(palette="Blues")+
  theme_minimal()+
  scale_y_continuous(limits = c(0,250))+
  theme(legend.position = "none") +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  labs(x = "Work from home strategy", 
       y = "Number of Travellers", 
       title = "Potential Public Transit users") 
  


p2 <- ggplot(data=listbar3, aes(x=wfh_more, y=count, fill=Public_transit_frequency)) +
  geom_bar(stat="identity",position = "dodge")+
  scale_fill_brewer(palette="Blues")+
  theme_minimal()+
  scale_y_continuous(limits = c(0,250)) +
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  labs(x = "Work from home frequency", 
       y = "", 
       title = "Others") 

library("cowplot")
plot_grid(p1, p2 , 
          labels=c("",""),
          nrow = 1,ncol = 2)


listbar <- list1 %>% select(wfh_change_reason, transit_less,transit_physical_distance)
library(dplyr)
listbar <- listbar %>% group_by(wfh_change_reason,transit_less,transit_physical_distance) %>% dplyr::summarize(count = n())
listbar <- mutate(listbar,wfh_change_reason = factor(wfh_change_reason,
                                              level = c(0,1),
                                              labels = c("Not affected", "Affected")))
listbar <- mutate(listbar,transit_less= factor(transit_less,
                                                              level = c(0,1),
                                                              labels = c("No", "Yes")))
listbar <- mutate(listbar,transit_physical_distance = factor(transit_physical_distance,
                                            level = c(0,1),
                                            labels = c("Would not use PT", "Would use PT")))
listbar$transit_physical_distance <- 
  ordered(listbar$transit_physical_distance, levels = c("Would use PT","Would not use PT"))

postscript(file="saving_plot4.ps",width=10.5, height=5)
ggplot(data=listbar, aes(x=wfh_change_reason, y=count, fill=transit_less)) +
  geom_bar(stat="identity",position = "dodge")+
  facet_wrap(~transit_physical_distance, nrow=1)+
  scale_fill_brewer(palette="Blues")+
  theme_classic()+
  geom_text(aes(label=count), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) +
  labs(x = "Work from home strategy", 
       y = "Number of Travellers",
       caption = "Data: COVID-19 Disruption on Travel Patterns data") +
  guides(fill=guide_legend(title="Use Public Transit Less"))+
  theme(text = element_text(size = 12),  strip.text = element_text(size=20))
dev.off()

########################################################################################################################


list6 <- cbind(list1, predict(logit3, newdata = list1, type = "link",
                                    se = TRUE))
list6 <- within(list6, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
as.numeric(list6$wfh_change_reason)
ggplot(list6, aes(x = as.numeric(wfh_change_reason), y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = transit_physical_distance), alpha = 0.2) + geom_line(aes(colour = transit_physical_distance),size = 1)
########################################################################################################################
listbar3 <- list3 %>% select(wfh_more, transit_less)
listbar3 <- listbar3 %>% mutate(count = 1)
ggplot(data=listbar3, aes(x=wfh_more, y=count, fill=transit_less)) +
  geom_bar(stat="identity",position = "dodge")+
  scale_fill_brewer(palette="Blues")+
  theme_minimal()



listbar2 <- list2 %>% select(wfh_more, transit_less)
#library(plyr) 
#listbar2 <- ddply(listbar2,.(wfh_more),transform,count=length(transit_less))
listbar2 <- listbar2 %>% mutate(count = 1)
ggplot(data=listbar2, aes(x=wfh_more, y=count, fill=transit_less)) +
  geom_bar(stat="identity")



summary(list2$transit_less)

logit <- glm(mode_car_bc~ transit_physical_distance, family="binomial", data=list4, x=T)

logit <- glm(transit_less~ gender+edu+hh_income+current_job+age+wfh_more, family="binomial", data=list3, x=T)

logit <- glm(transit_physical_distance~ mode_car_bc, family="binomial", data=list1, x=T)
library(car)
library(MASS)
logit %>% vif()
