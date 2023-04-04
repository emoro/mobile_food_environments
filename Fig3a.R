# code to reproduce the results in Figure 3a of the paper 
# "You are what you eat: Effect of mobile food environments on fast food visits"
# by Bernardo Garcia-Bulle et al. 
# Date: March 9, 2023
# Author: Esteban Moro

## libraries used
library(tidyverse)
library(data.table)
library(ggthemes)
library(latex2exp)
library(patchwork)
library(jtools)


## visualization settings
source("viz_settings.R")


## load data
users <- fread("./data/users.csv.gz")
actions_lunch <- fread("./data/actions_lunch.csv.gz")
actions_DMV <- fread("./data/actions_DMV.csv.gz")


## select only lunch outings to food
actions_lunch <- actions_lunch[is.food==1]

## fit the models for each set of food outing
# We use the fixest package
require(fixest)

table_coefficients <- data.table()

#All data (context before lunch)
m_glm_context <- feglm(yit==1 ~ phi  | day + user,
                       data=actions_lunch,
                       cluster=c("day","user"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="All",m_glm_context$coeftable))

#Only weekdays
m_glm_weekday <- feglm(yit==1 ~ phi | day + user,
                       data=actions_lunch[(day %% 7) %in% 1:5],
                       cluster=c("day","user"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Weekday",m_glm_weekday$coeftable))
#Only weekends
m_glm_weekend <- feglm(yit==1 ~ phi | day + user,
                       data=actions_lunch[(day %% 7)  %in% c(6,0)],
                       cluster=c("day","user"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Weekend",m_glm_weekend$coeftable))

#By Income quantiles (low and high)
m_glm_quant_1 <- feglm(yit==1 ~ phi | day + user,
                       data=actions_lunch[user %in% users$user[users$quant_income==1]],
                       cluster=c("day"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Income Q1",m_glm_quant_1$coeftable))

m_glm_quant_4 <- feglm(yit==1 ~ phi | day + user,
                       data=actions_lunch[user %in% users$user[users$quant_income==4]],
                       cluster=c("day"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Income Q4",m_glm_quant_4$coeftable))


#by quantile of Fast Food consumption
FFO_thr <- median(users$mu_FFO[users$mu_FFO>0],na.rm=T)
m_glm_FFO_1 <- feglm(yit==1 ~ phi | day + user,
                       data=actions_lunch[user %in% users$user[users$mu_FFO <= FFO_thr]],
                       cluster=c("day"),
                       family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Visits FFO Q1",m_glm_FFO_1$coeftable))
m_glm_FFO_2 <- feglm(yit==1 ~ phi | day + user,
                     data=actions_lunch[user %in% users$user[users$mu_FFO > FFO_thr]],
                     cluster=c("day"),
                     family="binomial")
table_coefficients <- rbind(table_coefficients,
                            data.table(type="Visits FFO Q2",m_glm_FFO_2$coeftable))


#outings after visit to DMV
#add the fraction of visits to FFO by user
actions_DMV <- actions_DMV %>% 
  merge(users %>% select(user,mu_FFO))
m_glm_DMV <- feglm(yit ==1 ~ phi + mu_FFO,
                   data=actions_DMV,
                   family="binomial")
mt <- data.table(m_glm_DMV$coeftable)[2,]
table_coefficients <- rbind(table_coefficients,
                            data.table(type="DMV",mt),use.names=F)


g1 <- table_coefficients %>%
  mutate(type=recode_factor(type,"Visits FFO Q1"="Low FFO Visits",
                            "Income Q1"="Low Income",
                            "Income Q4"="High Income",
                            "Visits FFO Q2"="High FFO Visits",
                            "DMV"="DMV")) %>% 
  filter(type %in% c("All","Weekday","Weekend","High Income","Low Income",
                     "High FFO Visits","Low FFO Visits","DMV")) %>% 
  mutate(type=factor(type,
                     levels=rev(c("All","Weekday","Weekend",
                                  "High Income","Low Income",
                                  "High FFO Visits","Low FFO Visits",
                                  "DMV")))) %>% 
  ggplot(aes(y=Estimate,x=type)) + 
  geom_errorbar(aes(x=type,ymin=Estimate-2*`Std. Error`,
                    ymax=Estimate+2*`Std. Error`),width=0.1)+
  geom_point(size=3,col=colors_publication[2]) + 
  scale_y_continuous(limits=c(-0.1,2.5))+ geom_hline(yintercept=0,linetype=2) +
  coord_flip() + labs(title="Effect of the mobile \n context in going to FFO",y=TeX("$\\beta$ (log-odds)"),x="Food outings group")+
  theme_publication()

g1
