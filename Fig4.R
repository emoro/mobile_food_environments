# code to reproduce the results in Figure 4 of the paper 
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
source("meta_data.R")


## load data
actions_all <- rbindlist(lapply(Sys.glob("./data/actions_all_?.csv.gz"),fread))
users <- fread("./data/users.csv.gz")


###############  Figure 4a  ##################

## generate the logistic regression model

actions_all[,phi:=nffood_context/nfood_context]

require(fixest)
m_glm_all <- feglm(yit==1 ~ phi | day + user,
                   data=actions_all,
                   cluster=c("day","user"),
                   family="binomial")


## get the results for the impact of the intervention

predictions <- predict(m_glm_all,type="response")
obs_remove <- m_glm_all$obs_selection$obsRemoved
actions_all_final <- actions_all[obs_remove]
actions_all_final[,prediction:= predictions]

##calculate the susceptibility part in the effect of intervention
actions_all_final[,ex:=predictions/(1-predictions)]
actions_all_final[,susceptibility:=ex/(1+ex)^2]


## Calculate the effect of the change of one FFO to a non-FFO by census tract
interventions_tract <- 
  actions_all_final[,.(noutings=.N,avg_phi=mean(phi),
                       avg_food=mean(nfood_context),
                       sum_intervention=sum(m_glm_all$coefficients*
                                            susceptibility/nfood_context)
                       ),.(tract)]

## Select different tracts according to different interventions. We select X top tracts according to different criteria. 
X <- 100

## We only consider census tracts with enough number of actions and food places.
threshold_N <- 100
threshold_nfood <- 10

## 1. Areas with worst food environemnt within the Low-income, low-access areas defined the USDA
usdaLILA <- fread("./data/usdaLILA.csv")
tracts_low_food_access <- interventions_tract %>%
  filter(tract %in% usdaLILA$tract) %>%
  filter(noutings > threshold_N & avg_food > threshold_nfood) %>% 
  arrange(-avg_phi) %>% head(X)

## 2. Areas with large fraction of fast-food. Food Swamps.
tracts_food_swamp <- interventions_tract %>%
  filter(noutings > threshold_N & avg_food > threshold_nfood) %>% 
  arrange(-avg_phi) %>% head(X)
  
## 3. Areas with the large number of food outings. Food hotspots.
tracts_food_hotspots <- interventions_tract %>%
  filter(noutings > threshold_N & avg_food > threshold_nfood) %>% 
  arrange(-noutings) %>% head(X)

## 4. Areas with the largest behavioral-environment intervention
tracts_behavioral <- interventions_tract %>% 
  filter(noutings > threshold_N & avg_food > threshold_nfood) %>% 
  arrange(-sum_intervention) %>% head(X)


## plot of Figure 4a

gfig4a <- interventions_tract %>%
  na.omit() %>% 
  filter(noutings > threshold_N & avg_food > threshold_nfood) %>% 
  ggplot() + geom_bin2d(aes(x=avg_phi,y=sum_intervention),bins=50) + 
  scale_x_log10() + scale_y_log10() +
  labs(x=latex2exp::TeX("Average of $\\phi$ in area $\\Omega"),
       y=latex2exp::TeX("$|\\Delta^{FFO}$ $(\\Omega)$|")) + theme_hc()  +
  scale_fill_gradient(low = "lightgray",high="black",name="Number of areas") +
  geom_vline(xintercept = tail(tracts_food_swamp$avg_phi,1),linetype=2) +
  geom_hline(yintercept = tail(tracts_behavioral$sum_intervention,1),linetype=2) +
  geom_point(data=tracts_food_swamp %>% na.omit(),
             aes(x=avg_phi,y=sum_intervention),col="darkred",alpha=0.5) +
  geom_point(data=tracts_behavioral %>% na.omit(),
             aes(x=avg_phi,y=sum_intervention),col="darkgreen",alpha=0.5) + 
  geom_point(data=tracts_low_food_access,
             aes(x=avg_phi,y=sum_intervention),col="orange",alpha=0.5)+ 
  geom_point(data=tracts_food_hotspots,
             aes(x=avg_phi,y=sum_intervention),col="darkblue",alpha=0.5)+
  theme(legend.position=c(0.13,0.88),legend.direction = "horizontal") +
  guides(fill=guide_colourbar(title.position = "top"))



###############  Figure 4b  ##################

## we do the analysis by quantile of income. Add it to the interventions_tracts
actions_all_final <- actions_all_final %>% 
  merge(users[,c("user","quant_income")],by="user")
# Low-income low access areas impact
impact_lila <- actions_all_final[tract %in% tracts_low_food_access$tract,
                            .(sum_intervention=sum(m_glm_all$coefficients*
                                                     susceptibility/nfood_context)),
                            .(quant_income)]
impact_lila$type <- "Low Food\nAccess\nintervention"

# Food swamps
impact_swamp <- actions_all_final[tract %in% tracts_food_swamp$tract,
                                 .(sum_intervention=sum(m_glm_all$coefficients*
                                                          susceptibility/nfood_context)),
                                 .(quant_income)]
impact_swamp$type <- "Food Swamp\nintervention"

# Food hotspots
impact_hotspots <- actions_all_final[tract %in% tracts_food_hotspots$tract,
                                  .(sum_intervention=sum(m_glm_all$coefficients*
                                                           susceptibility/nfood_context)),
                                  .(quant_income)]
impact_hotspots$type <- "Food hotspots\nintervention"

# Behavioral
impact_behavioral <- actions_all_final[tract %in% tracts_behavioral$tract,
                                     .(sum_intervention=sum(m_glm_all$coefficients*
                                                              susceptibility/nfood_context)),
                                     .(quant_income)]
impact_behavioral$type <- "Behavior-\nEnvironment\nintervention"

impact_total <- rbind(impact_lila,
                      impact_swamp,
                      impact_hotspots,
                      impact_behavioral)


gfig4b <- impact_total %>%
  mutate(type=factor(type,levels=c("Low Food\nAccess\nintervention",
                                   "Food Swamp\nintervention",
                                   "Food hotspots\nintervention",
                                   "Behavior-\nEnvironment\nintervention"))) %>%
  ggplot(aes(x=type,y=sum_intervention,fill=type,alpha=factor(quant_income))) + 
  geom_bar(stat="identity",width=0.6) +
  theme_hc() + 
  labs(x="",y="Total Increase in the number of non-FFO visits",fill="Income\nQuantile") +
  guides(alpha=guide_legend(nrow=1),fill="none") + 
  theme(legend.position = c(.77,0.165))+
  scale_fill_manual(values = c("orange","darkred","darkblue","darkgreen"))+
  scale_alpha_discrete(range=c(0.4,1),name="Income quantile") + coord_flip()



###############  Figure 4c  ##################

# load the poi data to do topic analysis
poi_all <- msas %>% map_dfr(function(x){
  poi_MSA <- fread(paste0("./data/poi_",x,".csv.gz"))
  data.table(type="All",
             poi_MSA)
})


# table of different categories by tract
poi_tract_cat <- poi_all[,.N,.(tract,cat)]

# build the Document-Term matrix. 
# Filter terms (cats) present in less than 20 documents (tracts)
require(tm)
require(quanteda)
require(tidytext)
poi_dtm <- poi_tract_cat %>% group_by(cat) %>%
  mutate(n_tracts=length(tract)) %>%
  filter(n_tracts > 20) %>% #trim the terms
  cast_dtm(tract,cat,N)


# LDA
require(topicmodels)
k <- 20
lda <- LDA(poi_dtm, k, method = "Gibbs", 
           control = list(verbose = F, seed = 123,
                          burnin = 100, iter = 500))


names_topics <- c("Industrial / \nFactory","Neighborhood (I)","Entertainment","Shopping Center","Recreational Area","Bus/Train","Garage","University","Residential","Office","Airport","Mall","Health","Road Service Area","Neighborhood (II)","Restaurants","Education","Seaside","Neighborhood (III)","Mixed")


# Probability of topics by document
gamma <- tidytext::tidy(lda, matrix = "gamma") %>%
  rename(tract="document") %>% 
  mutate(topic=names_topics[topic])


# Gamma by census tract selected in the intervetions

table_topics_interventions <- 
  rbind(gamma %>% mutate(type="All"),
        gamma %>% filter(tract %in% tracts_behavioral$tract) %>% 
          mutate(type="Behavior-\nEnvironment\nintervention"),
        gamma %>% filter(tract %in% tracts_food_swamp$tract) %>% 
          mutate(type="Food Swamp\nintervention"),
        gamma %>% filter(tract %in% tracts_low_food_access$tract) %>% 
          mutate(type="Low Food\nAccess\nintervention"),
        gamma %>% filter(tract %in% tracts_food_hotspots$tract) %>% 
          mutate(type="Food hotspots\nintervention")) %>%
  group_by(type,topic) %>%
  summarize(prob=mean(gamma),sd=sd(gamma)/sqrt(n()))

gfig4c <- table_topics_interventions %>%
  filter(topic %in% c("Airport","Industrial / \nFactory","Entertainment","Garage",
                      "Grocery","Mall","Office","Restaurants")) %>%
  group_by(topic) %>% mutate(mu = prob/prob[type=="All"]) %>%
  filter(type!="All") %>%
  ggplot(aes(x=reorder(topic,prob),y=mu,col=type,group=type)) +
  geom_segment(aes(x=reorder(topic,prob),xend=reorder(topic,prob),y=1,yend=mu))+
  geom_point(stat="identity",size=2,shape=21,fill="white",stroke=1.6) +
  facet_grid(~type) + geom_hline(linetype=2,aes(col=type,yintercept=1)) + coord_flip() +
  scale_fill_manual(values=c("darkgreen","darkblue","darkred","orange")) +
  scale_color_manual(values=c("darkgreen","darkblue","darkred","orange")) +
  labs(y="Relative frequency of POI topic in areas selected",x="Topic")+
  theme_hc()+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(0.5,3))

######### Figure 4


layout <- "
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAABBBB
AAAAA####
AAAAACCCC
AAAAACCCC
AAAAACCCC
AAAAACCCC
AAAAACCCC
AAAAACCCC
AAAAACCCC
AAAAACCCC
"

gfig4a + gfig4b + gfig4c + 
  plot_layout(design = layout) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 18,hjust=0,vjust=1),
        plot.tag.position = c(0, 1))

