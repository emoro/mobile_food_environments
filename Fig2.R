# code to reproduce the results in Figure 2 of the paper 
#"You are what you eat: Effect of mobile food environments on fast food visits"
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


#################### Figure 2a ######################

## load data
users <- fread("./data/users.csv.gz")

## preprocess
# use only users with enough number of stays and visits to food outlets
users_fit <- users %>% 
  filter(nstays>50 & nfood > 5 & nffood >0 & nffood < nfood) 


## fit the models for food and mobile environments and fraction of FFO visits
fit_env <- lm(phi_mobile ~ quant_income + quant_fblack + quant_fedu + 
                quant_femp + quant_fcom + quant_fpub + quant_flowskill + factor(msa),
              data=users_fit)
fit_env_home <- lm(phi_home ~ quant_income + quant_fblack + quant_fedu + 
                     quant_femp + quant_fcom + quant_fpub + quant_flowskill + factor(msa),
                   data=users_fit)
fit_visits <- lm(mu_FFO ~ quant_income + quant_fblack + quant_fedu + 
                   quant_femp + quant_fcom + quant_fpub + quant_flowskill + factor(msa),
                 data=users_fit)

## build the tables with the coefficients 
models_results <- rbind(data.table(jtools::tidy(fit_env),type="Mobile FF Environment"),
                        data.table(jtools::tidy(fit_env_home),type="Home FF Environment"),
                        data.table(jtools::tidy(fit_visits),type="Fraction of visits to FF"))


## plot
gfig2a <- models_results[!grepl("msa",term) & !grepl("Intercept",term)] %>% 
  mutate(type=factor(type,levels=c("Home FF Environment","Mobile FF Environment",
                                   "Fraction of visits to FF"))) %>%
  mutate(term=recode_factor(term,
                            "quant_income"="Median income","quant_fblack"="Black",
                            "quant_fedu"="Bachelor or more","quant_femp"="Employed",
                            "quant_fpub"="Trans. public",
                            "quant_fcom"="Long Commuting","quant_flowskill"="Low skill jobs")) %>%
  ggplot(aes(x=term,y=estimate,fill=type,col=type,
             ymin=estimate-2*std.error,ymax=estimate+2*std.error)) + 
  geom_hline(yintercept = 0,linetype=3)+
  geom_bar(position=position_dodge(width=0.6),width=0.57,stat="identity") + 
  geom_errorbar(position=position_dodge(width=0.6),width=.25,size=.3,col="black")+
  theme_publication() + theme(legend.position = "none") + 
  scale_colour_manual(values=c(home_color,context_color,fast_food_color))+
  scale_fill_manual(values=c(home_color,context_color,fast_food_color))+
  labs(x="",y="Estimate",fill="",col="")+
  scale_y_continuous(limits=c(-0.015,0.015)) + coord_flip() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE),col=guide_legend(nrow=3,byrow=TRUE))

gfig2a

#################### Figure 2b ######################

## load data
nstays_by_hour <- fread("./data/nstays_by_hour.csv")

## preprocess the data
table_data <- nstays_by_hour[,.(nstays=sum(nstays),nfood=sum(nfood),nffood=sum(nffood)),.(hour,wday)] %>%
  mutate(nday=wday) %>% 
  mutate(wday=factor(wday)) %>%
  mutate(wday=factor(wday,levels=c("1","2","3","4","5","6","7"),
                     labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
         `Fraction of FFO Visits`=nffood/nstays,
         `Fraction of FO Visits`=nfood/nstays) %>%
  rename(`Number of FO Visits`=nfood,
         `Number of FFO Visits`=nffood,
         `Number of Visits`=nstays) %>%
  pivot_longer(!c(hour,wday,nday)) %>%
  mutate(name=factor(name,levels=c("Number of Visits","Number of FO Visits",
                                   "Number of FFO Visits",
                                   "Fraction of FFO Visits",
                                   "Fraction of FO Visits")))

## plot
rectangles <- data.frame(xmin=11.5 + 0:6*24,xmax=14 + 0:6*24,
                         ymin=-Inf,ymax=Inf)
gfig2b <- table_data %>%
  filter(name %in% c("Number of FO Visits","Number of FFO Visits")) %>%
  mutate(date_plot=hour+(nday-1)*24) %>%
  ggplot(aes(x=date_plot,y=value,col=name)) + 
  ggplot2::annotate("rect",xmin=rectangles$xmin,xmax=rectangles$xmax,
           ymin=rectangles$ymin,ymax=rectangles$ymax,
           fill=context_color,alpha=0.15)+
  geom_vline(xintercept = seq(24,6*24,24),lwd=1,col="lightgray")+
  geom_line(size=1) + facet_grid(vars(name),scales = "free_y") +
  scale_x_continuous(breaks=seq(0,7*24,4),
                     labels= seq(0,7*24,4) %% 24,
                     limits = c(0,7*24),expand = c(0,0)) +
  theme(panel.spacing = unit(.0, "lines"))+
  theme_publication() +
  theme(strip.background =element_rect(fill=alpha("gray",0.4),
                                       color=alpha("gray",0.4)))+
  theme(strip.text = element_text(colour = 'black'))+
  labs(x="Hour",y="") + scale_y_continuous(labels = comma)+
  scale_color_manual(values=c(food_color,fast_food_color)) + theme(legend.position = "none") + 
  theme(panel.spacing.x = unit(.0, "lines")) 

#################### Final Figure 2 ######################

gfig2a + gfig2b + plot_annotation(tag_levels="A") + plot_layout(widths=c(3,7))

                                                                