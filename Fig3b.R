# code to reproduce the results in Figure 3b of the paper 
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


## load data
users <- fread("./data/users.csv.gz")
actions_lunch <- fread("./data/actions_lunch.csv.gz")

## analysis

# Get the users who have two different contexts before lunch in the period analyzed
# Note that context here refers to the census tract of the context stay

fraction_thr <- 0.3

table_context <- actions_lunch[,.N,.(user,tract)][,frac:=N/sum(N),.(user)]

# we select those users that have 2 contexts with more than fraction_thr each in different census tracts
table_two_context <- table_context[,.(ndays=sum(N[frac>fraction_thr]),
                                      ncontexts=sum(frac>fraction_thr)),.(user)]
table_two_context <- table_two_context[ncontexts==2  & ndays > 5]
actions_lunch0 <- actions_lunch[user %in% table_two_context$user]

# We created now a table in which for each of those users we get the time in which 
# they change the context. 

# To do that we are going to use the cpt.mean function in the changepoint library. We construct a time series like 11112222 and using that library we identify if there is a changepoint from 1 to 2. For example, 12121212 shouldn't give a change point.
require(changepoint)

table_changed <- data.table()
for(i in 1:nrow(table_two_context)){
  uu <- table_two_context$user[i]
  ac <- actions_lunch0[user==uu]
  trs <- table_context[user==uu][frac>fraction_thr]
  ac$tract_id <- match(ac$tract,trs$tract)-1
  ac <- ac[!is.na(tract_id)]
  ch.tt <- cpt.mean(ac$tract_id,penalty="Manual",pen.value="n^0.1")
  pts <- cpts(ch.tt)
  pts <- pts[pts!=length(ac$tract_id) & pts!=1]
  if(length(pts)==1){ 
    table_changed <- rbind(
      table_changed,data.table(user=uu,
                               phi_1 = mean(ac$phi[1:pts]),
                               phi_2 = mean(ac$phi[(pts+1):nrow(ac)]),
                               date_change=ac$day[pts])
    )
    if(nrow(table_changed) %/% 1000 == nrow(table_changed) / 1000) print(dim(table_changed))
  }
}

actions_context_change <- actions_lunch0[user %in% table_changed$user]
actions_context_change <- merge(actions_context_change,table_changed)


## Preprocess of the data for visualization

# We only use some days before and after the change. We make multiple of weeks to aggregate later
ini_time <- -54
fin_time <- 54
time_window <- ini_time:fin_time

# We define the threshold between High and Low FFO environments using the median of the fraction of FFO to FO
thr_low <- median(c(table_changed$phi_1,table_changed$phi_2))
thr_high <- thr_low

# Construct a time series of the averages of all data pairs for each situation
# From low -> high change
tt <- table_changed[phi_1 < thr_low & phi_2 > thr_high]
ac_new <- actions_context_change[user %in% tt$user]
averages_lh <- ac_new[,.(n_data_pairs=.N,
                         nuffood=sum(yit,na.rm = T),
                         muFFO=mean(yit[is.food==1])),
                      .(time=day-date_change)]
# From low -> low change
tt <- table_changed[phi_1 < thr_low & phi_2 < thr_low]
ac_new <- actions_context_change[user %in% tt$user]
averages_ll <- ac_new[,.(n_data_pairs=.N,
                         nuffood=sum(yit,na.rm = T),
                         muFFO=mean(yit[is.food==1])),
                      .(time=day-date_change)]
# From high -> low change
tt <- table_changed[phi_1 > thr_high & phi_2 < thr_low]
ac_new <- actions_context_change[user %in% tt$user]
averages_hl <- ac_new[,.(n_data_pairs=.N,
                         nuffood=sum(yit,na.rm = T),
                         muFFO=mean(yit[is.food==1])),
                      .(time=day-date_change)]
# From high -> high change
tt <- table_changed[phi_1 > thr_high & phi_2 > thr_high]
ac_new <- actions_context_change[user %in% tt$user]
averages_hh <- ac_new[,.(n_data_pairs=.N,
                         nuffood=sum(yit,na.rm = T),
                         muFFO=mean(yit[is.food==1])),
                      .(time=day-date_change)]

## Evaluate Causal Impact in each case
require(CausalImpact)
# 1. From low -> high change using low -> low as conterfactual
table_causal_lh <- merge(averages_lh[time %in% time_window],
                         averages_ll[time %in% time_window],by="time")
# Smooth the time series
table_causal_lh$muFFO.x <- ksmooth(table_causal_lh$time,
                                   table_causal_lh$muFFO.x,
                                   bandwidth = 14)$y
table_causal_lh$muFFO.y <- ksmooth(table_causal_lh$time,
                                   table_causal_lh$muFFO.y,
                                   bandwidth = 14)$y
table_causal_lh <- table_causal_lh[!is.na(muFFO.x)]
table_causal_lh <- table_causal_lh[order(time)]
ss <- zoo(table_causal_lh[,c("muFFO.x","muFFO.y")])
before <- which(table_causal_lh$time < 0)
after <- which(table_causal_lh$time >= 0)
pre.period <- c(min(before),max(before))
post.period <- c(min(after),max(after))
impact <- CausalImpact(ss[,c(1,2)], pre.period, post.period)

series <- impact$series
series$time <- table_causal_lh$time
colors_series <- c(colors_publication[1],"black")
color_ribbon <- colors_publication[1]
g_lh1 <- ggplot(series,aes(x=time)) + 
  geom_ribbon(aes(ymin=point.pred.lower,ymax=point.pred.upper),
              fill=color_ribbon,alpha=0.3) +
  geom_line(aes(y=point.pred,col="Predicted\ncounterfactual"),size=0.6,linetype=2)+
  geom_line(aes(y=response,col="Original data"),size=0.6) + 
  labs(x="",y="Fraction of FFO visits",title="Low to High FFO context change",color="") +
  theme_publication() + geom_vline(xintercept = 0,linetype=2)+
  scale_colour_manual(values=rev(colors_series)) +
  theme(plot.margin=margin(0,0,0,0))+
  theme(legend.direction = "vertical",legend.position=c(.22,0.84))+
  theme(legend.key.width = unit(1.5, "line"))+
  scale_y_continuous(limits=c(0.10,0.3))+
  scale_x_continuous(breaks=c(-50,-25,0,25,50),limits=c(-50,50))
g_lh1


g_lh2 <- ggplot(series,aes(x=time)) + 
  geom_ribbon(aes(ymin=cum.effect.lower,ymax=cum.effect.upper),
              fill=color_ribbon,alpha=0.3) +
  geom_line(aes(y=cum.effect),size=0.5)+
  labs(x="Days after changing context",y="Difference in FFO visits \n(Cumulative Effect)",color="") +
  theme_publication() + geom_vline(xintercept = 0,linetype=2)+
  scale_colour_manual(values=colors_series) +
  theme(plot.margin=margin(0,0,0,0))+
  scale_y_continuous(limits=c(0,4.5))+
  scale_x_continuous(breaks=c(-50,-25,0,25,50),limits=c(-50,50))

# 2. From high -> low change using high -> high as conterfactual
table_causal_hl <- merge(averages_hl[time %in% time_window],
                         averages_hh[time %in% time_window],by="time")
# Smooth the time series
table_causal_hl$muFFO.x <- ksmooth(table_causal_hl$time,
                                   table_causal_hl$muFFO.x,
                                   bandwidth = 14)$y
table_causal_hl$muFFO.y <- ksmooth(table_causal_hl$time,
                                   table_causal_hl$muFFO.y,
                                   bandwidth = 14)$y
table_causal_hl <- table_causal_hl[!is.na(muFFO.x)]
table_causal_hl <- table_causal_hl[order(time)]
ss <- zoo(table_causal_hl[,c("muFFO.x","muFFO.y")])
before <- which(table_causal_hl$time < 0)
after <- which(table_causal_hl$time >= 0)
pre.period <- c(min(before),max(before))
post.period <- c(min(after),max(after))
impact <- CausalImpact(ss[,c(1,2)], pre.period, post.period)

series <- impact$series
series$time <- table_causal_hl$time
colors_series <- c(colors_publication[1],"black")
color_ribbon <- colors_publication[1]
g_hl1 <- ggplot(series,aes(x=time)) + 
  geom_ribbon(aes(ymin=point.pred.lower,ymax=point.pred.upper),
              fill=color_ribbon,alpha=0.3) +
  geom_line(aes(y=point.pred,col="Predicted\ncounterfactual"),size=0.6,linetype=2)+
  geom_line(aes(y=response,col="Original data"),size=0.6) + 
  labs(x="",y="Fraction of FFO visits",title="Low to High FFO context change",color="") +
  theme_publication() + geom_vline(xintercept = 0,linetype=2)+
  scale_colour_manual(values=rev(colors_series)) +
  theme(plot.margin=margin(0,0,0,0))+
  theme(legend.direction = "vertical",legend.position=c(.22,0.25))+
  theme(legend.key.width = unit(1.5, "line"))+
  scale_y_continuous(limits=c(0.10,0.3))+
  scale_x_continuous(breaks=c(-50,-25,0,25,50),limits=c(-50,50))

g_hl2 <- ggplot(series,aes(x=time)) + 
  geom_ribbon(aes(ymin=cum.effect.lower,ymax=cum.effect.upper),
              fill=color_ribbon,alpha=0.3) +
  geom_line(aes(y=cum.effect),size=0.5)+
  labs(x="Days after changing context",y="Difference in FFO visits \n(Cumulative Effect)",color="") +
  theme_publication() + geom_vline(xintercept = 0,linetype=2)+
  scale_colour_manual(values=colors_series) +
  theme(plot.margin=margin(0,0,0,0))+
  scale_y_continuous(limits=c(-4.5,0))+
  scale_x_continuous(breaks=c(-50,-25,0,25,50),limits=c(-50,50))


require(patchwork)
(g_lh1 | g_hl1) / (g_lh2 | g_hl2)
