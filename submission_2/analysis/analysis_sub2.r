install.packages('acs')
library(acs)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)
#Take Puerto Rico and D.c. out

#Question 1
#Plot the share of the adult population with direct purchase health insurance over time.
data <- read.delim("/Users/safiaread/Desktop/homework_5/data/output/acs_medicaid.txt")
head(data)
data <- data%>%
subset(State != "District of Columbia" & State != "Puerto Rico")

figure_1 <- data%>%
group_by(year)%>%
summarise(adult_share = sum(ins_direct)/sum(adult_pop))%>%
ggplot(aes(y = adult_share, x = year))+
geom_line()+
labs(x = "Year", y = "% Direct Purchase Health Insurance", title = "Share of the Adult Population with Direct Purchase Health Insurance over Time")

#Question 2
#Discuss the reduction in direct purchase health insurance in later years. Can you list a couple of policies that might have affected the 
#success of the direct purchase insurance market?

#Question 3
#Plot the share of the adult population with Medicaid over time.
figure_3 <- data%>%
group_by(year)%>%
summarise(adult_share = sum(ins_medicaid)/sum(adult_pop))%>%
ggplot(aes(y = adult_share, x = year))+
geom_line()+
labs(x = "Year", y = "% Medicaid Insurance", title = "Share of the Adult Population with Medicaid Insurance over Time")

#Question 4
#Plot the share of uninsured over time, separately by states 
#that expanded Medicaid in 2014 versus those that did not. 
#Drop all states that expanded after 2014.
data_q4 <- data%>%
mutate(expand_14 = ifelse(expand_year != "2014" | is.na(expand_year), FALSE, TRUE))

table(data$expand_14)

figure_4 <- data_q4%>%
filter(is.na(expand_year)| expand_14 == TRUE)%>%
group_by(year, expand_14)%>%
summarise(adult_share = sum(uninsured)/sum(adult_pop))%>%
ggplot(aes(y = adult_share, x = year, color = expand_14))+
geom_line()+
labs(x = "Year", y = "% Uninsured", title = "Share of the Adult Population that is Uninsured", color = "Expanded Medicaid in 2014")


#Question 5
#Calculate the average percent of uninsured individuals in 2012 and 2015, 
#separately for expansion and non-expansion states. 
#Present your results in a basic 2x2 DD table.
data_q5 <- data%>%
mutate(expand_14 = ifelse(expand_year != "2014" | is.na(expand_year), "not expanded", "expanded"))

figure_5 <- data_q5%>%
filter(year == "2012" | year == "2015")%>%
group_by(year, expand_14)%>%
summarise(percent_uninsured = mean(uninsured/adult_pop, na.rm = T))%>%
pivot_wider(names_from = expand_14, values_from = percent_uninsured)

#Question 6
#Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, again focusing only on 
#states that expanded in 2014 versus those that never expanded.
library(modelsummary)

data_q6 <- data %>% filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

#summary(lm(perc_unins ~ post + expand_ever + post*expand_ever, data=reg.data))
reg_q6 <- lm(perc_unins ~ post + expand_ever + post*expand_ever, data=data_q6)
modelsummary(list("DD (2014)"=reg_q6),
             shape=term + statistic ~ model, 
             gof_map=NA,
             coef_omit='Intercept',
             vcov=~State
         )

#Model summary in QMD not in this file when making workspace.

#Question 7
#Include state and year fixed effects in your estimates. 
#Try using the lfe or fixest package to estimate this instead of directly 
#including the fixed effects.

library(fixest)

m.dd.7 <- lm(perc_unins ~ post + expand_ever + treat, data=data_q6)

m.twfe.7 <- feols(perc_unins ~ treat | State + year, data=data_q6)

modelsummary(list("DD"=m.dd.7, "TWFE"=m.twfe.7),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State
         )

#Question 8
#Repeat the analysis in question 7 but include all states 
#(even those that expanded after 2014). Are your results different? If so, why?
data_q8 <- data %>% filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)
         
data_q8_2 <- data %>% 
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014),
         treat=case_when(
    year>=expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    year<expand_year & !is.na(expand_year) ~ 0)
  )

m.dd.8 <- lm(perc_unins ~ post + expand_ever + treat, data=data_q8)
m.twfe.8 <- feols(perc_unins ~ treat | State + year, data=data_q8)
fe.est2.8 <- feols(perc_unins ~ treat | State + year, data=data_q8_2) 

modelsummary(list("DD"=m.dd.8, "TWFE"=m.twfe.8, "Time Variance" = fe.est2.8),
         shape=term + statistic ~ model, 
         gof_map=NA,
         coef_omit='Intercept',
         vcov=~State
         )


#Question 9
#Provide an “event study” graph showing the effects of Medicaid expansion in each year. Use the specification that includes 
#state and year fixed effects, limited to states that expanded in 2014 or never expanded.
data_q9 <- data %>% 
  filter(expand_year==2014 | is.na(expand_year), !is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever)

mod.twfe.9 <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=data_q9)

iplot(mod.twfe.9, 
      xlab = 'Time to treatment',
      main = 'Event study')

#Question 10
#Repeat part 9 but again include states that expanded after 2014. Note: this is tricky…you need to put all states onto 
#“event time” to create this graph.

data_q10 <- data %>% 
  filter(!is.na(expand_ever)) %>%
  mutate(perc_unins=uninsured/adult_pop,
         post = (year>=2014), 
         treat=post*expand_ever,
         time_to_treat = ifelse(expand_ever==FALSE, 0, year-expand_year),
         time_to_treat = ifelse(time_to_treat < -4, -4, time_to_treat))

mod.twfe.10 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=data_q10)

iplot(mod.twfe.10, 
      xlab = 'Time to treatment',
      main = 'Event study')

#Check this not exactly

rm(list=c("data", "data_q4", "data_q5", "data_q9", "data_q10")) # nolint
save.image("submission_2/Hwk5_workspace.Rdata")