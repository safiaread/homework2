# Homework 2 Workspace for Sub 1

## Summarize the Data
# 1. How many hospitals filed more than one report in the same year? 
# Show your answer as a line graph of the number of hospitals over time.


nrow(duplicate.hcris)
  q1 <- duplicate.hcris%>%
  group_by(fyear)%>%
  count()
  ggplot(q1, aes(x = fyear, y = n))+
  geom_line()+
  geom_point()
 summary(duplicate.hcris)

#2. After removing/combining multiple reports, how many unique hospital
# IDs (Medicare provider numbers) exist in the data?

hcris <- read_rds("data/output/HCRIS_Data.rds")
length(unique(hcris$provider_number))


#3. What is the distribution of total charges (tot_charges in the data) 
#in each year? Show your results with a “violin” plot, with charges on 
#the y-axis and years on the x-axis.

head(hcris)
summary(hcris)
hcris%>%
ggplot(aes(x = year, y = tot_charges))+
geom_jitter(alpha = .05) +
  geom_violin(aes(group = cut_width(year, 1)), scale = "width")

#4. What is the distribution of estimated prices in each year? Again present 
#your results with a violin plot, and recall our formula for estimating 
#prices from class. Be sure to do something about outliers and/or negative 
#prices in the data.

hcris_price <- hcris%>%
mutate(discount_factor = 1-tot_discounts/tot_charges)%>%
mutate(price_num = (ip_charges + icu_charges + ancillary_charges)*discount_factor - tot_mcare_payment, price_denom = tot_discharges - mcare_discharges)%>%
mutate(price = abs(price_num/price_denom))

#Come back and drop outliers!

hcris_price%>%
ggplot(aes(x = year, y = price))+
geom_jitter(alpha = .05) +
  geom_violin(aes(group = cut_width(year, 1)), scale = "width")

## Estimate ATEs

#5. Calculate the average price among penalized versus non-penalized 
#hospitals.
hcris_2012 <- hcris_price%>%
filter(year == 2012)%>%
mutate(penalty = ifelse(hvbp_payment + hrrp_payment > 0, 1, 0))

summary(hcris_2012)

hcris_2012%>%
group_by(penalty)%>%
summarise(mean(price, na.rm = TRUE))

#6. Split hospitals into quartiles based on bed size. To do this, 
#create 4 new indicator variables, where each variable is set to 1 
#if the hospital’s bed size falls into the relevant quartile. Provide
# a table of the average price among treated/control groups for each 
#quartile.
hcris_2012 <- hcris_2012 %>%
mutate(first_quartile = ifelse(beds <= quantile(hcris_2012$beds, 0.25, na.rm = TRUE), 1, 0))%>%
mutate(second_quartile = ifelse(beds <= quantile(hcris_2012$beds, 0.5, na.rm = TRUE) & beds > quantile(hcris_2012$beds, 0.25, na.rm = TRUE), 1, 0)) %>%
mutate(third_quartile = ifelse(beds <= quantile(hcris_2012$beds, 0.75, na.rm = TRUE) & beds > quantile(hcris_2012$beds, 0.5, na.rm = TRUE), 1, 0)) %>%
mutate(fourth_quartile = ifelse(beds > quantile(hcris_2012$beds, 0.75, na.rm = TRUE), 1, 0))

table(hcris_2012$first_quartile)

fq_mean <- hcris_2012%>%
filter(first_quartile == 1)%>%
group_by(penalty)%>%
summarise(first_mean = mean(price, na.rm = TRUE))

sq_mean <- hcris_2012%>%
filter(second_quartile == 1)%>%
group_by(penalty)%>%
summarise(second_mean = mean(price, na.rm = TRUE))

tq_mean <- hcris_2012%>%
filter(third_quartile == 1)%>%
group_by(penalty)%>%
summarise(third_mean = mean(price, na.rm = TRUE))

foq_mean <- hcris_2012%>%
filter(fourth_quartile == 1)%>%
group_by(penalty)%>%
summarise(foruth_mean = mean(price, na.rm = TRUE))

fq_mean%>%
left_join(sq_mean, by = "penalty")%>%
left_join(tq_mean, by = "penalty")%>%
left_join(foq_mean, by = "penalty")



#7. Find the average treatment effect using each of the following 
#estimators, and present your results in a single table:

#a. Nearest neighbor matching (1-to-1) with inverse variance distance 
#based on quartiles of bed size
install.packages("Matching")
library("dplyr")
library("tidyverse")
hcris.vars <- hcris_2012 %>% 
  select(penalty,price, first_quartile, second_quartile, third_quartile, fourth_quartile) %>%
  filter(complete.cases(.))
hcris.covs <- hcris_2012 %>%
select(first_quartile, second_quartile, third_quartile, fourth_quartile)
m.nn.var <- Matching::Match(Y=hcris_2012$price,
                            Tr=hcris_2012$penalty,
                            X=hcris.covs,
                            M=4,  #<<
                            Weight=1,
                            estimand="ATE")

v.name=data.frame(new=c("Beds","Medicaid Discharges", "Inaptient Charges",
                   "Medicare Discharges", "Medicare Payments"))

#b. Nearest neighbor matching (1-to-1) with Mahalanobis distance based
# on quartiles of bed size

m.nn.md <- Matching::Match(Y=hcris.vars$price,
                           Tr=hcris.vars$penalty,
                           X=hcris.covs,
                           M=1,
                           Weight=2,
                           estimand="ATE")

#c. Inverse propensity weighting, where the propensity scores are 
#based on quartiles of bed size

hcris.vars <- hcris.vars %>%
  mutate(ipw = case_when(
    penalty==1 ~ 1/ps,
    penalty==0 ~ 1/(1-ps),
    TRUE ~ NA_real_
  ))
mean.t1 <- hcris.vars %>% filter(penalty==1) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t0 <- hcris.vars %>% filter(penalty==0) %>%
  select(price, ipw) %>% summarize(mean_p=weighted.mean(price,w=ipw))
mean.t1$mean_p - mean.t0$mean_p

# d. Simple linear regression, adjusting for quartiles of bed size 
#using dummy variables and appropriate interactions as discussed in 
#class

reg.dat <- hcris.vars %>% ungroup() %>% filter(complete.cases(.)) %>%
  mutate(beds_diff = penalty*(beds - mean(beds)),
         mcaid_diff = penalty*(mcaid_discharges - mean(mcaid_discharges)),
         ip_diff = penalty*(ip_charges - mean(ip_charges)),
         mcare_diff = penalty*(mcare_discharges - mean(mcare_discharges)),
         mpay_diff = penalty*(tot_mcare_payment - mean(tot_mcare_payment)))
reg <- lm(price ~ penalty + beds + mcaid_discharges + ip_charges + mcare_discharges + tot_mcare_payment + 
            beds_diff + mcaid_diff + ip_diff + mcare_diff + mpay_diff,
          data=reg.dat)
summary(reg)

#8. With these different treatment effect estimators, are the results
# similar, identical, very different?

#9. Do you think you’ve estimated a causal effect of the penalty? 
#Why or why not? (just a couple of sentences)

#10. Briefly describe your experience working with these data (just a 
#few sentences). Tell me one thing you learned and one thing that 
#really aggravated or surprised you. hi


#rm(list=c()) # nolint


save.image("submission1/Hwk2_workspace.Rdata")