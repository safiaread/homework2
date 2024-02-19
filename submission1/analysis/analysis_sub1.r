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



#7. Find the average treatment effect using each of the following 
#estimators, and present your results in a single table:

#a. Nearest neighbor matching (1-to-1) with inverse variance distance 
#based on quartiles of bed size

#b. Nearest neighbor matching (1-to-1) with Mahalanobis distance based
# on quartiles of bed size

#c. Inverse propensity weighting, where the propensity scores are 
#based on quartiles of bed size

# d. Simple linear regression, adjusting for quartiles of bed size 
#using dummy variables and appropriate interactions as discussed in 
#class


#8. With these different treatment effect estimators, are the results
# similar, identical, very different?

#9. Do you think you’ve estimated a causal effect of the penalty? 
#Why or why not? (just a couple of sentences)

#10. Briefly describe your experience working with these data (just a 
#few sentences). Tell me one thing you learned and one thing that 
#really aggravated or surprised you.

#rm(list=c()) # nolint


save.image("submission1/Hwk2_workspace.Rdata")