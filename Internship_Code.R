# First we have to load libraries we'll need for this analysis and import our data; if you dont have these download them! 
library(tidyverse)
library(mosaic)
library(fpp)
library(ggplot2)
library(tseries)
library(vegan)
library(rstatix)
library(coin)
library(NSM3)
############## (1) Establishing Seasonality & Adjusting Data ##############

x1 = read.csv("Group_1.csv") # x1 is group 1 data
x2 = read.csv("Group_2.csv") # x2 is group 2 data 
# dimensions of raw data: 1644 rows by 6 columns
# Need to determine if seasonality can be demonstrated in this data. If there is, we need to adjust data to compensate for it.
# We will be using an additive model, which assumes the effect of one factor (time) is constant throughout all levels of the other (water temp)

inds = seq(as.Date("2018-01-01"), as.Date("2022-06-30"), by = "day") # created this index to help organize the next line, update as needed
inds2 = seq(as.Date("2018-01-01"), as.Date("2022-06-30"), by = "month") # will need monthly index in the next step
G1_ts = ts(x1$Water.Temperature, start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365) # creates a time series of water temp data (daily)
plot(G1_ts, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 1 Daily Water Temperature Over Time (Non-Interpolated)") # Visually, there does seem to be a consistent drop in water temp at the start of the year; 
# there are also clear gaps in the data. It looks like the seasonality stays relatively constant over time (assumption for additive model)

# There are NAs in our data; to complete the rest of this process, we need to first interpolate to fill in any missing values.
# This will effect our findings and it must be noted in our results/discussion
G1_tsint = na.approx(G1_ts) # this will fill any NA values in our data through interpolation
plot(G1_tsint, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 1 Daily Water Temperature Over Time (Interpolated)") # The holes in the data are gone now, ready to proceed

decompose_G1_tsint = decompose(G1_tsint, "additive") # This function will do all of the work for us in terms of extracting seasonality
plot(decompose_G1_tsint$seasonal) # this is the important one! This shows us the seasonal fluctuations of our data
plot(decompose_G1_tsint$trend) # this tells us the general trend of our overall data ; seems like water temp is going down over all (slightly)
plot(decompose_G1_tsint$random) # this tells us demonstrates the random noise in our data
plot(decompose_G1_tsint) # All graphs in 1 plot

adjust_G1_tsint =  G1_tsint - decompose_G1_tsint$seasonal # this is our seasonally adjusted data (subtract seasonal variation from observed data)
plot(adjust_G1_tsint, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 1 Daily Water Temperature Over Time (Adjusted)") # plots adjusted data as a time series
# Now that we have our seasonally adjusted data, we need to add this data to x1
ad.Water.Temperature1 = as.data.frame(adjust_G1_tsint)
x1$ad.Water.Temperature = ad.Water.Temperature1

# Now we do the same with group 2

G2_ts = ts(x2$Water.Temperature, start = c(2018, as.numeric(format(inds[1], "%j"))), frequency = 365)
plot(G2_ts, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 2 Daily Water Temperature Over Time (Raw)") # Visually, there does seem to be a consistent drop in water temp at the start/end of the year; 
# there are also clear gaps in the data. It looks like the seasonality stays relatively constant over time (assumption for additive model)

G2_tsint = na.approx(G2_ts) # this will fill in any NA values in our data through interpolation
plot(G2_tsint, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 2 Daily Water Temperature Over Time (Unadjusted)") # The holes in the data are gone now, ready to proceed

decompose_G2_tsint = decompose(G2_tsint, "additive") # This function will do all of the work for us in terms of extracting seasonality
plot(decompose_G2_tsint$seasonal) # this is the important one! This tells us the seasonal fluctuations of our data
plot(decompose_G2_tsint$trend) # this tells us the general trend of our overall data ; seems like water temp is going down over all (slightly)
plot(decompose_G2_tsint$random) # this tells us demonstrates the random noise in our data
plot(decompose_G2_tsint) # All graphs in 1 plot

adjust_G2_tsint =  G2_tsint - decompose_G2_tsint$seasonal 
plot(adjust_G2_tsint, xlab = "Time", ylab = "Water Temperature (C)", main = "Group 2 Daily Water Temperature Over Time (Adjusted)")
ad.Water.Temperature2 = as.data.frame(adjust_G2_tsint)
x2$ad.Water.Temperature = ad.Water.Temperature2

# In this step, we first filled in gaps in our data through interpolation. We then used the decompose function to extract the seasonal 
# variation and used this to adjust our data to account for seasonality. We then plotted our adjusted time series data. Next, we have
# to wrangle our data and calculate monhtly average water temperatures for groups 1 and 2

############## (2) Data Wrangling ##################

# First thing we wanna do is convert our holding tank data into a numeric value we can use
x1$Tank = gsub("MAT", "", as.character(x1$Tank)) # Removes characters from tank column so we can work w numbers 
x1$Tank = as.numeric(x1$Tank) # convert from character to numeric values 

#Now we can make the matrix and start adding stuff to it 
matrix_dataG1 = matrix(nrow=54, ncol =4) # need 54 rows x 4 columns (year, month, avg water temp, # spawns)
# Note: we are only including data up to June 2022, so we will only require 54 rows, change this value as needed 
for (i in 1:5) # i represent number of years we have of data; 2018-2022 = 5 (adjust as needed)
{
  yr = x1[x1$Year == 2017 + i, ] # subset by year, iterating through i
  for (j in 1:12)# j represents number of months in a year 
  {
    mon = yr[yr$Month == j, ] # subset by month, iterating through j
    matrix_dataG1[12*i - 12 + j , 1 ] = 2017 + i # adds years to matrix
    matrix_dataG1[12*i - 12 + j , 2] = j # adds month to matrix
    #matrix_dataG1[12*i - 12 + j, 3] = mean(mon$Water.Temperature, na.rm = TRUE) # avg monthly water temp to matrix
    matrix_dataG1[12*i - 12 + j, 3] = mean(mon$ad.Water.Temperature$x) # avg monthly water temp to matrix using seasonally adjusted data
    matrix_dataG1[12*i - 12 + j, 4] = sum(mon$Spawn, na.rm = TRUE) # monthly total spawns to matrix
    # na.rm will ignore the NA values so we dont get an error
    # will get an out of bounds error bc we dont have all the data for 2022

  }
}

# Now we do the same thing for group 2 
x2$Tank = gsub("MAT", "", as.character(x2$Tank)) # Removes characters from tank column so we can work w numbers 
x2$Tank = as.numeric(x2$Tank) # convert from character to numeric values 

matrix_dataG2 = matrix(nrow=54, ncol =4) # need 54 rows x 4 columns (month, year, avg water temp, # spawns)
for (i in 1:5) # i represent number of years we have of data; 2018-2021 = 5 (change as needed)
{
  yr = x2[x2$Year == 2017 + i, ] # subset by year, iterating through i
  for (j in 1:12)# j represents number of months in a year 
  {
    mon = yr[yr$Month == j, ] # subset by month, iterating through j
    matrix_dataG2[12*i - 12 + j , 1 ] = 2017 + i
    matrix_dataG2[12*i - 12 + j , 2] = j # adds month to matrix
    #matrix_dataG2[12*i - 12 + j, 3] = mean(mon$Water.Temperature, na.rm = TRUE) # avg monthly water temp to matrix (not used)
    matrix_dataG2[12*i - 12 + j, 3] = mean(mon$ad.Water.Temperature$x) # avg monthly water temp to matrix using seasonally adjusted data
    matrix_dataG2[12*i - 12 + j, 4] = sum(mon$Spawn, na.rm = TRUE) # monthly total spawns to matrix
    # na.rm will ignore the NA values
    # will get an out of bounds error bc we dont have all the data for 2022
  }
}

# In this step, we created a data frame (dF) that shows us year, month, average monthly water temperature, monthly spawn count, and mat tank
# where the cohort was held the longest in that particular month.
# We will be using this dF for the rest of the remaining analysis.

# We have to check for the normal distribution of our data to determine if we should 
# use parametric or non-parametric tests
shapiro.test(matrix_dataG1[,3]) # normality test for group 1 monthly water temp
shapiro.test(matrix_dataG1[,4]) # normality test for group 1 monthly spawn frequency
# HO : The sample comes from a normally distributed population
# HA : The sample does not come from a normally distributed population
# Monthly water temp results : W = 0.92083, p-value = 0.001606
# Monthly spawn freq results : W = 0.78344, p-value = 1.678e-07
# Shapiro test show that the samples are not normally distributed, we will have to run non-parametric tests

shapiro.test(matrix_dataG2[,3]) # normality test for group 2 monthly water temp
shapiro.test(matrix_dataG2[,4]) # normality test for group 2 monthly spawn frequency
# HO : The sample comes from a normally distributed population
# HA : The sample does not come from a normally distributed population
# Monthly water temp results : W = 0.95968, p-value = 0.02094
# Monthly spawn freq results : W = 0.57205, p-value = 2.663e-11
# Shapiro test show that the samples are not normally distributed, we will have to run non-parametric tests


############## (3) ACTIVITY 1: Comparing Average Monthly Water Temperature and Spawning Frequency Between Groups ##################
matrixG1_ts_temp = ts(matrix_dataG1[,3], start = c(2018, as.numeric(format(inds2[1], "%j"))), frequency = 12) # time series of avg monthly water temperature
plot(matrixG1_ts_temp, xlab = "Time", ylab = "Average Water Temperature (C)", main = "Group 1 Avg. Monthly Water Temperature Over Time")
# Looks pretty choppy after 2018
matrixG2_ts_temp = ts(matrix_dataG2[,3], start = c(2018, as.numeric(format(inds2[1], "%j"))), frequency = 12) # time series of avg monthly water temperature
plot(matrixG2_ts_temp, xlab = "Time", ylab = "Average Water Temperature (C)", main = "Group 2 Avg. Monthly Water Temperature Over Time )")
# Also looks quite choppy

# Let's first visually compare the two groups by putting the monthly avg time series' on the same plot
plot(matrixG1_ts_temp, col = "black", ylim = c(20,31), 
     xlab = "Time", ylab = "Avg. Water Temperature (C)")
lines(matrixG2_ts_temp, col = "gray")
lines(abline(h = 26, col = "black", lty = 4))
lines(abline(h = 28, col = "black", lty = 4))
legend("bottomright", inset=c(.03, .03), c("Group 1","Group 2"), lty = 1, col = c("black","gray"), cex = 0.85, title="Cohort")
# We can easily observe that group 2 has a fairly consistent change in water temperature, group 1 is definitely more choppy

water_temp = matrix(nrow = 108, ncol = 2) # matrix for box plot
water_temp[1:54,1] = matrix_dataG1[,3] # copy over year from earlier matrix
water_temp[55:108,1] = matrix_dataG2[,3] # copy over year from earlier matrix
water_temp[1:54,2] = 1 # Group number
water_temp[55:108,2] = 2 # Group Number
water_temp_df = as.data.frame(water_temp)
favstats(V1 ~ V2, data = water_temp_df)

MWU_water = group_by(water_temp_df, V2) %>%
  summarise(
    count = n(),
    mean = mean(V1, na.rm = TRUE),
    sd = sd(V1, na.rm = TRUE),
    median = median(V1, na.rm = TRUE),
    IQR = IQR(V1, na.rm = TRUE)
  )
print(MWU_water)

water_temp_df$V2 = as.factor(water_temp_df$V2)# Changing V2 to a factor so its counter as a categorical var (instead of integer)
box_water_temp = ggplot(data = water_temp_df, aes(x= V2, y= V1, group = V2, fill = V2), auto.key = T) +
  geom_boxplot() + labs(x = "Group", y = "Water Temperature (C)") + labs(fill="Group") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + scale_fill_manual(values = c(gray(0.45),gray(0.85)))
print(box_water_temp)
# visualizes the data as box plot


wilcox.test(V1~V2, data = water_temp_df)
water_temp_df %>% wilcox_effsize(V1~V2) #calculates effect size of the wilcox test
# HO : There is no statistically significant difference in median water temperature between groups 1 and 2
# HA : There is a statistically significant difference in median water temperature between groups 1 and 2
# W = 1667, p-value = 0.2002 r = 0.124
# small effect size (r) was detected
# Our calculated test statistic is W = 1667 & corresponding p=value is 0.2002. Since this p-value is not less than 0.05, we fail to reject the null hypothesis.
# There is no statistically significant difference in the average water temperature between groups 1 and 2

# Now, we'll do the same thing for spawn rates;
# HO : There is no statistically significant difference in median spawn frequency between groups 1 and 2
# HA : here is a statistically significant difference in median spawn frequency between groups 1 and 2
matrixG1_ts_spawn = ts(matrix_dataG1[,4], start = c(2018, as.numeric(format(inds2[1], "%j"))), frequency = 12) # time series of avg monthly water temperature
plot(matrixG1_ts_spawn, xlab = "Time", ylab = "Average Water Temperature (C)", main = "Group 1 Monthly Spawning Frequency Over Time")
# Looks pretty choppy after 2018
matrixG2_ts_spawn = ts(matrix_dataG2[,4], start = c(2018, as.numeric(format(inds2[1], "%j"))), frequency = 12) # time series of avg monthly water temperature
plot(matrixG2_ts_spawn, xlab = "Time", ylab = "Average Water Temperature (C)", main = "Group 2 Avg. Monthly Spawning Frequency Over Time")
# Also looks quite choppy

plot(matrixG1_ts_spawn, col = "black", ylim = c(0,21),
     xlab = "Time (Monthly)", ylab = "Spawn Frequency")
lines(matrixG2_ts_spawn, col = "gray")
legend("topright", inset=c(.03, .03), title = "Cohort", c("Group 1", "Group 2"), lty = 1, col = c("black","gray"), cex = .85) # NEED TO FIX LEGEND PLACEMENT

spawn = matrix(nrow = 108, ncol = 2) # matrix we will use for the analysis
spawn[1:54,1] = matrix_dataG1[,4] # copy over year from earlier matrix
spawn[55:108,1] = matrix_dataG2[,4] # copy over year from earlier matrix
spawn[1:54,2] = 1 # copy over month from earlier matrix
spawn[55:108,2] = 2
spawn_df = as.data.frame(spawn)
favstats(V1 ~ V2, data = spawn_df)

MWU_spawns = group_by(spawn_df, V2) %>%
  summarise(
    count = n(),
    mean = mean(V1, na.rm = TRUE),
    sd = sd(V1, na.rm = TRUE),
    median = median(V1, na.rm = TRUE),
    IQR = IQR(V1, na.rm = TRUE)
  )
print(MWU_spawns)

spawn_df$V2 = as.factor(spawn_df$V2) # Changing V2 to a factor so its counter as a categorical var (instead of integer)
box_spawn = ggplot(data = spawn_df, aes(x= V2, y= V1, group = V2, fill = V2), auto.key = T) +
  geom_boxplot() + labs(x = "Group", y = "Spawn Frequency") + labs(fill="Group") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + scale_fill_manual(values = c(gray(0.45),gray(0.85)))
print(box_spawn)

wilcox.test(V1~V2, data = spawn_df)
spawn_df %>% wilcox_effsize(V1~V2) #calculates effect size of the wilcox test
# HO : There is no statistically significant difference in median monthly spawn freq between groups 1 and 2
# HA : There is a statistically significant difference in median monthly spawn freq between groups 1 and 2
# W = 1813.5, p-value = 0.023, r = 0.219
# Our calculated test statistic is W = 1813.5 & corresponding p=value is 0.023 Since this p-value is less than 0.05, we have sufficient 
# evidence to reject the null hypothesis. A statistically significant difference monthly spawning frequency exists between groups 1 and 2

# NO DIFFERENCE IN WATER TEMP, BUT SIGNFICANT DIFF IN SPAWN FREQ. WILL LOOK AT THE CORRELATION BETWEEN WATER TEMP AND SPAWN FREQ


############## (4) ACTIVITY 2: Interactions Between Water Temperature and Spawning Frequency (Correlation) ##################
# To determine if a statistically significant correlation exists between average water temperature and spawning 
# frequency, we will be calculating Spearman correlation coefficients for groups 1 and 2 (our data is not normally distributed so we have to go w/ non-parametric)
# HO : no statistically significant correlation exists between average monthly water temperature and spawning frequency
# HA : a statistically significant correlation exists between average monthly water temperature and spawning frequency

# First, we need to add our monthly spawn counts to the monthly data frame
water_temp_df$Spawns[1:54] = matrix_dataG1[,4] # adds monthly spawns for group 1
water_temp_df$Spawns[55:108] = matrix_dataG2[,4] # adds monthly spawns for group 2

ggplot(water_temp_df[1:54,], aes(x = V1, y = Spawns)) + geom_point() + xlab("Water Temperature (C)") + ylab("Spawn Frequency") +
  geom_smooth(method = "lm",se = T, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic()
# Plots avg. monthly water temp (Group) on the x axis and Spawn Frequency on the right + linear model
# There appears to be a positive correlation (passes linearity check)

cor.test(water_temp_df[1:54,1], water_temp_df[1:54,3], method = c("kendall")) # lm (Group 1 Spawning Frequency as a function of Monthly Avg Water Temperature)
kendall.ci(water_temp_df[1:54,1], water_temp_df[1:54,3], alpha = 0.05, type = "t")
# HO : There is no statistically significant correlation between water temperature and spawning frequency in Group 1
# HA : There is a statistically significant correlation between water temperature and spawning frequency in Group 1
# z = 2.373, p-value = 0.01764 ; 95% CI = 0.052 - 0.422
# tau = 0.2370324
# Our calculated test statistic is z = 2.373 & corresponding p=value is 0.01764. Since this p-value is less than 0.05, we have sufficient 
# evidence to reject the null hypothesis. There is a statistically significant correlation between water temperature and spawning frequency in Group 1
# Our calculated kendall correlation coeffecient (tau) is 0.2370324. 

# Now we do the same for Group 2
ggplot(water_temp_df[55:108,], aes(x = V1, y = Spawns)) + geom_point() + xlab("Water Temperature (C)") + ylab("Spawn Frequency") +
  geom_smooth(method = "lm",se = T, color = 'black') +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic()
# Plots avg. monthly water temp (Group 2) on the x axis and Spawn Frequency on the right + linear model
# There doesnt seem to be any sort of association


cor.test(water_temp_df[55:108,1], water_temp_df[55:108,3], method = c("kendall")) # lm (Group 1 Spawning Frequency as a function of Monthly Avg Water Temperature)
kendall.ci(water_temp_df[55:108,1], water_temp_df[55:108,3], alpha = 0.05, type = "t")
# HO : There is no statistically significant correlation between water temperature and spawning frequency in Group 1
# HA : There is a statistically significant correlation between water temperature and spawning frequency in Group 1
# z = 0.85574, p-value = 0.3921 ; 95 % CI: -0.069 - 0.246
# tau = 0.08835089
# Our calculated test statistic is z = 0.85574 & corresponding p=value is 0.3921. Since this p-value is less than 0.05, we have sufficient 
# evidence to reject the null hypothesis. There is not statistically significant correlation between water temperature and spawning frequency in Group 2.
# Our calculated kendall correlation coeffecient (tau) is 0.08835089.

# STATISTICALLY SIGNIFICANT CORRELATION BETWEEN WATER TEMPERATURE AND SPAWNING 
# FREQUENCY EXISTS IN GROUP 1 BUT NOT IN GROUP 2!!!!

# IN THE FUTURE, IF WE USE MORE OF THE DAILY PARAMETERS (DO, NH4, ETC.) WE CAN CONDUCT AN ANOVA
# WE'RE ONLY USING TWO VARIABLES HERE SO WE CANT REALLY USE ANOVA, DRAWBACK OF THE STUDY

############## (5) ACTIVITY 3 : Integrating holding tanks into analysis (Kruskal Wallis) ##################
# Here, we are going to sort our groups by holding tank (categorical), and compare water temp. Since water temp and spawns were only 
# statistically significant in Group 1, lets just do this tests for group 1
# LETS GET A SPAWN FREQUENCY CHART OF SPAWNS BY HOLDING TANK
  
tank_totalsG1 = as.data.frame(matrix(nrow=1608, ncol =2)) # Create matrix w/ 2 columns, 1 is for the tank number and the other is for associated daily water temps
tank_totalsG1[1:611,1] = "1"
tank_totalsG1[612:1365,1] = "2"
tank_totalsG1[1364:1608,1] = "4"
T1_G1 = na.omit(x1[x1$Tank == '1', 7]) # This pulls all the adjusted daily water temperatures for holding tank 1 in Group 1
tank_totalsG1[1:611,2] = T1_G1[]
T2_G1 = na.omit(x1[x1$Tank == '2', 7]) # This pulls all the adjusted daily water temperatures for holding tank 2 in Group 1
tank_totalsG1[612:1363,2] = T2_G1[]
T4_G1 = na.omit(x1[x1$Tank == '4', 7]) # This pulls all the adjusted daily water temperatures for holding tank 4 in Group 1
tank_totalsG1[1364:1608,2] = T4_G1[]
tank_totalsG1[,1] = as.factor(tank_totalsG1[,1]) # Turns our hold tank number to categorical data, can use this to group data

water_tankG1 = group_by(tank_totalsG1, V1) %>%
  summarise(
    count = n(),
    mean = mean(V2, na.rm = TRUE),
    sd = sd(V2, na.rm = TRUE),
    median = median(V2, na.rm = TRUE),
    IQR = IQR(V2, na.rm = TRUE)
  )
print(water_tankG1)

ggplot(tank_totalsG1, mapping = aes(x = V1, y = V2, fill = V1), auto.key = T) + geom_boxplot() +
  labs(x = "Holding Tank", y = "Water Temperature(C)") + labs(fill="Tank") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + scale_fill_manual(values = c(gray(0.45),gray(0.65),gray(0.85)))
#ggplot(tank_totalsG1, mapping=aes(x=V1, y = V2, color = V1))+ geom_point() # x= water temp, y = spawn freq, color = holding tank GROUP 2  
kruskal.test(V2~V1, data = tank_totalsG1) # Non-parametric equivalent of ANOVA
# HO : There is no statistically significant difference between water temperature in holding tanks for Group 1
# HA : There is A statistically significant difference between water temperature in holding tanks for Group 1 
# Kruskal-Wallis chi-squared = 8.5114, df = 2, p-value = 0.01418
# As our p-val < 0.05, there is sufficient evidence to suggest that a significant difference in water temperature exists between holding tanks of group 1 

# We know theres a significant difference, but we dont know which pairs of groups are different. Will use pairwise wilcocx test to check
pw_testG1 = pairwise.wilcox.test(tank_totalsG1$V2, tank_totalsG1$V1, p.adjust.method = "BH")
print(pw_testG1)
#      1     2    
# 2  0.780   -    
# 4  0.012  0.012
# Pairwise comparison shows that holding tank 1 and 2 are not significantly different and are both significantly different from holding tank 4
# Holding tank 4 has about 1/3 less data than the others, this could contribute to that finding. However, tank 4 does have a ton of lower water temperatures

# Since there was significance between water temp and spawns in group 1 but not 2, we'll also do this analysis for group 2. 
tank_totalsG2 = as.data.frame(matrix(nrow=1620, ncol =2)) # Create matrix w/ 2 columns, 1 is for the tank number and the other is for associated daily water temps
tank_totalsG2[1:337,1] = "1"
tank_totalsG2[338:1035,1] = "2"
tank_totalsG2[1036:1511,1] = "4"
tank_totalsG2[1512:1620,1] = "6"
T1_G2 = na.omit(x2[x2$Tank == '1', 7]) # This pulls all the adjusted daily water temperatures for holding tank 1 in Group 1
tank_totalsG2[1:337,2] = T1_G2[]
T2_G2 = na.omit(x2[x2$Tank == '2', 7]) # This pulls all the adjusted daily water temperatures for holding tank 1 in Group 1
tank_totalsG2[338:1035,2] = T2_G2[]
T4_G2 = na.omit(x2[x2$Tank == '4', 7]) # This pulls all the adjusted daily water temperatures for holding tank 1 in Group 1
tank_totalsG2[1036:1511,2] = T4_G2[]
T6_G2 = na.omit(x2[x2$Tank == '6', 7]) # This pulls all the adjusted daily water temperatures for holding tank 1 in Group 1
tank_totalsG2[1512:1620,2] = T6_G2[]
tank_totalsG2[,1] = as.factor(tank_totalsG2[,1]) # Turns our hold tank number to categorical data, can use this to group data

water_tankG2 = group_by(tank_totalsG2, V1) %>%
  summarise(
    count = n(),
    mean = mean(V2, na.rm = TRUE),
    sd = sd(V2, na.rm = TRUE),
    median = median(V2, na.rm = TRUE),
    IQR = IQR(V2, na.rm = TRUE)
  )
print(water_tankG2)

ggplot(tank_totalsG2, mapping = aes(x = V1, y = V2, fill = V1), auto.key = T) + geom_boxplot() +
  labs(x = "Holding Tank", y = "Water Temperature(C)") + labs(fill="Tank") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + scale_fill_manual(values = c(gray(0.35),gray(0.45),gray(0.65),gray(0.80)))
#ggplot(tank_totalsG2, mapping=aes(x=V1, y = V2, color = V1))+ geom_point() # x= water temp, y = spawn freq, color = holding tank 

kruskal.test(V2~V1, data = tank_totalsG2) # Non-parametric equivalent of ANOVA
# HO : There is no statistically significant difference between daily water temperature in holding tanks for Group 2
# HA : There is A statistically significant difference between daily water temperature in holding tanks for Group 2 
# Kruskal-Wallis chi-squared = 282.6, df = 3, p-value < 2.2e-16
# As our p-val < 0.05, there is sufficient evidence to suggest that a significant difference in water temperature exists between holding tanks of group 2

pw_testG2 = pairwise.wilcox.test(tank_totalsG2$V2, tank_totalsG2$V1, p.adjust.method = "BH")
print(pw_testG2)
#       1          2       4      
# 2   2.5e-11      -       -      
# 4   2.6e-07    < 2e-16   -      
# 6   1.4e-05     0.96  < 2e-16

# Pairwise comparison shows that holding tank 1 is significantly different from tanks 2, 4, and 6
# Tank 2 is significantly different from 1 and 4, but not 6
# Tank 4 is significantly different from 1, 2, and 6 (look athose outliers!)
# Tank 6 is significantly different from 1 and 4, but not 2
# Holding tank 6 only has 100 data points, this could contribute to the results of the pairwise test.

# Tests show that a statistically significant difference exists between daily water temperatures in holding tanks for group 2.





