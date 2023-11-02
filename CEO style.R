sink(file='output.txt')
# Set plot size to 4 x 3
options(repr.plot.width=6, repr.plot.height=4)

# Load the data
CEOdata <- read.csv("investCEO.csv")

#EXPLORE THE DATA 

# What time period does it cover? 
df_1 <- as.data.frame(CEOdata$year)
# Create a year frequence dataframe using table function, which each year appears one time
fyear <- as.data.frame(table(df_1))
cat("the period covers years: ",paste(fyear$CEOdata.year))

# How many firms are included in the dataset?

df_2 <- as.data.frame(CEOdata$firmid)
# Create a firmid frequence dataframe using table function, which each firm appears one time 
ffirmid <- as.data.frame(table(df_2))
cat("\nThere are", nrow(ffirmid),"firms in the dataset")

# How many observations are in the dataset?

# Print the number of rows in the CEOdata as the result
cat('\nThere are', nrow(CEOdata), 'observations in the dataset')

# Is the dataset balanced?

#install.packages("plm")
library(plm)
# Using function is.pbalanced. If TRUE, the dataset is balanced
is.pbalanced(CEOdata, index = c("year","firmid"))

#Plot a histogram of Investment, and describe its distribution

# install.packages("ggplot2")
library(ggplot2)
ggplot(CEOdata, aes(x = I))+ # choose I as the data to demonstrate
  geom_histogram(fill = "steelblue", color ="black", alpha =0.6, bins =30)+ # add styling
  geom_vline(aes(xintercept=mean(I)), color="red", linetype="dashed", size=1)+  # present mean value of Investment
  ggtitle("Histogram of Investment(I)")+  # add title
  xlab("Investment(%)")+  # change horizontal axis's name 
  theme_minimal()  # minimal background     

#install.packages("psych")
library(psych)
# Describe the distributions statistically
describe(CEOdata$I) 

# Set the seed to your candidate number
set.seed(257084)

# Randomly choose a one year time period from the dataset, save the randomly selected time period as indext
indext <- sample(2014:2021,1)

# Report the value of indext
indext

# Create a subset of the dataset for your chosen time period. Label your new dataset as mydata
mydata <- subset(CEOdata, year == indext)

# Plot the distribution of Investment for CEOs classified as overconfidence and non - overconfidene 
# Choose I as the data to demonstrate
ggplot(mydata ,aes(x=I))+
  # Present the distribution of overconfident CEO's Investment, add styling
  geom_histogram(data=subset(mydata, TD == 1), aes(fill=factor(TD)), alpha = 0.3, bins = 40)+ 
  # Present the distribution of non - overconfident CEO's Investment, add styling       
  geom_histogram(data=subset(mydata, TD == 0), aes(fill=factor(TD)), alpha = 0.3, bins = 40)+
  # Label the plots with different colors  
  scale_fill_manual(name="TD", values=c("aquamarine3","red"),labels=c("non - overconfidenct CEO","overconfidenct CEO"))+
  # Add title       
  ggtitle("Histogram of Investment")+
  # Change horizontal axis's name 
  xlab("Investment (%)")+
  # Minimal background       
  theme_minimal()

# Describe the distributions statistically for Investment(%) of overconfident CEOs
cat('Overconfidence:')
describe(mydata$I[mydata$TD == 1])

# Describe the distributions statistically Investment(%) of non - overconfident CEOs
cat('Non - overconfidence:')
describe(mydata$I[mydata$TD == 0])


#Consider the following regression equation:
#I_i = β_0+ β_1 CF_i + β_2 size_i + β_3 MV_i + β_4 CG_i + β_5 TD_i + β_6 (CF_i × TD_i ) + u_i
# Estimate the regression equation using mydata with homoskedasticity only standard errors
reg_1 <- lm(I~CF+size+MV+CG+TD+CF:TD, data = mydata)

# install. packages("AER")
library(AER)
# t test of coefficients with heteroskedasticity robust standard errors
coeftest(reg_1, vcov.= vcovHC, type = "HC1")

# install.packages("stargazer")
library(stargazer)
# Heteroskedasticity robust standard errors
# Save robust standard errors
cov1 <- vcovHC(reg_1, type = "HC1")
robust_se <- sqrt(diag(cov1))
# Compare the default standard error and robust standard error 
stargazer(reg_1, reg_1, type="text", digits = 3, se=list(NULL,robust_se), column.labels = c("default SE", "robust SE"))

# Testing joint / multiple hypothesis tests with heteroskedastic-robust standard errors.
print(linearHypothesis(reg_1,c("TD=0","CF:TD=0"),white.adjust = "hc1"))


# Creat 2 subsets with TD = 1 and TD =0 from mydata
mydata_1 <- subset(mydata, mydata$TD == 1)
mydata_0 <- subset(mydata, mydata$TD == 0)

#install.packages("margins")
library(margins)
# Creat a margin table, which the "fitted" column is the predicted value 
margin_1 <- margins(reg_1,at = list(CF = mean(mydata_1$CF), size = mean(mydata_1$size), MV = mean(mydata_1$MV), 
                                    CG = mean(mydata_1$CG),TD = 1))[1,]
# Print the result with 2 decimals
cat("The predicted investment percentage for overconfident CEOs:",round((margin_1$fitted),2))

# Case 2: Non-Overconfident CEOs
# Creat a margin table, which the "fitted" column is the predicted value 
margin_0 <- margins(reg_1,at = list(CF = mean(mydata_0$CF), size = mean(mydata_0$size), MV = mean(mydata_0$MV), 
                                    CG = mean(mydata_0$CG),TD = 0))[1,]
# Print the result with 2 decimals
cat("\nThe predicted investment percentage for non- overconfident CEOs:",round((margin_0$fitted),2))

# The differences between these predicted values
diff <- margin_0$fitted - margin_1$fitted
cat("\nThe difference between these predicted values:",round(diff,2))

# Estimate the model
reg_2 <- lm(I~CF+CG+MV+TD+CF:TD+I(CF^2), data= mydata)
summary(reg_2)
# t test of coefficients with heteroskedasticity robust standard errors
coeftest(reg_2, vcov.= vcovHC, type = "HC1")

# Marginal effect of CF at CF = 10, TD = 0
cat('marginal effect of CF at CF = 10 and TD = 1:', summary(margins(reg_2,at = list(CF = 10, TD = 1)))[1,4])
# Marginal effect of CF at CF = 10, TD = 1
cat('\nmarginal effect of CF at CF = 10 and TD = 0:', summary(margins(reg_2,at = list(CF = 10, TD = 0)))[1,4])

# Alternative way
reg_sum_2 <- summary(reg_2)
me_1 <- reg_sum_2$coef[2,1] + reg_sum_2$coef[7,1] + 2*reg_sum_2$coef[6,1]*10
cat('\nmarginal effect of CF at CF = 10 and TD = 1 (alternative way):',me_1)
me_0 <- reg_sum_2$coef[2,1] + 2*reg_sum_2$coef[6,1]*10
cat('\nmarginal effect of CF at CF = 10 and TD = 0 (alternative way):',me_0)

# Testing joint hypothesis tests with heteroskedastic-robust standard errors.
print(linearHypothesis(reg_2,c("I(CF^2)","CF:TD"),vcov.= vcovHC, type = "HC1"))

# Estimate the regression
reg_3 <- plm(I~CF+size+MV+CG+TD+CF:TD,
             data = CEOdata,
             index = c("firmid","year"),
             model ="within",
             effect = "twoway")

# Test the coefficients
coeftest(reg_3, vcov.= vcovHC(reg_3, cluster = "group"))
sink(file=NULL)