# Credit default analysis

# OBJECTIVE
# Create a reliable classifier in order to correctly predict default based on 400 observations of 7 variables:
# Age
# Years.at.Employer
# Years.at.Address
# Income
# Credit.Card.Debt
# Automobile.Debt


# Import data
library(readxl)        #loading library for import data
credit_default_data <- read_excel("C:/Data_Analysis_Projects/Credit_Default/credit_default_data.xlsx")

# Dataset characteristics
summary(credit_default_data)
subset(credit_default_data, subset = credit_default_data$Default==1)

plot(credit_default_data,col=credit_default_data$Default+1) # color = 0 is white, for that reason color +1 was used
                                                            # no major separations were observed in the plot
# _______________________UNSUPERVISED______________________________

# Cluster Analysis

data.std=scale(credit_default_data[,2:7])
hc=hclust(dist(data.std),method="complete")     # using complete linkage (the distance between clusters is mesured taking the distance between the elements of each cluster that are farther away from each other)
plot(hc,labels=Default)                         # a group in the far left seens to have a particularly low default rate
groups.hc=cutree(hc,k=9)                                # the dendogram was cut in a way to isolate a group of potencial "good customers" in the dataset
plot(hc,labels = groups.hc)
data.std = data.frame(data.std,groups.hc,Default)       # updating the dataset with the group data from clustering
sub=subset(data.std,subset = groups.hc==5)              # subset with group 5 only
plot(sub,col=sub$Default+1)

summary(sub)
summary(data.std)
                                               # comparing the group 5 with the rest of the observations we can see that it has: higher median ages, years at empoyer, years at addres and income
                                               # no surprises here, so those variables should play an important role in the classifier
drate_sub=sum(sub$Default)/length(sub[,1])
drate_tot=sum(credit_default_data$Default)/length(credit_default_data[,1]) # indeed group 5 has a lower default rate (10%) compared to 25% for the total dataset
plot(data.std, col=groups.hc)

# Principal Components Analysis
pca.out=prcomp(data.std[,1:6])
biplot(pca.out, cex=0.7)   
                                               # The pca analysis shows that the bigger variance of the data is explained by the contrast of the variables
                                               # age, income and years at employer and the variables credit card debt and automobile debt, in consonance with the cluster analisys

# ___________________SUPERVISED______________________ 
library(boot)                                  #loading library for cross validation
