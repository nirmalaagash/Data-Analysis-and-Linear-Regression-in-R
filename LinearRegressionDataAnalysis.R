#include 
library(tidyverse)
rhm <- read_csv('housing.csv')

head(rhm)

#Finding the unique names and mapping it to number the ouput variable ocean_proximity.
output_var = 1:length(unique(rhm$ocean_proximity))
names(output_var) = unique(rhm$ocean_proximity)

output_var

#Mapped values to another output variable
rhm$ocean_proximity_mapped = output_var[rhm$ocean_proximity]
rhm_numeric <- subset(rhm, select= -c(ocean_proximity))

rhm_numeric <- na.omit(rhm_numeric)

rhm_numeric

require(corrplot)
corrOfRHM <- cor(rhm_numeric)
corrplot(corrOfRHM)

#Converting it in to a data frame and checking for the features that matter.
rhm_df <- as.data.frame(corrOfRHM)
rownames(rhm_df)[which(abs(rhm_df$median_house_value)>0.1)]  

rhm_df <- as.data.frame(corrOfRHM)
rownames(rhm_df)[which(abs(rhm_df$median_house_value)>0.2)]   

rhm_df <- as.data.frame(corrOfRHM)
rownames(rhm_df)[which(abs(rhm_df$median_house_value)>0.3)]   

rhm_df <- as.data.frame(corrOfRHM)
rownames(rhm_df)[which(abs(rhm_df$median_house_value)>0.4)]   

rhm_df <- as.data.frame(corrOfRHM)
rownames(rhm_df)[which(abs(rhm_df$median_house_value)>0.5)]   

require(MASS)

#linear model for only the features with correlation value greater than 0.1 with the output
lm.fit = lm(median_house_value~housing_median_age+ocean_proximity_mapped+median_income,longitude, data = rhm_numeric )
summary(lm.fit)

#linear model for only the features with correlation value greater than 0.2 with the output
lm.fit2 = lm(median_house_value~housing_median_age+ocean_proximity_mapped, data = rhm_numeric)
summary(lm.fit2)

#linear model for all the features
lm.fit3 = lm(median_house_value~. , data = rhm_numeric)
summary(lm.fit3)

colnames <- colnames(rhm_numeric)

for (colname in colnames){
  if( colname != "median_house_value"){
    plot<-ggplot(rhm_numeric,aes_string(colname,"median_house_value")) + geom_point()
    print(plot)
  }
}