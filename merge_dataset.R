library(dplyr)



# Create two sample datasets
df1 <- data.frame(ID = c("A", "B", "C"), Value1 = c(1, 2, 3))
df2 <- data.frame(ID = c("B", "C", "D"), Value2 = c(4, 5, 6))

# Merge datasets on the 'ID' column
merged_df <- merge(df1, df2, by = "ID")

# View the merged dataset
print(merged_df)




######################################################################################à



# We are using this dataset:
# https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2021
happiness_2021 <- read.csv("./datasets/world-happiness-report-2021.csv")

#View(happiness_2021)

# Check if any missing values exist in the data frame
# No missing values
any(is.na(happiness_2021))

# Get the labels of the columns
column_labels <- colnames(happiness_2021)
column_labels

# Specify the column names you want to remove
columns_to_remove <- c("Standard.error.of.ladder.score", "upperwhisker", "lowerwhisker",                         
                       "Ladder.score.in.Dystopia", "Explained.by..Log.GDP.per.capita",      
                       "Explained.by..Social.support", "Explained.by..Healthy.life.expectancy",   
                       "Explained.by..Freedom.to.make.life.choices", "Explained.by..Generosity",                 
                       "Explained.by..Perceptions.of.corruption", "Dystopia...residual")

# Remove the specified columns from the dataframe
happiness_2021 <- happiness_2021[, -which(names(happiness_2021) %in% columns_to_remove)]

# General Infos
summary(happiness_2021)





#############################################################################################################


# Country	
# Region	
# Population	
# Area (sq. mi.)	
# Pop. Density (per sq. mi.)	
# Coastline (coast/area ratio)	
# Net migration	
# Infant mortality (per 1000 births)	
# GDP ($ per capita)	
# Literacy (%)	
# Phones (per 1000)	
# Arable (%)	
# Crops (%)	
# Other (%)	
# Climate	
# Birthrate	
# Deathrate	
# Agriculture	
# Industry	
# Service



# We are using this dataset:
# https://www.kaggle.com/datasets/fernandol/countries-of-the-world
country.infos <- read.csv("./datasets/countries of the world.csv", sep = ",", dec = ",", stringsAsFactors = FALSE)


#View(happiness_2021)

# Check if any missing values exist in the data frame
# There are some missing values
any(is.na(country.infos))


# Check if values are numerical for each column
for (col in names(country.infos)) {
  if (!is.numeric(country.infos[[col]])) {
    print(paste("Column", col, "contains non-numeric values."))
  }
}

# Specify the column names you want to remove
columns_to_remove <- c("Region")

# Remove the specified columns from the dataframe
country.infos <- country.infos[, -which(names(country.infos) %in% columns_to_remove)]

# General Infos
summary(country.infos)



# Drop rows with NaN values using na.omit()
country.infos <- na.omit(country.infos)


#########################################################################################




# Merge datasets on the 'ID' column
merged_df <- merge(happiness_2021, country.infos, by = "Country")

print(nrow(merged_df))

# Check if any missing values exist in the data frame
# No missing values
any(is.na(merged_df))



print(nrow(merged_df))



lm.fit <- lm(Score ~ .-Country-Region-Continent-GDP, data = merged_df)
summary(lm.fit)


cor(merged_df[c('Population', 'Population.Density', 'Area', 'GDP', 'Logged.GDP.per.capita')])





#Forward selection
varmax <-20
mod.bw <- regsubsets(Score ~ .-Country-Region-Continent, nvmax=varmax, method= "backward", data=merged_df)
summary_bw <- summary(mod.bw)
summary_bw$rsq
summary_bw$which[20,]

mod.fw <- regsubsets(Score ~ .-Country-Region-Continent, nvmax=varmax, method= "forward", data=merged_df)
summary_fw <- summary(mod.fw)
summary_fw$rsq
summary_bw$which[20,]



###################################################################################

# Unemployment


# We are using this dataset:
# 
unemployment <- read.csv("./datasets/unemployment.csv")


#View(happiness_2021)

# Check if any missing values exist in the data frame
# No missing values
any(is.na(unemployment))


# Check if values are numerical for each column
for (col in names(unemployment)) {
  if (!is.numeric(unemployment[[col]])) {
    print(paste("Column", col, "contains non-numeric values."))
  }
}


unemployment <- unemployment[c('Country', 'Country.Code', 'X2021')]



# General Infos
summary(unemployment)





#########################################################################################




# Merge datasets on the 'ID' column
merged_df <- merge(happiness_2021, unemployment, by = "Country")

print(nrow(happiness_2021))
print(nrow(unemployment))
print(nrow(merged_df))

# Check if any missing values exist in the data frame
# No missing values
any(is.na(merged_df))





# Get the keys from df1 that didn't match
df1_not_matched <- setdiff(happiness_2021$Country, merged_df$Country)

# Get the keys from df2 that didn't match
df2_not_matched <- setdiff(unemployment$Country, merged_df$Country)

# Print the results
print(df1_not_matched)
#print(df2_not_matched)





lm.fit <- lm(Score ~ .-Country-Country.Code-Region-Continent, data = merged_df)
summary(lm.fit)


cor(merged_df[c('Population', 'Population.Density', 'Area', 'GDP', 'Logged.GDP.per.capita')])





##########################################################################################







