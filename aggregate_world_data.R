# We are using those datasets:
# https://www.kaggle.com/datasets/daniboy370/world-data-by-country-2020
fertility <- read.csv("./datasets/world_data/Fertility.csv")
#GDP.per.capita <- read.csv("./datasets/world_data/GDP per capita.csv")
life.expectancy <- read.csv("./datasets/world_data/Life expectancy.csv") # 0.98 corr with Healthy.life.expectancy 
#meat.consumption <- read.csv("./datasets/world_data/Meat consumption.csv")
median.age <- read.csv("./datasets/world_data/Median age.csv")
population.growth <- read.csv("./datasets/world_data/Population growth.csv")
sex.ratio <- read.csv("./datasets/world_data/Sex-ratio.csv")
suicide.rate <- read.csv("./datasets/world_data/Suicide rate.csv")
urbanization <- read.csv("./datasets/world_data/Urbanization rate.csv")


# Discard the "Country" column using subset()
fertility <- subset(fertility, select = -Country)
#GDP.per.capita <- subset(GDP.per.capita, select = -Country)
life.expectancy <- subset(life.expectancy, select = -Country)
#meat.consumption <- subset(meat.consumption, select = -Country)
median.age <- subset(median.age, select = -Country)
population.growth <- subset(population.growth, select = -Country)
sex.ratio <- subset(sex.ratio, select = -Country)
suicide.rate <- subset(suicide.rate, select = -Country)
urbanization <- subset(urbanization, select = -Country)


#############################################################################################################

# Plus this Unemployment dataset
# We are using this dataset:
# https://www.kaggle.com/datasets/pantanjali/unemployment-dataset
unemployment <- read.csv("./datasets/unemployment.csv")

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
colnames(unemployment)[colnames(unemployment) == "Country.Code"] <- "ISO.code"
colnames(unemployment)[colnames(unemployment) == "X2021"] <- "Unemployment.rate"

# General Infos
summary(unemployment)

#############################################################################################################



# Merge datasets on the 'ISO.code' column
merged_df <- merge(unemployment, fertility, by = "ISO.code")
print(nrow(merged_df))
merged_df <- merge(merged_df, life.expectancy, by = "ISO.code")
print(nrow(merged_df))
#merged_df <- merge(merged_df, GDP.per.capita, by = "ISO.code")
#print(nrow(merged_df))
#merged_df <- merge(merged_df, meat.consumption, by = "ISO.code")
#print(nrow(merged_df))
merged_df <- merge(merged_df, median.age, by = "ISO.code")
print(nrow(merged_df))
merged_df <- merge(merged_df, population.growth, by = "ISO.code")
print(nrow(merged_df))
merged_df <- merge(merged_df, sex.ratio, by = "ISO.code")
print(nrow(merged_df))
merged_df <- merge(merged_df, suicide.rate, by = "ISO.code")
print(nrow(merged_df))
merged_df <- merge(merged_df, urbanization, by = "ISO.code")
print(nrow(merged_df))


merged_df <- subset(merged_df, select = -Unemployment.rate)


# Check if any missing values exist in the data frame
# No missing values
any(is.na(merged_df))





#############################################################################################################


# We are using this dataset:
# https://www.kaggle.com/datasets/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report-2021.csv
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




# merging happiness_2021 and merge_df
final <- merge(merged_df, happiness_2021, by = "Country")
print(nrow(final))

print(nrow(happiness_2021))

# Check if any missing values exist in the data frame
# No missing values
any(is.na(final))

# Export dataframe
write.csv(final, file = "./datasets/happiness_2021_complete.csv", row.names = FALSE)
gg <- read.csv("./datasets/happiness_2021_complete.csv")




#############################################################################################################


# Linear Regression


attach(final)

# Check if values are numerical for each column
for (col in names(final)) {
  if (!is.numeric(final[[col]])) {
    print(paste("Column", col, "contains non-numeric values."))
  }
}


lm.fit <- lm(Score ~ .-Country-ISO.code-Region-Continent, data = final)
summary(lm.fit)

lm.fit <- lm(Score ~ Freedom.to.make.life.choices+Urbanization.rate+Fertility+Logged.GDP.per.capita, data = final)
summary(lm.fit)


cor(final[c('Logged.GDP.per.capita', 'Median.age')])
cor(final[c('Median.age', 'Life.expectancy', 'Population.growth', 'Urbanization.rate', 'Healthy.life.expectancy')])









# Code to check unmatched keys

# Get the keys from df1 that didn't match
df1_not_matched <- setdiff(suicide.rate$ISO.code, merged_df$ISO.code)
# Get the keys from df2 that didn't match
df2_not_matched <- setdiff(unemployment$ISO.code, merged_df$ISO.code)
# Print the results
print(df1_not_matched)
print(df2_not_matched)

