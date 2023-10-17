happiness_2021 <- read.csv("./datasets/happiness_2021_complete.csv")

# Check if any missing values exist in the data frame
# No missing values
any(is.na(happiness_2021))

# Get the labels of the columns
column_labels <- colnames(happiness_2021)
column_labels

str(happiness_2021)

# General Infos
summary(happiness_2021)
cor(happiness_2021[c(-1,-2,-10,-11)])
tmp_cor <- cor(happiness_2021[c(-1,-2,-10,-11)])
tmp_cor['Score',]

library(corrplot)
corrplot(cor(happiness_2021[c(-1,-2,-10,-11)]), method="circle", type="upper", is.corr=FALSE, tl.col = "black", tl.offset = 0.4)

attach(happiness_2021)



##################################################################################

# Count unique values in a specific column
unique_count <- length(unique(happiness_2021$Region))
cat("Number of unique values in Region:", unique_count, "\n")

# Print the unique values
cat("Unique values:\n")
unique_values <- unique(happiness_2021$Region)
print(unique_values)

# Count unique values in a specific column
unique_count <- length(unique(happiness_2021$Continent))
cat("Number of unique values in Continent:", unique_count, "\n")

# Print the unique values
cat("Unique values:\n")
unique_values <- unique(happiness_2021$Continent)
print(unique_values)

##################################################################################



# MERGING CONTINENTS

library(dplyr)
library(ggplot2)

new_data <- happiness_2021 %>%
  mutate(new_category = ifelse(Continent %in% c("Oceania", "North America", "Latin America"), "America and Oceania", Continent))

ggplot(new_data, aes(x = Logged.GDP.per.capita, fill = new_category)) +
  geom_density(alpha=0.5) +
  xlab("GDP") +
  ggtitle("Density Plot: GDP by continent")



happiness_2021 <- happiness_2021 %>%
  mutate(Continent = ifelse(Continent %in% c("Oceania", "Asia"), "Asia and Oceania", Continent))

happiness_2021 <- happiness_2021 %>%
  mutate(Continent = ifelse(Continent %in% c("North America", "Latin America"), "America", Continent))

attach(happiness_2021)



########################################################################################

# Exploratory Analysis

library(ggplot2)

ggplot(happiness_2021, aes(Logged.GDP.per.capita, Score, color = Continent)) +
  geom_point() +
  labs(x = "Logged GDP", y = "Score") +
  scale_color_discrete(name = "Category")

ggplot(happiness_2021, aes(Social.support, Score, color = Continent)) +
  geom_point() +
  labs(x = "Social Support", y = "Score") +
  scale_color_discrete(name = "Category")

ggplot(happiness_2021, aes(Healthy.life.expectancy, Score, color = Continent)) +
  geom_point() +
  labs(x = "Healthy Life Expectancy", y = "Score") +
  scale_color_discrete(name = "Category")

ggplot(happiness_2021, aes(Freedom.to.make.life.choices, Score, color = Continent)) +
  geom_point() +
  labs(x = "Freedom to make life choices", y = "Score") +
  scale_color_discrete(name = "Category")

ggplot(happiness_2021, aes(Perceptions.of.corruption, Score, color = Continent)) +
  geom_point() +
  labs(x = "Perceptions of Corruption", y = "Score") +
  scale_color_discrete(name = "Category")

ggplot(happiness_2021, aes(Generosity, Score, color = Continent)) +
  geom_point() +
  labs(x = "Generosity", y = "Score") +
  scale_color_discrete(name = "Category")


###################################################################################

# Comparison between GDP and Logged GDP

library(ggplot2)
library(gridExtra)

plot1 <- ggplot(happiness_2021, aes(Logged.GDP.per.capita, Score, color = Continent)) +
  geom_point() +
  labs(x = "Logged GDP", y = "Score") +
  scale_color_discrete(name = "Category") +
  theme(legend.position = "none")

# Non-Logged GDP / Score plot
plot2 <- ggplot(happiness_2021, aes(exp(Logged.GDP.per.capita), Score, color = Continent)) +
  geom_point() +
  labs(x = "Logged GDP", y = "Score") +
  scale_color_discrete(name = "Category") +
  theme(legend.position = "none")

# Arrange the plots in a grid layout
grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

grid.arrange(
  arrangeGrob(plot1,nrow = 1),
  arrangeGrob(plot2, nrow = 1),
  nrow = 1
)




# Define colors for each category
category_colors <- c("Africa" = "darkred", "Asia" = "orange", "Oceania" = "magenta", "Europe" = "darkgreen", 
                    "North America" = "#87CEEB", "Latin America" = "blue")
# Plot using pairs() with colored points
pairs(happiness_2021[c(-1,-2,-3,-7,-8,-10,-11,-15)], col = category_colors[happiness_2021$Continent], pch = 16)




# Calculate cumulative sum by group
cumulative_sum <- aggregate(Score ~ Continent, data = happiness_2021, FUN = mean)

# Create bar plot
barplot(cumulative_sum$Score, names.arg = cumulative_sum$Continent,
        xlab = "Group", ylab = "Score Mean", main = "Cumulative Mean Bar Plot by Groups",
        col = "steelblue")




dens <- density(happiness_2021$Score, na.rm=TRUE)
plot(dens, main="Score Kernel Density")



library(scales)
happiness_2021$rescaled_GDP <- rescale(Logged.GDP.per.capita)

ggplot(happiness_2021, aes(x = Score, fill = Continent)) +
  geom_density(alpha=0.5) +
  xlab("Score") +
  ggtitle("Density Plot: score by continent")

ggplot(happiness_2021, aes(x = Fertility , fill = Continent)) +
  geom_density(alpha=0.5) +
  xlab("Fertility") +
  ggtitle("Density Plot: Fertility by continent")

ggplot(happiness_2021, aes(x = Life.expectancy , fill = Continent)) +
  geom_density(alpha=0.5) +
  xlab("Life expectancy") +
  ggtitle("Density Plot: Life expectancy by continent")

ggplot(happiness_2021, aes(x = Logged.GDP.per.capita, fill = Continent)) +
  geom_density(alpha=0.5) +
  xlab("GDP") +
  ggtitle("Density Plot: GDP by continent")

ggplot(happiness_2021, aes(x = rescaled_GDP , fill = Continent)) +
  geom_density(alpha=0.5) +
  xlab("GDP") +
  ggtitle("Density Plot: GDP by continent")

ggplot(happiness_2021, aes(x=Logged.GDP.per.capita, fill = Continent)) +
  geom_histogram(aes(y=..density..),binwidth=.9, alpha=.5, position="identity") + geom_density(alpha=.3)

ggplot(happiness_2021, aes(x=rescaled_GDP, fill = Continent)) +
  geom_histogram(aes(y=..density..),binwidth=.5, alpha=.5, position="identity") + geom_density(alpha=.3)

happiness_2021 <- subset(happiness_2021, select = -rescaled_GDP)



boxplot(Score ~ Continent)

boxplot(Logged.GDP.per.capita ~ Continent)

a = happiness_2021[happiness_2021$Continent =="Africa", ]
ordered_subset <- a[order(a$Logged.GDP.per.capita), ]
print(ordered_subset)
# Mauritius

a = happiness_2021[happiness_2021$Continent =="America", ]
ordered_subset <- a[order(a$Logged.GDP.per.capita), ]
print(ordered_subset)
# Haiti




# PLOT MAP

# Install required packages if not already installed
#install.packages("maps")
#install.packages("ggplot2")

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

merged_df <- merge(happiness_2021, world, by.x = "ISO.code", by.y="iso_a3", all.y=TRUE)

# Fill missing values in column C with 0
#merged_df$Score[is.na(merged_df$Score)] <- 0

df1_selected <- merged_df[c("ISO.code", "Country", "Score")]

merged_2_df <- merge(world, df1_selected, by.x = "iso_a3", by.y="ISO.code", all.x=TRUE)

#world$Score <- merged_2_df$Score
#merged_2_df$Score <- ifelse(merged_2_df$Score == 0, NaN, merged_2_df$Score)

ggplot(data = merged_2_df) + geom_sf(aes(fill=Score)) + scale_fill_viridis_c(option = "plasma")


##################################################################################

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

merged_df <- merge(happiness_2021, world, by.x = "ISO.code", by.y="iso_a3", all.y=TRUE)

df1_selected <- merged_df[c("ISO.code", "Country", "Median.age")]

merged_2_df <- merge(world, df1_selected, by.x = "iso_a3", by.y="ISO.code", all.x=TRUE)

ggplot(data = merged_2_df) + geom_sf(aes(fill=Median.age)) + scale_fill_viridis_c(option = "plasma")

##################################################################################




# Create a scatter plot with different colors based on a categorical variable
ggplot(happiness_2021, aes(Healthy.life.expectancy, Fertility, color = Continent)) +
  geom_point() +
  labs(x = "Healthy Life Expectancy", y = "Fertility") +
  scale_color_discrete(name = "Category")

# happiness_2021$category <- factor(happiness_2021$Life.expectancy, levels = happiness_2021$Life.expectancy[order(happiness_2021$Life.expectancy, decreasing = TRUE)])
# x_reordered <- Life.expectancy[order(Fertility)]




library(scales)

happiness_2021$Life.expectancy <- rescale(happiness_2021$Life.expectancy) 
happiness_2021$Fertility <- rescale(happiness_2021$Fertility)
happiness_2021$Urbanization.rate <- rescale(happiness_2021$Urbanization.rate)
happiness_2021$Median.age <- rescale(happiness_2021$Median.age)
happiness_2021$Fertility <- rescale(happiness_2021$Fertility)
happiness_2021$Population.growth <- rescale(happiness_2021$Population.growth)
happiness_2021$Sex.ratio <- rescale(happiness_2021$Sex.ratio)
happiness_2021$Suicide.rate <- rescale(happiness_2021$Suicide.rate)
happiness_2021$Logged.GDP.per.capita <- rescale(happiness_2021$Logged.GDP.per.capita)
happiness_2021$Healthy.life.expectancy <- rescale(happiness_2021$Healthy.life.expectancy)
happiness_2021$Generosity <- rescale(happiness_2021$Generosity)


#########################################################################################################

# Pay attention:
# in this box you should use rescaled version of the variables
# right now the original variables are rescaled in place
cumulative_le <- aggregate(Life.expectancy ~ Continent, data = happiness_2021, FUN = mean)
cumulative_f <- aggregate(Fertility ~ Continent, data = happiness_2021, FUN = mean)

#freq_key_mode <- table(cumulative_lf$Scaled.life.expectancy, cumulative_f$Fertility)
order_indices <- order(cumulative_le$Life.expectancy)

c_Continent <- cumulative_le$Continent[order_indices]
c_Scaled.life.expectancy <- cumulative_le$Life.expectancy[order_indices]
c_Fertility <- cumulative_f$Fertility[order_indices]

#labels_in_order <- cumulative_le$Continent[order(cumulative_le$Scaled.life.expectancy)]

# Create bar plot
barplot(rbind(c_Fertility, c_Scaled.life.expectancy), beside = TRUE, names.arg = c_Continent,
        xlab = "Group", ylab = "Cumulative Sum", main = "Cumulative Mean Bar Plot by Groups",
        col = c("red", "light blue"), ylim = c(0, 1))
legend(1,1, legend = c("Life Expectancy", "Fertility"), fill = c("red", "light blue"))

#########################################################################################################


lm.fit <- lm(Score ~ .-Country-Region-Continent-ISO.code+Logged.GDP.per.capita:Median.age, data = happiness_2021)
summary(lm.fit)


# Linear Regression

lm.fit <- lm(Score ~ .-Country-Region-Continent-ISO.code, data = happiness_2021)
summary(lm.fit)

par(mfrow=c(2,2)) 
plot(lm.fit) 
 
par(mfrow=c(1,1))

#Stepwise feature selection

library(leaps)

varmax <-14
mod.bw <- regsubsets(Score ~ .-Score-Country-Region-Continent-ISO.code, nvmax=varmax, method= "forward", data=happiness_2021)
summary_bw <- summary(mod.bw)
summary_bw$rsq
summary_bw$which[5,] #last element added to the model is Generosity

varmax <-14
mod.bw <- regsubsets(Score ~ .-Score-Country-Region-Continent-ISO.code, nvmax=varmax, method= "backward", data=happiness_2021)
summary_bw <- summary(mod.bw)
summary_bw$rsq
summary_bw$which[5,]

#split 

set.seed(1)

data_size <- floor(0.85 * nrow(happiness_2021))


train_sample <- sample(seq_len(nrow(happiness_2021)), size = data_size, replace = FALSE)

train <- happiness_2021[train_sample, ]
test <- happiness_2021[-train_sample, ]



#COLLINEARITY CHECK

library(car)


lin_all <- lm(Score ~ .-Score-ISO.code-Continent-Region-Country, data = train)
lin_to_try <- lm (Score ~ .-Score-ISO.code-Continent-Region-Country-Logged.GDP.per.capita-Healthy.life.expectancy-Median.age-Fertility, data=train)
lin_m <- lm(Score ~ .-Score-ISO.code-Continent-Region-Country-Healthy.life.expectancy-Median.age-Fertility-Logged.GDP.per.capita-Life.expectancy, data=train )

summary(lin_to_try)

#corrplot(cor(happiness_2021[c(-1,-2,-10,-11)]), method="circle", type="upper", is.corr=FALSE, tl.col = "black", tl.offset = 0.4)

vif(lin_to_try)
lin_m$coefficients

columns_to_retain <- c("Population.growth", "Sex.ratio", 
                       "Suicide.rate", "Urbanization.rate", "Social.support",
                       "Freedom.to.make.life.choices", "Generosity", "Perceptions.of.corruption", "Life.expectancy", "Score")

train <- train[, columns_to_retain]
test <- test[, columns_to_retain]

corrplot(cor(train[c(-1,-2,-10,-11)]), method="circle", type="upper", is.corr=FALSE, tl.col = "black", tl.offset = 0.4)







# Leverage points analysis

par(mfrow=c(2,2)) 
plot(lin_m) 
par(mfrow=c(1,1))

# Leverage Point:
# - United Arab Emirates (index 129)
leverage.point.index <- 129
print(train[2,])

# here we romoved it 
train_leverage <- train[-2,]
lin_m <- lm(Score ~ .-Score-Life.expectancy, data=train_leverage )

par(mfrow=c(2,2)) 
plot(lin_m) 
par(mfrow=c(1,1))

summary(lin_m)

# Outliers
# - Botswana (index 15)












library(leaps)

# Regression model

varmax <-10
mod.bw <- regsubsets(Score ~ ., nvmax=varmax, method= "backward", data=train)
summary_bw <- summary(mod.bw)
summary_bw$rsq
summary_bw$which[1,]

mod.fw <- regsubsets(Score ~ ., nvmax=varmax, method= "forward", data=train)
summary_fw <- summary(mod.fw)

summary_bw$rsq
seq_len(varmax+1)




par(mfrow=c(2,2))


#FORWARD SELECTION PLOT ANALYSIS

# residual sum of squares
plot(summary_fw$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(summary_fw$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
max_r2 <- which.max(summary_fw$adjr2)
points(max_r2,summary_fw$adjr2[max_r2], col="red",cex=2,pch=20)

summary_fw$which[max_r2,]

# Mallow's Cp with its smallest value
plot(summary_fw$cp,xlab="Number of Variables",ylab="Cp",type='l')
min_cp <-which.min(summary_fw$cp)
points(min_cp,summary_fw$cp[min_cp],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(summary_fw$bic,xlab="Number of Variables",ylab="BIC",type='l')
min_bc <- which.min(summary_fw$bic)
points(min_bc,summary_fw$bic[min_bc],col="red",cex=2,pch=20)






#BACKWARD SELECTION PLOT ANALYSIS

# residual sum of squares
plot(summary_bw$rss,xlab="Number of Variables",ylab="RSS",type="l")

# adjusted-R^2 with its largest value
plot(summary_bw$adjr2,xlab="Number of Variables",ylab="Adjusted Rsq",type="l")
max_r2 <- which.max(summary_bw$adjr2)
points(max_r2,summary_bw$adjr2[max_r2], col="red",cex=2,pch=20)

summary_bw$which[max_r2,]

# Mallow's Cp with its smallest value
plot(summary_bw$cp,xlab="Number of Variables",ylab="Cp",type='l')
min_cp <- which.min(summary_bw$cp)
points(min_cp,summary_bw$cp[min_cp],col="red",cex=2,pch=20)

# BIC with its smallest value
plot(summary_bw$bic,xlab="Number of Variables",ylab="BIC",type='l')
min_bc <- which.min(summary_bw$bic)
points(min_bc,summary_bw$bic[min_bc],col="red",cex=2,pch=20)

par(mfrow=c(1,1))

plot(Urbanization.rate, Logged.GDP.per.capita)

coef(mod.bw,9)
coef(mod.fw,10)

plot(seq_len(varmax), summary_bw$rsq, xlab="# parameters", ylab = "R squared ")+ lines(seq_len(varmax), summary_bw$rsq, lwd=2, col = "red") + lines(seq_len(varmax), summary_fw$rsq, lwd=2, lty = "dashed", col = "blue")
legend("topleft", legend=c("backward", "forward"),
       col=c("red", "blue"), lty = 1:2)





regfit.best <- regsubsets(Score~., data=train, nvmax=9)

val.mat <- model.matrix(Score~., data=test)

val.errors <- rep(NA,9)
for(i in 1:9){
  coefi <- coef(regfit.best, id=i)
  pred <- val.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((val$Score-pred)^2)
}
val.errors

plot(val.errors,type='b')
min_cross <- which.min(val.errors)
points(min_cross,val.errors[min_cross], col="red",cex=2,pch=20)

set.seed(1)
k=10
folds <- sample(1:k, nrow(train), replace=TRUE)
table(folds)

cv.errors <- matrix(NA,k,9)
colnames(cv.errors) <- 1:9


for(j in 1:k){
  best.fit <- regsubsets(Score~., data=train[folds!=j,], nvmax=9)
  test.mat <- model.matrix(Score~., data=train[folds==j,])
  for(i in 1:9){
    coefi <- coef(best.fit, id=i)
    pred <- test.mat[,names(coefi)]%*%coefi
    cv.errors[j,i] <- mean( (train$Score[folds==j]-pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors,2,mean)
mean.cv.errors

plot(mean.cv.errors, type='b')
min_point <- which.min(mean.cv.errors)
points(min_point,mean.cv.errors[min_point], col="red",cex=2,pch=20)
reg.best <- regsubsets(Score~.,data=val, nvmax=9)
coef(reg.best,min_point)

model_fw_lin <- lm(Score ~ .-Score, data = train)
summary(model_fw_lin)

library(car)

vif(model_fw_lin)


#model_bw_lin <- lm(Score ~ .-Score-Country-ISO.code-Region-Continent-Sex.ratio-Population.growth-Life.expectancy-Suicide.rate-Perception.of.corrpution, data = train)
#summary(model_fw_lin)

#RIDGE REGRESSION
library(glmnet)
#grid <- 10^seq(10, -2, length=100)
X <- train[c(-10)]
X

X_val <- test[c(-10)]
ridge.mod <- glmnet(X, train$Score, alpha=0, thresh = 1e-12, data=train)

plot(ridge.mod, xvar="lambda")
# add labels to identify the variables
plot(ridge.mod, xvar="lambda", label=TRUE)

plot(ridge.mod, xvar="norm", label=TRUE)
plot(ridge.mod, xvar="dev", label=TRUE)

# apply lasso to the training set 
lasso.mod <- glmnet(X, train$Score, alpha=1, thresh = 1e-12, data=train)

lasso.mod

plot(lasso.mod, xvar="lambda", label=TRUE)
plot(lasso.mod, xvar="norm", label=TRUE)

# apply 10fold cross-validation to the training set

set.seed(10)
cv.out.lasso <- cv.glmnet(as.matrix(X), train$Score, alpha=1)
plot(cv.out.lasso)

# estimated test MSE

bestlam <- cv.out.lasso$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=as.matrix(X_val))
mse.lasso <- mean((lasso.pred-val$Score)^2)
mse.lasso

bestlam

best_ridge <- glmnet(X, train$Score, alpha=1, lambda = bestlam, thresh = 1e-12, data=train)

out <- glmnet(X, train$Score, alpha=1, lambda=grid)
lasso.coef <- predict(out,type="coefficients",s=bestlam)[1:11,]
lasso.coef

lasso.coef[lasso.coef!=0]
length(lasso.coef[lasso.coef==0])
which(lasso.coef==0)

corrplot(cor(train))

# coef() gives a matrix of coefficients

dim(coef(ridge.mod))

colnames(X)

model_try <- lm(Score ~ .-Score-Population.growth-Sex.ratio-Suicide.rate-Generosity, data = train)
summary(model_try)
linear_pred <- predict(model_try, X_val)
mse.linear <- mean((linear_pred-val$Score)^2)
mse.linear

SSE <- sum((lasso.pred - (val$Score))^2)
SST <- sum((val$Score - mean(val$Score))^2)
R_square <- 1 - SSE / SST
n=117
k=7
adj_R_square <- 1 - ((1-R_square)* (n-1)/ (n-k-1))

model_bw_lin <- lm(Score ~ Freedom.to.make.life.choices+Social.support)

ridge.mod$lambda.min
coef(ridge.mod)[,98]


pred <- predict(model_1_lin, newdata = val[c(-1,-2,-10,-11)])
residuals <- val$price - pred
residuals_s <- sample(seq_len(length(residuals)), size = floor(0.05 * length(residuals)), replace = FALSE)
residuals_sample <- residuals[residuals_s]
pred_sample <- pred[residuals_s]
                      


type <- rep(0, nrow(train))
type[train$color == "white"] <- 1
train$type <- type
pred.mod <- glm(type ~ .-type-color, data = train, family = binomial)
logistic.prob <- predict(pred.mod, val,  type="response")
logistic.pred <- rep("red", nrow(val))
logistic.pred[logistic.prob>0.5] <- "white"
tail(logistic.pred)
tail(val)


lda.fit <- lda(happiness_2021 ~ .- , data=lyft_train)
lda.fit 


# CLASSIFICATION 

logistic_merged_df <- merge(happiness_2021, merged_2_df, by.x = "ISO.code", by.y="iso_a3", all.x=TRUE)

str(logistic_merged_df)


unique_values <- unique(logistic_merged_df$economy)
starts_with_number <- grepl("^[0-9]", unique_values)

starts_with_1_to_3 <- function(x) {
  substr(x, 1, 1) %in% c("1", "2", "3","4","5")
}


#mapping_dict <- setNames(as.integer(starts_with_number), unique_values)

logistic_merged_df$logistic_economy <- as.integer(starts_with_1_to_3(logistic_merged_df$economy))
#merged_2_df$logistic_economy <- mapping_dict[match(merged_2_df$economy, names(mapping_dict))]

unique(logistic_merged_df$logistic_economy)
table(logistic_merged_df$logistic_economy)



set.seed(1)

data_size <- floor(0.85 * nrow(logistic_merged_df))


train_sample <- sample(seq_len(nrow(logistic_merged_df)), size = data_size, replace = FALSE)

train <- logistic_merged_df[train_sample, ]
test <- logistic_merged_df[-train_sample, ]

type <- rep(1, nrow(train))
type
type[train$logistic_economy == "1"] <- 1
train$type <- type
pred.mod <- glm(logistic_economy ~  Score, data = train, family = binomial)
logistic.prob <- predict(pred.mod, test,  type="response")
logistic.pred <- rep("0", nrow(test))
logistic.pred[logistic.prob>0.5] <- "1"
head(logistic.pred)
head(test$logistic_economy)

confusion_matrix <- table(logistic.pred, test$logistic_economy)
confusion_matrix

true_positives <- confusion_matrix[2, 2]  # True positives
false_positives <- confusion_matrix[1, 2]  # False positives
false_negatives <- confusion_matrix[2, 1]  # False negatives

# Calculate precision
precision <- true_positives / (true_positives + false_positives)

# Calculate recall
recall <- true_positives / (true_positives + false_negatives)

# Calculate F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print the F1 score
print(f1_score)


library(pROC)

roc.out <- roc(test$logistic_economy, logistic.prob, direction = "<")
plot(roc.out)


#LDA

library(MASS)

lda.fit <- lda(logistic_economy ~ Score , data=train)
lda.fit

plot(lda.fit)
plot(lda.fit, type="density")

val.pred <- predict(lda.fit, test)
confusion_matrix <- table(val.pred$class, test$logistic_economy)
confusion_matrix
#compute_er(confusion_matrix)

names(val.pred)

lda.class <- val.pred$class
table(lda.class,test$logistic_economy)

# error rate
mean(lda.class!=test$logistic_economy)

# take a look at the vectors component lda.pred

sum(val.pred$posterior[,1]>=.5)
sum(val.pred$posterior[,1]<.5)
val.pred$posterior[1:20,1]
lda.class[1:20]
sum(val.pred$posterior[,1]>.9)

true_positives <- confusion_matrix[2, 2]  # True positives
false_positives <- confusion_matrix[1, 2]  # False positives
false_negatives <- confusion_matrix[2, 1]  # False negatives

# Calculate precision
precision <- true_positives / (true_positives + false_positives)

# Calculate recall
recall <- true_positives / (true_positives + false_negatives)

# Calculate F1 score
f1_score <- 2 * precision * recall / (precision + recall)

# Print the F1 score
print(f1_score)


