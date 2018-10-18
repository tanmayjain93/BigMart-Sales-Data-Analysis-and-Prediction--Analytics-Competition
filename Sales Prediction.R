# We are going to install and load several packages 
#for reading data, manipulation of data, visualization of data, and finally for modeling.

install.packages(('data.tables'))
library(data.table) # used for reading and manipulation of data
library(dplyr)      # used for data manipulation and joining
library(ggplot2)    # used for ploting 
library(caret)       # used for modeling
library(corrplot)   # used for making correlation plot
library(xgboost)    # used for building XGBoost model. XGBoost is a powerful tool to make classification and regression . It used to make predictions and evaluate the credibility of the predictions.
library(cowplot)    # used for combining multiple plots 



#We will use read() function of data.table package to read the datasets.Already separated training and testing dataset. A final data to apple our model to.
train = fread("Train_UWu5bXk.csv") 
test = fread("Test_u94Q5KV.csv")
final = fread("SampleSubmission_TmnO39y.csv")

#Exploring our dataset
dim(train)
#> dim(train)
#[1] 8523   12

dim(test)
#[1] 5681   11

names(train)
[1] "Item_Identifier"           "Item_Weight"               "Item_Fat_Content"          "Item_Visibility"          
[5] "Item_Type"                 "Item_MRP"                  "Outlet_Identifier"         "Outlet_Establishment_Year"
[9] "Outlet_Size"               "Outlet_Location_Type"      "Outlet_Type"               "Item_Outlet_Sales"

names(test)
[1] "Item_Identifier"           "Item_Weight"               "Item_Fat_Content"          "Item_Visibility"          
[5] "Item_Type"                 "Item_MRP"                  "Outlet_Identifier"         "Outlet_Establishment_Year"
[9] "Outlet_Size"               "Outlet_Location_Type"      "Outlet_Type"

#Item_Outlet_Sales is present in train but not in test dataset because this is the target variable that we have to predict.



# Find the structure
str(train)

Classes 'data.table' and 'data.frame':  8523 obs. of  12 variables:
  $ Item_Identifier          : chr  "FDA15" "DRC01" "FDN15" "FDX07" ...
$ Item_Weight              : num  9.3 5.92 17.5 19.2 8.93 ...
$ Item_Fat_Content         : chr  "Low Fat" "Regular" "Low Fat" "Regular" ...
$ Item_Visibility          : num  0.016 0.0193 0.0168 0 0 ...
$ Item_Type                : chr  "Dairy" "Soft Drinks" "Meat" "Fruits and Vegetables" ...
$ Item_MRP                 : num  249.8 48.3 141.6 182.1 53.9 ...
$ Outlet_Identifier        : chr  "OUT049" "OUT018" "OUT049" "OUT010" ...
$ Outlet_Establishment_Year: int  1999 2009 1999 1998 1987 2009 1987 1985 2002 2007 ...
$ Outlet_Size              : chr  "Medium" "Medium" "Medium" "" ...
$ Outlet_Location_Type     : chr  "Tier 1" "Tier 3" "Tier 1" "Tier 3" ...
$ Outlet_Type              : chr  "Supermarket Type1" "Supermarket Type2" "Supermarket Type1" "Grocery Store" ...
$ Item_Outlet_Sales        : num  3735 443 2097 732 995 ...
- attr(*, ".internal.selfref")=<externalptr>
  
  str(test)
Classes 'data.table' and 'data.frame':  5681 obs. of  11 variables:
  $ Item_Identifier          : chr  "FDW58" "FDW14" "NCN55" "FDQ58" ...
$ Item_Weight              : num  20.75 8.3 14.6 7.32 NA ...
$ Item_Fat_Content         : chr  "Low Fat" "reg" "Low Fat" "Low Fat" ...
$ Item_Visibility          : num  0.00756 0.03843 0.09957 0.01539 0.1186 ...
$ Item_Type                : chr  "Snack Foods" "Dairy" "Others" "Snack Foods" ...
$ Item_MRP                 : num  107.9 87.3 241.8 155 234.2 ...
$ Outlet_Identifier        : chr  "OUT049" "OUT017" "OUT010" "OUT017" ...
$ Outlet_Establishment_Year: int  1999 2007 1998 2007 1985 1997 2009 1985 2002 2007 ...
$ Outlet_Size              : chr  "Medium" "" "" "" ...
$ Outlet_Location_Type     : chr  "Tier 1" "Tier 2" "Tier 3" "Tier 2" ...
$ Outlet_Type              : chr  "Supermarket Type1" "Supermarket Type1" "Grocery Store" "Supermarket Type1" ...
- attr(*, ".internal.selfref")=<externalptr> 
  
  
#Combining training and test dataset.
# Combining train and test sets saves a lot of time and effort because
#if we have to make any modification in the data, we would make the change only in the combined data and not in train and test data separately.
#Later we can always split the combined data back to train and test.
# It is still  a topic for discussion whether the train data and test data should be combined or be kept separated and modifications or changes be applied separately.
# For this case , lets combine the dataset.
test[,Item_Outlet_Sales := NA]
combi = rbind(train, test) # combining train and test datasets
dim(combi)  

#Lets do exploratory data analysis on our data. It helps us in understanding the nature of data in terms of distribution of the individual variables,
#finding missing values, relationship with other variables etc.
#Let's start with univariate EDA. It involves exploring variables individually.
#We will try to visualize the continuous variables using histograms and categorical variables using bar plots.

#Since our target variable is continuous(item_outlet_Sales), we can visualise it by plotting its histogram.
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "blue") +
  xlab("Item_Outlet_Sales")


#let's check the numeric independent variables.
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package


# Lets try to explore and gain some insights from the categorical variables.
#A categorical variable or feature can have only a finite set of values.
#Let's first plot Item_Fat_Content.

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "darkgreen")

#In the figure above, 'LF', 'low fat', and 'Low Fat' are the same category and can be combined into one.
#Similarly we can be done for 'reg' and 'Regular' into one. 
#After making these corrections we'll plot the same figure again.
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "darkgreen")

# plot for Item_Type

p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "darkgreen") +
  xlab("") +
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Item_Type")

# plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "darkgreen") +
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "darkgreen") +
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

#Let's check remaining variables
# plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + 
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "darkgreen") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +
  xlab("Outlet_Establishment_Year") +
  theme(axis.text.x = element_text(size = 8.5))

# plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + 
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "darkgreen") +
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +
  theme(axis.text.x = element_text(size = 8.5))

# ploting both plots together
plot_grid(p7, p8, ncol = 2)


#Bivariate Analysis
#After looking at every feature individually, let's now do some bivariate analysis.
#Here we'll explore the independent variables with respect to the target variable. 
#The objective is to discover hidden relationships between the independent variable and the target variable
train = combi[1:nrow(train)] # extracting train data from the combined data

#Let's explore the numerical variables first.
# Item_Weight vs Item_Outlet_Sales
p9 = ggplot(train) + 
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_Visibility vs Item_Outlet_Sales
p10 = ggplot(train) + 
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

# Item_MRP vs Item_Outlet_Sales
p11 = ggplot(train) + 
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow = 2)


#Let's explore categorical variables now.
# Item_Type vs Item_Outlet_Sales
p12 = ggplot(train) + 
  geom_point(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8.5))
# Item_Fat_Content vs Item_Outlet_Sales
p13 = ggplot(train) + 
  geom_point(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

# Outlet_Identifier vs Item_Outlet_Sales
p14 = ggplot(train) + 
  geom_point(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8.5))

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)

#Outlet_size vs Item_Outlet_Sale
ggplot(train) + geom_point(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

# Treating Missing Values
sum(is.na(combi$Item_Weight))
#[1] 2439

# Imputing missing value in weight
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = T)
}
sum(is.na(combi$Item_Weight))

#[1] 0

# Imputing missing value in visibility

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#replacig zeroes with mean of visibility on basis of item_identifier 
zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
  
}

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#Converting categorical variables to numerical variables for correlation and regression.
#We will use label encoding and one hot encoding to the above.

#Label Encoding
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,
                                 ifelse(Outlet_Size == "Medium", 1, 2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]
#One hot Encoding
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier", "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)
# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#Preprocessing data before it is fed to the algorithm.
#Let's try remov skweness in item_visibility
combi[,Item_Visibility := log(Item_Visibility)] # log + 1 to avoid division by zero

num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL] # removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

# Splitting the datasets back to train and test
train = combi[1:nrow(train)]
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL] # removing Item_Outlet_Sales as it contains only NA for test dataset

#Examining the correlation among variables
library('corrplot')
cor_train = cor(train[,-c("Item_Identifier","Item_Visibility")])

#Heatmap for Correlation
library(gplots)
heatmap.2((cor_train), Rowv = FALSE, Colv = FALSE, dendrogram = "none", cellnote = round(cor_train,2), notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#Applying Prediction Model
# Linear Regression

Pred = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
Pred

#Strong item outlest sales data in a spearate file 
final$Item_Outlet_Sales = predict(Pred, test[,-c("Item_Identifier")])
write.csv(final, "SampleSubmission_TmnO39y.csv", row.names = F)

#Performing XGboost algrorithm on train and test dataset.
param_list = list(
  
  objective = "reg:linear",
  eta=0.01,
  gamma = 1,
  max_depth=6,
  subsample=0.8,
  colsample_bytree=0.5
)
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]), label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))


#Performing cross validation
set.seed(112)
xgbcv = xgb.cv(params = param_list, 
               data = dtrain, 
               nrounds = 1000, 
               nfold = 5, 
               print_every_n = 10, 
               early_stopping_rounds = 30, 
               maximize = F)

#As per the above set, we got the best validation/test score at the 628th iteration. Hence, we will use nrounds = 628 for building the XGBoost model.
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 628)


#Let's use Xgboost importance function to Show Importance Of Features In A Model
var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")), 
                        model = xgb_model)
head(var_imp)
xgb.plot.importance(var_imp)
