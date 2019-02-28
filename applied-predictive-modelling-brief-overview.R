########################################################
# Featured Prediction Competition
# Home Credit Default Risk
# Can you predict how capable each applicant is of repaying a loan?
# Script based on book "Applied Predictive Modeling" and 
# https://inclass.kaggle.com/moizzz/applied-predictive-modelling-brief-overview
# 
# Data Download:
# https://www.kaggle.com/c/home-credit-default-risk/data
# put all extracted data files into a folder called input,
# which should be on the same level as the R script.
########################################################

setwd("/home/wxc011/RProjectsGIT/HomeCreditDefaultRisk")

# Applied Predictive Modelling book by Max Kuhn Author and Kjell Johnson Author

########################################################
# Let’s begin by loading the libraries and functions
########################################################

sample_size <- 1

load.libraries <- c('plyr', 'dplyr','data.table', 'readxl', 'reshape2', 'stringr', 'stringi', 'ggplot2', 'tidyverse', 'gridExtra','matrixStats','lubridate','corrplot','e1071','xgboost','caret','zoo','factoextra','plotly','DT')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

########################################################
# Utility functions
########################################################


#Function to change index to column
index_to_col <- function(data, Column_Name){
  data <- cbind(newColName = rownames(data), data)
  rownames(data) <- 1:nrow(data)
  colnames(data)[1] <- Column_Name
  return (data)
}

#Loading all the plotting functions
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=x)) + geom_histogram(bins=100, fill="#0072B2", alpha = .9) + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

plotBar <- function(data_in, i) {
  data <- data.frame(cbind(x=data_in[[i]],y=dt1_tran[,c("TARGET")]))
  data %>%
    mutate(x = x,
           class = as.character(y.TARGET)) %>%
    group_by(x, class) %>%
    summarise(count_class = n()) %>%
    group_by(x) %>%
    mutate(count_man = sum(count_class)) %>%
    mutate(percent = count_class / count_man * 100) %>%
    ungroup()  %>%
    ggplot(aes(x = x,
               y = percent,
               group = class)) +
    geom_bar(aes(fill = class, color=class), 
             stat = "identity") +
    geom_text(aes(label = sprintf("%0.1f%%", percent)),
              position = position_stack(vjust = 0.5)) + theme_light() + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))+ theme(legend.position='none')
  
}

plotCorr <- function(data_in, list1,list2,i){
  data <- data.frame(x = data_in[[list1[i]]], y = data_in[[list2[i]]])
  p <- ggplot(data, aes(x = x, y = y)) + geom_smooth(method = lm ) + geom_point(aes(x = x, y = y)) +
    geom_jitter(width = 0.1, height = 0.1)  + xlab(paste0(list1[i], '\n', 'R-Squared: ', round(cor(data_in[[list1[i]]], data_in[[list2[i]]], use = 'pairwise.complete.obs'), 3))) + theme_light() + ylab(paste0(list2[i]))
  return(suppressWarnings(p))
}

doPlotsCorr <- function(data_in, fun, list1,list2,ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, list1,list2,i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=x)) + geom_density(aes(group=as.factor(dt1_tran$TARGET),color=as.factor(dt1_tran$TARGET),fill=as.factor(dt1_tran$TARGET), alpha=0.2)) + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1)) + theme(legend.position='none')
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

########################################################
# Data Overview
########################################################
# For simplicity, the application_train data will be used
# According to the data there are 307511 rows and 122 columns

## ----echo=FALSE----------------------------------------------------------
dt1 <- fread('input/application_train.csv', showProgress = FALSE)
# 307511 obs. of  122 variables:
test <- fread('input/application_test.csv', showProgress = FALSE)
# [1] 48744   121

########################################################
# Cleaning the data
########################################################

# Cleaning Rule 1
# There was a mistake in the data set which kagglers found and following is what the competition host said: “Thanks for asking the question as encoding of missing values hasn’t been explained. Value 365243 denotes infinity in DAYS variables in the datasets, therefore you can consider them NA values. Also XNA/XAP denote NA values”

# https://www.kaggle.com/c/home-credit-default-risk/discussion/57247
# According to the above, all the 365243 were replaced with NA
## ----include=FALSE-------------------------------------------------------
dt1$DAYS_EMPLOYED <- replace(dt1$DAYS_EMPLOYED,dt1$DAYS_EMPLOYED == 365243,NA)

# Cleaning Rule 2
# There are predictors in the data which has all negative values and cannot be used to calculate skewness, hence I am changing the data which has negative values to abs to perform skewness analysis

## ----include=FALSE-------------------------------------------------------
# test if all values in the column are negative
col_neg <- unlist(dt1[, sapply(dt1, FUN = function(x) all(x <= 0, na.rm = TRUE))])
# Coerce lists and data.frames to data.table by reference
dt1_abs <- setDT(dt1)[,..col_neg]
dt1_abs <- abs(dt1_abs)

# get the names of the column with negative values
rm_col <- colnames(dt1_abs)
# remove original negative columns
dt1 <- as.data.frame(dt1)[, !(colnames(dt1) %in% rm_col)]
# add the new absolute value columns
dt1 <- cbind(dt1_abs,dt1)

########################################################
# Exploratory (Exhaustive) Data Analysis
########################################################

# Lets see the distribution of all 122 numeric columns in the data
numeric_list <- unlist(lapply(dt1, is.numeric))
dt1_num <- setDT(dt1)[,..numeric_list]
# 106
## ----fig1, fig.height = 20, fig.width = 10-------------------------------
doPlots(dt1_num, plotHist, ii = 1:20)

## ----fig2, fig.height = 20, fig.width = 10-------------------------------
# doPlots(dt1_num, plotHist, ii = 21:41)

## ----fig3, fig.height = 20, fig.width = 10-------------------------------
#doPlots(dt1_num, plotHist, ii = 42:62)

## ----fig4, fig.height = 20, fig.width = 10-------------------------------
#doPlots(dt1_num, plotHist, ii = 63:83)

## ----fig5, fig.height = 20, fig.width = 10-------------------------------
# doPlots(dt1_num, plotHist, ii = 84:106)

# Skewness of the data
# The data also needs to be transformed because of the skewness (right skewed or left skewed). Replacing the data with log, sqrt or inverse may help to remove the skewness.
# Lets look at the skewness of all the columns. The table below shows a summary
skewValues <- as.data.frame(apply(dt1_num, 2, function(x) skewness(x, na.rm = TRUE)))
colnames(skewValues)[1] <- "skew_values"
skewValues <- index_to_col(skewValues,'Column')
skewValues <- setDT(skewValues)[order (skew_values, decreasing = TRUE)]
skewValues[sample(1:nrow(skewValues), size = nrow(skewValues)),] %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = F
  ))

## ------------------------------------------------------------------------
# Box and Cox propose a family of transformations indexed by a parameter, denoted as lambda. 
# In addition to log transformations, this family can identify square transformations, square root, inverse and others.
# Box and Cox show how to use maximum likelihood estimation to determine the transformation parameter. 
# This procedure will be applied independently to each predictor data that contain values greater than zero.
BoxCoxValues <- apply(dt1_num, 2, function(x) BoxCoxTrans(x, na.rm = TRUE))
x = list()

for (i in 1:ncol(dt1_num)){
     lambda <- BoxCoxValues[[i]][[1]]
     x[[i]] <- lambda
}

lambda = do.call(rbind, x)
lambda_df <- as.data.frame(cbind(colnames(dt1_num),lambda))
colnames(lambda_df)[1] <- "Column"
colnames(lambda_df)[2] <- "lambda"
knitr::kable(setDT(lambda_df)[!is.na(lambda)])
# The following table shows the Columns that needs to be transformed according to the Box and Cox and the respective lambda
#   |Column                      |lambda              |
#   |:---------------------------|:-------------------|
#   |DAYS_BIRTH                  |0.6                 |
#   |SK_ID_CURR                  |0.7                 |
#   |AMT_INCOME_TOTAL            |-0.0999999999999999 |
#   |AMT_CREDIT                  |0.2                 |
#   |AMT_ANNUITY                 |0.2                 |
#   |AMT_GOODS_PRICE             |0.2                 |
#   |REGION_POPULATION_RELATIVE  |0.3                 |
#   |CNT_FAM_MEMBERS             |0.2                 |
#   |REGION_RATING_CLIENT        |1                   |
#   |REGION_RATING_CLIENT_W_CITY |1                   |
#   |EXT_SOURCE_1                |0.9                 |
#   |EXT_SOURCE_2                |1.5                 |
#   |EXT_SOURCE_3                |1.2                 |

## ------------------------------------------------------------------------
# Pre-processing transformation (centering, scaling etc.) can be estimated from the training data and applied to any data set with the same variables.
preProcValues <- preProcess(dt1, method = "BoxCox")
preProcValues
# There are 13 columns that are identified for transformation as per the Box Cox
# Created from 10746 samples and 29 variables
# 
# Pre-processing:
#   - Box-Cox transformation (13)
# - ignored (16)
# 
# Lambda estimates for Box-Cox transformation:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.1000  0.2000  0.6000  0.6077  1.0000  1.5000 
dt1_tran <- predict(preProcValues, dt1)

#Recreate numeric list with new dt1_tran
numeric_list <- unlist(lapply(dt1_tran, is.numeric))
dt1_num <- setDT(dt1_tran)[, ..numeric_list]

## ------------------------------------------------------------------------
# An example below shows one of the columns that have been transformed and visualized
col_trans <- lambda_df[!is.na(lambda)]$Column
i = 5
x <- list(
  title = as.character(col_trans[i])
)
p1 <- plot_ly(x = ~setDT(dt1)[,get(as.character(col_trans[i]))], type = "histogram", autobinx = FALSE) %>% layout(showlegend = FALSE) 
p2 <- plot_ly(x = ~setDT(dt1_tran)[,get(as.character(col_trans[i]))], type = "histogram", autobinx = FALSE) %>% layout(showlegend = FALSE)
subplot(p1,p2)

########################################################
# Transformed Predictors (Variables)
########################################################

# Looking at the before and after distribution for all the transformed variables

## ----fig t1, fig.height = 20, fig.width = 10-----------------------------
doPlots(as.data.frame(dt1)[, (colnames(dt1) %in% as.character(col_trans))], plotHist, ii = 1:length(col_trans))

## ----fig t2, fig.height = 20, fig.width = 10-----------------------------
doPlots(as.data.frame(dt1_tran)[, (colnames(dt1_tran) %in% as.character(col_trans))], plotHist, ii = 1:length(col_trans))
# Transformation to resolve Outliers
# Outlier is defined as samples that are exceptionally far from the mainstream of the data. When one or more samples are identified as outliers then it is important to make sure that the values are scientifically valid. Several predictor models like Tree-based and SVMs are resistant to outliers.

# If a model is considered to be sensitive to outliers, then a spatial data transformation is used (it is important to scale it before doing the transformation)

########################################################
# Impute Missing Values
########################################################

# Finding the % of missing values for all columns
mv <- as.data.frame(apply(dt1_tran, 2, function(col)sum(is.na(col))/length(col)))
colnames(mv)[1] <- "missing_values"
mv <- index_to_col(mv,'Column')
mv <- setDT(mv)[order (missing_values, decreasing = TRUE)]

ggplot (mv[1:40,], aes (reorder(Column, missing_values), missing_values)) + geom_bar (position = position_dodge(), stat = "identity") + coord_flip () + xlab('Columns') + ylab('Missing Value %')

## ------------------------------------------------------------------------
# In this dataset, I would be using the imputation technique. Using the means to impute the missing values as a quick fix. I will be looking into this further to remove any columns with missing values greater than 60% and using methods suggested in the kaggle discussion section
# Replace NA by Aggregation. default function is mean.
dt1_num2 <- na.aggregate(dt1_num,2, FUN = mean)

########################################################
# Data Reduction and Feature Extraction
########################################################
# PCA is a commonly used data reduction technique. This method seeks to find the linear combination of the predictors known as (PCs). The PC is defined as the linear combination of the predictors that captures the most variability of all possible linear combinations. The coefficients help us understand the component weights and which predictors are most important to each PCs

# Before running a PCA analysis, it is best to first transform the skewed predictors and then center and scale. The PCA analysis is an unsupervised learning technique and does not consider the modelling objective or response variable when summarizing variability.

regexp <- "[[:digit:]]+"
pcaObject <- prcomp(dt1_num2,  scale = TRUE, center = TRUE)
eig_tb <- cbind(Dimensions = rownames(get_eig(pcaObject)), get_eig(pcaObject))
ts <- setDT(eig_tb)[cumulative.variance.percent > 80][1,1]
ts <- str_extract(as.character(ts[[1]]), regexp)

n <- as.numeric(ts)
col_list <- list()
for (i in 1:n){ 
 col_list[i]<-paste('rotation.PC',i, sep="") 
} 

pca_df <- as.data.frame(pcaObject[2])
pca_df <- pca_df[,colnames(pca_df) %in% col_list]
pca_df <- cbind(Features = rownames(pca_df), pca_df)
pca_df <- setDT(pca_df)[order (rotation.PC1, decreasing = TRUE)]

## ------------------------------------------------------------------------
# The scree plot is used to determine the number of components and the variability
fviz_eig(pcaObject)
# The number of PCA before 80% of the variance is captured is 42
# In a classification problem, the PCA can be used to show potential seperation of classes.
# Overlapping classes does not mean that other models, especially ones that can accomodate non linear relationship will reach the same conclusion

# PCA can also be used to check if there are any blatant outliers

## ----echo=FALSE----------------------------------------------------------
# comp <- data.frame(pcaObject$x[,1:4])
# plot(comp, col=dt1_tran$TARGET, pch=16)

########################################################
# Removing Predictors
########################################################
# There are 3 advantages of removing variables
# 1) Decrease in computational time and complexity
# 2) Removing highly correlated variables with same underlying information
# 3) Removing predictors with degenerate distributions

## ------------------------------------------------------------------------
nzv <- nearZeroVar(dt1,saveMetrics= TRUE)
nzv <- index_to_col(nzv,"Column")
nzv_tb <- setDT(nzv)[nzv == TRUE | zeroVar ==TRUE]
nzv_tb[sample(1:nrow(nzv_tb), size = nrow(nzv_tb)),] %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = T
  ))

#Saving columns with nzv
rm_col_nzv <- as.character(setDT(nzv)[nzv == TRUE | zeroVar ==TRUE]$Column)
# There are 33 columns that have been identified with near zero variance


########################################################
# Between Predictor-Correlations
########################################################

# Collinearity is the technical term for the situation where a pair of predictor variables have a substantial correlation with each other. 
# It is also possible to find relationships between multiple predictors at once (called multi-collinearity)

# When the dataset consists of too many predictors to examine visually techniques such as PCA can be used to characterize the magnitude of the problem.

# Reasons to avoid highly correlated data
# 1.Redundant predictors frequently add more complexity to the model than information they provide to the model
# 2.Results in unstable model, numerical errors and degraded predictive performance

dt1_num_corr <- dt1_num
colnames(dt1_num_corr)[1:ncol(dt1_num_corr)] <- c(1:ncol(dt1_num_corr))

correlations <- cor(na.omit(dt1_num_corr))
corrplot(correlations, method="square")
# Visually on a quick glance, we can see that there is a portion of predictors that are highly correlated (the big blue box in the middle)

## ------------------------------------------------------------------------
df_corr = cor(dt1_num2, use = "pairwise.complete.obs")
hc = findCorrelation(df_corr, cutoff=0.80)
hc = sort(hc)
dt1_num3 = as.data.frame(dt1_num2)[,-c(hc)]

rm_col_hc <- setdiff(colnames(dt1_num2),colnames(dt1_num3))
rm_col_hc
# There are 38 columns that have been identified with high correlation with a cutoff set at 0.80

## ------------------------------------------------------------------------
#Highly correlated vairables table format
# Below is the table showing the variables that have a correlation abs > 0.8
df_corr2 <- df_corr %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  gather(var2, value, -var1) %>%
  arrange(desc(value)) %>%
  group_by(value)

corr_tb <- setDT(df_corr2)[abs(value) > 0.8 & var1 != var2 & var1 != "Ttl_Rating"  & var2 != "Ttl_Rating"]
corr_tb <- corr_tb[!duplicated(corr_tb$value),]

l1 <- corr_tb$var1
l2 <- corr_tb$var2

corr_tb[sample(1:nrow(corr_tb), size = nrow(corr_tb)),] %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = T
  ))

# Scatter Plots (Highly Correlated Variables)
doPlotsCorr(dt1_num2,plotCorr,l1,l2,1:6)

## ----fig s2, fig.height = 15, fig.width = 10-----------------------------
#doPlotsCorr(dt1_num2,plotCorr,l1,l2,13:27)

## ----fig s3, fig.height = 15, fig.width = 10-----------------------------
#doPlotsCorr(dt1_num2,plotCorr,l1,l2,71:83)

## ----fig s4, fig.height = 15, fig.width = 10-----------------------------
#doPlotsCorr(dt1_num2,plotCorr,l1,l2,58:70)

## ------------------------------------------------------------------------
rm_col_all <- append(rm_col_hc,rm_col_nzv)
dt1_tran <- as.data.frame(dt1_tran)[, !(colnames(dt1_tran) %in% rm_col_all)]

## ----echo=FALSE----------------------------------------------------------
#Recreate numeric list with new dt1_tran
numeric_list <- unlist(lapply(dt1_tran, is.numeric))
dt1_num <- setDT(dt1_tran)[,..numeric_list]

# Transformed Predictors (Variables)
# Now that all the pre processing for numeric variables is done, lets have a look at the density plot to compare the target variable
doPlots(dt1_num2, plotDen, ii = 1:20)

## ----fig d2, fig.height = 20, fig.width = 10-----------------------------
doPlots(dt1_num2, plotDen, ii = 21:40)

########################################################
# Adding Predictors
########################################################

# When a predictor is categorical then it is common to decompose the predictor into a set of more specific variables. The dummy variables are always n-1 with n being the levels of the variable. The decision to include all the variables depends on the type of model we would be using. If a model that is sensitive to that type of information is being used, such as linear regression, then it is important to use n-1. Otherwise, using n (complete set of dummy variables) would help improve interpretation of the model

# Dummy variables are also called “One Hot Encoding”. It is important to note that there are several ways of apporaching dummy variables depending on the type of categorical feature i.e. whether it is ordinal or not. An example of an ordinal but a categorical feature would be size of a shirt (XS, S, M, L, XL). In that case, the feature should be changed to an integer (XS =1, S=2 etc.). An example of NOT an ordinal but a categorical feature would be sex (M, F). In that case, the feature should be “One Hot Encoded”

# Before changing the categorical variables to a dummy variable, lets look at how the relationship is with the Target variable using Box Plots

non_numeric_list <- unlist(lapply(dt1_tran, is.character))
dt1_non_num <- setDT(dt1_tran)[,..non_numeric_list]

## ----fig b1, fig.height = 10, fig.width = 10-----------------------------
doPlots(dt1_non_num, plotBar, ii = 1:9)

## ----fig b2, fig.height = 10, fig.width = 10-----------------------------
doPlots(dt1_non_num, plotBar, ii = c(9,11,13:16))

## ----fig b3, fig.height = 10, fig.width = 10-----------------------------
grid.arrange(plotBar(dt1_non_num, 10),plotBar(dt1_non_num, 12), ncol=1, nrow=2)

## ------------------------------------------------------------------------
# Changing the categorical to dummy variables
dt1_non_num <- cbind(dt1_non_num,dt1_tran[,'TARGET'])
dummies <- dummyVars(TARGET ~ ., data = dt1_non_num, drop2nd = TRUE)
dt1_non_num_dum <- predict(dummies, newdata = dt1_non_num)
# Number of columns with character variables are 17 and after creating dummy variables there the number of columns are 146

## ------------------------------------------------------------------------
# Attaching numeric and non numeric columns
dt1_preproc <- cbind(dt1_non_num_dum,dt1_num)

mv <- as.data.frame(apply(dt1_preproc, 2, function(col)sum(is.na(col))/length(col)))
colnames(mv)[1] <- "missing_values"
mv <- index_to_col(mv,'Column')
mv <- setDT(mv)[order (missing_values, decreasing = TRUE)]

ggplot (mv[1:40,], aes (reorder(Column, missing_values), missing_values)) + geom_bar (position = position_dodge(), stat = "identity") + coord_flip () + xlab('Columns') + ylab('Missing Value %')
# There are some categorical values which after doing the one hot encoding have missing values. For the sake of ease, I will be using the mean of the column to fill in the NA values
dt1_preproc <- na.aggregate(dt1_preproc,2, FUN = mean)

# check missing values again:
mv <- as.data.frame(apply(dt1_preproc, 2, function(col)sum(is.na(col))/length(col)))
colnames(mv)[1] <- "missing_values"
mv <- index_to_col(mv,'Column')
mv <- setDT(mv)[order (missing_values, decreasing = TRUE)]

ggplot (mv[1:40,], aes (reorder(Column, missing_values), missing_values)) + geom_bar (position = position_dodge(), stat = "identity") + coord_flip () + xlab('Columns') + ylab('Missing Value %')


########################################################
# Chapter 4: Over-Fitting and Model Tuning
########################################################
# Overfitting refers to a model that models the training data too well.
# Overfitting happens when a model learns the detail and noise in the training data to the extent that it negatively impacts the performance of the model on new data.

# Model Tuning
# Many models have tuning parameters which cannot be directly estimated from the data. Since many of these parameters control the complexity of the model, poor choices for the values can result in overfitting

# Data Splitting
# Data Splitting usually entails a training, validation and test set
set.seed(1234)
dt1_preproc_sample <- setDT(dt1_preproc)[sample(nrow(dt1_preproc), round(nrow(dt1_preproc)*0.01,0)),]
# [1] 3075  187

## ---- echo=T-------------------------------------------------------------
# control <- rfeControl(functions=rfFuncs, method="cv", number=3)
# trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary)
# 
# results <- rfe(as.data.frame(dt1_preproc_sample)[,-c(153)],
#                as.data.frame(dt1_preproc_sample)[,c(153)], sizes=c(1:100),
#                rfeControl=control,
#                method="rf",
#                metric = "AUC",
#                trControl = trainctrl)
# print(results)
# predictors(results)
# plot(results, type=c("g", "o"))

## ----Boruta--------------------------------------------------------------
#boruta.train <- Boruta(TARGET~., data = dt1_preproc, doTrace = 2)
#print(boruta.train)

## ------------------------------------------------------------------------
#cols_to_keep <- c(predictors(results),"TARGET")
cols_to_keep <- c('FLAG_OWN_CARN','`ORGANIZATION_TYPEIndustry: type 1`','DAYS_ID_PUBLISH','SK_ID_CURR','REG_CITY_NOT_LIVE_CITY','YEARS_BEGINEXPLUATATION_MODE','COMMONAREA_MODE','FLOORSMAX_MODE','LIVINGAPARTMENTS_MODE','YEARS_BUILD_MEDI','CODE_GENDERM','OCCUPATION_TYPEWaiters/barmen staff','TARGET','EXT_SOURCE_1','EXT_SOURCE_2','EXT_SOURCE_3')
dt1_preproc_sample <- as.data.frame(dt1_preproc_sample)[, (colnames(dt1_preproc_sample) %in% cols_to_keep)]


## ----include=FALSE-------------------------------------------------------
predictors <- setDT(dt1_preproc_sample)[,-c('TARGET')]
classes <- as.factor(dt1_preproc_sample$TARGET)
trainingRows <- createDataPartition(y=classes, p = 0.80, list =FALSE)
trainPredictors <- predictors[trainingRows,]
trainclasses <- classes[trainingRows]
testPredictors <- predictors[-trainingRows,]
testClasses <- classes[-trainingRows]

## ------------------------------------------------------------------------
########################################################
# Resampling Techniques
########################################################

# K-Fold Cross-Validation
# The samples are randomly partitioned into k sets of roughly equal size. A model is fit using the all samples except the first subset (called the first fold). The held-out samples are predicted by this model and used to estimate performance measures. The first subset is returned to the training set and procedure repeats with the second subset hold out and so on. The k sampling estimates of performance are summarized and used to understand the relationship between the tuning parameter and model utility.

# A slight variant is to select the k partitions in a way that makes the folds balanced with respect to the outcome (Stratified random sampling)

# Another version, leave-one-out cross-validation (LOOCV) is the special case where k is the number of samples

# The choice of k is between 5 and 10 but there is no formal rule.

# K-Fold generally has a larger variance as compared to other medhots and for that reason might not be attractive. However, with large training sets the potential issues with variance and bias become negligible. Larger values of k are also more computationally burdensome.


cvSplits <- createFolds(trainclasses, k = 10, returnTrain = TRUE)

## ------------------------------------------------------------------------
# Repeated Training Test Splits
# Also knows as the “leave-group-out cross validation”, the technique simply creates multiple splits of the data into modelling and prediction sets. The number of repetitions is important. Increasing the number of subsets has the effect of decreasing the uncertianity of the performance estimates. To get stable estimates of performance, it is suggested to choose a large number of repetitions (say 50-200)
repeatedSplits <- createDataPartition(trainclasses, p =0.8, times = 3)

## ------------------------------------------------------------------------
# The Bootstrap
# A bootstrap sample is a random sample of the data taken with replacement. This means that after a data point is selected for the subset, it is still available for further selection. In general, bootstrap methods have less uncertainity than k-fold cross-validation

bsSplits <- createResample(trainclasses, times = 10, list = TRUE)


## ----DataPartition-------------------------------------------------------
# Data Splitting Recommendations
# If the sample size is small, we recommend repeated 10-fold cross-validation for several reasion: The bias and variance properties are good and given the sample size the computational costs are not large

# If the goal is to choose between models as opposed to getting the best indicator of performance, a strong case can be made for using one of the bootstrap procedures since these have very low variance

# For large sample sizes, the differences between resampling methods become less pronounced and computational efficiency increases in importance. Here simple 10-fold cross-validation should provide acceptable variance, low bias and is relatively quick to compare

########################################################
# Choosing Between Models
########################################################

# Start with several models that are the least interpretable and most flexible
# Investigate the simple models that are less opaque (are not complete black boxes)
# Consider using the simplest model that reasonably approximates the performance of the more complex model
# The modeller can then discover the “performance ceiling”

dt1_preproc_sample <- mutate(dt1_preproc_sample, TARGET = ifelse(TARGET == 0,'Yes',"No"))
dt1_preproc_sample$TARGET <- as.factor(dt1_preproc_sample$TARGET)

inTrain <- createDataPartition(dt1_preproc_sample$TARGET, p = .8)[[1]]
dtTrain <- dt1_preproc_sample[ inTrain, ]
# 2461    15
dtTest  <- dt1_preproc_sample[-inTrain, ]
# 614   15

## ----echo=T, results='hide'----------------------------------------------
traincntrl <- trainControl(method = 'repeatedcv',
                                         number = 5,
                                         repeats = 2,
                                         classProbs = TRUE, 
                                         sampling = "down",
                                         summaryFunction = twoClassSummary)

## ------------------------------------------------------------------------
trainPredictors <- as.matrix(trainPredictors)

knnFit <- train(TARGET ~.,
                data = na.omit(dtTrain),
                method = "knn",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(.k = 2:10),
                trControl = traincntrl)
knnFit$results

## ----SVM-----------------------------------------------------------------
svmFit <- train(TARGET ~.,
                data = na.omit(dtTrain),
                method = 'svmRadial',
                preProc = c('center','scale'),
                tuneLength = 7,
                trControl = traincntrl)

svmFit

plot(svmFit, scales = list(x=list(log =2)))
predictClasses <- predict(svmFit, dtTest)
predictProbs <- predict(svmFit, newdata = dtTest, type = "prob")

## ------------------------------------------------------------------------
# svmFit$results %>%
#   mutate(accuracySD_low = Accuracy - 2*(AccuracySD/sqrt(svmFit$control$number * svmFit$control$repeats)),
#          accuracySD_high = Accuracy + 2*(AccuracySD/sqrt(svmFit$control$number * svmFit$control$repeats))) %>%
#   ggplot(aes(x = C)) +
#   geom_line(aes(y = Accuracy)) +
#   geom_point(aes(y = Accuracy)) + theme_classic() +
#   scale_x_log10() + #correct spacing of the cost parameter
#   ylim(0.50, 0.70) + #set correct y-axis
#   geom_errorbar(aes(ymin=accuracySD_low, ymax=accuracySD_high), 
#                 colour="gray50",
#                 width=.1) +
#   labs(title="Estimates of prediction accuracy\nwith 2 SD errror bars")
#  


## ----LogisticRegression--------------------------------------------------
logisticReg <- train(TARGET ~.,
                     data = na.omit(dtTrain),
                     method = 'glm',
                     trControl = traincntrl)



## ----Comparingresults----------------------------------------------------
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg, KNN = knnFit))
summary(resamp)

########################################################
# Chapter 5: Measuring Performance in Regression Models
########################################################

# When the outcome is a number, the most common method for characterizing a model’s predictive capabilities is to use the root mean squared error (RMSE).

# The mean squared error (MSE) is calculated by squaring the residuals and summing them. The RMSE is then calculated by taking the square root of the MSE so that it is in the same units as the original data.

# Another common metric is the coefficient of determination, commonly written as R2. An R2 value of 0.75 implies that the model can explain three-quarters of the variation in the outcome. R2 is a measure of correlation and not accuracy.

# One might view a model with a 90% R2 positively, but the RMSE may be in the tens of thousands of dollars-poor predictive accuracy for anyone selling a moderately priced property.

# Variance-Bias Trade off
# Complex models have very high variance which leads to overfitting while simple models have very high bias which leads to underfitting

########################################################
# Chapter 7: Nonlinear Regression Models
########################################################

# Support Vector Machines (SVM)
# The chapter will focus on e-insensitive regression.

# SVMs for regression use a function similar to the Huber function, with an important difference. Given a threshold set by the user (denoted as e), data points with residuals within the threshold do not contribute to the regression fit while data points with an absolute difference greater than the threshold contribute a linear-scale amount.

# There are several consequences to this approach:
  
  # Since the squared residuals are not used, large outliers have a limited effect on the regression equation.
# Samples that the model fits well (i.e., the residuals are small) have no effect on the regression equation. In fact, if the threshold is set to a relatively large value, then the outliers are the only points that define the regression line
# Parameters in SVM:
  # The book mentions that there are 4 types of kernels which can be changed as stated below:
  
  # Linear
# Radial
# Polynomial
# Hyperbolic tangent
# Which kernel function should be used? This depends on the problem. The radial basis function has been shown to be very effective. However, when the regression line is truly linear, the linear kernel function will be a better choice.
# 
# Note that some of the kernel functions have extra parameters. For example, the polynomial degree in the polynomial kernel must be specified. Similarly, the radial basis function has a parameter (??) that controls the scale. These parameters, along with the cost value, constitute the tuning parameters for the model.
# 
# The cost parameter is the main tool for adjusting the complexity of the model. When the cost is large, the model becomes very flexible since the effect of errors is amplified. When the cost is small, the model will “stiffen” and become less likely to over-fit (but more likely to underfit) because the contribution of the squared parameters is proportionally large in the modified error function. The tuneLength parameter changes the cost parameter in SVM.
# 
# SVM also has a gamma parameter which can be used. Technically speaking, large gamma leads to high bias and low variance models, and vice-versa.


## ----SVM_Tuning----------------------------------------------------------
svmFitRadial <- svmFit

svmFitLinear <- train(TARGET ~.,
                data = dtTrain,
                method = 'svmLinear',
                preProc = c('center','scale'),
                metric = "ROC",
                tuneLength = 7,
                trControl = traincntrl)


# svmFitPoly <- train(TARGET ~.,
#                 data = dtTrain,
#                 method = 'svmPoly',
#                 preProc = c('center','scale'),
#                 tuneLength = 7,
#                 trControl = traincntrl)


## ------------------------------------------------------------------------
# Comparing all the SVM models
resamp <- resamples(list(SVM_Radial = svmFit, SVM_Linear = svmFitLinear))
summary(resamp)

## ----KNN_Tuning----------------------------------------------------------
# K-Nearest Neighbors (KNN)
# The KNN approach simply predicts a new sample using the K-closest samples from the training set. The basic KNN method as described above depends on how the user defines distance between samples. Euclidean distance.

# It is easy to see that when q = 2, then Minkowski distance is the same as Euclidean distance. When q = 1, then Minkowski distance is equivalent to Manhattan (or city-block) distance, which is a common metric used for samples with binary predictors.

# Because the KNN method fundamentally depends on distance between samples, the scale of the predictors can have a dramatic influence on the distances among samples. To avoid this potential bias and to enable each predictor to contribute equally to the distance calculation, we recommend that all predictors be centered and scaled prior to performing KNN.

# Upon pre-processing the data and selecting the distance metric, the next step is to find the optimal number of neighbors. Like tuning parameters from other models, K can be determined by resampling.
# Two commonly noted problems are computational time and the disconnect between local structure and the predictive ability of KNN.

knnFit <- train(TARGET ~.,
                data = dtTrain,
                method = "knn",
                preProc = c("center", "scale"),
                metric = "ROC",
                tuneGrid = data.frame(.k = 1:20),
                trControl = traincntrl)

knnFit$results

## ------------------------------------------------------------------------
########################################################
# Chapter 11: Measuring Performance in Classification Models
########################################################

# Class Predictions
# Classification models usually generate two types of predictions. Like regression models, classification models produce a continuous valued prediction, which is usually in the form of a probability

# Most classification models generate predicted class probabilities. However, when some models are used for classification, like neural networks and partial least squares, they produce continuous predictions that do not follow the definition of a probability-the predicted values are not necessarily between 0 and 1 and do not sum to 1

# For classification models like these, a transformation must be used to coerce the predictions into “probability-like” values so that they can be interpreted and used for classification. One such method is the softmax transformation

# https://medium.com/@uniqtech/understand-the-softmax-function-in-minutes-f3a59641e86d

dtTest$svmFitLinearclass <- predict(svmFitLinear, dtTest)
dtTest$svmFitLinearprobs <- predict(svmFitLinear, newdata = dtTest , type = "prob")

dtTest$logclass <- predict(logisticReg, dtTest)
dtTest$logprobs <- predict(logisticReg, newdata = dtTest , type = "prob")

## ------------------------------------------------------------------------
# Well-Calibrated Probabilities

# One way to assess the quality of the class probabilities is using a calibration plot. For a given set of data, this plot shows some measure of the observed probability of an event versus the predicted class probability. If the points fall along a 45??? line, the model has produced well-calibrated probabilities
calCurve <- calibration(TARGET ~ svmFitLinearprobs[,1] + logprobs[,1], data = dtTest)
calCurve

xyplot(calCurve, auto.key = list(columns = 2))

## ------------------------------------------------------------------------
# Presenting Class Probabilities

# Visualizations of the class probabilities are an effective method of communicating model results.

# Example

# The top panel of figure below shows histograms of the test set probabilities for the logistic regression model (the panels indicate the true credit status). The probability of bad credit for the customers with good credit shows a skewed distribution where most customers’ probabilities are quite low. In contrast, the probabilities for the customers with bad credit are flat (or uniformly distributed), reflecting the model’s inability to distinguish bad credit cases

# Evaluating Predicted Classes
# A common method for describing the performance of a classification model is the confusion matrix.

# https://medium.com/greyatom/performance-metrics-for-classification-problems-in-machine-learning-part-i-b085d432082b

# The simplest metric is the overall accuracy rate (or, for pessimists, the error rate). This reflects the agreement between the observed and predicted classes and has the most straightforward interpretation

# However, there are a few disadvantages to using accuracy:
  
  # Overall accuracy counts make no distinction about the type of errors being made
# One must consider the natural frequencies of each class (Class Imbalance)
# The Kappa statistic was originally designed to assess the agreement between two raters (Cohen 1960). Kappa takes into account the accuracy that would be generated simply by chance.

# http://www.statisticshowto.com/cohens-kappa-statistic/
  
  # Depending on the context, Kappa values within 0.30 to 0.50 indicate reasonable agreement

# Two Class Problems

# The sensitivity of the model is the rate that the event of interest is predicted correctly for all samples having the event. The sensitivity is sometimes considered the true positive rate since it measures the accuracy in the event population. It is also known as Recall or True Positive.

# The specificity is defined as the rate that nonevent samples are predicted as nonevents. The false-positive rate is defined as one minus the specificity.

# Intuitively, increasing the sensitivity of a model is likely to incur a loss of specificity, since more samples are being predicted as events. Potential trade-offs between sensitivity and specificity may be appropriate when there are different penalties associated with each type of error.

# Following is the confusion matrix of the SVM polynomial model

confusionMatrix(data = dtTest$svmFitLinearclass,
 reference = dtTest$TARGET,
 positive = "Yes")

## ------------------------------------------------------------------------
confusionMatrix(data = dtTest$logclass,
 reference = dtTest$TARGET,
 positive = "Yes")

## ------------------------------------------------------------------------
# Evaluating Class Probabilities
# The receiver operating characteristic (ROC) curve is one technique for evaluating this trade-off. The ROC curve is created by evaluating the class probabilities for the model across a continuum of thresholds. For each candidate threshold, the resulting true-positive rate (i.e., the sensitivity) and the false-positive rate (one minus the specificity) are plotted against each other.
# 
# The optimal model should be shifted towards the upper left corner of the plot. Alternatively, the model with the largest area under the ROC curve would be the most effective.
# 
# One advantage of using ROC curves to characterize models is that, since it is a function of sensitivity and specificity, the curve is insensitive to disparities in the class proportions.
# 
# A disadvantage of using the area under the curve to evaluate models is that it obscures information

library(pROC)
# Chapter 12.2: Logistic Regression
# Linear regression (Sect. 6.2) forms a model that is linear in the parameters, and these parameters are obtained by minimizing the sum of the squared residuals. It turns out that the model that minimizes the sum of the squared residuals also produces maximum likelihood estimates of the parameters when it is reasonable to assume that the model residuals follow a normal (i.e., Gaussian) distribution.
# 
# https://towardsdatascience.com/the-logistic-regression-algorithm-75fe48e21cfa
# 
# Also, this model produces linear class boundaries, unless the predictors used in the model are Once we find ?? values that appear to maximize the likelihood for our data, these values would be used to predict sample outcomes.
# 
# ROC and AUC

rocCurve <- roc(response = dtTest$TARGET, predictor = dtTest$svmFitLinearprobs[,1], levels = rev(levels(dtTest$TARGET)))

plot(rocCurve, legacy.axes = TRUE)
auc(rocCurve)
# Area under the curve: 0.7471

## ----Logreg--------------------------------------------------------------
rocCurve <- roc(response = dtTest$TARGET, predictor = dtTest$logprobs[,1], levels = rev(levels(dtTest$TARGET)))

plot(rocCurve, legacy.axes = TRUE)
auc(rocCurve)
# Area under the curve: 0.7581

## ------------------------------------------------------------------------
########################################################
# Chapter 14: Classification Trees and Rule-Based Models
########################################################

# Classification trees fall within the family of tree-based models and, similar to regression trees, consist of nested if-then statements
# 
# They can be highly interpretable, can handle many types of predictors as well as missing data, but suffer from model instability and may not produce optimal predictive performance
# 
# The aim of classification trees is to partition the data into smaller, more homogeneous groups. A simple way to define purity in classification is by maximizing accuracy or equivalently by minimizing misclassification error.
# 
# Two alternative measures, the Gini index (Breiman et al. 1984) and cross entropy, which is also referred to as deviance or information shift the focus from accuracy to purity.
# 
# https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
#   
#   Variable importance can be computed for classification trees by assessing the overall improvement in the optimization criteria for each predictor
# 
# Handling Categorical Variables
# 
# For tree models, the splitting proceduremay be able to make more dynamic splits of the data, such as groups of two or more categories on either side of the split. However, to do this, the algorithm must treat the categorical predictors as an ordered set of bits. Therefore, when fitting trees and rule-based models, the practitioner must make a choice regarding the treatment of categorical predictor data:
#   
#   Each categorical predictor can be entered into the model as a single entity so that the model decides how to group or split the values. In the text, this will be referred to as using grouped categories
# 
# Categorical predictors are first decomposed into binary dummy variables. In effect, splitting on a binary dummy variable prior to modeling imposes a “one-versus-all” split of the categories. This approach will be labelled as using independent categories.
# 
# If a subset of the categories are highly predictive of the outcome, the first approach is probably best.
# 
# However, this choice can have a significant effect on the complexity of the model (interpretability) and, as a consequence, the performance.
# 
# Rule Based Models
# C4.5Rules
# 
# For a rule, the model first calculates a baseline pessimistic error rate, then removes each condition in the rule in isolation. Once a condition is removed, the pessimistic error rate is recomputed. If any error rate is smaller than the baseline, the condition associated with the smallest error rate is removed. The process is repeated until all conditions are above the baseline rate or all conditions are removed.
# 
# PART
# 
# Here, a pruned C4.5 tree is created from the data and the path through the tree that covers the most samples is retained as a rule. The samples covered by the rule are discarded from the data set and the process is repeated until all samples are covered by at least one rule. Although the model uses trees to create the rules, each rule is created separately and has more potential freedom to adapt to the data.
# 
# Bagged Trees
# 
# In bagged trees, each model in the ensemble is used to predict the class of the new sample. Since each model has equal weight in the ensemble, each model can be thought of as casting a vote for the class it thinks the new sample belongs to. The total number of votes within each class are then divided by the total number of models in the ensemble (M) to produce a predicted probability vector for the sample. The new sample is then classified into the group that has the most votes, and therefore the highest probability.
# 
# Random Forests
# 
# As with bagging, each tree in the forest casts a vote for the classification of a new sample, and the proportion of votes in each class across the ensemble is the predicted probability vector. While the type of tree changes in the algorithm, the tuning parameter of number of randomly selected predictors to choose from at each split is the same (denoted as mtry). As in regression, the idea behind randomly sampling predictors during training is to de-correlate the trees in the forest
# 
# AdaBoost
# 
# AdaBoost generates a sequence of weak classifiers, where at each iteration the algorithm finds the best classifier based on the current sample weights. Samples that are incorrectly classified in the kth iteration receive more weight in the (k + 1)st iteration, while samples that are correctly classified receive less weight in the subsequent iteration. This means that samples that are difficult to classify receive increasingly larger weights until the algorithm identifies a model that correctly classifies these samples. Therefore, each iteration of the algorithm is required to learn a different aspect of the data, focusing on regions that contain difficult-to-classify samples. At each iteration, a stage weight is computed based on the error rate at that iteration. The nature of the stage weight described in Algorithm 14.2 implies that more accurate models have higher positive values and less accurate models have lower negative values.5 The overall sequence of weighted classifiers is then combined into an ensemble and has a strong potential to classify better than any of the individual classifiers.

dtFitCART <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'rpart',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)

dtFitPART <- train(x= setDT(dtTrain)[,-c('TARGET')],
                  y= dtTrain$TARGET,
                  method = 'PART',
                  preProc = c('center','scale'),
                  tuneLength = 7,
                  metric = "ROC",
                  trControl = traincntrl)

dtFitBagged <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'treebag',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)

dtFitrf <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'rf',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)

dtFitAdaboost <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'adaboost',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)

dtFitXGboost <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'xgbTree',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)

dtFitC5.0 <- train(x= setDT(dtTrain)[,-c('TARGET')],
                y= dtTrain$TARGET,
                method = 'C5.0',
                preProc = c('center','scale'),
                tuneLength = 7,
                metric = "ROC",
                trControl = traincntrl)



## ------------------------------------------------------------------------
alltreemodels <- resamples(list(CART = dtFitCART
                                ,PART = dtFitPART
                                ,Bagged = dtFitBagged
                                ,RF = dtFitrf
                                # ,AdaBoost = dtFitAdaboost
                                # ,XGBoost = dtFitXGboost
                                ,C5.0 = dtFitC5.0
                                )
                           )
summary(alltreemodels)


## ------------------------------------------------------------------------
# Use the model with the best Accuracy
dtTest$C5.0class <- predict(dtFitC5.0, dtTest)
dtTest$C5.0probs <- predict(dtFitC5.0, newdata = dtTest , type = "prob")

confusionMatrix(data = dtTest$C5.0class,
 reference = dtTest$TARGET,
 positive = "Yes")

## ------------------------------------------------------------------------
# make test predictions
#lgb_pred <- predict(dtFitXGboost, data = data.matrix(test), n = dtFitXGboost[1])
#result <- data.frame(SK_ID_CURR = Id, TARGET = lgb_pred)
#write.csv(result,"LGBM.csv", row.names = F)

