rm(list=ls())
#install.packages("corplot")
#install.packages("ggthemes")
#install.packages("scales")
#install.packages("mice")
#install.packages("gridExtra")
#install.packages("corrplot")
library(ggplot2)
library(ggthemes) 
library(scales)
library(dplyr) 
library(mice)
library(randomForest) 
library(data.table)
library(gridExtra)
library(corrplot) 
library(GGally)
library(e1071)

train = read.csv("houseprices_train.csv")
test = read.csv("houseprices_test.csv")

str(train)

charactor_var <- names(train)[which(sapply(train, is.character))]
charactor_car <- c(character, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric <- names(train)[which(sapply(train, is.numeric))]
#Creating one training dataset with categorical variable and one with numeric variable
train1_charactor<-train[charactor_var]
train1_numeric<-train[numeric]
# Bar plot function
histogram <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =2))
  return (p)
}
# Density plot function
Density <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

# Function to call both Bar plot and Density plot function
plots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}
#Barplots
plots(train1_charactor, fun = histogram, ii = 1:4, ncol = 2)
plots(train1_charactor, fun = histogram, ii  = 5:8, ncol = 2)
plots(train1_charactor, fun = histogram, ii = 9:12, ncol = 2)
plots(train1_charactor, fun = histogram, ii = 19:22, ncol = 2)
#box plot
ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

plots(train1_numeric, fun = histogram , ii = 18:23, ncol = 2)
#histogram of sale price
hist(train$SalePrice)
ggplot(train,aes(y=SalePrice,x=GrLivArea))+geom_point()

# Logistic regression

model_1 = glm(BsmtFullBath~MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea,data =train,family = binomial)#creating logistin function
model_2 = glm(BsmtHalfBath~.,data =train,family = binomial)#creating logistin function
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels

#treemodel
library(rpart)
model_1 = rpart(Foundation~MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea,data =train,method = "class")#creating logistin function
plot(model_1, uniform=TRUE, 
     main="Classification Tree for House Foundation")
text(model_1, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")

model_2 = rpart(LotArea~MSSubClass+LotFrontage+OverallQual+OverallCond+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea,data =train,method = "class")#creating logistin function
plot(model_2, uniform=TRUE, 
     main="Classification Tree for Lot Area")
text(model_2, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")

model_3 = rpart(Street~MSSubClass+LotFrontage+OverallQual+OverallCond+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea,data =train,method = "class")#creating logistin function
plot(model_3, uniform=TRUE, 
     main="Classification Tree for Street")
text(model_3, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")