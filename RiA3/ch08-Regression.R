#---------------------------------------------------------------------#
# R in Action (3rd ed): Chapter 8                                     #
# Regression                                                          #
# requires the car, MASS, effects, bootstrap and leaps packages       #                 #
# install.packages(c("car", "MASS", "leaps", "effects", "bootstrap")) #               #
#---------------------------------------------------------------------#


######################################################################## 8.2 回归模型的定义与拟合（单因变量、多因变量、线性、非线性、交互效应）
######################################################################


# Listing 8.1 - Simple linear regression
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)

plot(women$height,women$weight, 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
abline(fit)


# Listing 8.2 - Polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

plot(women$height,women$weight,
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))



# Listing 8.3 - Examining bivariate relationships
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, smooth=FALSE,
                  main="Scatter Plot Matrix")


# Listing 8.4 - Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)


# Listing 8.5 - Mutiple linear regression with a significant interaction term
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)

library(effects)  # 一个用于线性回归、广义线性回归的显示效果包
plot(effect("hp:wt", fit,, list(wt=c(2.2, 3.2, 4.2))), lines=c(1,2,3), multiline=TRUE)

######################################################################## 8.3 回归结果的诊断
## 常见的诊断类型：
##  （1）因变量的正态性（Q-Q 图）
##  （2）因变量的独立性或自相关性（Durbin–Watson 检验）
##  （3）因变量的同方差性（ncv 检验或 spread-Level 图）
##  （4）自变量与因变量之间是否呈线性关系（部分残差图）
##  （5）多个自变量与因变量之间是否存在多重共线性关系（vif 检验）
#####################################################################

##  Q-Q 图： 检查因变量的正态性，正确值应位于 45 度对角线上
##  残差-拟合值图： 用于判断模型中是否存在未被建模的系统模式，正确建模应当是以 0 残差为均值线的无规律分布
##  尺度-位置图： 用于判断因变量是否为同方差，正确的建模应当是以某个常数为均值的带状随机分布，不存在其他系统模式
## 残差-杠杆图：用于分析数据中存在的杠杆值、异常值和重要影响值
######################################################################

# simple regression diagnostics
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))


# Assessing normality
library(car)  # car 包提供了一系列用于回归诊断的工具
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, simulate=TRUE, labels=row.names(states),
       id=list(method="identify"), main="Q-Q Plot")


# Independence of errors
durbinWatsonTest(fit)

# Assessing linearity
library(car)
crPlots(fit)


# Listing 8.6 - Assessing homoscedasticity
library(car)
ncvTest(fit)
spreadLevelPlot(fit)



# Listing 8.7 - Evaluating multi-collinearity
library(car)
vif(fit) 
vif(fit) > 10 # problem?



######################################################################## 8.4 异常值的诊断
## 常见的诊断类型：
##  （1）异常值（Outliers）：模型未能很好预测的观测，通常表现为异常高的残差，可以使用 outlierTest 函数进行检验
##  （2）高杠杆点（Leverage-Points）：也称离群点，指自变量取值显著区别于其他观测的那些点，高杠杆点与因变量无关，只与自变量的取值有关。一个拟合中是否存在高杠杆点，可以用帽统计量（ 或 hat 统计量）制图来检查
##  （3）高影响力观测：对模型参数取值具有特殊（不成比例）影响的那些观测。高影响力观测可以通过 库克距离（或 D 统计量）和相应的制图来检查
#####################################################################

# Assessing outliers
library(car)
outlierTest(fit)

#  Identifying high leverage points
# redo as ggplot2?
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

# Identifying influential observations

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")


# Added variable plots
# add id.method="identify" to interactively identify points
library(car)
avPlots(fit, ask=FALSE, id=list(method="identify"))


# Influence Plot
library(car)
influencePlot(fit, id="noteworthy", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

####################################################################
## 8.5 校正措施
## （1）删除观测
## （2）转换变量
## （3）添加或删除因变量
## （4）使用另一种回归方法
####################################################################


# Listing 8.10 - Box-Cox Transformation to normality
library(car)
summary(powerTransform(states$Murder))

# Box-Tidwell Transformations to linearity
library(car)
boxTidwell(Murder~Population+Illiteracy,data=states)


####################################################################
## 8.6 回归模型的比较与选择
## （1）嵌套模型比较法：比较含和不含某些自变量时的模型
## （2）变量的选择：逐步回归法（一次增加或删除一个自变量）、穷举法（所有可能的自变量组合）
####################################################################

# Listing 8.11 - Comparing nested models using the anova function
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
anova(fit2, fit1)


# Listing 8.12 - Comparing models with the AIC
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
AIC(fit1,fit2)


# Listing 8.13 - Backward stepwise selection
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)
step(fit, direction="backward")


# Listing 8.14 - All subsets regression
library(leaps)
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])

leaps <-regsubsets(Murder ~ Population + Illiteracy + Income +
                     Frost, data=states, nbest=4)

subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}

subsTable(leaps, scale="adjr2")


####################################################################
## 8.7 测试验证
## （1） 泛化能力：K 折交叉验证：
## （2）变量选择：相对重要性、相对权重
####################################################################

# Listing 8.15 - Function for k-fold cross-validated R-square
library(bootstrap)

shrinkage <- function(fit, k=10, seed=1){
  require(bootstrap)
  
  theta.fit <- function(x,y){lsfit(x,y)}                     
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}     
  
  x <- fit$model[,2:ncol(fit$model)]                         
  y <- fit$model[,1] 
  
  set.seed(seed)
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)  
  r2    <- cor(y, fit$fitted.values)^2                         
  r2cv  <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
}

states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Income + Illiteracy + Frost, data=states)
shrinkage(fit)

fit2 <- lm(Murder ~ Population + Illiteracy,data=states)
shrinkage(fit2)

install.packages("relaimpo")

# Listing 8.16 rlweights function for calculating relative importance of predictors
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}

# Listing 8.17 - Applying the relweights function
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
relweights(fit, col="blue")


####################################################################
## 总结 
## （1）回归分析是一种高度交互和迭代的方法，涉及拟合模型、评估其与统计假设的拟合度、修改数据和模型以及重新拟合以获得最终结果。 
## （2）回归诊断用于评估数据与统计假设的拟合程度，并选择修改模型或数据的方法以更紧密地满足这些假设。 
## （3）有多种方法可用于选择要包含在最终回归模型中的变量，包括使用显着性检验、拟合统计和自动化解决方案（例如逐步回归和所有子集回归）。 
## （4） 交叉验证可用于评估预测模型在新数据样本上的可能性能。
## （5） 相对权重方法可用于解决变量重要性的棘手问题：确定哪些变量对于预测结果最重要。
####################################################################
