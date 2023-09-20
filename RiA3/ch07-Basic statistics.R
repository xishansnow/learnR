#------------------------------------------------------------#
# R in Action (3rd ed): Chapter 7                            #
# Basic statistics                                           #
# requires the Hmisc, pastecs, psych, dplyr, carData, vcd    #
#    gmodels, ggm, and MASS packages                         #
# install.packages(c("Hmisc", "pastecs", "psych", "dplyr",   #   
#                    "carData", "vcd", "gmodels", "ggm",     #   
#                    "MASS"))                                #
#------------------------------------------------------------#

# Listing 7.1 Descriptive statistics with summary()
myvars <- c("mpg", "hp", "wt")
summary(mtcars[myvars])

# Listing 7.2 Descriptive statistics via sapply()
mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, stdev=s, 
           skew=skew, kurtosis=kurt))
}

myvars <- c("mpg", "hp", "wt")
sapply(mtcars[myvars], mystats)

# Listing 7.3 Descriptive statistics via describe() in the Hmisc package
library(Hmisc) # 一个 混合工具包，支持很多杂七杂八的计算、绘图等函数
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])

# Listing 7.4 Descriptive statistics via stat.desc() in the pastecs package 
library(pastecs)
myvars <- c("mpg", "hp", "wt")
stat.desc(mtcars[myvars])

# Listing 7.5 Descriptive statistics via describe() in the psych package
library(psych)  # 一个心理学软件包，支持多种描述性统计
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])

# Listing 7.6 Descriptive statistics by group using by()
dstats <- function(x)sapply(x, mystats)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], mtcars$am, dstats)

# Listing 7.7 Descriptive statistics for groups defined by multiple variables
dstats <- function(x)sapply(x, mystats, na.omit=TRUE)
myvars <- c("mpg", "hp", "wt")
by(mtcars[myvars], 
   list(Transmission=mtcars$am,
        Engine=mtcars$vs), 
   FUN=dstats)

# Section 7.1.4
# Summarizing data interactively with dplyr

library(dplyr) # 一个对 tibble 表进行数据管理和描述性统计的包，特点式支持 %>% pipe 运算符
library(carData)
Salaries %>%
  summarize(med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  summarize(n = length(salary),
            med = median(salary), 
            min = min(salary), 
            max = max(salary))

Salaries %>%
  group_by(rank, sex) %>%
  select(yrs.service, yrs.since.phd) %>%
  summarize_all(mean)

# Section 7.2 
# Frequency tables
library(vcd)  # 一个关于离散型变量的描述性和可视化统计包
head(Arthritis)

# one way table
mytable <- table(Arthritis$Improved)  # table 支持简单的单（多）路列联表计数，来自 base 库
mytable                       # counts 计数
prop.table(mytable)           # proportions 占比，来自 base 库
prop.table(mytable)*100       # percents 百分比

# two way table
mytable <- xtabs(~Treatment+Improved, data=Arthritis) # xtabs 支持用公式指定列联表的形式，支持更多统计，输出为 table 或 xtabs 的 S3 array 或 dgTMatrix 的 S4 类，来自 stats 库
mytable  # counts

mytable01 <- table(Arthritis$Treatment,Arthritis$Improved)  # 此处用 table 得到的双路列联表与上面的等效
mytable01  # counts

# 注意： table 和 xtab 输出的列联表均为 

margin.table(mytable, 1)    # 按边缘进行计数，来自 base 库
prop.table(mytable, 1)      # 按边缘计算占比，来自 base 库
margin.table(mytable, 2)    # total counts for Improved
prop.table(mytable, 2)      # column proportions (columns add to 1)

prop.table(mytable)         # 计算全表占比cell proportions (all cells add to 1)


mytable <- xtabs(~Treatment+Improved, data=Arthritis) # xtabs 支持用公式指定列联表的形式，支持更多统计，来自 stats 库
mytable  # counts
addmargins(mytable)         # 为计数表显示边缘汇总列，来自 stats 库 cell counts with row and column sums
addmargins(prop.table(mytable)) #为占比表显示边缘汇总总列， cell proportions with row and column proportions

addmargins(prop.table(mytable, 1), 2) # 为计数表显示边缘汇总行， row proportions with row sums
addmargins(prop.table(mytable, 2), 1) #  为占比表显示边缘汇总行， column proportions with column sums

# Listing 7.8 Two-way table using CrossTable
library(gmodels)    # 用于拟合的软件包，如：计算交叉表、按照分组进行拟合、计算估计的置信区间、
CrossTable(Arthritis$Treatment, Arthritis$Improved) # 可以用于生成更为复杂的列联表（交叉表），来自 gmodels 包

# Listing 7.9 Three-way contingency table
mytable <- xtabs(~ Treatment+Sex+Improved, data=Arthritis) # 生成三列联表
mytable          
margin.table(mytable, 1)  # totals for Treatment
margin.table(mytable, 2)  # totals for Sex
margin.table(mytable, 3)  # totals for Improved
margin.table(mytable, c(1, 3)) # totals for Treatment by Improved

# Treatment by Sex for each Level of Improved
ftable(mytable)  # 生成三列联表的紧凑形式，输出为 ftable 对象 
ftable(prop.table(mytable, c(1, 2))) # proportions sum to 1 over index omitted
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) 
ftable(addmargins(prop.table(mytable, c(1, 2)), 3)) * 100

##############################################################
### 小结：生成列联表的方式主要有 三种
### table 函数： base 包提供，输出是一个 xtabs/table 数组（非稀疏情况） 
### xtabs 函数： stats 包提供，输出是一个 xtabs/table 数组（非稀疏情况）
### ftable 函数： stats 包提供，输出是一个 ftable 对象
### crosstable 函数： gmodels 包提供，输出是一个 list
#########################################################3###


##############################################################
### 离散型变量的相关性：检验与量化指标
#########################################################3###

# Listing 7.10 Chi-square test of independence
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)          
chisq.test(mytable) # 两个随机变量之间的卡方独立性检验，原假设为两者独立

mytable <- xtabs(~Improved+Sex, data=Arthritis)              
chisq.test(mytable) 

# Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable) # 两个随机变量之间的 Fisher 独立性检验，原假设为两者独立

# Chochran-Mantel-Haenszel test
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable) # 在指定第三个变量的条件下，两个随机变量之间的卡方条件独立性检验，原假设为三者之间不存在交互


##############################################################
### 小结：三种离散型随机变量之间独立性检验的方法
### chisq.test 函数：卡方独立性检验函数，stats 包提供 
### fisher.test 函数： Fisher 独立性检验函数，stats 包提供
### mantelhaen.test 函数：三方的 Cochran-Mantel-Haenszel 卡方检验， stats 包提供
#########################################################3###

# Listing 7.11 Measures of association for a two-way table
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable) # 前面几种方法主要用于检验是否独立，如果不独立的话，我们进一步会关心两个随机变量之间的相关性到底如何，此函数给出了三种量化指标
# Kappa(mytable)


##############################################################
### 小结：离散型随机变量相关性的量化方法
### assocstats 函数：两个变量之间相关性大小的量化指标，vcd 包提供 
### vcd::Kappa 函数：输入为混淆矩阵，输出为 Cohen's Kappa 一致性指标，vcd 包提供
#########################################################3###


##############################################################
### 连续型变量的相关性及其量化
#########################################################3###

## 相关性大小（协方差、相关系数、偏相关系数）的量化计算
# Listing 7.12 Covariances and correlations
states<- state.x77[,1:6]
cov(states) # 协方差计算函数（绝对量）, 来自 stats 包
cor(states) # 相关系数计算函数（相对量），来自 stats 包
cor(states, method="spearman") # 指定哪种相关系数指标

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

# partial correlations
library(ggm)
colnames(states)
pcor(c(1,5,2,3,6), cov(states))

## 相关性大小的显著性检验
## 原假设： 随机变量之间不存在相关性（即相关系数为 0）
## cor.test 函数： 检验两个随机变量之间的相关显著性
## corr.test 函数： 计算并检验多个随机变量之间的相关显著性
## pcor.test 函数： 
## r.test 函数：

# Listing 7.13 Testing a correlation coefficient for significance
cor.test(states[,3], states[,5])

# Listing 7.14 Correlation matrix and tests of significance via corr.test()
library(psych)
corr.test(states, use="complete")


##############################################################
### 注意：
### 不同类型随机变量之间的相关性及其量化： 参见 polycor 软件包
### 该包中的函数可以计算异构随机变量之间的相关矩阵，包括：
### 数值变量与数值变量、数值变量与定序变量、定序变量与定序变量、
### 名义变量等之间的相关性。
#########################################################3###

##############################################################
### 组间差异的检验
##############################################################


##############################################################
### 假设组间没有差异，来自同一总体，且该总体分布可以参数化
### 典型如：总体来自正态分布，此时可以采用 T-检验
##############################################################

# t-tests
### 原假设： 两个组之间相互独立，且来自均值相等的正态总体（方差可能不同）
library(MASS)
t.test(Prob ~ So, data=UScrime)  ## 按照 So 进行分组检验

### 原假设： 两个组别之间不存在差异（即组间差异呈标准正态，差的均值为 0 ）

sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))

##############################################################
## 当存在多个组别时，需要使用 方差分析 ANOVA
## 见 第 9 章
##############################################################

##############################################################
### 组间差异的非参数检验： U-检验
### 假设组间来自同一分布，但该分布并不存在参数化形式
##############################################################

## 两个组之间的的分析
# Mann-Whitney U-test
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)
sapply(UScrime[c("U1","U2")], median)
with(UScrime, wilcox.test(U1, U2, paired=TRUE))

## 多个组之间的分析
# Kruskal-Wallis test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

# Listing 7.15 Nonparametric multiple comparisons
source("https://rkabacoff.com/RiA/wmc.R")              
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data=states, method="holm")

######################################################################
### 总结
### 1、连续型变量：采用描述性统计来汇总，R 提供了多个工具包
### 2、类比型变量： 采用频率表或列联表来描述，
### 3、连续型变量的组间检验： t 检验（正态分布）、U 检验（非参数化分布）
### 4、连续型变量的相关性检验：相关系数
### 5、类别型变量的相关性检验：卡方检验
### 6、上述描述性信息结合图形化表示才有说服力
######################################################################



# install.packages("polycor")
