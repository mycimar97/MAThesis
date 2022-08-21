###Generic Setup
install.packages("readxl")
install.packages("rmarkdown")
install.packages("tinytex")
install.packages("icr")
install.packages('Rcpp')
library(readxl)
library(tinytex)
library(ggplot2)
library(icr)
library(Rcpp)
setwd("D:/Drive/Uni/Masterarbeit/Replication Package")

###Calculation of Krippalpha
codes <- read.csv("coding_raw.csv",header=TRUE,sep=";")
codes_trans = t(codes)
krippalpha(codes_trans)
###Frequent itemset analysis
library(arules)


data <- read.csv("log_transaction.csv", header=F, sep=";", na.strings=c("","NA"))
data$V1 <- NULL
list2env(setNames(split(as.matrix(data),
                        row(data)), paste0("Row",1:25)),
         envir=.GlobalEnv)
market_basket <- list(Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8, Row9, Row10, Row11, Row12, Row13, Row14, Row15, Row16, Row17, Row18, Row19, Row20, Row21, Row22, Row23, Row24, Row25)
names(market_basket) <- paste("T", c(1:25), sep = "")
market_basket
trans <- as(market_basket, "transactions")
dim(trans)
itemLabels(trans)
summary(trans)
image(trans)
rules <- apriori(trans, 
                 parameter = list(supp=0.2, conf=0.7, 
                                  maxlen=10, minlen=2,
                                  target= "rules"))

itemsets <- apriori(trans,
                 parameter = list(supp=0.2, conf=0.7, 
                                  maxlen=10, minlen=2, 
                                  target= "maximally frequent itemsets"))
inspect(itemsets)

is.generator(itemsets)


summary(rules)
inspect(rules)
is.closed(itemsets)
write.csv(inspect(itemsets), "maximally_frequent.csv")