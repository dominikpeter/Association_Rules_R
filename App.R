
rm(list = ls())

library(data.table)
library(magrittr)
library(Matrix)
library(arules)
library(stringr)
library(readxl)

# "http://pbpython.com/market-basket-analysis.html"

DT <- read_excel("Data/Online Retail.xlsx") %>%
  as.data.table()

DT <- DT[, `:=` (InvoiceNo = InvoiceNo %>% as.factor,
                 Description = Description %>% as.factor,
                 Quantity = ifelse(is.na(Quantity), 0, Quantity))] %>% 
  na.omit()

g <- sparseMatrix(i = DT$Description %>% as.integer(),
                  j = DT$InvoiceNo %>% as.integer(),
                  x = DT$Quantity)

colnames(g) = levels(DT$InvoiceNo)
rownames(g) = levels(DT$Description)

itemsets <- as(g, "ngCMatrix") %>% 
  as("transactions") %>% 
  apriori(parameter = list(target = "rules",
                           support=0.0001,
                           confidence=0.5,
                           minlen = 2, maxlen=2))

g <- NULL
DT <- NULL

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = itemsets)

clean_rule <- function(x){
  x %>% str_replace_all("\\{|\\}", "")
}


DT <- data.table(
  lhs = labels(lhs(itemsets)),
  rhs = labels(rhs(itemsets)), 
  itemsets@quality)

DT <- DT[,`:=` (antecedants = lhs %>% clean_rule,
                consequents = rhs %>% clean_rule)] %>% 
  .[, .(antecedants, consequents, support, confidence, lift)]


DT %>% setkey(antecedants)
DT[, rank := frank(-lift, ties.method = "dense"), by = key(DT)]




