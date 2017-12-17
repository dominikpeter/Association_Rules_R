
rm(list = ls())

library(tidyverse)
library(Matrix)
library(arules)
library(readxl)


# "http://pbpython.com/market-basket-analysis.html"

df <- read_excel("Data/Online Retail.xlsx")

df <- df %>% 
  mutate(InvoiceNo = InvoiceNo %>% as.factor,
         Description = Description %>% as.factor,
         Quantity = ifelse(is.na(Quantity), 0, Quantity)) %>% 
  tidyr::drop_na()


g <- sparseMatrix(i = df$Description %>% as.integer(),
             j = df$InvoiceNo %>% as.integer(),
             x = df$Quantity)

colnames(g) = levels(df$InvoiceNo)
rownames(g) = levels(df$Description)

m <- as(g, "ngCMatrix")
tr <- as(m, "transactions")



summary(tr)

itemsets <- apriori(tr, parameter = list(target = "rules",
                                            supp=0.0001, minlen = 2, maxlen=3))




quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = itemsets)


clean_rule <- function(x){
  x %>% str_replace_all("\\{|\\}", "")
}


rulesdf = data.frame(
  lhs = labels(lhs(itemsets)),
  rhs = labels(rhs(itemsets)), 
  itemsets@quality) %>% 
  mutate(antecedants = lhs %>% clean_rule,
         consequents = rhs %>% clean_rule)
# %>% 
#   separate(col=antecedants,into=paste("antecedants ", 1:3), sep = ",",extra="merge")







