
rm(list = ls())

library(tidyverse)
library(Matrix)
library(arules)
library(readxl)

df <- read_excel("Data/Online Retail.xlsx")
data("Adult")

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

itemsets <- apriori(tr, parameter = list(target = "frequent",
                                            supp=0.001, minlen = 2, maxlen=1000))



itemsets
inspect(head(sort(itemsets), n=10))

quality(itemsets)$lift <- interestMeasure(itemsets, measure="lift", trans = tr)

library(arulesViz)
plot(head(sort(itemsets, by = "lift"), n=50), method = "graph", control=list(cex=.8))




r <- apriori(tr, parameter = list(supp=0.001, maxlen=400))


inspect(head(sort(r, by="lift"), n=10))




lhs(r)


plot(r)
plot(head(sort(r, by="lift"), 50),
     method="graph", control=list(cex=.7))





















