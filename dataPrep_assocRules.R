# Set working directory
setwd("C:/Users/tanyl/OneDrive - Nanyang Technological University/Desktop/BC2407/BC2407")

library(data.table)
library(arules)
library(arulesViz)
library("reshape2")

data = read.csv("training.csv")

# Separate diagnosis and symptoms - only looking for associations between symptoms
diagnosis = subset(data, select = prognosis)
symptoms = subset(data, select = -c(prognosis, X))
symptoms = data.frame(lapply(symptoms, as.logical))

# Convert to transactions datatype
symptomsTrans <- as(symptoms, "transactions")  
inspect(symptomsTrans)


# Generating rules at support 0.1: 131 rules
rules1 <- apriori(data = symptomsTrans, parameter = 
                    list(minlen = 2, supp=0.1, conf = 0.1, target = "rules"))
summary(rules1)
inspect(head(rules1, n = 10, by ="lift"))
rule.table1 <- inspect(rules1)

# cleaning redundant rules from rules1
# 102 left, minimum count 510
sum(!is.redundant(rules1))
rules1.clean = rules1[!is.redundant(rules1)]
rules1.clean.df = as(rules1.clean, "data.frame")
inspect(head(rules1.clean, n = 10, decreasing = FALSE, by ="count"))


# Generating rules at support 0.01: 1630161 rules
# minimum count is 66
rules2 <- apriori(data = symptomsTrans, parameter = 
                    list(minlen = 2, supp=0.01, conf = 0.1, target = "rules"))
rule.table2 <- inspect(rules1)
inspect(head(rules2, n = 10, decreasing = TRUE, by ="lift"))

# 10227 not redundant rules, min count 78
sum(is.redundant(rules2))
rules2.clean = rules2[!is.redundant(rules2)]
rules2.clean.df = as(rules2.clean, "data.frame")
inspect(head(rules2.clean, n = 10, decreasing = FALSE, by ="count"))
