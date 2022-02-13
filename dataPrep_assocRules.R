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
symptomsTrans.df<-as(symptomsTrans,"data.frame")
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

# Too big, takes a very long time to run. Refer to cleaned df rules2.clean.df below instead
# rule.table2 <- inspect(rules2)
inspect(head(rules2, n = 10, decreasing = TRUE, by ="lift"))


# 10227 not redundant rules, min count 78
sum(is.redundant(rules2))
rules2.clean = rules2[!is.redundant(rules2)]
rules2.clean.df = as(rules2.clean, "data.frame")
inspect(head(rules2.clean, n = 10, decreasing = FALSE, by ="count"))

#-----Trying with wide data format with factor variables
symptomsFac<-data.frame(symptoms)
symptomsFac[] <- lapply(symptomsFac, factor)
symptomsTransFac <- as(symptomsFac, "transactions")  
symptomsTransFac.df<-as(symptomsTransFac,"data.frame")
inspect(symptomsTransFac)
# Generating rules at support 0.1: 72,422,843 rules
rules3 <- apriori(data = symptomsTransFac, parameter = 
                    list(minlen = 2, supp=0.1, conf = 0.1, target = "rules"))
summary(rules3)
inspect(head(rules3, n = 10, by ="lift"))
rule.table3 <- inspect(rules3)

# cleaning redundant rules from rules1
# 2,930,561 unredundant rules
sum(!is.redundant(rules3))
rules3.clean = rules3[!is.redundant(rules3)]
rules3.clean.df = as(rules3.clean, "data.frame")
inspect(head(rules3.clean, n = 10, decreasing = FALSE, by ="count"))

# Generating rules at support 0.1 and conf 0.9: 56,571,092 rules
rules4 <- apriori(data = symptomsTransFac, parameter = 
                    list(minlen = 2, supp=0.1, conf = 0.9, target = "rules"))
summary(rules4)
inspect(head(rules4, n = 10, by ="lift"))
rule.table4 <- inspect(rules4)

# cleaning redundant rules from rules1
# 71,854 unredundant rules
sum(!is.redundant(rules4))
rules4.clean = rules4[!is.redundant(rules4)]
rules4.clean.df = as(rules4.clean, "data.frame")
inspect(head(rules4.clean, n = 10, decreasing = FALSE, by ="count"))
# churn_rules<-subset(rules, subset=(rhs %pin% 'rolls/buns')& support >= 0.001 & confidence>=0.65)
# inspect(churn_rules)

#--------- arulesViz plots -----------#

#Extract top 20 rules from rules2.clean
top20rules = head(rules2.clean, n = 20, decreasing = TRUE, by ="lift")

# Plot rules in a graphical manner - easier to see clusters
plot(top20rules, method = "graph",  engine = "htmlwidget")

# Sometimes the clusters are not that clear
plot(rules1.clean, method = "graph",  engine = "htmlwidget")

# Grouped matrix plot with arulesViz??
plot(top20rules, method = "grouped")

# arulesViz plots source: https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf

#--------- Clustering the rules -----------#
# documentation: 
# https://cran.r-project.org/web/packages/cluster/cluster.pdf
# https://www.rdocumentation.org/packages/cluster/versions/2.1.2

library(cluster)

# idk what this means copied from: https://stackoverflow.com/questions/51206025/how-can-i-show-exact-association-rules-belong-clusters-with-made-by-pam-method-i
d = dissimilarity(top20rules, method = "Jaccard")
clustering = pam(d, k=8)
summary(clustering)

d = dissimilarity(rules1.clean, method = "Jaccard")
clustering = pam(d, k=8)
summary(clustering)
