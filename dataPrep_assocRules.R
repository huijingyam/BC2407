library(data.table)
library(arules)
library(arulesViz)
library("reshape2")
library(dplyr)
library(tidyr)

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

############## Generating rules at support 0.1: 72,422,843 rules
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

#Filter to find key rules
##Filtering to find rules with 1 item in antecedent. Then  rules with a true->b true, a false->b false
##21,952 rules have 1 item in their antecedent
rules3.singleante.df=rules3.clean.df[!grepl(",", rules3.clean.df[["rules"]]), ]
rules3.singleante2.df<-rules3.singleante.df %>% separate(rules, c("antecedent", "consequent"), " => ")

#Formating the dataframe to atomize the rules to antecedent and consequent
##Formating Antecedent
rules3.singleante3.df<-rules3.singleante2.df[(grepl("TRUE", rules3.singleante2.df[["antecedent"]]) & grepl("TRUE", rules3.singleante2.df[["consequent"]])) | (grepl("FALSE", rules3.singleante2.df[["antecedent"]]) & grepl("FALSE", rules3.singleante2.df[["consequent"]])), ]
rules3.singleante3.df<-rules3.singleante3.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules3.singleante3.df$antecedent=gsub("\\{","",rules3.singleante3.df$antecedent)
rules3.singleante3.df$antecedent_logic=gsub("\\}","",rules3.singleante3.df$antecedent_logic)
##Formating Consequent
rules3.singleante3.df<-rules3.singleante3.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules3.singleante3.df$consequent=gsub("\\{","",rules3.singleante3.df$consequent)
rules3.singleante3.df$consequent_logic=gsub("\\}","",rules3.singleante3.df$consequent_logic)


#Filtering to find rules with only true -> true or false->false
key_rules1<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules3.singleante3.df$antecedent)){
  tmp.df<-rules3.singleante3.df[rules3.singleante3.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules1 <- rbind(key_rules1, rules3.singleante3.df[(grepl(tmp.df[row_no,"antecedent"],rules3.singleante3.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules3.singleante3.df[["consequent"]])),])
    }
  }
}
key_rules1
#Can be used to verify the values
#rules3.singleante3.df[(grepl("yellowing_of_eyes",rules3.singleante3.df[["antecedent"]]) & grepl("loss_of_appetite",rules3.singleante3.df[["consequent"]])),]

write.csv(key_rules1,"key_rules1.csv", row.names = FALSE)

############### Generating rules at support 0.1 and conf 0.9: 56,571,092 rules
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

#Filter to find key rules
##Filtering to find rules with 1 item in antecedent. Then  rules with a true->b true, a false->b false
##16,749 rules have 1 item in their antecedent
rules4.singleante.df=rules4.clean.df[!grepl(",", rules4.clean.df[["rules"]]), ]
rules4.singleante2.df<-rules4.singleante.df %>% separate(rules, c("antecedent", "consequent"), " => ")

#Formating the dataframe to atomize the rules to antecedent and consequent
##Formating Antecedent
rules4.singleante3.df<-rules4.singleante2.df[(grepl("TRUE", rules4.singleante2.df[["antecedent"]]) & grepl("TRUE", rules4.singleante2.df[["consequent"]])) | (grepl("FALSE", rules4.singleante2.df[["antecedent"]]) & grepl("FALSE", rules4.singleante2.df[["consequent"]])), ]
rules4.singleante3.df<-rules4.singleante3.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules4.singleante3.df$antecedent=gsub("\\{","",rules4.singleante3.df$antecedent)
rules4.singleante3.df$antecedent_logic=gsub("\\}","",rules4.singleante3.df$antecedent_logic)
##Formating Consequent
rules4.singleante3.df<-rules4.singleante3.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules4.singleante3.df$consequent=gsub("\\{","",rules4.singleante3.df$consequent)
rules4.singleante3.df$consequent_logic=gsub("\\}","",rules4.singleante3.df$consequent_logic)


#Filtering to find rules with only true -> true or false->false
key_rules2<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules4.singleante3.df$antecedent)){
  tmp.df<-rules4.singleante3.df[rules4.singleante3.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules2 <- rbind(key_rules2, rules4.singleante3.df[(grepl(tmp.df[row_no,"antecedent"],rules4.singleante3.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules4.singleante3.df[["consequent"]])),])
    }
  }
}
key_rules2
#Can be used to verify the values
#rules4.singleante3.df[(grepl("yellowing_of_eyes",rules4.singleante3.df[["antecedent"]]) & grepl("loss_of_appetite",rules4.singleante3.df[["consequent"]])),]

#---------------Same as rules3, but limit maxlen of rules to 2-----------------
# 21952 rules 
rules5 = apriori(data = symptomsTransFac, parameter = 
                   list(minlen = 2, maxlen = 2, supp=0.1, conf = 0.1, target = "rules"))
summary(rules5)
inspect(head(rules5, n = 10, by ="lift"))

# No redundant rules by definition since no subset is possible
sum(is.redundant(rules5))

rules5.df = as(rules5, "data.frame")

# Breaks the rules column into antecedent and consequent columns
rules5.singleante.df<-rules5.df %>% separate(rules, c("antecedent", "consequent"), " => ")

rules5.singleante2.df<-rules5.singleante.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules5.singleante2.df$antecedent=gsub("\\{","",rules5.singleante2.df$antecedent)
rules5.singleante2.df$antecedent_logic=gsub("\\}","",rules5.singleante2.df$antecedent_logic)

rules5.singleante2.df<-rules5.singleante2.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules5.singleante2.df$consequent=gsub("\\{","",rules5.singleante2.df$consequent)
rules5.singleante2.df$consequent_logic=gsub("\\}","",rules5.singleante2.df$consequent_logic)

#Filtering to find rules with only true -> true or false->false
key_rules3<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules5.singleante2.df$antecedent)){
  tmp.df<-rules5.singleante2.df[rules5.singleante2.df$antecedent==i,]
  # dont understand
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules3 <- rbind(key_rules3, rules5.singleante2.df[(grepl(tmp.df[row_no,"antecedent"],rules5.singleante2.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules5.singleante2.df[["consequent"]])),])
    }
  }
}
key_rules3
#Can be used to verify the values
#rules5.singleante2.df[(grepl("yellowing_of_eyes",rules5.singleante2.df[["antecedent"]]) & grepl("loss_of_appetite",rules5.singleante2.df[["consequent"]])),]

write.csv(key_rules3,"key_rules3.csv", row.names = FALSE)


#---------support 0.1, confidence 0.5-----
rules6 = apriori(data = symptomsTransFac, parameter = 
                   list(minlen = 2, maxlen = 2, supp=0.1, conf = 0.5, target = "rules"))
summary(rules6)

rules6.df = as(rules6, "data.frame")

# Breaks the rules column into antecedent and consequent columns
rules6.singleante.df<-rules6.df %>% separate(rules, c("antecedent", "consequent"), " => ")

rules6.singleante2.df<-rules6.singleante.df %>% separate(antecedent, c("antecedent", "antecedent_logic"), "=")
rules6.singleante2.df$antecedent=gsub("\\{","",rules6.singleante2.df$antecedent)
rules6.singleante2.df$antecedent_logic=gsub("\\}","",rules6.singleante2.df$antecedent_logic)

rules6.singleante2.df<-rules6.singleante2.df %>% separate(consequent, c("consequent", "consequent_logic"), "=")
rules6.singleante2.df$consequent=gsub("\\{","",rules6.singleante2.df$consequent)
rules6.singleante2.df$consequent_logic=gsub("\\}","",rules6.singleante2.df$consequent_logic)

rules6.singleante2.df = rules6.singleante2.df[(grepl("1", rules6.singleante2.df[["antecedent_logic"]]) & grepl("1", rules6.singleante2.df[["consequent_logic"]])) | (grepl("0", rules6.singleante2.df[["antecedent_logic"]]) & grepl("0", rules6.singleante2.df[["consequent_logic"]])), ]

key_rules5<-data.frame(matrix(ncol=9,nrow=0, dimnames=list(NULL, c("antecedent", "antecedent_logic", "consequent","consequent_logic","support","coverage","confidence","lift","count"))))
for (i in unique(rules6.singleante2.df$antecedent)){
  tmp.df<-rules6.singleante2.df[rules6.singleante2.df$antecedent==i,]
  tmp.df<-subset(tmp.df,duplicated(consequent))
  if (nrow(tmp.df)!=0){
    for (row_no in 1:nrow(tmp.df)){
      key_rules5 <- rbind(key_rules5, rules6.singleante2.df[(grepl(tmp.df[row_no,"antecedent"],rules6.singleante2.df[["antecedent"]]) & grepl(tmp.df[row_no,"consequent"],rules6.singleante2.df[["consequent"]])),])
    }
  }
}
key_rules5

write.csv(key_rules5,"key_rules5.csv", row.names = FALSE)
write.csv(rules6.singleante2.df, "rules6.singleante2.df.csv", row.names = FALSE)


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

#----------helpful references------------#
##How to filter and subset dataframe based on certain values in columns
###https://stackoverflow.com/questions/40032674/filter-subset-if-a-string-contains-certain-characters-in-r
####rules4.singleante3.df<-rules4.singleante2.df[(grepl("TRUE", rules4.singleante2.df[["antecedent"]]) & grepl("TRUE", rules4.singleante2.df[["consequent"]])) | (grepl("FALSE", rules4.singleante2.df[["antecedent"]]) & grepl("FALSE", rules4.singleante2.df[["consequent"]])), ]


##Add a new row to a dataframe
###https://stackoverflow.com/questions/28467068/how-to-add-a-row-to-a-data-frame-in-r
#####rules4.singleante3.df[nrow(rules4.singleante3.df) + 1,] = c("malaise","TRUE","runny_nose","TRUE",0.1353659,0.1353659,0.1353659,0.1353659,666)

##Adding one data frame to the end of another data frame in R
###https://stackoverflow.com/questions/10358680/adding-a-one-dataframe-to-the-end-of-another-data-frame-in-r

## Create empty data frame with column names by assigning a string vector
### https://stackoverflow.com/questions/32712301/create-empty-data-frame-with-column-names-by-assigning-a-string-vector

##Export data.frame to csv file
###https://datatofish.com/export-dataframe-to-csv-in-r/
