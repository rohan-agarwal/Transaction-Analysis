#------------------------PART 1: INITIALIZATION--------------------------

#importing sku info
skuinfo<-read.csv("skuinfo.csv")
names(skuinfo)<-c("sku", "dept", "classid", 
                  "upc", "style", "color", 
                  "size", "packsize", "vendor", "brand")
skuList<-data.frame(skuinfo$sku)

#input store info
strinfo<-read.csv("strinfo.csv", header = TRUE)
names(strinfo)<-c("Store","City","State","ZIP","?")

#number of stores to sample per state - 40 stores used in analysis
#suggested to run code with a small sample (5-10 stores)
ratio<-(round(sort(prop.table(table(strinfo$State)))*40))

#formatting
ratio<-data.frame(names(ratio),as.numeric(ratio))
names(ratio)<-c("state","count")
ratio<-ratio[ratio$count>0,]

#selecting list of stores with transactions
library(sqldf)
conn<-dbConnect(SQLite(), dbname="trnsact.db")
storeList<-dbGetQuery(conn = conn,
                    "SELECT store,COUNT(*) FROM trnsact GROUP BY store")
dbDisconnect(conn)
detach(package:sqldf)

#fomatting
storeList<-storeList$store

#printing list of stores to be hardcoded into next query
cat(storeList,sep=",")

#running a query to select all stores in strinfo
  #which have transactions in trnsact
#shop numbers have been hardcoded
library(sqldf)
strinfo<-sqldf("SELECT * FROM strinfo WHERE Store IN 
               (1002,1003,1004,1007,1009,102,103,107,
               1102,1103,1104,1107,1109,1202,1203,1204,
               1207,1302,1304,1307,1402,1403,1404,1407,
               1502,1509,1602,1607,1609,1702,1703,1704,
               1707,1709,1804,1807,1904,1909,2004,2007,
               2009,202,203,204,209,2102,2103,2104,2109,
               2202,2203,2204,2207,2209,2303,2304,2307,
               2309,2402,2404,2407,2409,2502,2503,2504,
               2509,2602,2603,2604,2607,2609,2702,2704,
               2707,2804,2807,2809,2904,2907,2909,3002,
               3003,3004,3007,3009,302,303,304,307,309,
               3102,3103,3107,3109,3202,3204,3207,3209,
               3302,3303,3304,3307,3309,3402,3403,3407,
               3409,3502,3503,3504,3507,3509,3602,3603,
               3604,3607,3609,3702,3704,3707,3709,3802,
               3804,3807,3809,3902,3909,4003,4004,4007,
               402,403,404,407,409,4102,4103,4104,4107,
               4109,4202,4203,4204,4207,4209,4302,4303,
               4307,4309,4402,4403,4404,4407,4409,4502,
               4503,4504,4507,4603,4604,4607,4702,4703,
               4704,4707,4802,4803,4804,4807,4902,4903,
               4904,4907,5002,5004,502,503,504,507,509,
               5102,5103,5104,5202,5203,5204,5302,5303,
               5304,5402,5403,5404,5502,5503,5504,5602,
               5603,5604,5703,5704,5802,5803,5804,5903,
               5904,6002,6004,6009,603,604,609,6102,6104,
               6109,6202,6203,6204,6209,6302,6304,6402,6403,
               6404,6503,6604,6703,6704,6803,6804,6902,6903,
               6904,7002,7003,7004,7007,703,704,707,709,7102,
               7103,7104,7107,7202,7203,7204,7302,7303,7304,
               7307,7402,7403,7404,7407,7502,7503,7507,7602,
               7603,7604,7702,7703,7704,7707,7804,7807,7902,
               7904,7907,8002,8004,8007,802,807,809,8102,8109,
               8202,8204,8209,8302,8304,8309,8402,8403,8404,
               8407,8409,8503,8504,8507,8602,8603,8604,8607,
               8609,8702,8703,8707,8709,8802,8803,8902,9002,
               902,903,907,909,9103,9104,9202,9204,9209,9302,
               9303,9304,9309,9402,9404,9409,9503,9504,9603,
               9604,9609,9704,9709,9804,9806,9906,9909)")
detach(package:sqldf)

#generating store numbers based on state
stores<-vector()
x <- 1;
for (i in 1:nrow(ratio)) {
  df<-strinfo[strinfo$State==ratio[i,1],]
  for (j in 1:ratio[i,2]) {
    stores[x] <- df[sample(nrow(df), 1),1]
    x = x+1
  }
}
#printing store numbers to console
cat(stores,sep=",")
#output will be copied and hardcoded into queries

#------------------------PART 2: FUNCTIONS--------------------------

###selecting a subset of the data
library(sqldf)
conn<-dbConnect(SQLite(), dbname="trnsact.db")
#hardcoded store numbers for sample
##use smaller samples for faster run time
trnsact<-dbGetQuery(conn = conn,
                    "SELECT * FROM trnsact WHERE stype='P' AND store IN 
                    (2209,3109,9202,6009,6204,8004,3902,8209,
                    8703,3403,9704,6402,2804,2004,1403,1403,
                    3204,8702,4804,4604,6703,7003,909,509,204,
                    604,502,3202,3602,5002,7307,3704,2707,2907,
                    4407,8407,8407)")
dbDisconnect(conn)
detach(package:sqldf)

#function to exact transactions for a single store
SingleStoreTransactions<-function(storeNumber) {
  
  #selecting one store from trnsact
  storeData<-trnsact[trnsact$store==storeNumber,]
  return(storeData)
}

#function to generate unique identifier for transactions
FormatTransactions<-function(storeData) {
  
  #creating a variable of unique combinations of trannum/saledate/register
  id = as.integer(interaction(storeData$trannum,
                              storeData$saledate,
                              storeData$register))
  
  #creating a new data frame with id and sku
  trans<-data.frame(id,storeData$sku)
  names(trans)<-c("id","sku")
  return(trans)
}

#function to execute apriori algorithm
Apriori<-function(trans) {
  write.csv(trans,"trans.csv", row.names = FALSE)
  #overriding the table as a transaction object
  library(arules)
  trans <- read.transactions("trans.csv", 
                             format = "single", 
                             sep = ",", 
                             cols = c("id","sku"), 
                             rm.duplicate = TRUE)
  #performing apriori with parameters of support = .0001 and confidence = .0001
  rules<-apriori(trans, parameter=list(support=0.0001, 
                                       confidence = 0.0001, 
                                       minlen=2))
  
  #extracting rules as data frame
  rulesDF<-as(rules, "data.frame")
  detach(package:arules)
  #string splitting - removing {} and =>
  ruleSKU<-data.frame(do.call('rbind',
                              strsplit(as.character(rulesDF$rules),
                                       " ",fixed=TRUE)))
  rulesDF<-cbind(ruleSKU[,1],ruleSKU[,3],rulesDF[,2:4])
  names(rulesDF)[1:2]<-c("ruleLHS","ruleRHS")
  return(rulesDF)
}

#function to rules further
FormatRules<-function(rulesDF,storeData) {
  
  #string manipulation
  rulesDF$ruleLHS<-substr(rulesDF$ruleLHS,2,1000)
  rulesDF$ruleRHS<-substr(rulesDF$ruleRHS,2,1000)
  for (i in 1:nrow(rulesDF)) {
    x <- rulesDF[i,1]
    y <- rulesDF[i,2]
    rulesDF[i,1] <- substr(x, 1, nchar(x)-1)
    rulesDF[i,2] <- substr(y, 1, nchar(y)-1)
  }
  
  #selecting rules with valid SKUs
  library(sqldf)
  rulesDF<-sqldf("SELECT * FROM rulesDF 
                 WHERE ruleRHS in skuList 
                 AND ruleLHS in skuList")
  detach(package:sqldf)
  
  return(rulesDF)
}

#function to select the departments for items in rules
GetDepartments<-function(rulesDF) {
  
  departmentRight<-vector()
  departmentLeft<-vector()
  
  #collecting departments
  for (i in 1:nrow(rulesDF))  {
    x<-skuinfo$sku==rulesDF[i,2]
    departmentRight[i] <- skuinfo[x,2]
    departmentLeft[i] <- skuinfo[x,2]
  }
  
  #merging with rulesDF
  rulesDF<-cbind(rulesDF,
                 departmentLeft,
                 departmentRight)
  
  return(rulesDF)
}

#function to get price for RHS of rule
GetPrices<-function(rulesDF,storeData) {
  
  skuPrices<-vector()
  
  #collecting prices
  for (i in 1:nrow(rulesDF)) {
    skuPrices[i] <- round(mean(as.numeric(
      storeData[storeData$sku==rulesDF[i,2],10])))
  }
  
  #merging to rulesDF
  rulesDF<-cbind(rulesDF,
                 price=skuPrices)
  return(rulesDF)
}

#function to get the final set of rules
GetFinalRules<-function(rulesDF,numRules,store) {
  
  #creating benefit metric, named metric
  rulesDF<-cbind(rulesDF,metric=rulesDF$support*rulesDF$price,store)
  
  #binding to rulesDF and renaming for clarity
  finalRules<-rulesDF[order(rulesDF$metric,
                            decreasing=TRUE),][1:numRules,]
  return(finalRules)
}

#------------------------PART 3: EXECUTION--------------------------

#creating master set of rules across stores
ruleSet<-data.frame(ruleLHS=character(),
                    ruleRHS=character(),
                    support=numeric(),
                    confidence=numeric(),
                    lift=numeric(),
                    departmentRight=character(),
                    departmentLeft=character(),
                    price=numeric(),
                    metric=numeric(),
                    store=character())

#executing all functions across sample of stores
for (i in 1:length(stores)) {
  
  storeData<-SingleStoreTransactions(stores[i])
  trans<-FormatTransactions(storeData)
  rulesDF<-Apriori(trans)
  rulesDF<-FormatRules(rulesDF,storeData)
  rulesDF<-GetDepartments(rulesDF)
  rulesDF<-GetPrices(rulesDF,storeData)
  #note that all rules from a store are selected here
  #but a subset can also be selected by entering different parameters
  finalRules<-GetFinalRules(rulesDF,nrow(rulesDF),stores[i])
  ruleSet<-rbind(ruleSet,finalRules)
}

#plotting all rules
library(ggplot2)
qplot(ruleSet$confidence,
      ruleSet$metric,
      color=ruleSet$lift,
      xlab="Confidence",
      ylab="Benefit")
detach(package:ggplot2)

#selecting top 100 rules by ordering on metric (benefit)
topRules<-ruleSet[order(ruleSet$metric, decreasing=TRUE),][1:100,]

#plotting top 100 rules
library(ggplot2)
qplot(topRules$confidence,
      topRules$metric,
      color=topRules$lift,
      xlab="Confidence",
      ylab="Benefit")
detach(package:ggplot2)

#writing top rules to CSV
write.csv(topRules,file="topRules.csv")