
#Created by: Akbar maulana

library(arules)
library(arulesViz)
library(Matrix)
data=read.csv(file.choose(),header=TRUE,sep=";")
head(data)

#Data Transaksi
trans=as(split(data[,3],data[,7]),"transactions")
trans

#Item terbanyak
win.graph()
itemFrequencyPlot(trans,type="absolute",topN=10)
itemFrequencyPlot(trans,type="relative",topN=10)

#Rule
summary(trans)
rule=apriori(trans,parameter=list(supp=0.05,conf=0.2,minlen=1))
rule
rule2=apriori(trans,parameter=list(supp=0.04,conf=0.2,minlen=1))
rule2
inspect(sort(rule2))

#Lift Rasio terbesar
crossTable(trans,measure="lift",sort=TRUE)[1:5,1:5]
win.graph()
plot(sort(rule2,by="lift"),method="graph",control=list(type="items"))

#Rule dengan barang terlaku
rule3=apriori(trans,parameter=list(supp=0.04,conf=0.2,minlen=2),appearance =list(default="rhs",lhs="REGENCY CAKESTAND 3 TIER"))
rule3
inspect(sort(rule3))
plot(sort(rule3,by="lift"),method="graph",control=list(type="items"))
plot(rule,interactive = TRUE)
rule4=apriori(trans,parameter=list(supp=0.04,conf=0.2,minlen=2),appearance =list(default="rhs",lhs="WHITE HANGING HEART T-LIGHT HOLDER"))
rule4
inspect(sort(rule4))
win.graph()
plot(sort(rule4,by="lift"),method="graph",control=list(type="items"))
plot(rule4,interactive = TRUE)
