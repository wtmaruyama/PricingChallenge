library(sqldf)

# Read data
sales_data <- read.csv('sales.csv', header = TRUE, sep = ",")

#####################
# Summary per PROD  #
#####################
prod_id = c('P1','P2','P3','P4','P5','P6','P7','P8','P9')

for (prod in prod_id) {
  sql<-paste0("SELECT * FROM sales_data WHERE PROD_ID='",prod,"'")
  data <- sqldf(sql)
  print(summary(data))
}

####################################
# REVENUE total per days and Month #
####################################
total_agg_day <- sqldf("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE,strftime('%d', DATE_ORDER) as 'day' FROM sales_data GROUP BY strftime('%d', DATE_ORDER)")
plot(total_agg_day$day,total_agg_day$sum_REVENUE, type = "l",ylab = "Total Revenue",xlab = "Day", main="Total Revenue per day")

total_agg_month <- sqldf("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE,strftime('%m', DATE_ORDER) as 'month' FROM sales_data GROUP BY strftime('%m', DATE_ORDER)")
plot(total_agg_month$month,total_agg_month$sum_REVENUE, type = "l",ylab = "Total Revenue",xlab = "Month", main="Total Revenue per month")

####################
# PRODUCT - BOXPLOT#
####################
prod_id = 'P1'
# POR MES DE UM PRODUTO
p1_sum <- sqldf("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE,DATE_ORDER FROM sales_data WHERE PROD_ID='P1' GROUP BY strftime('%d-%m-%Y', DATE_ORDER)")
# Select PROD ID and create column day
p1_day <- sqldf("SELECT *,strftime('%d', DATE_ORDER) as 'day' FROM p1_sum")
# REVENUE/QTY_ORDER
p1_day$REV_QTY <- p1_day$sum_REVENUE/p1_day$sum_QTY_ORDER
# BOX PLOT
boxplot(p1_day$REV_QTY~p1_day$day, data=p1_day, main=paste("Price per day of ",prod_id), xlab="Day", ylab="Price")

# POR DIA DE UM PRODUTO
# Select PROD ID and create column month
p1_month <- sqldf("SELECT *,strftime('%m', DATE_ORDER) as 'day' FROM p1_sum")
p1_month$REV_QTY <- p1_month$sum_REVENUE/p1_month$sum_QTY_ORDER
boxplot(p1_month$REV_QTY~p1_month$day, data=p1_month, main=paste("Price per month of ",prod_id), xlab="Month", ylab="Price")


############################
# PRODUCTS - PRICE X MONTH #
############################
prod_id = c('P1','P2','P3','P4','P5','P6','P7','P8','P9')
l = list()
# par(xpd=FALSE) # this is usually the default

for (prod in prod_id) {
  sql<-paste0("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE, strftime('%m', DATE_ORDER) as month FROM sales_data WHERE PROD_ID='",prod,"' GROUP BY strftime('%m', DATE_ORDER)")
  p_m <- sqldf(sql)
  p_m$REV_QTY <- p_m$sum_REVENUE/p_m$sum_QTY_ORDER
  l[[prod]] <-p_m
}
plot(x=l$P1$month, y=l$P1$REV_QTY,type="l",col="blue", main = "PRODUCTS", xlab = 'Month', ylab = 'Price',ylim = c(350,1800), xlim = c(1,12))
lines(x=l$P2$month, y=l$P2$REV_QTY,type="l",col="green")
lines(x=l$P3$month, y=l$P3$REV_QTY,type="l",col="gold")
lines(x=l$P4$month, y=l$P4$REV_QTY,type="l",col="gray")
lines(x=l$P5$month, y=l$P5$REV_QTY,type="l",col="black")
lines(x=l$P6$month, y=l$P6$REV_QTY,type="l",col="pink")
lines(x=l$P7$month, y=l$P7$REV_QTY,type="l",col="red")
lines(x=l$P8$month, y=l$P8$REV_QTY,type="l",col="orange")
lines(x=l$P9$month, y=l$P9$REV_QTY,type="l",col="purple")
legend(10.2,1400 , prod_id, cex=0.8,  col=c("blue","green","gold","gray","black","pink","red","orange","purple"), lwd=2, bty="n")

###########################
# PRODUCTS - QTY X MONTH  #
###########################
prod_id = c('P1','P2','P3','P4','P5','P6','P7','P8','P9')
l = list()
# par(xpd=FALSE) # this is usually the default

for (prod in prod_id) {
  sql<-paste0("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE, strftime('%m', DATE_ORDER) as month FROM sales_data WHERE PROD_ID='",prod,"' GROUP BY strftime('%m', DATE_ORDER)")
  p_m <- sqldf(sql)
  l[[prod]] <-p_m
}


### Plot 1 - ylim = c(100,7000)
plot(x=l$P3$month, y=l$P3$sum_QTY_ORDER,type="l",col="gold", main = "Quantity per month", xlab = 'Month', ylab = 'Quantity',ylim = c(100,7000),xlim = c(1,12))
lines(x=l$P4$month, y=l$P4$sum_QTY_ORDER,type="l",col="gray")
lines(x=l$P6$month, y=l$P6$sum_QTY_ORDER,type="l",col="pink")
lines(x=l$P9$month, y=l$P9$sum_QTY_ORDER,type="l",col="purple")
lines(x=l$P1$month, y=l$P1$sum_QTY_ORDER,type="l",col="blue")
legend("topright", c("P1","P3","P4","P6","P9"), cex=0.8,  col=c("blue","gold","gray","pink","purple"), lwd=2, bty="n")

### Plot 2 - ylim = c(100,35000)
plot(x=l$P2$month, y=l$P2$sum_QTY_ORDER,type="l",col="green",main="Quantity per month", xlab = 'Month',ylim = c(100,35000), ylab = 'Quantity',xlim = c(1,12))
lines(x=l$P5$month, y=l$P5$sum_QTY_ORDER,type="l",col="black")
lines(x=l$P7$month, y=l$P7$sum_QTY_ORDER,type="l",col="red")
lines(x=l$P8$month, y=l$P8$sum_QTY_ORDER,type="l",col="orange")
legend("topright" , c("P2","P5","P7","P8"), cex=0.8,  col=c("green","black","red","orange"), lwd=2, bty="n")

# All
# plot(x=l$P1$month, y=l$P1$sum_QTY_ORDER,type="l",col="blue", main = "PRODUCTS", xlab = 'Month', ylab = 'Quantity',ylim = c(100,4000),xlim = c(1,12))
# lines(x=l$P2$month, y=l$P2$sum_QTY_ORDER,type="l",col="green")
# lines(x=l$P3$month, y=l$P3$sum_QTY_ORDER,type="l",col="gold")
# lines(x=l$P4$month, y=l$P4$sum_QTY_ORDER,type="l",col="gray")
# lines(x=l$P5$month, y=l$P5$sum_QTY_ORDER,type="l",col="black")
# lines(x=l$P6$month, y=l$P6$sum_QTY_ORDER,type="l",col="pink")
# lines(x=l$P7$month, y=l$P7$sum_QTY_ORDER,type="l",col="red")
# lines(x=l$P8$month, y=l$P8$sum_QTY_ORDER,type="l",col="orange")
# lines(x=l$P9$month, y=l$P9$sum_QTY_ORDER,type="l",col="purple")
# legend(10.2,2000 , prod_id, cex=0.8,  col=c("blue","green","gold","gray","black","pink","red","orange","purple"), lwd=2, bty="n")

###########################
# PRODUCTS - QTY X Day  #
###########################
prod_id = c('P1','P2','P3','P4','P5','P6','P7','P8','P9')
l = list()
# par(xpd=FALSE) # this is usually the default

for (prod in prod_id) {
  sql<-paste0("SELECT sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE, strftime('%d', DATE_ORDER) as day FROM sales_data WHERE PROD_ID='",prod,"' GROUP BY strftime('%d', DATE_ORDER)")
  p_d <- sqldf(sql)
  l[[prod]] <-p_d
}

### Plot 1 - ylim = c(100,5700)
plot(x=l$P1$day, y=l$P1$sum_QTY_ORDER,type="l",col="blue", main = "Quantity per day", xlab = 'Day', ylab = 'Quantity',ylim = c(100,2200),xlim = c(1,32))
lines(x=l$P3$day, y=l$P3$sum_QTY_ORDER,type="l",col="gold")
lines(x=l$P4$day, y=l$P4$sum_QTY_ORDER,type="l",col="gray")
lines(x=l$P5$day, y=l$P5$sum_QTY_ORDER,type="l",col="black")
lines(x=l$P6$day, y=l$P6$sum_QTY_ORDER,type="l",col="pink")
lines(x=l$P8$day, y=l$P8$sum_QTY_ORDER,type="l",col="orange")
lines(x=l$P9$day, y=l$P9$sum_QTY_ORDER,type="l",col="purple")
legend("topright", c('P1','P3','P4','P5','P6','P8','P9'), cex=0.8,  col=c("blue","gold","gray","black","pink","orange","purple"), lwd=2, bty="n")

### Plot 2 - ylim = c(800,13000)
plot(x=l$P2$day, y=l$P2$sum_QTY_ORDER,type="l",col="green", main = "Quantity per day", xlab = 'Day', ylab = 'Quantity',ylim = c(800,13000),xlim = c(1,32))
lines(x=l$P7$day, y=l$P7$sum_QTY_ORDER,type="l",col="red")
legend("topright" , c("P2","P7"), cex=0.8,  col=c("green","red"), lwd=2, bty="n")