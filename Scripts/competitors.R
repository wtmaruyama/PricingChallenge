library(sqldf)

# Read datas
comp_data <- read.csv('comp_prices.csv', header = TRUE, sep = ",")
sales_data <- read.csv('sales.csv', header = TRUE, sep = ",")

###########################
# Summary Comp per PROD   #
###########################
prod_id = c('P1','P2','P3','P4','P5','P6','P7','P8','P9')

# Summary per PROD_ID
for (prod in prod_id) {
  sql<-paste0("SELECT * FROM comp_data WHERE PROD_ID='",prod,"'")
  data <- sqldf(sql)
  print(sqldf("SELECT * FROM data WHERE ='",prod,"'")) 
  print(summary(data))
}

# sqldf("SELECT * FROM comp_data WHERE COMPETITOR_PRICE=(Select max(COMPETITOR_PRICE) FROM comp_data WHERE PROD_ID='P7')")
# Select Max COMPETITOR_PRICE
for (prod in prod_id) {
  sql<-paste0("Select *,max(COMPETITOR_PRICE) FROM comp_data WHERE PROD_ID='",prod,"'")
  data <- sqldf(sql)
  print(data)
}

#########################
#      Comp X Sales     #  
#########################
# Select a competitor, product and type of payment
select_comp <- function(comp, id, pay, comp_data) {
  # Select Product and competitor
  sql <- paste("select * from comp_data where COMPETITOR='",comp,"' and PROD_ID='",id,"' and PAY_TYPE =",pay," and DATE_EXTRACTION <> '2015-10-14 08:11:39'",sep="")
  cp <- sqldf(sql)
  return (cp)
}

# Grouping by month
group_month <- function (cp_data) {
  cp_agg <- sqldf("SELECT avg(COMPETITOR_PRICE) as avg_PRICE, strftime('%m', DATE_EXTRACTION) as 'month' FROM cp_data GROUP BY strftime('%m', DATE_EXTRACTION)")
  return(cp_agg)
  }


# For plot
prod_id = "P4"
pay_id = 2
c_id = c('C1','C2','C3','C4','C5','C6')

# Groups sales of a product a month, sum all REVENUE and sum all QTY_ORDER
sql_sale <- paste0("select sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE, strftime('%m', DATE_ORDER) as 'month' FROM sales_data WHERE PROD_ID='",prod_id,"' GROUP BY strftime('%m', DATE_ORDER)")
sales_agg <- sqldf(sql_sale)
# Price per Unit
sales_agg$REV_QTY <- sales_agg$sum_REVENUE/sales_agg$sum_QTY_ORDER
l = list()

# List of competitors
for (c in c_id) {
  c_m <- select_comp(c, prod_id, pay_id, comp_data) 
  c_gg <- group_month(c_m)
  l[[c]] <- c_gg
}

plot(x=sales_agg$month, y=sales_agg$REV_QTY, type="l",col="blue", main = paste("Product",prod_id,"and Pay type",pay_id), xlab = 'Month', ylab = 'Price', ylim = c(440,650))
lines(x=l$C1$month, y=l$C1$avg_PRICE,type="l",col="green")
lines(x=l$C2$month, y=l$C2$avg_PRICE,type="l",col="gold")
lines(x=l$C3$month, y=l$C3$avg_PRICE,type="l",col="gray")
lines(x=l$C4$month, y=l$C4$avg_PRICE,type="l",col="black")
lines(x=l$C5$month, y=l$C5$avg_PRICE,type="l",col="pink")
lines(x=l$C6$month, y=l$C6$avg_PRICE,type="l",col="red")
legend("topright" , c("B2W",c_id), cex=0.8,  col=c("blue","green","gold","gray","black","pink","red","orange","purple"), lwd=2, bty="n")