library(sqldf)
library(cluster)

comp_data <- read.csv('comp_prices.csv', header = TRUE, sep = ",")
sales_data <- read.csv('sales.csv', header = TRUE, sep = ",")

# Select a competitor, product and type of payment
select_comp <- function(comp, id, pay, comp_data) {
  # Select Product and competitor
  sql <- paste("select * from comp_data where COMPETITOR='",comp,"' and PROD_ID='",id,"' and PAY_TYPE =",pay," and DATE_EXTRACTION <> '2015-10-14 08:11:39'",sep="")
  cp <- sqldf(sql)
  return (cp)
}

# Prices for Sales
select_sale<-function(prod_id){
  sql_sale <- paste0("select sum(QTY_ORDER) as sum_QTY_ORDER, sum(REVENUE) as sum_REVENUE FROM sales_data WHERE PROD_ID='",prod_id,"'")
  sales_agg <- sqldf(sql_sale)
  # Price per Unit
  sales_agg$REV_QTY <- sales_agg$sum_REVENUE/sales_agg$sum_QTY_ORDER
  return (sales_agg)
}

# AVG Price
avg_price <- function (cp_data) {
  #cp_agg <- sqldf("SELECT avg(COMPETITOR_PRICE) as avg_PRICE, strftime('%m', DATE_EXTRACTION) as 'month' FROM cp_data GROUP BY strftime('%m', DATE_EXTRACTION)")
  cp_agg <- sqldf("SELECT avg(COMPETITOR_PRICE) as avg_PRICE FROM cp_data")
  return(cp_agg)
}


# Create data.frame
# each row corresponds to an observation, and each column corresponds to a variable
prod_id = c("P1","P2","P3","P4","P5","P6","P7","P8","P9")
pay_id = 1
c_id = c('C1','C2','C3','C4','C5','C6')
sale <- c()
data_full <-data.frame(row.names=c_id)

for(prod in prod_id){
  dataf <- data.frame(row.names=c_id)
  # List of competitors
  for (c in c_id) {
    c_m <- select_comp(c, prod, pay_id, comp_data) 
    c_gg <- avg_price(c_m)
    dataf <- rbind(dataf,c_gg)
  }
  data_full <- cbind(data_full,dataf)
  s <-select_sale(prod)
  sale<-c(sale,s$REV_QTY)
}

data_full <- rbind(data_full,sale)
rownames(data_full)<-c(c_id,"B2W")
colnames(data_full)<-prod_id

#############
#   Agnes   #
#############
agn <- agnes(data_full,method = "average") # default method = "average")

plot(agn)

n=7
plot((n - 1):1,agn$height, pch = 20, xlab = "Number of clusters",ylab = "Height", main = "")

grupls <- cutree(agn, k = 2)
grupls

#############
#   Diana   #
#############
dia <- diana(data_full)
plot(dia)
plot((n - 1):1,dia$  height, pch = 20, xlab = "Number of clusters",ylab = "Height", main = "")

