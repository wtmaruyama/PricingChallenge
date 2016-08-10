library(sqldf)

# Select QTY_ORDER,REVENUE per PROD ID
# id: PROD ID
# sales_data: data
select_product<-function(id,sales_data){
  # Same result sales_data[sales_data$PROD_ID == id, ]
  sql<-paste("select QTY_ORDER,REVENUE from sales_data where PROD_ID='",id,"'",sep="")
  p_data <- sqldf(sql)
  return(p_data)
}

# Create linear regression model for predict QTY_ORDE for each product id from revenue
# formula: is a symbol presenting the relation between x and y.
# id: Product id
create_model <- function(id, formula, p_data) {
  relation <- lm(formula,data=p_data)
  return (relation)
}

# Creates the graphic of model
# prod_id: PROD ID
# sales_data: data
plot_model <- function (select_data, prod_id){
  name_x <-'QTY_ORDER'
  name_y <-'REVENUE'
  x <- select_data[,name_x]
  y <- select_data[,name_y]
  #plot(x,y,col = "blue",main = paste(name_y, " & ", name_x, "Linear Regression ",prod_id,sep=""),abline(lm(y~x)),xlab = name_x,ylab = name_y)
  plot(x,y,col = "blue",main = paste("Linear Regression ",prod_id,sep=""),abline(lm(y~x)),xlab = name_x,ylab = name_y)
}

# Execute prediction
# id: PROD ID
# sales_data: data
predict_qty <- function(sales_data, id, revenues){
  p_data <- select_product(id,sales_data)
  relation <- create_model(REVENUE~QTY_ORDER , p_data)
  summary(relation)
  qty_pred <-c()
    for(value in revenues) {
      qty <- predict(relation,newdata=data.frame(REVENUE = value))
      qty_pred <- c(qty_pred, round(qty))
    }
  data_result<-data.frame(revenues, qty_pred)
  return (data_result)
}

############################################################
# Execution
# Read data 
sales_data <- read.csv('sales.csv', header = TRUE, sep = ",")
comp_data <- read.csv('comp_prices.csv', header = TRUE, sep = ",")

select_data <- sales_data[, c('QTY_ORDER','REVENUE',"PROD_ID")]

# Model for PROD_ID
prod_id = "P1"
plot_model(select_product(prod_id,sales_data),prod_id)

# Prices
prices<-c(25887.6,8994.00,5450.00, 6812.50)
results <- predict_qty(sales_data,prod_id,prices)
print(results) 


###########################################################
# Correlation between QTY and REVENUE
cor(select_data$QTY_ORDER,select_data$REVENUE)

########### Validation model ####################
# K-fold cross-validation
library(DAAG)
p_data <- select_product(prod_id,sales_data)
fit <- create_model(QTY_ORDER~REVENUE , p_data)
cv.lm(p_data, fit, m=10)