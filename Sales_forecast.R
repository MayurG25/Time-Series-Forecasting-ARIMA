library(tseries)
library(forecast)
library(dplyr)
library(xlsx)
library(readxl)
library(ggplot2)

# Assigning working directory
setwd("F:/Mayur/ANALYTICS/Data scientist/Data scienc with R/Time series")

# importing data
df = read_excel("Sample - Superstore.xls",sheet = "Orders")

# exploring the data
View(head(df))
glimpse(df)

## checking missing values in the data set
colSums(is.na(df)) # no missing value

## summary of the data
summary(df)

## exploring sales variable
summary(df$Sales)
boxplot(df$Sales)
hist(df$Sales)
plot(df$Sales)

## observations with sales >=12000
View(df %>% filter(Sales>=10000))# techonology related products

## checking unique products
length(unique(df$`Product Name`))
length(unique(df$`Product ID`)) # one product id is assigned to different product names

length(unique(df$`Customer ID`))
length(unique(df$`Customer Name`))

product_list = df %>% group_by(`Product ID`,`Product Name`) %>% 
            summarise(count = n()) %>% arrange(`Product ID`)

length(unique(product_list$`Product Name`))
length(unique(product_list$`Product ID`))
dim(product_list)

product_id = product_list %>% group_by(`Product ID`) %>% mutate(count = row_number())

## finding out the prducts with similar product id

similar_id = product_id %>% filter(count >= 2)

View(df %>% filter(`Product ID` %in% similar_id$`Product ID`
                  ) %>% 
         arrange(`Product ID`))

View(df %>% filter(`Product Name`%in% similar_id$`Product Name`) %>% 
    arrange(`Product ID`))

# keeping the records though it has similar product id
# the records are less than 4% and sales value will not have major impact with inclusion of these records

## category and sales comparison
df %>% group_by(Category) %>% summarise(N = n(),
                                        Sales_Max = max(Sales),
                                        Sales_min = min(Sales),
                                        Sales_Total = sum(Sales),
                                        Sales_mean = mean(Sales))
                                        
df %>% filter(Sales<10000) %>% group_by(Category) %>% summarise(N = n(),
                                        Sales_Max = max(Sales),
                                        Sales_min = min(Sales),
                                        Sales_Total = sum(Sales),
                                        Sales_mean = mean(Sales))



# drastic change in maximum sales amount, mean has around 8% change
# removing observations with >=10000 sales amount
# only 5 observations will get deleted

df = df %>% filter(Sales < 10000)

product_id = NULL
product_list = NULL
similar_id = NULL

## exploring country, state, city 

# total countries in the data set
table(df$Country) # only united states

# total states in the data set
# visualising top sales contributors
df %>% group_by(State) %>% summarise(Total_Sales = sum(Sales)) %>% 
        ggplot() + geom_bar(aes(reorder(State,-Total_Sales),Total_Sales),stat = "identity")+
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 80, hjust = 1)) + 
        labs(title = 'Statewise sales',x = 'State') 

# California and New York are the top contributors

View(df %>% group_by(State) %>% summarise(N = n(),
                                Sales_Max = max(Sales),
                                Sales_min = min(Sales),
                                Sales_Total = sum(Sales),
                                Sales_mean = mean(Sales))
                                %>% arrange(desc(Sales_Total)))

View(df %>% group_by(State,Category) %>% summarise(N = n(),
                                          Sales_Max = max(Sales),
                                          Sales_min = min(Sales),
                                          Sales_Total = sum(Sales),
                                          Sales_mean = mean(Sales))
                                        %>% arrange(State,desc(Sales_Total)))

# from the top 5 sales contributors only washington has high sales contribution by furniture category
# all other top 4 sales contributord are having technology as highest sales contributor

# relation between quantity and sales
plot(df$Sales,df$Quantity)

## which category has maximum quantity sold
df %>% group_by(Category) %>% summarise(total_quantity = sum(Quantity),
                                        total_sales = sum(Sales)) %>% arrange(desc(total_quantity))

# Office supplies are having high quantity numbers where as technology sales amount is higher
# technology items could be more expensive

# pulling out year and month out of order date variable
library(lubridate)
summary(df$`Order Date`)
df = df %>% mutate(month_year = format(as.Date(`Order Date`),"%Y-%m"))

copy = function(df)
{
    write.table(df, "clipboard", sep="\t", row.names = FALSE)
}

copy(df %>% group_by(`Order Date`) %>% summarise(sale = sum(Sales)))
# sale is increasing at the end of the year
# year 2017 is showing increase in sales compare to 2014,2015 and 2016

# Exploring sub category variable
View(df %>% group_by(`Sub-Category`) %>% summarise(Freq = n(),
                                                   Qty = sum(Quantity)) %>% 
         arrange(desc(Qty)))
# Papers and Binders are the top 2 items in terms of quantity sold

View(df %>% group_by(`Sub-Category`) %>% summarise(Freq = n(),
                                                   Qty = sum(Quantity),
                                                    Total_Sale = sum(Sales)) %>% 
         arrange(desc(Total_Sale)))
# sales is dominated by phones and chairs sub category

# checking the sales percentage share of each category
Sales_Share = df %>% group_by(Category) %>% 
              summarise(Sales = sum(Sales))

View(Sales_Share %>% mutate(Share_Per = paste0(round(Sales/sum(Sales)*100,2),"%")) %>% 
         arrange(desc(Share_Per)))

# all three categories seems to have quiet equal share

# time series forecasting
# forecasting at united states level
ts_data = df %>% filter(Country == "United States",
                        Category %in% c("Technology",
                                        "Furniture",
                                        "Office Supplies")) %>% 
                group_by(month_year) %>% summarise(Total_Sales = sum(Sales)) %>% 
                arrange(month_year) %>% select(Total_Sales)

# converting data into ts class
ts_df = ts(ts_data,frequency = 12,start = c(2014,1))
end(ts_df)

plot.ts(ts_df) 
plot(decompose(ts_df))

# statistical check for stationarity
adf.test(ts_df) #p-value is 0.07 

# differencing time series
ts_diff = diff(ts_df,differences = 2)
plot.ts(ts_diff)
adf.test(ts_diff)

# time series looks stationary
# selecting p,d,q
acf(ts_diff, lag.max = 20)
pacf(ts_diff, lag.max = 20)

# selecting p as 1 and q as 0
ts_model = arima(ts_diff,order = c(1,2,1))
ts_model

# forecasting for next two years
ts_forecast = forecast:::forecast.Arima(ts_model,h=6)
plot(ts_forecast)
ts_forecast$fitted
ts_forecast$mean

# checking residuals
acf(ts_forecast$residuals, lag.max = 20)
plot.ts(ts_forecast$residuals) # residuals looks random
adf.test(ts_forecast$residuals)
acf(ts_forecast$residuals)


Box.test(ts_forecast$residuals,lag = 20,type = 'Ljung-Box')
# p value is < 0.05
# residuals are not independent
# forecasting needs improvement


# checking with auto arima
ts_auto_arima = auto.arima(ts_df,seasonal = TRUE,stationary = TRUE)
ts_auto_arima

auto_arima_forecast = forecast:::forecast.Arima(ts_auto_arima,h=12)
plot(auto_arima_forecast)
auto_arima_forecast

library(TSPred)
plotarimapred(ts_df,auto_arima_forecast$fitted,xlim = c(2014,2017))
# predicted vd actual graph


# checking residuals auto corelation
acf(auto_arima_forecast$residuals)

# checking 
plot(auto_arima_forecast$residuals) 
# residuals are random

Box.test(auto_arima_forecast$residuals,lag = 20,type = "Ljung-Box")
# p value is 0.33 i.e. we can not reject null hypothesis
# we can say residuals are independent

# mean squared error
mean(auto_arima_forecast$residuals)
sqrt(mean(auto_arima_forecast$residuals))

# checking mean absolute percentage error
mean((abs((ts_df-auto_arima_forecast$fitted)/ts_df))*100)
# the model is off by 29%
# the model is performing well at (100-29) 71%


# developing forecasting model for Technology category
# ts_data = df %>% filter(Country == "United States",
#                         Category %in% c("Technology",
#                                         "Furniture",
#                                         "Office Supplies")) %>% 
#     group_by(month_year) %>% summarise(Total_Sales = sum(Sales)) %>% 
#     arrange(month_year) %>% select(Total_Sales)

Category_Forecast = function(data,Catg)
{
  ts_data = data %>% filter(Category == Catg) %>%
          group_by(month_year) %>% summarise(Total_Sales = sum(Sales)) %>%
          arrange(month_year) %>% select(Total_Sales)

  # converting data into ts class
  ts_df = ts(ts_data,frequency = 12,start = c(2014,1))
  end(ts_df)

  plot.ts(ts_df) 
  #plot(decompose(ts_df))

  # statistical check for stationarity
  adf.test(ts_df) 
  
  # checking with auto arima
  ts_auto_arima = auto.arima(ts_df,seasonal = TRUE,stationary = TRUE)
  ts_auto_arima
  
  auto_arima_forecast = forecast:::forecast.Arima(ts_auto_arima,h=12)
  auto_arima_forecast
  
  # predicted vd actual graph
  library(TSPred)
  plotarimapred(ts_df,auto_arima_forecast$fitted,xlim = c(2014,2017))
   
  # checking residuals auto corelation
  acf(auto_arima_forecast$residuals)
  
  # checking 
  plot(auto_arima_forecast$residuals) 
  # residuals are random
  
  plot(auto_arima_forecast)
  
  # mean squared error
  mean(auto_arima_forecast$residuals)
  sqrt(mean(auto_arima_forecast$residuals))
  
  # checking mean absolute percentage error
  mean((abs((ts_df-auto_arima_forecast$fitted)/ts_df))*100)
  
  Box.test(auto_arima_forecast$residuals,lag = 20,type = "Ljung-Box")

  forecasted_values = as.data.frame(auto_arima_forecast)
  
  return(forecasted_values)
}

Technology_forecast = Category_Forecast(data = df,Catg = "Technology")


