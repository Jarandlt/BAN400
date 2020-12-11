library(httr)
library(jsonlite)
library(tidyquant)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(blastula)
library(tidyr)
library(docstring)
library(shinyalert)
library(curl)#used to check internet connection
library(shinydisconnect)

# #Do parallel computing with maximum number of course for this operation
# #https://davisvaughan.github.io/furrr/
library(furrr)
library(doParallel)


############ Choose how many stocks of s&p600 to include
stocks_to_include=100
###########################

task_theme<-function(){
  #' Customized ggplot theme for the whole task
  #' Includes setting the correct background color, customize x-axis and y-axis and adjust the labels on the axis
  theme(legend.position = "top", legend.key=element_blank(),panel.background =element_rect(fill = "white",colour = "white"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5),
        axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
        text = element_text(size = 15),legend.text=element_text(size=12))
 } 
  

#########################################
###############Tab 1 - Chosen stock###
##############################################
  

print_stock_prices<-function(data, company){
  #'Makes a ggplot of a ceritan stock
  #'
  #'  Function that makes a ggplot that visualizes the close price for a time period.
  #' If the line is green then you are looking at the close price the last 20 days.
  #' @param data is a dataframe containing info about a chosen stock. Call for instance data.frame(tq_get("M", get = "stock.prices", from = "2020-10-17", to= "2020-11-17")) to see how the dataframe looks
  #' @param company is the symbol of the company we investigate. EG "M"
  #' @example print_stock_prices(data=data.frame(tq_get("M", get = "stock.prices", from = "2020-10-17", to= "2020-11-17")), company="M")
  ggplot(data, aes(x = date, y = close,colour = date >=Sys.Date()-20)) +
    geom_line(stat = "identity",size=1) +
    geom_point(size=4)+
    scale_colour_manual(name = 'Last 20 days from today', values = setNames(c('red','green'),c(F, T))) +
    task_theme()+
    ylab("Close price")+ labs(title=paste("Price over time for", company))+
    xlab("Date")
}





visualize_index<-function(df, stock){
  #'Makes a ggplot of a chosen stock and the index
  #'
  #' 1. The Function finds the value of the index by summing the close price of all stocks in S&P600 for each day
  #' and dividies this sum on the number of stocks.
  #' 2. The function then selects only the the estimated index and the chosen stock and the date and estimate 
  #' the change in price from the first date (20 days from today)
  #' 3. Calls the function plot_index that visualizes the change in the index and the change in the selected stock
  #' 
  #' @param df data frame that shows the close price to all stocks the last 20 days (df=get_all_stock_data())
  #' @param stock chosen stock symbol EG. "M"
  #' @examples visualize_index(df=get_all_stock_data(), stock="M")
  
  df$index = 0 #Column to store values for our self made index
  ncols <- ncol(df) -1  #number of All columns except the new column (index)
  #1. Calculate the value of the index
  for (i in 1:nrow(df)){
    df$index[i] = ((rowSums(df[i,2:ncols])) / (ncol(df)-2))#Divided by the number of stocks (2 columns needs to not be included (date and index))
  }
  #2. Select only date, index and the relevant stock(s)
  df <- df[colnames(df)=="Date" | colnames(df)=="index" | colnames(df)==stock]
  colnames(df)[2] <- "stock"
  for (i in 1:nrow(df)){
    df$stock_pct[i] = df$stock[i]/df$stock[1] #Estimate the change in price from the first date
    df$index_pct[i] = df$index[i]/df$index[1]
  }
  
  df = df[,c(1,4,5)]#Select only relevant columns from data frame (Date, change in stock price and change in index price)
  df = gather(df, Instrument, Change, stock_pct, index_pct)
  plot_index(df, Date, Change, Instrument, stock) #Plot the index against the chosen stock
}


plot_index<-function(df, Date, Change, Instrument, stock){
  #' Plot a index against a chosen stock
  #' 
  #' @param df a dataframe containing the date in the first column, instrument (stock or index) in the second column and change in stock price in the third column
  #' @param Date date
  #' @param Change relative change in close price from day 1
  #' @param Instrument stock or index
  #' @param stock a stock symbol EG "M"
  #' 
  #' @example This function is called within the visualize_index() function: Try: visualize_index(df=get_all_stock_data(), stock="M")
  ggplot(df, aes(x = Date, y = Change, color = Instrument)) +
    geom_line(stat = "Identity", size=1)+
    geom_point(size=4)+
    task_theme()+
    scale_x_date(breaks = df$Date) +
    scale_color_manual(labels = c("Index", stock), values = c("lightskyblue", "green")) +
    ylab("Change")+ labs(title=paste(stock, "vs index the last 20 days"))+
    xlab("Date")
}




#########################################
###############Tab 2 & 3 - Top/Bottom 5 stocks###
##################################################

get_trading_date<-function(){
  #' Get the dates of the trading days the last 20 days
  #' 
  #' This will be less than 20 dates as one can not trade in the weekends
  #' @return vector with all trading dates the last 20 days
  date=as.Date(Sys.time())
  tq_get("AAPL", get = "stock.prices", from = date-20, to=date)$date %>%
    return()
}



get_close_prices<-function(stock){
  #' Get the close price for all stocks the last 20 days
  #' 
  #' @param stock is the chosen stock symbol EG "M"
  #' @return The close price to the selected stock for the last 20 trading days
  #' @example get_close_prices(stock="M")
  date=as.Date(Sys.time())
  
  #If there are missing data about ant of the stocks this will not crash the program

  all_stock_prices<- tq_get(stock, get = "stock.prices", from = date-20, to=date,complete_cases = TRUE)#The last 20 closing stock prices
  if(is.null(dim(all_stock_prices)) & is.na(all_stock_prices)){
    return(NA) #Handle if one can not find the stock
  } 
  close_price<-all_stock_prices[,6]#Get the close price
  colnames(close_price)<-stock
  return(close_price)#Get the close price
}



#The symbols to all chosen stocks in SP600
#We may not include all as this takes a lot of prosessing time
ticker_symbols<-tq_index("SP600")$symbol[1:stocks_to_include] 


get_all_stock_data<-function(){
  #' Function that gets the close price to all stocks on S&p 600
  #' 
  #' Function that uses parallel programming to get the close price to all stocks for the last 20 days
  #' 
  #' @return data frame with the date and the close price to all stocks in sp600

  plan(multisession, workers = parallel::detectCores()-1)
  #Create a data frame with all close prices
  close_price<-ticker_symbols%>%
    future_map_dfc(~get_close_prices(.))
  
  close_price=cbind(get_trading_date(), close_price)#Add date
  close_price =close_price[ , colSums(is.na(close_price)) == 0]#Remove columns with NA
  colnames(close_price)[1]="Date"
  return(close_price)
}

####################################

#(EG: get_best_or_worst_stocks(get_all_stock_data(), "best")) 
get_best_or_worst_stocks <-function(close_price, type){
  #'Function that finds the 5 worst or best stocks the last 20 trading days
  #'
  #'The function saves the 5 stocks that have the largest increase/decrease in 
  #'closing price from 20 days ago until today. 
  #'After finding the best/worst stocks we find the daily relative change in close price
  #'for each stock
  #'
  #' @param close_pricetable with the date and close price to stocks the last 20 days (not trading days - hence, less than 20 values for each stock)
  #' @param type either "worst" or "best" depending on which data you want
  #' @return returns a stacked data frame with close price, stock, date and relative change in price. 
  #'
  #' @examples  get_best_or_worst_stocks(close_price=get_all_stock_data(), type="best") 
  #'  get_best_or_worst_stocks(close_price=get_all_stock_data(), type="worst") 
  change_in_price<-((close_price[nrow(close_price),-1])-close_price[1,-1])/close_price[1,-1] #percentage change in stock price (last price -price 20 days ago)/price 20 days ago
  
  close_price<-rbind(close_price,as.data.frame(c(Date=NA,change_in_price )))
  last_vals = unlist(close_price[nrow(close_price),-1])
  df=data.frame()
  
  if(type=="best") df = close_price[, c(colnames(close_price)[1], names(sort(last_vals, decreasing = TRUE)))]#best
  
  else if (type=="worst") df = close_price[, c(colnames(close_price)[1], names(sort(last_vals, decreasing = FALSE)))] #worst
  
  df<-df[-nrow(df),1:6]#remove the change in stock price and look at top 5 values
  #turn the long df to a stacked df
  stacked_df<-df[,2:ncol(df)] %>%
    stack() %>%
    cbind(rep(df[,1]))
  colnames(stacked_df ) <-c("Close_price", "Stock","Date")
  
  #Lastly, add a column with relative change in price for each stock
  updated_stacked_df<-find_relative_price_change(stacked_df)
  return(updated_stacked_df) 
}


find_relative_price_change<-function(df){
  #' Find the relative change in stock prices from trading day to trading day
  #' 
  #' @param df A stacked data frame with "Close_price", "Stock" and "Date" column.
  #' @return A dataframe similar to the one as input parameter but with an additional column with the relative
  #' difference in price form day to day.
  df$relative_diff<-NA
  for(i in 2:nrow(df)){#find relative change in price for all stocks (no value in the first date)
    if(is.na(df$Stock[i]) | is.na(df$Stock[i-1]) ) next #move to next row if missing value
    if(df$Stock[i]==df$Stock[i-1]) df$relative_diff[i]<-(df$Close_price[i]-df$Close_price[i-1])/df$Close_price[i-1]
  }
  return(df)
}


vizulize_stocks<-function(stacked_df, type){
  #' Function that visualize the top or bottom 5 stocks the last 20 days
  #' 
  #' @param stacked_df get_best_or_worst_stocks(get_all_stock_data(), "best")
  #' @param type "best" or "worst"
  #' @examples vizulize_stocks(stacked_df=get_best_or_worst_stocks(get_all_stock_data(), "best"), type="best")
  #' vizulize_stocks(stacked_df=get_best_or_worst_stocks(get_all_stock_data(), "worst"), type="worst")
  #' 
  ggplot(stacked_df, aes(x = Date, y = Close_price, color=Stock)) +
    geom_line(stat = "identity", size=1) +
    geom_point(size=4)+
    task_theme()+
    scale_x_date(breaks = stacked_df$Date) +
    ylab("Close price")+ labs(title=paste(type, "5 stocks the last 20 days"))+
    xlab("Date")
}  




vizulize_relative_change <-function(stacked_df, type){
  #' Visualize the relative daily change for the best/worst stocks
  #' 
  #' Visualize the relative daily change for the best/worst stocks. When the lines are in the green area 
  #' of the graph, there has been an increase in the stock price from the day before.
  #'When the lines are in the red area, the stock price has decreased from the day before. 
  #'  
  #' @param stacked_df get_best_or_worst_stocks(get_all_stock_data(), "best")
  #' @param type "best" or "worst"
  #' 
  #' @examples vizulize_relative_change(stacked_df=get_best_or_worst_stocks(get_all_stock_data(), "best"), type="best")
  #' vizulize_relative_change(stacked_df=get_best_or_worst_stocks(get_all_stock_data(), "worst"), type="worst")
  #' 
  stacked_df %>% na.omit() %>% 
    ggplot(aes(x = Date, y = relative_diff, color=Stock)) +
    geom_line(stat = "identity", size=1) +
    geom_hline(yintercept=0, linetype="dashed", color = "black",size=2)+
    
    annotate("rect", fill = "green", alpha = 0.1, 
             xmin = Sys.Date()-20, xmax = Sys.Date(),
             ymin = 0, ymax = Inf) +
    annotate("rect", fill = "red", alpha = 0.1, 
             xmin = Sys.Date()-20, xmax = Sys.Date(),
             ymin = 0, ymax = -Inf)+
    
    geom_point(size=4)+
    task_theme()+
    scale_x_date(breaks = stacked_df$Date) +
    ylab("Daily relative change in close price")+ labs(title=paste("Daily relative change in close pricee for\n", type, "5 stocks the last 20 days"))+
    xlab("Date")
  
}

########################################
#############Tab 4 -Check stock price
###########################################

#'Function that plots the selected stock and a a predefined threshold value.
#'
#'The stock is colored with green if the time when the stock was above the
#'threshold and with red when it was below the price threshold.
#'@param stock a selected stock EG "M"
#'@param price the threshold price  EG 10
#'@example Visulize_stock_over_price("M",)
Visulize_stock_over_price<-function(stock, price){
  close_prices <-as.data.frame(data[,stock])
  close_prices<-data.frame(cbind(close_prices, data[,"Date"]))
  colnames(close_prices)<-c("Close_price", "Date")
  
  close_prices %>% 
  ggplot(aes(x = Date, y = Close_price, color=Close_price >price))+
    geom_line(stat = "identity", size=1) +
    geom_hline(yintercept=price, linetype="dashed", color = "black",size=2)+#Make a dashed line on the limit from the user
    scale_colour_manual(name = paste('Close price larger than', price), values = setNames(c('red','seagreen'),c(F, T))) +
    geom_point(size=4)+
    task_theme()+
    scale_x_date(breaks = close_prices$Date) +
    ylab("Close price")+ labs(title=paste("The stock price to stock", stock, "over the past 20 days"))+
    xlab("Date")

}



##############################################################################################################
##############Make the UI and server to the app#################################################################
##############################################################################################################

#######################UI

# Define user interface
ui <- fluidPage(
  
  # App title
  headerPanel("Stock information"),
  
  # Sidebar panel for inputs
  sidebarPanel(
    
    selectizeInput(
      'ticker', 'Choose a company', choices = tq_index("SP600")$symbol[1:stocks_to_include], multiple = TRUE),
    #Added arguments to specify
    dateRangeInput("date",
                   label="Choose a date interval",
                   weekstart = 1,
                   start = Sys.Date()-10,
                   end = Sys.Date(),
                   max = Sys.Date()),
    plotOutput("graph")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h4("Investigate the price over time of your favorite stock, learn what sotcks have performed the best/worst the last 20 days and check if your favorite stock have currently a higher price than a treshold!"),
  
    tabsetPanel(
      tabPanel("Chosen stock",
               h5("The plot shows the price of your chosen stock in a time periode. The line is green for all dates that are within the last 20 days."),
               h5("The green line in this graph will be exactly equal to the green line in the graph below "),
               plotOutput("Results") %>% withSpinner(color="#00a2ed"),
               p(""),
               h5("The green line is the chosen stock and the blue line is the index."),
               plotOutput("Index_stock") %>% withSpinner(color="#00a2ed")),
      
      tabPanel("Top 5 stocks", 
               h5("The five stocks below are the stocks that have performed the BEST the last 20 days. "),
               plotOutput("Best_5") %>% withSpinner(color="#00a2ed"),
               plotOutput("Best_5_relative_change") %>% withSpinner(color="#00a2ed"),
               h5("The green area shows that the stock price has increased while the red area shows a daily decrease in the sock price.")),
      
      tabPanel("Bottom 5 stocks", 
               h5("The five stocks below are the stocks that have performed the WORST the last 20 days. "),
               plotOutput("Worst_5") %>% withSpinner(color="#00a2ed"),
               plotOutput("Worst_5_relative_change") %>% withSpinner(color="#00a2ed"),
               h5("The green area shows that the stock price has increased while the red area shows a daily decrease in the sock price.")),
      
      tabPanel("Check stock price",
               sidebarPanel(
                 selectizeInput('stock_check', 'Which stock should we check?',choices = tq_index("SP600")$symbol, multiple = TRUE),
                 numericInput('lowest_value', 'What is the lowest closing price',  value=100)),
               
               useShinyalert(),  # Set up shinyalert
               sidebarPanel(
                 h3("Press Check stock to see the result and a plot"),
               actionButton("preview", "Check stock")),
               mainPanel(
               plotOutput("Stock_vs_treshold"))
      )
    )
  )
)

#####################SERVER###################

server <- function(input, output, session) {
  
  #check if user has internet
  if(has_internet()==F){
    showModal(modalDialog(
      title = "Internet warning",
      "You do not have internet connection. Connect to internet and restart the app!"
    ))
    #The message will be visable in 5 seconds before the app is closed
    Sys.sleep(5)
    session$close()
 
  }
  
  data<-get_all_stock_data()#Get prices for all the stocks

  output$Results <- renderPlot({
    
      shiny::validate(
        need(input$date[1]<input$date[2], "Please select a time period where date 1 is smaller than date 2"))#The end date must be larger than the start date
      
      shiny::validate(
        need(input$ticker !="" & length(input$ticker) ==1, "Please select a stock (and only one stock)"))#
      company=input$ticker
      stock_prices  <- data.frame(tq_get(company, get = "stock.prices", from = input$date[1], to= input$date[2]))
  
      print_stock_prices(stock_prices, company)
      })
  
  #Plot stock vs index last 20 days
  output$Index_stock<-renderPlot({
    shiny::validate(
      need(input$ticker !="" & length(input$ticker) ==1, "Please select a stock (and only one stock)"))
    company=input$ticker
    visualize_index(data, company)
  })
  ########################  
  
  
  #Plot top 5 best stocks
  output$Best_5<-renderPlot({
    best_stocks<-get_best_or_worst_stocks(data, "best")
    vizulize_stocks(best_stocks, "Best")
  })
  output$Best_5_relative_change<-renderPlot({
    best_stocks<-get_best_or_worst_stocks(data, "best")
    vizulize_relative_change(best_stocks, "Best")
  })
  
  ######################
  
  #Plot top 5 worst stocks
  output$Worst_5<-renderPlot({
    worst_stocks<-get_best_or_worst_stocks(data, "worst")
    vizulize_stocks(worst_stocks, "Worst")
  })
  
  output$Worst_5_relative_change<-renderPlot({
    worst_stocks<-get_best_or_worst_stocks(data, "worst")
    vizulize_relative_change(worst_stocks, "Worst")
  })
  ###################
  
  #Check if a stock price is lower than a certain value
  observeEvent(input$preview, {
   
    stock<-input$stock_check #The stock we want to test
    price<-input$lowest_value
    
    shiny::validate(
      need(stock !="" & length(stock) ==1, "Please select a stock (and only one stock)"))#
    
    
    current_value = data[nrow(data),stock] #The value of this stock 
    if(input$lowest_value > current_value) {
      shinyalert(paste("Oops! The current stock price of ", stock, "
                         is low! It is only ", toString(round(current_value,2)), " dollars"), type = "warning")
      
    }else shinyalert(text=paste("You are fine! The current close price is ", round(current_value,2)), type = "info",
                     showCancelButton=T)
    
    
    output$Stock_vs_treshold<-renderPlot({
      Visulize_stock_over_price(stock, price )
    })
  })
  
  
}

#Make the shiny app
shinyApp(ui, server)

