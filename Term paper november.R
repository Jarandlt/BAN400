library(httr)
library(jsonlite)
library(tidyquant)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(blastula)
library(tidyr)

# #Do paralell computing with maximum number of cours for this operation
# #This is done to speed up the code!
# #https://davisvaughan.github.io/furrr/
library(furrr)
library(doParallel)

library(shinyalert)
library(curl)#used to check internet connection



############ Choose how many stocks of s&p600 to include
stocks_to_include=100
###########################

#########################################
###############All functions for the app###
##############################################

print_stock_prices<-function(data, company){
  ggplot(data, aes(x = date, y = close)) +
    geom_line(stat = "identity") +

    theme(legend.position = "top", legend.key=element_blank(),panel.background =element_rect(fill = "white",colour = "white"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5),
          axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
          text = element_text(size = 15),legend.text=element_text(size=12))+
    ylab("Close price")+ labs(title=paste("Price over time for", company))+
    xlab("Date")
}



##################################################
#Find top and bottom 5 stocks with max stock price today for the last 20 days
###############################################

#Save the trading date
#This function is made to only get the tradfing date. Therefore, it does not matter which stock we input in the tq_get function
get_trading_date<-function(){
  date=as.Date(Sys.time())
  tq_get("AAPL", get = "stock.prices", from = date-20, to=date)$date %>%
    return()
}

##Get the close price fro all stocks in a time interval
get_close_prices<-function(stock){
  date=as.Date(Sys.time())
  all_stock_prices<- tq_get(stock, get = "stock.prices", from = date-20, to=date)#The last 20 closing stock prices
  if(is.na(all_stock_prices)) return(NA) #Handle if one can not find the stock
  close_price<-all_stock_prices[,6]#Get the close price
  colnames(close_price)<-stock
  return(close_price)#Get the close price
}


ticker_symbols<-tq_index("SP600")$symbol[1:stocks_to_include] 

#Get close price to all stocks on S&p 500
get_all_stock_data<-function(){
  plan(multisession, workers = parallel::detectCores())
  #Create a data frame with all close prices
  close_price<-ticker_symbols%>%
    future_map_dfc(~get_close_prices(.))
  
  close_price=cbind(get_trading_date(), close_price)#Add date
  close_price =close_price[ , colSums(is.na(close_price)) == 0]#Remove columns with NA
  
  colnames(close_price)[1]="Date"
  return(close_price)
}

####################################
#Saves the 5 worst or best stocks
# type= worst or best
#close_price= table with the date and close price to stocks the last 20 days (not trading days - hence, less than 20 values for each stock)
#returns a stacked data frame close price, stock, date and relative change in price. 
#(EG: get_best_or_worst_stocks(get_all_stock_data(), "best")) 
get_best_or_worst_stocks <-function(close_price, type){
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

#Find the relative change in stock prices from trading day to trading day
find_relative_price_change<-function(df){
  df$relative_diff<-NA
  for(i in 2:nrow(df)){#find relative change in price for all stocks (no value in the first date)
    if(is.na(df$Stock[i]) | is.na(df$Stock[i-1]) ) next #move to next row if missing value
    if(df$Stock[i]==df$Stock[i-1]) df$relative_diff[i]<-(df$Close_price[i]-df$Close_price[i-1])/df$Close_price[i-1]
  }
  return(df)
}



#Vizulize stock data
#Type= best or worst
vizulize_stocks<-function(stacked_df, type){
  ggplot(stacked_df, aes(x = Date, y = Close_price, color=Stock)) +
    geom_line(stat = "identity", size=1) +
    geom_point(size=4)+
    theme(legend.position = "top", legend.key=element_blank(),panel.background =element_rect(fill = "white",colour = "white"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5),
          axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
          text = element_text(size = 15),legend.text=element_text(size=12))+
    scale_x_date(breaks = stacked_df$Date) +
    ylab("Close price")+ labs(title=paste(type, "5 stocks the last 20 days"))+
    xlab("Date")
}  



##Visulize the relative daily change for the vest/worst stocks
vizulize_relative_change <-function(stacked_df, type){
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
    theme(legend.position = "top", legend.key=element_blank(),panel.background =element_rect(fill = "white",colour = "white"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.7),
          axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
          text = element_text(size = 15),legend.text=element_text(size=12))+
    scale_x_date(breaks = stacked_df$Date) +
    ylab("Daily relative change in close price")+ labs(title=paste("Daily relative change in close pricee for\n", type, "5 stocks the last 20 days"))+
    xlab("Date")
  
}

#test
df %>% na.omit() %>% 
ggplot(aes(x = Date, y = relative_diff, color=Stock)) +
  geom_line(stat = "identity", size=1) +
  #geom_hline(yintercept=0, linetype="dashed", color = "black",size=2)+
  
  
  geom_point(size=4)+
  theme(legend.position = "top", legend.key=element_blank(),panel.background =element_rect(fill = "white",colour = "white"),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.7),
        axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
        text = element_text(size = 15),legend.text=element_text(size=12))+
  scale_x_date(breaks = df$Date)+
  annotate("rect", fill = "green", alpha = 0.1, 
           xmin = Sys.Date()-20, xmax = Sys.Date(),
           ymin = 0, ymax = Inf) +
  annotate("rect", fill = "red", alpha = 0.1, 
           xmin = Sys.Date()-20, xmax = Sys.Date(),
           ymin = 0, ymax = -Inf)



###
visualize_index<-function(df, stock){
  new = df
  new$index = 0
  ncols <- ncol(new) -1
  for (i in 1:nrow(new)){
    new$index[i] = ((rowSums(new[i,2:ncols])) / (ncol(new)-2))
  }
  new <- new[colnames(df)=="Date" | colnames(df)=="index" | colnames(data)==stock]
  colnames(new)[2] <- "stock"
  for (i in 1:nrow(new)){
    new$stock_pct[i] = new$stock[i]/new$stock[1]
    new$index_pct[i] = new$index[i]/new$index[1]
  }
  new = new[,c(1,4,5)]
  new = gather(new, Instrument, Change, stock_pct, index_pct)
  ggplot(new, aes(x = Date, y = Change, color = Instrument)) +
    geom_line(stat = "Identity")+
    theme(panel.background =element_rect(fill = "white",colour = "white"),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5),
          axis.text.x=element_text(size=rel(1.2), angle = 45, vjust = 1, hjust=1),axis.text.y=element_text(size=rel(1)),
          text = element_text(size = 15),legend.text=element_text(size=12))+
    scale_color_manual(labels = c("Index", stock), values = c("blue", "red")) +
    ylab("Change")+ labs(title=paste(stock, " vs index"))+
    xlab("Date")
}







########################################
##############Make the UI of the app#########
##############################################
#https://shiny.rstudio.com/articles/build.html
#Make the shiny app
library(shinydisconnect)
# Define user interface
ui <- fluidPage(
  
  #control for internet connection
  disconnectMessage(),
  actionButton("disconnect", "Disconnect the app"),
  
  # App title
  headerPanel("Financial advisor"),
  
  
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
    tabsetPanel(
      tabPanel("Chosen stock", plotOutput("Results") %>% withSpinner(color="#00a2ed")),
      tabPanel("Top 5 stocks", plotOutput("Best_5") %>% withSpinner(color="#00a2ed"),
               plotOutput("Best_5_relative_change") %>% withSpinner(color="#00a2ed")),
      
      tabPanel("Index", plotOutput("Index") %>% withSpinner(color="#00a2ed")),
      tabPanel("Bottom 5 stocks", plotOutput("Worst_5") %>% withSpinner(color="#00a2ed"),
               plotOutput("Worst_5_relative_change") %>% withSpinner(color="#00a2ed")),
      tabPanel("Check stock price",
               sidebarPanel(
                 selectizeInput(
                   'stock_check', 'Which stock should we check?',choices = tq_index("SP600")$symbol, multiple = TRUE),
                 numericInput(
                   'lowest_value', 'What is the lowest closing price',  value=1000)),
               
               useShinyalert(),  # Set up shinyalert
               actionButton("preview", "Check stock")
      )
      #tabPanel("Plot", helpText("Something"))
    )
  )
)


# assemble inputvalues to output values
server <- function(input, output, session) {
  
  #check if user has internet
  if(has_internet()==F){
    showModal(modalDialog(
      title = "Internet warning",
      "You do not have internet connection. Connect to internet and restart the app!"
    ))
  }

  #End session (close app window)
  observeEvent(input$disconnect, {
    session$close()
  })
  
  data<-get_all_stock_data()#Get prices for all the stocks
  
  output$Results <- renderPlot({
    
    shiny::validate(
      need(input$date[1]<input$date[2], "Please select a time period where date 1 is smaller than date 2")
    )#The end date must be larger than the start date
    
    shiny::validate(
      need(input$ticker !="" & length(input$ticker) ==1, "Please select a stock (and only one stock)")
    )#
    
    company=input$ticker
    stock_prices  <- data.frame(tq_get(company, get = "stock.prices", from = input$date[1], to= input$date[2]))
    print_stock_prices(stock_prices, company)
    
  })
  #Plot top 5 best stocks
  output$Best_5<-renderPlot({
    best_stocks<-get_best_or_worst_stocks(data, "best")
    vizulize_stocks(best_stocks, "Best")
  })
  output$Best_5_relative_change<-renderPlot({
    best_stocks<-get_best_or_worst_stocks(data, "best")
    vizulize_relative_change(best_stocks, "Best")
  })
  
  #Plot stock vs index last 20 days
  output$Index<-renderPlot({
    shiny::validate(
      need(input$ticker !="" & length(input$ticker) ==1, "Please select a stock (and only one stock)")
    )
    visualize_index(data, input$ticker)
  })
  
  #Plot top 5 worst stocks
  output$Worst_5<-renderPlot({
    worst_stocks<-get_best_or_worst_stocks(data, "worst")
    vizulize_stocks(worst_stocks, "Worst")
  })
  
  output$Worst_5_relative_change<-renderPlot({
    worst_stocks<-get_best_or_worst_stocks(data, "worst")
    vizulize_relative_change(worst_stocks, "Worst")
  })
  
  
  #Alert
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    
    stock<-input$stock_check #The stock we want to test
    current_value = data[nrow(data),stock] #The value of this stock 
    if(input$lowest_value > current_value) {
      shinyalert(paste("Oops! The current stock price of ", stock, "
                         is low! It is only ", toString(round(current_value,2)), " dollars"), type = "warning")
      
    }else shinyalert(text=paste("You are fine! The current close price is ", round(current_value,2)), type = "info",
                     showCancelButton=T)
    
  })
}

#Make the shiny app
shinyApp(ui, server)

