# BAN400 Term Paper

## How to read the R-script
The R-script is called ****** and it contains code run by a shiny app. To start the app one needs install all necesary packages. Further, one needs to run all the code in the script. Finally, when one runs the last code line in the script *shinyApp(ui, server)* the app will be initialized.  

## About the project
In this project we use stock data from Yahoo finance which one can uploade direcly into R-studio by the use of the *tidyquant* library. This library has stock data from S&P600. We use the function *tq_get* to get the data from Yahoo. For instance, if we want stock data about apple, google and facebook we can call the following function:  

**c("AAPL", "GOOG", "FB") %>%
    tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")**  

If one tries to get stock data about a stock that does not exsist or there is an error prohibits the function from extracting the data, the function will retun a warning and *NA*.    

### Number of stocks included
We have chosen not to include all stocks in S&P600. This is due to computing time. It takes a lot of time to upload stock prices for multiple dates for all 600 stocks. Therefore, in the beginning of the project we have specified that we only wish to include 100 stocks (*stocks_to_include=100*).  
*(you can see which stocks we included by running the code: tq_index("SP600")$symbol[1:100]* 

#### Our own index
As we in this project pretend that there are only 100 stocks in the S&P600, we wish to compare them to an index which is based on these 100 stocks. Therefore, we make our own index where the daily price equals the sum of the prices of all the stocks divided by the number of stocks.  

### Fucntions in the project
All self made functions in the projects have docstrings. This means that anyone is unsure about how a function works one can call docstring(*name of function*). In many functions there are also example(s) that gives the user the oportunity to vislize graphts outside the shiny app.  

One important note is that on should run all functions before checking the docstring. This is because some functions depends on data from other functions.   

## About our Shiny app
Our Shiny app presents stock information in four tabs. Each tab shows different information.  

In the first tab the user selects a stock and a time period. After some loading time the tab will visulize the price of this stock in the given time period. It will also visulize how the stock price has changed from day one and compare it with our self made index.    

The second tab shows the user which stocks are the best stocks the last 20 days whicle the tab shows which stocks has been the worst the last 20 days. The best/worst stocks are the ones that has highest/lowest relative change in price from day 1 to day 20.  

The fourth tab helps you check if the price of your favorite stock is above a certian threshold. If it is not one gets a warning message. The tab also visulize the close price of the chosen stock the last 20 trading days. The line in the graph will be green if the price has stayed above the threshold while it will be red in the time period it was/is under the threshold. 
Note that you will not see the graph before you have selected a new stock and clicked on the *Check stock* button. Also, it migth take some time before the button works as the app needs to have time to upload the data.


## What have we done to avoid errors
We have included various controll functions in the server so that the app will be less likely to crash if the user uses is wrongly.  

### Internet conection
The app needs internet conection in order to download the data from Yahoo finance. Therefore, the first check the server does is to check if the user has internet connection. If this is not the case then the user will get a warning message which is visable in 5 seconds before the app closes.  

### Validate input
The Shiny app uses the *validate*-function to control that the user input is correct. Hence, whenever it is possible for the user to fill in an input argument we have made a condition ensuring that the user will not crash the app. One example where we check if the user selected a stock and only one stock (and not more) as input is show bellow:  
 **shiny::validate(
      need(stock !="" & length(stock) ==1, "Please select a stock (and only one stock)"))**  
      

    


