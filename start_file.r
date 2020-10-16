library(httr)
library(jsonlite)
library(tidyquant)

#DU ER KUUUULERE! 

#ISS API
res = GET("http://api.open-notify.org/astros.json")
res
rawToChar(res$content)
data = fromJSON(rawToChar(res$content))
names(data)
data$people

res = GET("http://api.open-notify.org/iss-pass.json", query = list(lat = 40.7, lon = -74))
res    

#Tidyquant
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

data <- getSymbols(c("AAPL", "AMZN", "MSFT"), from = "2020-09-10",
           to = "2020-09-28",warnings = FALSE,
           auto.assign = TRUE)

AAPL
AMZN
MSFT

sp500 <- tq_index("SP500")



### Alpha vantage
api_key <- "FLM01ESUYD833KGT"


function(input, output, session) {
  
  data <- reactivePoll(1000, session,
                       # This function returns the time that log_file was last modified
                       checkFunc = function() {
                         if (file.exists(log_file))
                           file.info(log_file)$mtime[1]
                         else
                           ""
                       },
                       # This function returns the content of log_file
                       valueFunc = function() {
                         read.csv(log_file)
                       }
  )
  
  output$dataTable <- renderTable({
    data()
  })
}
