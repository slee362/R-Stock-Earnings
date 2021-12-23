library(shiny)
library(quantmod)
library(rvest)
library(lubridate)
library(data.table)
library(ggplot2)

#function to get dates of earnings releases off of Yahoo
earnings = function(symbol)
{
  webadd <- paste0("https://finance.yahoo.com/calendar/earnings?failsafe=1&ynet=0&_device=desktop&device=desktop&symbol=",symbol)
  data <- read_html(webadd)
  data <- data %>% html_nodes("table") %>% html_table_fix()
  data <- as.data.frame(data)
  data[,"Earnings Date"] <- as.POSIXct(as.character(data[,"Earnings Date"]), format = "%b %d, %Y")
  data = data[,"Earnings Date"]
  data = data[data<Sys.Date()]
  data = substring(data, nchar(as.character(data)) - 9)
  data = unique(data)
  data
}

#function (taken from Github & not my code) because fun: "html_table" caused issues
html_table_fix <- function(x, header = NA, trim = TRUE,
                           fill = FALSE, dec = ".") {
  
  stopifnot(html_name(x) == "table")
  
  # Throw error if any rowspan/colspan present
  rows <- html_nodes(x, "tr")
  n <- length(rows)
  cells <- lapply(rows, "html_nodes", xpath = ".//td|.//th")
  
  ncols <- lapply(cells, html_attr, "colspan", default = "1")
  # Replace empty values of colspan with "1"
  ncols <- lapply(ncols, function(x) {x[x==""] <- "1"; x})
  ncols <- lapply(ncols, as.integer)
  nrows <- lapply(cells, html_attr, "rowspan", default = "1")
  nrows <- lapply(nrows, as.integer)
  
  p <- unique(vapply(ncols, sum, integer(1)))
  maxp <- max(p)
  
  if (length(p) > 1 & maxp * n != sum(unlist(nrows)) &
      maxp * n != sum(unlist(ncols))) {
    # then malformed table is not parsable by smart filling solution
    if (!fill) { # fill must then be specified to allow filling with NAs
      stop("Table has inconsistent number of columns. ",
           "Do you want fill = TRUE?", call. = FALSE)
    }
  }
  
  values <- lapply(cells, html_text, trim = trim)
  out <- matrix(NA_character_, nrow = n, ncol = maxp)
  
  # fill colspans right with repetition
  for (i in seq_len(n)) {
    row <- values[[i]]
    ncol <- ncols[[i]]
    col <- 1
    for (j in seq_len(length(ncol))) {
      out[i, col:(col+ncol[j]-1)] <- row[[j]]
      col <- col + ncol[j]
    }
  }
  
  # fill rowspans down with repetition
  for (i in seq_len(maxp)) {
    for (j in seq_len(n)) {
      rowspan <- nrows[[j]][i]; colspan <- ncols[[j]][i]
      if (!is.na(rowspan) & (rowspan > 1)) {
        if (!is.na(colspan) & (colspan > 1)) {
          # special case of colspan and rowspan in same cell
          nrows[[j]] <- c(utils::head(nrows[[j]], i),
                          rep(rowspan, colspan-1),
                          utils::tail(nrows[[j]], length(rowspan)-(i+1)))
          rowspan <- nrows[[j]][i]
        }
        for (k in seq_len(rowspan - 1)) {
          l <- utils::head(out[j+k, ], i-1)
          r <- utils::tail(out[j+k, ], maxp-i+1)
          out[j + k, ] <- utils::head(c(l, out[j, i], r), maxp)
        }
      }
    }
  }
  
  if (is.na(header)) {
    header <- all(html_name(cells[[1]]) == "th")
  }
  if (header) {
    col_names <- out[1, , drop = FALSE]
    out <- out[-1, , drop = FALSE]
  } else {
    col_names <- paste0("X", seq_len(ncol(out)))
  }
  
  # Convert matrix to list to data frame
  df <- lapply(seq_len(maxp), function(i) {
    utils::type.convert(out[, i], as.is = TRUE, dec = dec)
  })
  names(df) <- col_names
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  
  if (length(unique(col_names)) < length(col_names)) {
    warning('At least two columns have the same name')
  }
  
  df
}

#function to create 6-day range around each earnings release date
range_days = function(earn_date, stock_data, num_days)
{
    pos <- which(index(stock_data) == earn_date)
    rev <- index(stock_data[pos-num_days])
    fwd = index(stock_data[pos+num_days])
    stock_data = stock_data[paste0(rev, "/", fwd)]
    stock_data
}

#function to index list and return correct days (beginning and end) for cumulative return calc. for desired earnings period
list_return = function(l)
{
  return(l[c(1,7)])
}
  
# Define server logic 
shinyServer(function(input, output){
  
    options("getSymbols.warning4.0" = FALSE)
    
    #1 - Ticker All-Time Earnings Performance
    output$totalPlot <- renderPlot({
        
        #initialize ticker and stock data
        symbol = input$tick
        earningsdate=earnings(symbol)
        df = getSymbols(symbol, from = "1900-01-01", auto.assign = FALSE)
        
        #use ticker stock data and corresponding earnings to find range and returns of each earnings period
        range_return = lapply(as.list(1:length(earningsdate)), function(x) range_days(earn_date = as.Date(earningsdate[x]), stock_data = df, num_days = 3))
        range_return = lapply(range_return, Ad)
        range_return = lapply(range_return, list_return)
        range_return = lapply(range_return, function(x) ROC(x,n=1,"discrete")*100)
        range_ret2 = lapply(range_return, function(x) na.omit(coredata(x)))
        range_ret2 = as.data.frame(range_ret2)
        range_ret2 = transpose(range_ret2)
    
        #create boxplot of ticker's all-time earnings performance
        if(median(range_ret2$V1) >= 0)
          {
          boxplot(range_ret2, ylab="Earnings Period % Return", col = "green", main = symbol)
        } else {
          boxplot(range_ret2, ylab="Earnings Period % Return", col = "red", main = symbol)
        }
    })
    
    #2Show stock chart around user-inputted earnings period
    output$chooseEarning <- renderPlot({
      
      #get ticker and corresponding earnings data
      symbol = input$tick
      earningsdate=earnings(symbol)
      
      #make sure user cannot input number of quarters back prior to company's first earnings
      if(input$bins>length(earningsdate)){
        chosen_date = tail(earningsdate)
      }else{
        chosen_date = earningsdate[input$bins]
      }
      #set days around chosen earnings period and call stock data
      df = getSymbols(symbol, from = "1900-01-01",  auto.assign = FALSE)
      a = which(index(df) == chosen_date)
      fwd = index(df[a + 4])
      backwd = index(df[a - 2])
      df = getSymbols(symbol, from = backwd, to = fwd,  auto.assign = FALSE)
      
      #create candle chart
      myPars <- chart_pars()
      myPars$mar <- c(6, 6, 0, .2)
      chart_Series(df,pars=myPars, name =symbol)
      x_vline = xts(TRUE, as.Date(chosen_date))
      add_TA(x_vline, on = -1, col = "lightgrey", pch = 1)
      
    }
      
    )
    #3 earnings line chart 
    output$allEarning <- renderPlot({
      #initialize ticker, earnings, and stock data
      symbol = input$tick
      earningsdate=earnings(symbol)
      df = getSymbols(symbol, from = "1900-01-01", auto.assign = FALSE)
      
      #calculate daily stock returns three days around each earnings date
      range_return = lapply(as.list(1:length(earningsdate)), function(x) range_days(earn_date = as.Date(earningsdate[x]), stock_data = df, num_days = 3))
      range_return = lapply(range_return, Ad)
      range_return = lapply(range_return, function(x) ROC(x,n=1,"discrete")*100)
      range_ret2 = lapply(range_return, function(x) na.omit(coredata(x)))
      
      #draw line chart for all earnings dates
      plot(cumsum(range_ret2[[1]]), type ='l', ylim = c(-25,25), ylab = "Daily % Return", xlab = "Day")
      #ifstatement if user inputs greater number of earnings period than company
      if(input$bins > length(range_ret2))
      {
        for(i in 1:length(range_ret2))
        {
          lines(cumsum(range_ret2[[i]]))
        }
      }else{
        for(i in 1:input$bins)
        {
          lines(cumsum(range_ret2[[i]]))
        }
      }
    })
    
    #4 Seasonality Analysis
    output$quarterEarning <- renderPlot({
      #initialize ticker and stock data
      symbol = input$tick
      earningsdate=earnings(symbol)
      df = getSymbols(symbol, from = "1990-01-01", auto.assign = FALSE)
      
      #create empty vectors
      q1 = list()
      q2 = list()
      q3 = list()
      q4 = list()
      
      #loop to group earnings dates by quarter of the year
      for(i in 1:length(earningsdate)){
        tmp = earningsdate[i]
        if(quarter(earningsdate[[i]]) ==1 ){
          q1 = append(q1, tmp)
        } else if(quarter(earningsdate[[i]])==2){
          q2 = append(q2, tmp)
        }else if(quarter(earningsdate[[i]])==3){
          q3 = append(q3, tmp)
        }else{
          q4 = append(q4, tmp)
        }
      }
      #create dataframe for q1 earnings releases with 6-day period % returns
      as.character(q1)
      q1 = lapply(as.list(1:length(q1)), function(x) range_days(earn_date = q1[[x]], stock_data = df, num_days = 3))
      q1 = lapply(q1, Ad)
      q1 = lapply(q1, list_return)
      q1 = lapply(q1, function(x) ROC(x,n=1,"discrete"))
      q1 = lapply(q1, function(x) na.omit(coredata(x)))
      q1 = as.data.frame(q1)
      q1 = transpose(q1)
      q1$Quarter <- "Q1"
      
      #q2 repeat
      as.character(q2)
      q2 = lapply(as.list(1:length(q2)), function(x) range_days(earn_date = q2[[x]], stock_data = df, num_days = 3))
      q2 = lapply(q2, Ad)
      q2 = lapply(q2, list_return)
      q2 = lapply(q2, function(x) ROC(x,n=1,"discrete"))
      q2 = lapply(q2, function(x) na.omit(coredata(x)))
      q2 = as.data.frame(q2)
      q2 = transpose(q2)
      q2$Quarter <- "Q2"
      
      #q3 repeat
      as.character(q3)
      q3 = lapply(as.list(1:length(q3)), function(x) range_days(earn_date = q3[[x]], stock_data = df, num_days = 3))
      q3 = lapply(q3, Ad)
      q3 = lapply(q3, list_return)
      q3 = lapply(q3, function(x) ROC(x,n=1,"discrete"))
      q3 = lapply(q3, function(x) na.omit(coredata(x)))
      q3 = as.data.frame(q3)
      q3 = transpose(q3)
      q3$Quarter <- "Q3"
      
      #q4 repeat
      as.character(q4)
      q4 = lapply(as.list(1:length(q4)), function(x) range_days(earn_date = q4[[x]], stock_data = df, num_days = 3))
      q4 = lapply(q4, Ad)
      q4 = lapply(q4, list_return)
      q4 = lapply(q4, function(x) ROC(x,n=1,"discrete"))
      q4 = lapply(q4, function(x) na.omit(coredata(x)))
      q4 = as.data.frame(q4)
      q4 = transpose(q4)
      q4$Quarter <- "Q4"
      
      #merge four quarter-grouped dataframes into one & change decimals to percents
      season_earnings = rbind(q1, q2, q3, q4)
      season_earnings$V1 = season_earnings$V1*100
      
      #plot boxplot
      ggplot(season_earnings, aes(x=Quarter, y=V1, fill=Quarter)) + geom_boxplot() +ylab("6-Day Earnings-Period % Return") 
    })
})
