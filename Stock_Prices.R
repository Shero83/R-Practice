
######################################################################
# Examples of R packages useful for automating data extractions
# D3M, NYU STERN
#####################################################################

# Example 1: Stock Prices: package "quantmod"
# http://www.quantmod.com/

# install.packages('quantmod')
library(quantmod)

# Get YAHOO! Price from Google Finance
getSymbols("YHOO",  src="google")

# Get Google Price from Yahoo Finance (can use the source as google or yahoo however pay attention to the date range)
getSymbols("GOOG",src="google")  

# Built in graphics in the package for typical stock price data
barChart(GOOG) 

# All kind of options avaialble. Visit the web page to learn more
candleChart(GOOG,multi.col=TRUE) 

# Add labels to your chart
chartSeries(GOOG,name="GOOGLE Stock Prices") 

# Simple aggregation to get WEEKLY vs. Daily returns
chartSeries(to.weekly(GOOG),up.col='blue',dn.col='red') 



#########################################################################
# Example 2: QUANDL--this a startup with ambition to become Wikipedia for DATA
# All kinds of data avaialble. Visit the web page and create an account
# There is also an EXCEL add in to get data directly to Excel
#########################################################################

install.packages("Quandl")

library(Quandl)

# Stock price data from their WIKI. This has more useful information since 
# if provides adjusted price for Dividends/stock splits as well

Quandl.auth('UsrosziwzonbVrV8pote')
FB_stock = Quandl("WIKI/FB")

# You can get multiple stocks at one time that are automatically merged by time
google_apple = Quandl (c("WIKI/GOOG","WIKI/AAPL" ))

# Save data to csv and analyze in Tableau
write.csv(FB_stock, "FB_Stock_Prices.csv")
