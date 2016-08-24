### R version of returnCalculations spreadsheet 
### and is assignment 1 of Computational Finance

# Assign the URL to the CSV file
data_url <- "http://assets.datacamp.com/course/compfin/sbuxPrices.csv"

# Download file
download.file(data_url, destfile="sbuxPrices.csv", method="curl")
# Load the data frame using read.csv
sbux_df = read.csv("./sbuxPrices.csv", header = TRUE, stringsAsFactors = FALSE)

# Check the structure of 'sbux_df'
str(sbux_df)

# Check the first and last part of 'sbux_df'
head(sbux_df)
tail(sbux_df)

# Get the class of the Date column of 'sbux_df'
class(sbux_df$Date)

# Assign to the variable closing_prices all the adjusted closing prices while preserving the dimension information.
closing_prices <- sbux_df[, 2, drop=FALSE]
# alternatively this could be sbux_df[, "Adj.Close", Drop=FALSE]
closing_prices <- sbux_df[, "Adj.Close", drop=FALSE]

# Find indices associated with the dates 3/1/1994 and 3/1/1995
index_1 <- which(sbux_df$Date == "3/1/1994")
index_2 <- which(sbux_df$Date == "3/1/1995")

# Extract prices between 3/1/1994 and 3/1/1995
some_prices <-  sbux_df[index_1:index_2, 2]


# Create a new data frame that contains the price data with the dates as the row names
sbux_prices_df <- sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) <- sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.
price_1 <- sbux_prices_df["3/1/1994", 1]
price_2 <- sbux_prices_df["3/1/1995", 1]

## Plot chart
# Now add all relevant arguments to the plot function below to get a nicer plot
plot(sbux_df$Adj.Close, type="l", col="blue", lwd=2, ylab="Adjusted close", main="Monthly closing price of SBUX")
legend(x='topleft',legend='SBUX', lty=1, lwd=2, col='blue')

## Calculate simple returns
# Denote n the number of time periods
n <- nrow(sbux_prices_df)
Pt = sbux_prices_df[2:n,1]
Ptminus1 = sbux_prices_df[1:(n-1),1]
sbux_ret <- (Pt - Ptminus1) / Ptminus1

# Notice that sbux_ret is not a data frame object
class(sbux_ret)

# Now add dates as names to the vector and print the first elements of sbux_ret to the console to check
somenames <- sbux_df[2:nrow(sbux_df),1]
names(sbux_ret) <- somenames
head(sbux_ret)

## Compute continuously compounded 1-month returns
Pt = sbux_prices_df[2:n,1]
Ptminus1 = sbux_prices_df[1:(n-1),1]
sbux_ccret <- log(Pt) - log(Ptminus1) 

# Assign names to the continuously compounded 1-month returns
somenames <- sbux_df[2:nrow(sbux_df),1]
names(sbux_ccret) <- somenames

# Show sbux_ccret
head(sbux_ccret)

## Compare the simple and cc returns
head(cbind(sbux_ret, sbux_ccret))

## Plot Simple and CC returns on the same chart
# Plot the returns on the same graph
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# Add horizontal line at zero
abline(h=0)

# Add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# add sbux_ccret to the chart
lines(sbux_ccret, type="l", col="red", lwd=2)

# Compute gross returns
sbux_gret <- sbux_ret + 1

# Compute future values
sbux_fv <- cumprod(sbux_gret)

# Plot the evolution of the $1 invested in SBUX as a function of time
plot(sbux_fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")

