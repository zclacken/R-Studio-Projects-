# Assign $100 to variable X
x <- 100

# Arithmetic operators
# Addition
3 + 4

# Subtraction
8 - 2

# Multiplication
5*9

# Division
8 / 2

# Exponentiation
2^5



# Modulo
9 %% 2


# Assign $150 in variable y
y <- 150

# Add x and y
x + y

# Add x and y, save the result to new variable z
z <- x + y


# Create variable x with a vector (1, 2, 3, 4, 5)
x <- c(1:5)
x

# Create variable y with a vector (6, 7, 8, 9, 10)
y <- c(6:10)
y

# Save the result of multiplying x and y to z
z <- x * y
z


# Create a variable for starting_mysaving
starting_saving <- 100

# How much do you have at the end of January?
jan_mysaving <- starting_saving*1.03
jan_mysaving


# How much do you have at the end of February?
feb_mysaving <- starting_saving*1.03*1.05
feb_mysaving

#or
feb_mysaving <- jan_mysaving*1.05
feb_mysaving


# Assign the numeric 120 to samsung_stock
samsung_stock <- 120

# Assign the charater "AA" to credit_rating
credit_rating <- "AA"

# You like the Samsung stock. TURE or FALSE?
answer <- FALSE

# Print answer
answer

# Check data type
class(samsung_stock)
class(credit_rating)
class(answer)


# Create a character vector of bond credit ratings
credit_rating <- c("AAA", "AA", "BBB", "BB", "B")
credit_rating

logic <- c(TRUE, TRUE, FALSE)
logic


# Put returns into a vector
ret <- c(5, 2)

# Add names on vector ret
names(ret) <- c("Jan", "Feb")
ret


# Weighted average
# Weights and returns
ret_A <- 3
ret_B <- 5
weight_A <- 0.3
weight_B <- 0.7

# Portfolio return
ret_portfolio <- weight_A*ret_A+weight_B*ret_B
ret_portfolio

# Alternative way (Portfolio return)
ret <- c(3, 5)
weight <- c(0.3, 0.7)

mul_ret_weight <- ret*weight

ret_portfolio <- sum(mul_ret_weight)
ret_portfolio

# Equally weighted average
weight <- .5
mul_ret_weight <- ret*weight
ret_portfolio <- sum(mul_ret_weight)
ret_portfolio


# Vector subsetting
# 6 months of returns
ret <- c(5, 4, 6, 2, 1, 4)

# Select the first month return
first_ret <- ret[1]
first_ret

# First month return 
first_ret <- ret["Jan"]
first_ret

# Selecting by name
# Add names on vector ret
names(ret) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
ret

#Select the first month by name
ret["Jan"]


# Creating 2x2 matrix
matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)

# Creating 2x2 matrix using a vector
mat_vector <- c(1, 2, 3, 4)
matrix(data = mat_vector, nrow = 2, ncol = 2)


# Creating vectors of stock prices
samsung <- c(123.49, 124.39, 123.83, 127.39, 128.39, 127.48, 129.83, 128.93, 129.23)
apple <- c(100.12, 102.53, 101.29, 102.93, 104.21, 103.39, 104.95, 104.25, 105.47)

# # cbind (combining by column)
cbind(samsung, apple)

# rbind (combining by row)
rbind(samsung, apple)

# Plot the matrix for stock prices of Samsung and Apple.
samsung_apple_matrix <- cbind(samsung, apple)
plot(samsung_apple_matrix)


# Calculation of correlation coefficient
cor(samsung, apple)

# Creating a correlation matrix
cor(samsung_apple_matrix)

# Select the first row and the second column
samsung_apple_matrix[1, 2]

# Select the first three rows
samsung_apple_matrix[1:3, ]

# Select the entire first column
samsung_apple_matrix[, 1]

# Select the entire second column by name
samsung_apple_matrix[, "apple"]


# Create data frame
stock_dataframe <- data.frame(company = c("Samsung", "Apple", "IBM"), stock_price = c(110, 104, 100), num_shares = c(1000, 800, 1200))
stock_dataframe

# Alternative way to create data frame
company <- c("Samsung", "Apple", "IBM")
stock_price <- c(110, 104, 100)
num_shares <- c(1000, 800, 1200)
data.frame(company, stock_price, num_shares) 


# Print out the first 2 rows
head(stock_dataframe, n = 2)

# Print out the last 2 rows
tail(stock_dataframe, n = 2)

# Check out the structure of data frame, stock_dataframe
str(stock_dataframe)

# Change the column names to "comp_name", "stockprice", and "numshares", respectively
colnames(stock_dataframe) <- c("comp_name", "stockprice", "numshares")

# Print out the column names of stock_dataframe
colnames(stock_dataframe)




# Select the first row
stock_dataframe[1, ]

# Select the second column
stock_dataframe[ , 2]

# Select the first column by name
stock_dataframe[ , "comp_name"]

# Select the first column
stock_dataframe$comp_name

# Select the stock prices of Apple
subset(stock_dataframe, comp_name == "Apple")

# Add new column, sales, to data frame
stock_dataframe$sales <- c(1000, 1200, 1400)
stock_dataframe


# Create a factor of credit ratings
credit_rating <- c("AAA", "AA", "A", "BBB", "AA", "A", "BB", "A", "BBB", "B")

# Create a factor of credit ratings
credit_factor <- factor(credit_rating)
credit_factor

# Access the levels of the factor
levels(credit_factor)

# Change the name of the factor levels
levels(credit_factor) <- c("Single_A", "Double_A", "Triple_A", "Single_B", "Double_B", "Triple_B")
credit_factor

# Summarize factor levels
summary(credit_factor)

# Create plot of factor levels
plot(credit_factor)




# Create a numeric vector including 1 to 100 ranking
AA_ranking <- c(3, 45, 23, 18, 95, 38, 58, 67, 73, 83, 74, 27, 4, 48, 38, 28, 21, 13, 98, 73)

# Create a factor with 4 evenly spaced groups
AA_factor <- cut(AA_ranking, breaks = c(0, 25, 50, 75, 100))
AA_factor

# Rename the levels
levels(AA_factor) <- c("Low", "Medium", "High", "Very High")
AA_factor

# Create an ordered factor
credit_factor_ordered <- factor(credit_rating, ordered = TRUE, levels = c("AAA", "AA", "A", "BBB", "BB", "B"))
credit_factor_ordered


# Print credit_factor
credit_factor

# Remove the Triple_A from credit_factor
credit_factor[-1]

credit_factor[-1, drop = TRUE]



# Create data frame (stringsAsFactors = FALSE)
stock_dataframe <- data.frame(company = c("Samsung", "Apple", "IBM"), stock_price = c(110, 104, 100), num_shares = c(1000, 800, 1200), stringsAsFactors =  FALSE)
str(stock_dataframe)

# Create list components
name <- "Correlation Matrix: Samsung and Apple Stock"
stockprice_samsung <- c(100.0, 101.3, 103.4, 100.9, 99.2)
stockprice_apple <- c(110.3, 112.8, 114.1, 111.2, 109.8)
cor_matrix <- cor(cbind(stockprice_samsung, stockprice_apple))

# Create a list
my_portfolio <- list(name, stockprice_samsung, stockprice_apple, cor_matrix)

# See the list
my_portfolio


# Create a list with names of components
my_portfolio_new <- list(Portfolio_Name = name, Samsung = stockprice_samsung, Apple = stockprice_apple, Correlation = cor_matrix)
my_portfolio_new


# Create a list with names of components using names() function
names(my_portfolio) <- c("Portfolio Name", "Samsung", "Apple", "Correlation")
my_portfolio

# Subsetting a list
my_portfolio[1]

my_portfolio[c(1, 2)]

my_portfolio$Correlation



# Adding new component, weight
my_portfolio$Weight <- c(Samsung = 0.4, Apple = 0.6)
my_portfolio


# Adding IBM stock prices in the list, my_portfolio
my_portfolio$IBM <- c(150.6, 153.2, 145.6, 147.8, 143.5)
my_portfolio


# Adding IBM stock prices in the list, my_portfolio
my_portfolio$IBM <- NULL
my_portfolio

# Split
group <- stock_dataframe$company
split_stock_dataframe <- split(stock_dataframe, group)
split_stock_dataframe

# Unsplit
unsplit_stock_dataframe <- unsplit(split_stock_dataframe, group)
unsplit_stock_dataframe

# Access each company's number of shares column and apply transfroms
split_stock_dataframe$Samsung$num_shares <- split_stock_dataframe$Samsung$num_shares*2
split_stock_dataframe$Apple$num_shares <- split_stock_dataframe$Apple$num_shares*3
split_stock_dataframe$IBM$num_shares <- split_stock_dataframe$IBM$num_shares*4
split_stock_dataframe

# Recombine separated data frames back into one data frame
new_stock_dataframe <- unsplit(split_stock_dataframe, group)
new_stock_dataframe


# See attributes of stock_dataframe
attributes(stock_dataframe)

# Access an attribute, names
attr(stock_dataframe, which = "names")
