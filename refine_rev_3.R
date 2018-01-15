### Springboard ###
## Chapter 3: Data Wrangling

# Exercise 1: Basic Data Manipulation


### Step 0: Import refine_original.csv as a data frame.
mydata <- read.csv(file = "/Users/Kevin/Documents/refine_original.csv", header=TRUE, stringsAsFactors = FALSE)

### Step 1: Clean up brand names

# Convert all strings in the first column to lower case 
mydata$company <- tolower(mydata$company)

# Create a vector identifying all of the companies
companies <- c("philips", "akzo", "van houten", "unilever")

# Use a nested 'for' loop to correct spelling and punctuation errors
for(i in 1:length(companies)) {
  for (j in 1:nrow(mydata)){
    if (adist(mydata$company[j], companies[i]) <= 4) {
      mydata$company[j] <- companies[i]
    }
  }
}

### Step 2: Separate product and code number using the 'word' function in the 'stringr' package
library(stringr)
product_code <- word(mydata$Product.code...number, 1, sep = "-")
product_number <- word(mydata$Product.code...number, -1, sep = "-")
mydata <- cbind(mydata, product_code, product_number)

### Step 3: Add a new column to the table identifying the product categories
# Create two new vectors to identify the product codes and matching product categories
code <- c("p", "v", "x", "q")
category <- c("Smartphone", "TV", "Laptop", "Tablet")

# Create a new vector to by used as the new product_category column
product_category <- rep(NA, length(product_code))

new.func <- function(product_code, code, category) {
  for (i in 1:length(product_code)) {
    product_category[i] <- category[which(product_code[i] == code)]  # all values of 'code' are compared with the value of 'product_code' and if the two are equal, the 'which' statement returns the cell reference.
  }
  return(product_category)
}
mydata$product_category <- new.func(product_code, code, category) # Call the function and add the new column

### Step 4: Add full address for geocoding
for (i in 1:nrow(mydata)) {
  mydata$full_address[i] <- paste(mydata$address[i], mydata$city[i], mydata$country[i], sep=", ")
}

### Step 5A: Use a function to create a new column for each company with a 1 or 0 corresponding to the name of the company in the 'company' column.
generic_column <- rep(0, times=nrow(mydata))

binary <- function(company_column, comp_name) {
  for (i in 1:nrow(mydata)) {
    if (mydata$company[i]==comp_name) {
      company_column[i] <- 1
    }
  }
  return(company_column)
}

mydata$company_philips <- binary(generic_column, "philips")
mydata$company_akzo <- binary(generic_column, "akzo")
mydata$company_van_houten <- binary(generic_column, "van houten")
mydata$company_unilever <- binary(generic_column, "unilever")

### Step 5B: Create dummy variables for product category.  The function and code in Step 5A could be modified to accomplish this step as well.

product_smartphone <- rep(0, times=nrow(mydata))
product_smartphone[grep("Smartphone", mydata$product_category)] <- 1

product_tv <- rep(0, times=nrow(mydata))
product_tv[grep("TV", mydata$product_category)] <- 1

product_laptop <- rep(0, times=nrow(mydata))
product_laptop[grep("Laptop", mydata$product_category)] <- 1

product_tablet <- rep(0, times=nrow(mydata))
product_tablet[grep("Tablet", mydata$product_category)] <- 1

mydata <- cbind(mydata, product_smartphone, product_tv, product_laptop, product_tablet)

### Generate a csv file
write.csv(mydata, "/Users/Kevin/Documents/refine_clean_rev_3.csv", quote = TRUE)

