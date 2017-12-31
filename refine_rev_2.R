### Springboard ###
## Chapter 3: Data Wrangling

# Exercise 1: Basic Data Manipulation


# Step 0: Import refine_original.csv as a data frame.

mydata <- read.csv(file = "/Users/Kevin/Documents/refine_original.csv", header=TRUE, stringsAsFactors = FALSE)

# Step 1: Clean up brand names

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

# Step 2: Separate product and code number using the 'word' function in the 'stringr' package
library(stringr)
product_code <- word(mydata$Product.code...number, 1, sep = "-")
product_number <- word(mydata$Product.code...number, -1, sep = "-")
mydata <- cbind(mydata, product_code, product_number)

# Step 3: Create a new vector and add product categories before adding vector to table as a new column

product_category <- rep(0, times=nrow(mydata))
product_category[grep("p", mydata$product_code)] <- "Smartphone"
product_category[grep("v", mydata$product_code)] <- "TV"
product_category[grep("x", mydata$product_code)] <- "Laptop"
product_category[grep("q", mydata$product_code)] <- "Tablet"

mydata <- cbind(mydata, product_category)

# Step 4: Add full address for geocoding

for (i in 1:nrow(mydata)) {
  mydata$full_address[i] <- paste(mydata$address[i], mydata$city[i], mydata$country[i], sep=", ")
}

# Step 5A: Create dummy variables for company names

company_philips <- rep(0, times=nrow(mydata))
company_philips[grep("philips", mydata$company)] <- 1

company_akzo <- rep(0, times=nrow(mydata))
company_akzo[grep("akzo", mydata$company)] <- 1

company_van_houten <- rep(0, times=nrow(mydata))
company_van_houten[grep("van houten", mydata$company)] <- 1

company_unilever <- rep(0, times=nrow(mydata))
company_unilever[grep("unilever", mydata$company)] <- 1

mydata <- cbind(mydata, company_philips, company_akzo, company_van_houten, company_unilever)

# Step 5B: Create dummy variables for product category

product_smartphone <- rep(0, times=nrow(mydata))
product_smartphone[grep("Smartphone", mydata$product_category)] <- 1

product_tv <- rep(0, times=nrow(mydata))
product_tv[grep("TV", mydata$product_category)] <- 1

product_laptop <- rep(0, times=nrow(mydata))
product_laptop[grep("Laptop", mydata$product_category)] <- 1

product_tablet <- rep(0, times=nrow(mydata))
product_tablet[grep("Tablet", mydata$product_category)] <- 1

mydata <- cbind(mydata, product_smartphone, product_tv, product_laptop, product_tablet)

# Generate a csv file
write.csv(mydata, "/Users/Kevin/Documents/refine_clean_rev_2.csv", quote = TRUE)

