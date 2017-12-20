# Data Wrangling Exercise 1: Basic Data Manipulation
# Springboard
# Section 3.1 (4)


# Step 0: Import refine_original.csv as a data frame.

mydata <- read.csv(file = "/Users/Kevin/Documents/refine_original.csv", header=TRUE, stringsAsFactors = FALSE)

# Step 1: Clean up brand names

mydata$company <- tolower(mydata$company)

for (i in 1:nrow(mydata)){
  if (adist(mydata$company[i], "philips") <= 3) {
    mydata$company[i] <- "philips"
  }
  if (adist(mydata$company[i], "akzo") <= 2) {
    mydata$company[i] <- "akzo"
  }
  if (adist(mydata$company[i], "unilever") <= 2) {
    mydata$company[i] <- "unilever"
  }
}

# Step 2: Separate product and code number

product_code <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_code)

for (i in 1:nrow(mydata)) {
  mydata$product_code <- substr(mydata$Product.code...number, start=1, stop=1)
}

product_number <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_number)

for (i in 1:nrow(mydata)) {
  mydata$product_number <- substr(mydata$Product.code...number, start=3, stop=4)
}

# Step 3: Add product categories

product_category <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_category)

for (i in 1:nrow(mydata)) {
  if (mydata$product_code[i] == "p") {
    mydata$product_category[i] <- "Smartphone"
  } else {
    if (mydata$product_code[i] == "v") {
      mydata$product_category[i] <- "TV"
    } else {
      if (mydata$product_code[i] == "x") {
        mydata$product_category[i] <- "Laptop"
      } else {
        if (mydata$product_code[i] == "q") {
          mydata$product_category[i] <- "Tablet"
        }
      }
    }
  }
}

# Step 4: Add full address for geocoding

full_address <- rep("none", times=nrow(mydata))
mydata <- cbind(mydata, full_address, stringsAsFactors=FALSE)

for (i in 1:nrow(mydata)) {
  mydata$full_address[i] <- paste(mydata$address[i], mydata$city[i], mydata$country[i], sep=", ")
}

# Step 5: Create dummy variables for company and product category

company_philips <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, company_philips)

company_akzo <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, company_akzo)

company_van_houten <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, company_van_houten)

company_unilever <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, company_unilever)

for (i in 1:nrow(mydata)) {
  if (mydata$company[i] == "philips") {
    mydata$company_philips[i] <- 1
  } else {
    if (mydata$company[i] == "akzo") {
      mydata$company_akzo[i] <- 1
    } else {
      if (mydata$company[i] == "van houten") {
        mydata$company_van_houten[i] <- 1
      } else {
        if (mydata$company[i] == "unilever") {
          mydata$company_unilever[i] <- 1
        }
      }
    }
  }
}

product_smartphone <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_smartphone)

product_tv <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_tv)

product_laptop <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_laptop)

product_tablet <- rep(0, times=nrow(mydata))
mydata <- cbind(mydata, product_tablet)

for (i in 1:nrow(mydata)) {
  if (mydata$product_code[i] == "p") {
    mydata$product_smartphone[i] <- 1
  } else {
    if (mydata$product_code[i] == "v") {
      mydata$product_tv[i] <- 1
    } else {
      if (mydata$product_code[i] == "x") {
        mydata$product_laptop[i] <- 1
      } else {
        if (mydata$product_code[i] == "q") {
          mydata$product_tablet[i] <- 1
        }
      }
    }
  }
}

# Generate a csv file
write.csv(mydata, "/Users/Kevin/Documents/refine_clean.csv", quote = TRUE)
