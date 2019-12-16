# load the RDS file created in `geocoding_script.R`
businesses <- readRDS("business_licenses_geocoded.rds")

# reduce to downtown South Bend, 2008 to 2018, and required columns
reduced <- businesses %>%
    filter(Street_Address != "0 OUTSIDE CITY LIMITS",
           State == "IN",
           City == "SOUTH BEND",
           Zip_Code == "46601",
           License_Fiscal_Year >= 2008) %>%
    select("name" = Business_Name,
           "category" = Classification_Description,
           "year" = License_Fiscal_Year,
           "address" = Full_Address, 
           lon,
           lat)

# combine similar / identical categories
reduced$category[str_detect(reduced$category, "RESTAURANT")] <- "RESTAURANT"
reduced$category[str_detect(reduced$category, "TAXI")] <- "TAXI Services"
reduced$category[str_detect(reduced$category, "TATTOO")] <- "TATTOO AND PIERCING"
reduced$category[str_detect(reduced$category, "MASSAGE")] <- "MASSAGE"
reduced$category[str_detect(reduced$category, "OPEN AIR VENDORS")] <- "OPEN AIR VENDORS (Public/Private)"
reduced$category[str_detect(reduced$category, "TRANSIENT MERCHANT")] <- "TRANSIENT MERCHANT"

# further clean up of values
reduced$category[reduced$category == "#N/A"] <- NA
reduced$category <- str_to_title(reduced$category)

# catches all variants of "do not use"
reduced$name[str_detect(reduced$name, "- DO|-DO")] <- NA 

# isolate chains' names, eg. Subway #1, Subway #2 becomes Subway
reduced$name <- str_remove(reduced$name, "#.+$")
reduced$name[str_detect(reduced$name, "MC DONALD'S")] <- "Mc Donald's"
reduced$name <- str_to_title(reduced$name)

reduced$address[str_detect(reduced$address, "OUT OF AREA")] <- NA
reduced$address <- str_to_title(reduced$address)


# remove rows with missing values
reduced <- na.omit(reduced)

# remove duplicated rows
reduced <- distinct_all(reduced)


# store so don't have to run on app
saveRDS(reduced, file = "business_clean.rds")
write_csv(reduced, "business_clean.csv")





