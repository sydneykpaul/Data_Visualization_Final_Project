# setup
library(ggmap)
business_licenses <- read_csv("Business_Licenses.csv")

# geocode
register_google(key = "AIzaSyAe2Brb2eyF0ZeyEncpk_36JQkC-o9Xyvg")
geocoded_df <- geocode(as.character(business_licenses$Full_Address), output = "more")

# combine back with businesses
business_licenses_geocoded <- cbind(business_licenses, geocoded_df)

# store so that we won't have to geocode again
saveRDS(business_licenses_geocoded, file = "business_licenses_geocoded.rds")
write_csv(business_licenses_geocoded, "business_licenses_geocoded.csv")