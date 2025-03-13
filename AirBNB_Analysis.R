# source("C:/Users/ianmh/OneDrive/Desktop/Shortcuts!/Programming/AirBNB/AirBNB_Analysis.R")
# https://www.kaggle.com/datasets/arianazmoudeh/airbnbopendata
# https://data.dathere.com/dataset/nyc-neighborhoods
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/

rm(list=ls())
setwd("C:/Users/ianmh/OneDrive/Desktop/Shortcuts!/Programming/AirBNB")

# Objectives
# Determine most popular areas for AirBNBs and their prices
# - Number of listings per area
# - Prices for popular areas
# - Average review rating per area
# - Availability

require(dplyr)
require(sf)
require(leaflet)
require(ggplot2)
require(ggthemes)
require(corrplot)
require(leaps)
require(randomForest)
require(caret)

data_raw <- read.csv("raw/AirBNB_Open_Data.csv", stringsAsFactors=FALSE)

# Clean data
# Duplicates
length(unique(data_raw$id)) == nrow(data_raw)

duplicate_count <- data.frame(table(data_raw$id))
length(duplicate_count$Freq[duplicate_count$Freq > 1]) # 541 duplicates
duplicates <- data_raw[data_raw$id %in% duplicate_count$Var1[duplicate_count$Freq > 1],]

data_v1 <- data_raw[!duplicated(data_raw$id),]

# Check missing values & empty vectors
sapply(data_v1, function(x) {
  count_NA <- sum(is.na(x))
  count_empty <- ifelse(
    is.na(sum(x=="")),
    0,
    sum(x==""))
  if(count_NA > 0 | count_empty > 0) {
    return(paste(count_NA, " NA values, ", count_empty, " empty vectors.", sep=""))
  } else return("No missing values or empty vectors")
})

cols_to_replace <- c("NAME", "host.name", "neighbourhood", "neighbourhood.group", "price", "service.fee", "house_rules", "license")

data_v2 <- data_v1
data_v2[cols_to_replace] <- as.data.frame(lapply(data_v2[cols_to_replace], function(x){
  x[x==""] <- NA
  x
}))

# Host Identity Verified
data_v2$host_identity_verified <- ifelse(
  data_v2$host_identity_verified=="verified",
  1,
  0)

# Empty Characters & Missing Values
sapply(data_v2, function(x) sum(x == ""))
colSums(is.na(data_v2))

data_v2$country[which(data_v2$country=="")] <- "United States"
data_v2[which(!data_v2$license==""),]
data_v2 <- data_v2[-which(!data_v2$license=="")[2],]


# Availability.365
data_v2$availability.365[data_v2$availability.365<0] <- 0
data_v2$availability.365[data_v2$availability.365>365] <- 365

# Fixing Neighbourhood.Group strings
data_v2$neighbourhood.group[which(data_v2$neighbourhood.group=="brookln")] <- "Brooklyn"
data_v2$neighbourhood.group[which(data_v2$neighbourhood.group=="manhatan")] <- "Manhattan"
data_v2$neighbourhood[which(data_v2$neighbourhood=="DUMBO")] <- "Dumbo"

# Imputation for neighborhood + neighborhood group
data_no_missing <- data_v2[which(!is.na(data_v2$lat) | !is.na(data_v2$long)),]

airbnb_sf <- st_as_sf(data_no_missing, coords=c("long", "lat"), crs=4326)
neighborhoods <- st_read("raw/Neighborhoods_Data.geojson")
neighborhoods$neighborhood[which(neighborhoods$neighborhood=="DUMBO")] <- "Dumbo"

st_crs(neighborhoods) == st_crs(airbnb_sf) # ensure coordinate reference system (CRS) matches

data_v3 <- st_join(airbnb_sf, neighborhoods, join=st_within)
data_v3 <- as.data.frame(data_v3)

data_v4 <- merge(data_v2, data_v3[, c("id", "neighborhood", "borough")],
                 by="id", all.x=TRUE)

data_v4$neighbourhood <- ifelse(
  is.na(data_v4$neighbourhood),
  data_v4$neighborhood,
  data_v4$neighbourhood
)

data_v4$neighbourhood.group <- ifelse(
  is.na(data_v4$neighbourhood.group),
  data_v4$borough,
  data_v4$neighbourhood.group
)

sum(is.na(data_v4$neighbourhood))
sum(is.na(data_v4$neighbourhood.group))

# Calculate and impute centroid for data with missing longitude/latitude
neighborhoods$centroid <- st_centroid(neighborhoods$geometry)

neighborhood_centroids <- data.frame(
  neighbourhood = neighborhoods$neighborhood,
  centroid_long = st_coordinates(neighborhoods$centroid)[, 1],
  centroid_lat = st_coordinates(neighborhoods$centroid)[, 2]
)

data_v5 <- merge(data_v4, neighborhood_centroids, by="neighbourhood", all.x=TRUE)

data_v5$long <- ifelse(
  is.na(data_v5$long),
  data_v5$centroid_long,
  data_v5$long
)

data_v5$lat <- ifelse(
  is.na(data_v5$lat),
  data_v5$centroid_lat,
  data_v5$lat
)

cols_to_remove <- c("country.code", "neighborhood", "borough", "centroid_long", "centroid_lat", "license")
cols_to_rename <- c("neighbourhood", "neighbourhood.group")
new_names <- c("neighborhood", "borough")

data_v6 <- data_v5[, setdiff(names(data_v5), cols_to_remove)]
names(data_v6)[match(cols_to_rename, names(data_v6))] <- new_names

# Convert Price and Service Fee to integer
data_v6$price <- gsub(pattern = "[$.,]", replacement="", data_v6$price)
data_v6$service.fee <- gsub(pattern = "[$.,]", replacement="", data_v6$service.fee)

# Convert all columns to preferred type
sapply(data_v6, typeof)

data_v6$borough <- as.character(data_v6$borough)
data_v6$neighborhood <- as.character(data_v6$neighborhood)
data_v6$host_identity_verified <- as.logical(data_v6$host_identity_verified)
data_v6$price <- as.numeric(data_v6$price)
data_v6$service.fee <- as.numeric(data_v6$service.fee)
data_v6$last.review <- as.Date(data_v6$last.review, "%m/%d/%Y")
data_v6$last.review[data_v6$last.review > as.Date("2022-12-31")] <- NA
data_v6$reviews.per.month <- as.integer(data_v6$reviews.per.month)

data <- data_v6

# save data for dashboard
file_path <- "clean/AirBNB_Data_Cleaned.RData"
if(!exists(file_path)){
  save(data, file=file_path)
}

# Leaflet Interactive Map
nyc_map <- neighborhoods[, names(neighborhoods) %in% c("neighborhood", "borough", "geometry")]
merge <- merge(nyc_map[, names(nyc_map) %in% c("neighborhood", "borough", "geometry")], 
               data %>% count(neighborhood), all = TRUE)

merge$popup <- paste("<strong>", merge$borough, "</strong><br>",
                     merge$neighborhood, "<br>",
                     "Number of AirBNB Listings (2023): ",
                     prettyNum(merge$n, big.mark = ","))

# Plot variables
lat_min <- -74.4
lat_max <- -73.55
long_min <- 40.45
long_max <- 40.95

NYCbins <- c(0,50,200,500,1000,3000,5000,8000)
NYCpal <- colorBin(palette = "YlGnBu", domain = merge$n, bins=NYCbins)

# Leaflet Interactive Map
leaflet() %>%
  addTiles() %>%
  setView(lng = -73.935242, lat = 40.730610, zoom = 10) %>%
  setMaxBounds(
    lng1 = -74.25, lat1 = 40.25,  # SW corner of NYC
    lng2 = -73.75, lat2 = 41   # NE corner of NYC
  ) %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addPolygons(
    data = merge,
    color = "blue",
    smoothFactor = 0.3,
    weight = 1,
    opacity = 0.5,
    fillColor = ~NYCpal(n),
    fillOpacity = 0.7,
    popup = ~popup,
    highlightOptions = highlightOptions(color = "#E2068A", weight = 2,
                                        bringToFront = TRUE,
                                        fillOpacity = 0.9)
  ) %>% 
  addLegend("bottomright",opacity = 1,
            colors =c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
            title = "AirBNB Listings (2023)</br>New York City",
            labels= c("<50","50 - 199","200 - 499","500 - 999","1,000 - 2,999","3,000 - 4,999","5,000 - 8,000"))

# Pie Chart of Borough
pie_borough <- as.data.frame(round(prop.table(table(data$borough)), 3))
pie_borough$Var1 <- factor(pie_borough$Var1, levels = c("Staten Island", "Bronx", "Queens", "Brooklyn", "Manhattan"))

ggplot(pie_borough, aes(x="", y=Freq, fill=factor(Var1))) +
  geom_bar(stat="identity", width=1, size=1, color="white") +
  coord_polar(theta="y") +
  geom_text(aes(x=ifelse(Freq < 0.025, 1.6, 1.8), label = scales::percent(Freq, accuracy=0.1)),
            position=position_stack(vjust=0.5)) +
  geom_col(aes(x=-0.5, y=0)) +
  labs(fill="Borough", y=NULL, x=NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Weighted Rating
global_avg_rating <- mean(data$review.rate.number, na.rm=TRUE)
min_reviews <- 10

data <- data %>% 
  mutate(weighted_rating = round((review.rate.number * number.of.reviews + global_avg_rating * min_reviews) / (number.of.reviews + min_reviews), 2))

# Boxplot of Weighted Rating by Borough
ggplot(data, aes(x=borough, y=weighted_rating, fill=borough)) +
  geom_boxplot() +
  labs(title="Average Weighted Rating",
       x="Borough",
       y="Rating") +
  theme_clean() +
  guides(fill="none")

# Boxplot of Weighted Rating by Neighborhood
avg_rating_neighborhood <- data %>% 
  group_by(borough, neighborhood) %>% 
  summarise(avg_wgt_rating = round(mean(weighted_rating, na.rm=TRUE), 2)) %>% 
  arrange(desc(avg_wgt_rating))

ggplot(filter(data, neighborhood %in% head(avg_rating_neighborhood$neighborhood, 10)), aes(x=neighborhood, y=weighted_rating, fill=borough)) +
  geom_boxplot() +
  labs(title="Weighted Rating by Neighborhood (Top 10)",
       x="Neighborhood",
       y="Weighted Rating") +
  theme_clean()

# Plotting weighted rating ~ price
ggplot(data, aes(x=price, y=weighted_rating, color=borough)) +
  geom_smooth(method="loess", se=FALSE) +
  labs(title="Price ~ Weighted Rating",
       x="Price",
       y="Weighted Rating") +
  theme_clean()

# Count of Room Type
room_type_by_borough <- data %>% 
  group_by(borough, room.type) %>% 
  summarise(count = n())
room_type_by_borough <- room_type_by_borough[room_type_by_borough$count > 100,]

ggplot(room_type_by_borough, aes(x=room.type, y=count, fill=borough)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=count), position=position_dodge(0.9), vjust=-0.5, size=3) +
  labs(title="Count of Room Types",
       x="Room Type",
       y="Count",
       fill="Borough") +
  scale_y_continuous(limits=c(0,30000), breaks=seq(0,30000,5000), expand=c(0,0)) +
  theme_clean()

# Distribution of Price Density, by Room Type
ggplot(data, aes(x=price, group=room.type, fill=room.type)) +
  geom_density(adjust=1.5, alpha=0.3) +
  facet_wrap(~ borough) +
  labs(fill="Room Type") +
  theme_clean() +
  theme(legend.position="bottom")

# Count of Room Type by Price
ggplot(data, aes(x=price, y=number.of.reviews, color=borough)) +
  geom_smooth(method="loess", se=FALSE) +
  theme_clean()

# Market Sizing
market_sizing <- data %>% 
  group_by(borough, room.type) %>% 
  summarise(mkt_size = sum((price * availability.365) / 1000000, na.rm=TRUE))

data %>% 
  group_by(borough, room.type) %>% 
  summarise(mean_price = mean(price, na.rm=TRUE))

market_sizing <- market_sizing %>% 
  mutate(size_category = ifelse(mkt_size > median(mkt_size), "High Revenue", "Low Revenue"))

ggplot(market_sizing, aes(x = borough, y = mkt_size, fill = room.type)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,3000,500), expand=c(0,0)) +
  labs(x = "Borough", y = "Market Size ($ Mil.)", fill = "Room Type") +
  theme_clean()

# Comparing median rent
rent_data <- read.csv("raw/StreetEasy_MedianAskingRent.csv", stringsAsFactors=TRUE, header=TRUE)
rent_data <- rent_data[, c(colnames(rent_data)[1:3], "X2022.01", "X2022.02", "X2022.03")]
rent_data$asking_rent <- coalesce(rent_data[, "X2022.02"], rent_data[,"X2022.03"], rent_data[,"X2022.01"])
rent_data <- rent_data[rent_data$areaType=="neighborhood", ]
colnames(rent_data)[1] <- "neighborhood"

revenue_data <- data %>% 
  filter(room.type == "Entire home/apt") %>% 
  group_by(neighborhood) %>% 
  summarise(avg_price = round(mean(price, na.rm=TRUE), 2))

scatter_data <- merge(rent_data, revenue_data, by="neighborhood")



stop()

# Modeling and Forecasting
# Data Preparation
data <- data[!is.na(data$price),]
data <- data[!is.na(data$availability.365),]
data <- data[data$availability.365!=0,]
data$revenue <- data$price * data$availability.365

data$neighborhood <- as.factor(data$neighborhood)
data$borough <- as.factor(data$borough)
data$host_identity_verified <- as.factor(data$host_identity_verified)
data$instant_bookable <- as.factor(data$instant_bookable)
data$cancellation_policy <- as.factor(data$cancellation_policy)
data$room.type <- as.factor(data$room.type)
data$Construction.year <- as.factor(data$Construction.year)

# Splitting data into train and test sets
smp_size <- floor(0.80 * nrow(data))
set.seed(123)
train_index <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_index,]
train$neighborhood <- as.numeric(as.factor(train$neighborhood))
test <- data[-train_index,]
test$neighborhood <- as.numeric(as.factor(test$neighborhood))

# Best Subsets
model_subsets <- regsubsets(price ~ neighborhood + borough + room.type + lat + long, data=train)
residual_summary_subsets <- summary(model_subsets)

# Index of Best Subsets model
data.frame(Adj.R2 = which.max(residual_summary_subsets$adjr2),
           CP = which.min(residual_summary_subsets$cp),
           BIC = which.min(residual_summary_subsets$bic))


# Random Forest
model_rf <- randomForest(price ~ neighborhood + borough + room.type + lat + long, data=train, maxcat=300, ntree=100, importance=TRUE)
rf_imp <- as.data.frame(importance(model_rf))
rf_imp$Var.Names <- row.names(rf_imp)

train_predictions <- predict(model_rf, train)
train_rmse <- sqrt(mean((train$price - train_predictions)^2))
r2 <- 1 - sum((train$price - train_predictions)^2) / sum((train$price - mean(train$price))^2)

test_predictions <- predict(model_rf, test)
test_rmse <- sqrt(mean((test$price - test_predictions)^2))

# Visualize Model Importance
ggplot(rf_imp, aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = 'skyblue') +
  geom_point(aes(size = IncNodePurity), color = "steelblue", alpha=0.6) +
  coord_flip() +
  theme_clean() +
  theme(legend.position = 'bottom',
        panel.grid.major.y = element_blank())


# K-Fold Cross Validation
train_control <- trainControl(method="cv", number = 5) # 5-fold CV
model_cv <- train(price ~ neighborhood + borough + room.type + lat + long,
                  data = train,
                  method = "rf",
                  trControl = train_control)
