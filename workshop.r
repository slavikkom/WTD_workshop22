test <- read.csv("./data/test.csv")


# Set the initial parameters for Dry edge estimation
minimal_ndvi <- 0.1 # min NDVI value for which the Dry edge will be calculated
maximal_ndvi <- 0.7 # max NDVI value for which the Dry edge will be calculated
sub_number <- 10 # number of subintervals in each interval
step <- 0.001 #  step for intervals

# Within the NDVI-STR space we derive the min STR value for each NDVI subinterval
# This min STR value is arranged with the median NDVI value of each NDVI subinterval
max_i <- (maximal_ndvi - minimal_ndvi) / step
print(max_i)
median_ndvi_intervals <- list()
min_str_intervals <- list()
for (i in 1:max_i) {

  current_low <- minimal_ndvi + step*(i-1)*(maximal_ndvi - minimal_ndvi)
  current_high <- minimal_ndvi + step*i*(maximal_ndvi - minimal_ndvi)
  
  # filter dataframe by current_high > ndvi > current_low
  current_df <- test[(test$NDVI < current_high) & (test$NDVI >= current_low),]
  
  # derive the min STR within the working subinterval 
  min_str <- max(current_df$STR)
  min_str_intervals[[length(min_str_intervals)+1]] <- min_str
  
  # derive the median NDVI value of the working subinterval
  current_median_ndvi <- median(current_df$NDVI)
  median_ndvi_intervals[[length(median_ndvi_intervals)+1]] <- current_median_ndvi
}


# min STR values within each NDVI subinterval
print(unlist(min_str_intervals))

subinterval_data <- data.frame(STR=unlist(min_str_intervals), NDVI=unlist(median_ndvi_intervals))
#  NDVI AND STR VALUES FOR DRY EDGE ESTIMATION

# Create the empty variables for the further loop

total_int_number <- max_i / sub_number # number of intervals

# Each NDVI interval has subintervals within which we derived min STR values.
# Now, within each interval we calculate the median and std of min STR values.
# Within each interval, we filter out min STR values that are bigger than median min STR + std min STR.
# The remained min STR values are averaged (median) and associated with the median NDVI value within each interval.
filtered_min_str <- list()
filtered_median_ndvi <- list()
# total_int_number
for (i in 1:total_int_number) {
  #
  current_data_chunk <- subinterval_data[round((i-1)*sub_number, 0):round(i*sub_number, 0),]
  # Within each interval find the min STR values that is lower than median min STR + std min STR
  str_threshold <- median(current_data_chunk$STR) + sd(current_data_chunk$STR)

  # Filter based on this condition
  filtered_data <- current_data_chunk[current_data_chunk$STR < str_threshold,]
    
  # Average remained min STR values for each interval and calculate median NDVI within each interval
  filtered_min_str[[length(filtered_min_str)+1]] <- median(filtered_data$STR)
  filtered_median_ndvi[[length(filtered_median_ndvi)+1]] <- median(filtered_data$NDVI)
}

# NDVI and STR values that are used for further dry edge estimation
print(unlist(filtered_min_str))




# PARAMETERS FOR DRY EDGE

# Linear model with all the min STR and NDVI values
interval_data <- data.frame(STR=unlist(filtered_min_str), NDVI=unlist(filtered_median_ndvi))
relation <- lm(STR~NDVI, data=interval_data)

print(relation)

# To ensure that there are not STR outliers for dry edge estimation - 
# we calculate RMSE of the resulted points. We filter out the min STR
# values that do not lie within the range modelled min STR ± 2 * RMSE
double_rmse <- 2*sqrt(c(crossprod(relation$residuals))/ length(relation$residuals))
print(double_rmse)
residuals_abs <- abs(relation$residuals)

filtered_interval_data <-interval_data[residuals_abs < double_rmse,]
relation2 <- lm(filtered_interval_data$NDVI~filtered_interval_data$STR)

print(relation2)

