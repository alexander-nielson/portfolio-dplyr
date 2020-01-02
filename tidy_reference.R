#Gather and Spreading 101
#

#General formatting

# spread(
#     name_of_data,
#     column_wanted_spread,
#     column_that_contains_values_to_spread_against
# )
# 
# gather(
#     yourdata,
#     column_name_to_gather_selected_columns_into,
#     column_name_to_gather_values_into,
#     select_desired_columns
# )

library(tidyr)
# Create a messy dataset
messy <- data.frame(
    country = c("A", "B", "C"),
    q1_2017 = c(0.03, 0.05, 0.01),
    q2_2017 = c(0.05, 0.07, 0.02),
    q3_2017 = c(0.04, 0.05, 0.01),
    q4_2017 = c(0.03, 0.02, 0.04))
messy

#notice that messy is currently wide data. 

# Reshape the data
make_messy_long <- gather(
    messy,
    quarter,
    growth,
    (2:5)
)

tidier <- gather(
    messy,
    quarter,
    growth,
    -country
)

spread(
    tidier,
    quarter,
    growth
)


separate_tidier <-tidier %>%
    separate(quarter, c("Qrt", "year"), sep ="_")

head(separate_tidier)
    
unite_tidier <- separate_tidier %>%
    unite(Quarter, Qrt, year, sep ="_")
head(unite_tidier)
