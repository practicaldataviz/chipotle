setwd("~/Dropbox/pdv/data/chipotle")
library(dplyr)
library(stringr)

# load order data
orders <- read.csv("orders.tsv", sep="\t", stringsAsFactors = FALSE)

# expand duplicated orders into multiple rows
for(rownum in 1:nrow(orders)) {
  quantity <- orders$quantity[rownum]
  while(quantity > 1) {
    new_rows <- rep(orders[rownum, ], times = 1)
    orders <- rbind(orders, new_rows)
    quantity <- quantity - 1
  }
}

# drop quantity column, as it is no longer needed
orders$quantity <- NULL

# Drop diet sodas and water, because they will not contribute to calorie totals
orders <- subset(orders, item_name != "Bottled Water")
orders <- subset(orders, !str_detect(choice_description, "Diet"))

# Create categories of items: drinks, sides, or entrees
# Drinks and sides are defined here, any other item is classified as an entree
drink_list <- c("Bottled Water", 
                "Canned Soda", 
                "Canned Soft Drink",
                "Izze", 
                "Nantucket Nectar",
                "6 Pack Soft Drink")

side_list <- c("Chips",
               "Chips and Fresh Tomato Salsa",
               "Chips and Guacamole",
               "Chips and Mild Fresh Tomato Salsa",
               "Chips and Roasted Chili Corn Salsa",
               "Chips and Roasted Chili-Corn Salsa",
               "Chips and Tomatillo Green Chili Salsa",
               "Chips and Tomatillo-Green Chili Salsa",
               "Chips and Tomatillo Red Chili Salsa",
               "Chips and Tomatillo-Red Chili Salsa",
               "Side of Chips")


# condense item information into single string: item_description
orders$item_description <- ifelse(orders$choice_description == "NULL", 
                                  orders$item_name,
                                  NA)

orders$item_description <- ifelse(orders$item_name %in% drink_list,
                                  orders$item_name,
                                  orders$item_description)

orders$item_description <- ifelse(is.na(orders$item_description),
                                  paste(orders$item_name, str_replace_all(orders$choice_description, pattern="\\[|\\]", ""), sep=", "),
                                  orders$item_description)

orders$item_description <- str_replace_all(orders$item_description, pattern=fixed(" and "), ", ")

# Generate a list of all possible components in an item
items <- str_split(orders$item_description, ", ")
items <- unlist(items)
items <- unique(items)

# write item list to file, manually add calorie counts
# write.csv(items, "item_list.csv", row.names=FALSE)

##############################################
## Calculate calorie counts of each item (row)
# read in calorie count data

calorie_counts <- read.csv("calorie_counts.csv", stringsAsFactors=FALSE)

calculate_calories <- function(item_description){
  calorie_total <- 0
  components <- unlist(str_split(item_description, ", "))
  for (component in components) {
    component_calories <- calorie_counts$calories[calorie_counts$item==component]
    calorie_total <- component_calories + calorie_total
  }
  return(calorie_total)
}

orders$calorie_count <- NA
orders$calorie_count <- sapply(orders$item_description, FUN=calculate_calories)

# label items as entrees, sides, or drinks

orders$item_type <- NA
orders$item_type[orders$item_name %in% drink_list] <- "drink" 
orders$item_type[orders$item_name %in% side_list] <- "side" 
orders$item_type[is.na(orders$item_type)] <- "entree" 

# drop old columns
# orders$item_name <- NULL
orders$choice_description <- NULL
orders$item_price <- NULL

#################################################
### Split orders into meals

# sort by order_id
orders <- orders[order(orders$order_id),]

# Assumptions: 
# 1. One entree per meal
# 2. Sides are split between all meals in an order
# 3. Drinks are split between all meals in an order

# Give each entree a unique index: entree_id

sides_and_drinks <- orders %>% 
                      group_by(order_id) %>%
                      summarize(
                        num_entrees = sum(item_type == "entree"),
                        order_side_calories = sum(calorie_count[item_type == "side"]),
                        order_drink_calories = sum(calorie_count[item_type == "drink"])
                        ) %>%
                      mutate(meal_side_calories = order_side_calories / num_entrees,
                             meal_drink_calories = order_drink_calories / num_entrees) %>%
                      filter(num_entrees > 0) # drop 2 orders without entrees

entrees <- orders %>% 
            arrange(order_id) %>%
            filter(item_type == "entree") %>%
            mutate(meal_id = row_number()) %>%
            rename(meal_entree_calories = calorie_count)

meals <- inner_join(entrees, sides_and_drinks) %>%
          mutate(meal_total_calories = meal_entree_calories + meal_side_calories + meal_drink_calories) %>%
          arrange(order_id, meal_id)

meals_out <- meals %>%
              select(order_id, 
                     meal_id, 
                     meal_entree_calories, 
                     meal_side_calories, 
                     meal_drink_calories, 
                     meal_total_calories) %>%
              rename(entree_calories = meal_entree_calories,
                     side_calories = meal_side_calories,
                     drink_calories = meal_drink_calories,
                     total_calories = meal_total_calories)

write.csv(meals_out, "meals.csv", row.names=FALSE)
