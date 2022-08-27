library(dplyr)
toy_data = data.frame(c1 = sample(letters[1:5], 25, replace = TRUE), 
                      c2 = sample(LETTERS[1:5], 25, replace = TRUE))

print.data.frame(toy_data)

table0 <- table(toy_data$c1, toy_data$c2)
print.table(table0)

table1 <- as.data.frame.matrix(table0) # convert it to dataframe
print.data.frame(table1)

table2 <- prop.table(table0, margin = 1) %>% 
  as.data.frame.matrix() # convert it to dataframe   
# have a look at the table
print.data.frame(table2, digits = 2)

table3 <- prop.table(table0, margin = 2) %>% 
  as.data.frame.matrix() # convert it to dataframe   
# have a look at the table
print.data.frame(table3, digits = 2)