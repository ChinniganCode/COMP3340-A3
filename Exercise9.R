library("foreign")
library("RWeka")
library("arules")

# Read the dataset
data <- read.csv("datasets/USPresidency.csv", header = TRUE, sep = ",")
#prevent numeric
data$Year <- as.character(data$Year)
# Convert 0 to '?' and 1 to the respective 'Q' column name

to_transaction <- function(df) {
  # Remove non-Q columns
  df <- df[, startsWith(names(df), "Q")]

  # Create a transaction list
  trans_list <- apply(df, 1, function(row) {
    items <- names(row)[which(row == 1)]
    return(items)
  })

  return(as(trans_list, "transactions"))
}
remove_single_quotes <- function(arff_file_path) {
  arff_content <- readLines(arff_file_path, warn = FALSE)
  arff_content <- gsub("'", "", arff_content)
  writeLines(arff_content, con = arff_file_path)
}
print("START")
# Split the dataset based on the 'Target' value
incumbent_data <- subset(data, Target == 1)
challenger_data <- subset(data, Target == 0)

# Convert to desired format
incumbent_trans  <- to_transaction(incumbent_data)
challenger_trans  <- to_transaction(challenger_data)

# Apply Apriori on incumbent transactions
incumbent_rules <- apriori(incumbent_trans,
                           parameter = list(supp = 0.5, conf = 0.8, target = "rules"))

# Apply Apriori on challenger transactions
challenger_rules <- apriori(challenger_trans,
                            parameter = list(supp = 0.5, conf = 0.8, target = "rules"))

print("Apriori applied")

# Inspect the rules
inspect(incumbent_rules)
inspect(challenger_rules)

print("Data converted to transactions")
# Save as .arff format

write.arff(x=incumbent_data, file = "outputs/US-Incumbent.arff")
write.arff(x=challenger_data, file = "outputs/US-Challenger.arff")


remove_single_quotes("outputs/US-Incumbent.arff")
remove_single_quotes("outputs/US-Challenger.arff")

print("arff files saved")
incumbent_file <- read.arff("outputs/US-Incumbent.arff")
challenger_file <- read.arff("outputs/US-Challenger.arff")
print("arff files read")

#exclude tarkget
Apriori(edited_file)
