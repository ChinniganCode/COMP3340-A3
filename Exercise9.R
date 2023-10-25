library(dplyr)

# Read the CSV file
data <- read.csv("datasets/USPresidency.csv", header = TRUE)

incumbents <- filter(data, Target == 1)
challengers <- filter(data, Target == 0)

generate_arff <- function(df, relation_name) {
  header <- sprintf("@RELATION %s\n\n", relation_name)

  attributes <- sapply(names(df), function(att) {
    if (att != "Year" && att != "Target") {
      return(sprintf("@ATTRIBUTE %s {0, 1}", att))
    }
  })

  attributes <- attributes[!is.na(attributes)]

  instances <- apply(df[, -c(1, 14)], 1, function(row) {
    paste(row, collapse = ",")
  })

  arff_content <- paste(
    header,
    paste(attributes, collapse = "\n"),
    "\n@DATA\n",
    paste(instances, collapse = "\n")
  )

  return(arff_content)
}

# Write to ARFF files
incumbents_arff <- generate_arff(incumbents, "incumbents")
writeLines(incumbents_arff, "outputs/incumbents.arff")

challengers_arff <- generate_arff(challengers, "challengers")
writeLines(challengers_arff, "outputs/challengers.arff")