library(haven)

# Read the original Stata dataset
survey <- read_dta("Project-5-Political-Survey/MEX1979-Guadalajara-Study-v1.0/MEX1979-Guadalajara Study-v1.0.dta")

# Check the dimensions and variable names
dim(survey)
names(survey)
str(survey)

# Export the original dataset to CSV
write.csv(survey, "mexico_survey_1979.csv", row.names = FALSE)

# Extract variable names and descriptions
codebook <- data.frame(
  variable = names(survey),
  label = sapply(survey, function(x) {
    label <- attr(x, "label")
    if (is.null(label)) NA_character_ else label
  }),
  row.names = NULL
)

# Export the variable descriptions
write.csv(codebook, "mexico_variable_labels.csv", row.names = FALSE)

# Extract response labels from labelled variables
response_labels <- do.call(
  rbind,
  lapply(names(survey), function(var) {
    
    labels <- attr(survey[[var]], "labels")
    
    if (is.null(labels)) return(NULL)
    
    data.frame(
      variable = var,
      value = unname(labels),
      response = names(labels)
    )
  })
)

# Export response labels
if (!is.null(response_labels)) {
  write.csv(response_labels,"mexico_response_labels.csv", row.names = FALSE)
}

# Confirm the files were created
list.files(pattern = "mexico_.*\\.csv$")

# Import the translated CSV files
variable_labels_en <- read.csv("Project-5-Political-Survey/mexico_variable_labels_english.csv")
response_labels_en <- read.csv("Project-5-Political-Survey/mexico_response_labels_english.csv")

# Create a copy of the original survey
survey_english <- survey

# Replace numeric responses with English labels
for (var in unique(response_labels_en$variable)) {
  
  if (var %in% names(survey_english)) {
    
    lookup <- response_labels_en[
      response_labels_en$variable == var, 
    ]
    
    # Match numeric responses to their English labels
    codes <- as.character(survey_english[[var]])
    matched <- match(codes, as.character(lookup$value))
    
    translated <- lookup$response_english[matched]
    
    # Preserve unmatched numeric values
    translated[is.na(matched) & !is.na(codes)] <-
      codes[is.na(matched) & !is.na(codes)]
    
    survey_english[[var]] <- translated
  }
}

# Attach English descriptions to the columns
for (var in names(survey_english)) {
  
  matched <- match(var, variable_labels_en$variable)
  
  if (!is.na(matched)) {
    attr(survey_english[[var]], "label") <-
      variable_labels_en$label_english[matched]
  }
}

# Export the English-labelled dataset
write.csv(
  survey_english,
  "mexico_survey_1979_english.csv",
  row.names = FALSE
)

# Verify the results
dim(survey_english)
head(survey_english)
head(survey_english$v1)

# View the English description of a variable
attr(survey_english$v1, "label")

# Save the original survey dataset
saveRDS(
  survey,
  "Project-5-Political-Survey/mexico_survey_1979_original.rds"
)

# Save the English-labelled survey dataset
saveRDS(
  survey_english,
  "Project-5-Political-Survey/mexico_survey_1979_english.rds"
)

# Verify that both files were created
list.files(
  "Project-5-Political-Survey",
  pattern = "\\.rds$"
)
