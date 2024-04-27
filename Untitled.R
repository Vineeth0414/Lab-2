# Check with getwd() and change with setwd() as needed.

# Loading the student data using base R read.csv function
students <- read.csv("/Users/vineethgoudboda/Downloads/studentInfo.csv")

# Display the unique values in the 'final_result' column
unique_results <- unique(students$final_result)
print(unique_results)

# Check if "Distinction" and "Pass" exist in unique_results
if ("Distinction" %in% unique_results & "Pass" %in% unique_results) {
  # Data preprocessing: Transform 'final_result' into a binary variable and 'disability' into a factor
  students$pass <- as.factor(ifelse(students$final_result == "Distinction" | students$final_result == "Pass", 1, 0))
} else {
  # Use existing values in the 'final_result' column to create the 'pass' variable
  students$pass <- as.factor(ifelse(students$final_result %in% unique_results, 1, 0))
}

# Convert 'studied_credits' and 'imd_band' accordingly
students$studied_credits <- as.factor(students$studied_credits)  # Assuming 'studied_credits' is the column name for credits
students$imd_numeric <- as.numeric(factor(students$imd_band, levels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")))

# Creating train and test sets manually
set.seed(20230712)  # Setting seed for reproducibility
sample_count <- floor(0.8 * nrow(students)) # Splitting the data into 80% as train data and 20% as test data.
training <- sample(seq_len(nrow(students)), size = sample_count)

train_data <- students[training, ]
test_data <- students[-training, ]

# Building a logistic regression model with glm (Generalized Linear Model) in base R
logit_model <- glm(pass ~ studied_credits + imd_numeric, family = binomial(link = "logit"), data = train_data)

# Model summary display
summary(logit_model)

# Plotting the model
plot(logit_model)

# Prediction on test data
tryCatch({
  test_predictions <- predict(logit_model, newdata = test_data, type = "response")
}, error = function(e) {
  print("Error occurred during prediction:")
  print(e)
})

# Check if test_predictions is created
if (!exists("test_predictions")) {
  print("test_predictions object not found.")
} else {
  # Creating predicted outcome
  predicted_outcome <- ifelse(test_predictions > 0.5, 1, 0)
  
  # Accuracy calculation
  true_outcomes <- as.numeric(test_data$pass) - 1  # Adjusting factor levels from 1 to 0 and 1 for comparison
  model_accuracy <- mean(predicted_outcome == true_outcomes)
  print(paste("Model Accuracy:", model_accuracy))
}


