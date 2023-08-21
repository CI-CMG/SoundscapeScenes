# NNET testing with HMD 1-minute data labeled with Dets and RRPCA 
rm(list=ls()) 

library(caret)
library(nnet)

# Load or create your dataset (replace "your_data" with your actual dataset)
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetsRpca", full.names = T)
pltf = 0

# Set the seed for reproducibility
set.seed(123)
subsample_percentage = 0.10 
sample_step = 10 
TRAIN = NULL
KEEP = NULL
for (f in 1:length(inFiles)) {
  
  load(inFiles[f])
  
  num_rows <- nrow(tst) # Calculate the number of rows
  start_row <- sample(1:num_rows, size = 1) # randomly select a start row
  
  # Calculate indices to be sampled
  indices_to_sample <- c()
  current_index <- start_row
  # Loop until all desired indices are sampled
  while (length(indices_to_sample) < num_rows/sample_step) {
    indices_to_sample <- c(indices_to_sample, current_index)
    current_index <- current_index + sample_step
    if (current_index > num_rows) {
      current_index <- current_index - num_rows
    }
  }
  
  # Subsample the matrix based on the selected indices
  train_matrix <- tst[indices_to_sample, , drop = FALSE]
  pT = (nrow(train_matrix)/ num_rows) *100
  TRAIN = rbind(TRAIN, train_matrix)
  
  # Create a logical vector indicating rows to keep
  rows_to_keep <- rep(TRUE, num_rows)
  rows_to_keep[indices_to_sample] <- FALSE
  keep_matrix  <-  tst[rows_to_keep, , drop = FALSE]
  pK = (nrow(keep_matrix)/ num_rows) *100
  KEEP  = rbind(KEEP, keep_matrix)
  
  cat(basename( inFiles[f] ), ":", pT ," % TRAIN",  pK," % KEEP \n" )
}

sum( duplicated((TRAIN)) )
sum( duplicated((KEEP)) )

 # PLAYING AROUND ####

# Create an index for data partitioning (70% for training, 30% for testing)
train_index <- createDataPartition(your_data$Category, p = 0.7, list = FALSE)

# Create training and testing datasets
training_data <- your_data[train_index, 2:ncol(your_data) ]
testing_data <- your_data[-train_index, 2:ncol(your_data) ]

# Assuming you want to predict the Species column
unique(your_data$Category)

target_col <- "Category"

# Prepare the formula for the nnet model
formula <- as.formula(paste(target_col, "~ ."))

# Train the neural network model
nnet_model <- nnet(formula, data = training_data, size = 5, MaxNWts = 500, trace = FALSE)

# Make predictions on the testing dataset
predictions <- predict(nnet_model, newdata = testing_data, type = "class")

# Compare predictions with actual values
confusion_matrix <- table(predictions, testing_data$Species)
print(confusion_matrix)





