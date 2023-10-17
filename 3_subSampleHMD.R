# NNET testing with HMD 1-minute data labeled with Dets and RRPCA 

# subsample, combine, export for matlab

rm(list=ls()) 

library(caret)
library(R.matlab)
library(nnet)


# Load datasets ####
inDir = (  "F:\\SanctSound\\analysis\\combineFiles_AcousticScene" )
inFiles = list.files( inDir, pattern = "HMDdetsRpca", full.names = T)
pltf = 0

# DIVIDE UP DATA and COMBINE 
# starts at random row (time step) and grabs every 10t
TRAIN = NULL
TEST = NULL
VALID = NULL

testPercentage  = 0.60 
sampleTest_step = 10 # minimum distance between samples
trainPercentage = 0.30 

#loop through each site and get samples for each category
for (f in 1:length(inFiles)) {
  
  load(inFiles[f])
 
  num_rows = nrow(tst) # Calculate the number of rows
  current_index = sample(1:num_rows, size = 1) # randomly select a start row
  
  #calculate number of samples in each category
  testSample  = round( num_rows*testPercentage )
  trainSample = round( num_rows*trainPercentage )
  validSample = num_rows - (testSample + trainSample)
  # CHECK: num_rows - (testSample + trainSample + validSample)
  
  # TEST data
  indices_to_sample <- c()
  while (length(indices_to_sample) < testSample) {
    indices_to_sample <- c(indices_to_sample, current_index)
    current_index <- current_index + sampleTest_step
    if (current_index > num_rows) {
      current_index <- current_index - num_rows
    }
  }
  #length( indices_to_sample ) - testSample
  keep_matrix  <-  tst[indices_to_sample, ]
  #CHECK: nrow(keep_matrix) - length(unique(keep_matrix$dateTime)) # all data is unique and correct length
  TEST  = rbind(TEST, keep_matrix)
  
  # TRAIN DATA
  # Create a logical vector indicating rows to keep
  rows_to_keep <- rep(TRUE, num_rows)
  rows_to_keep[indices_to_sample] <- FALSE
  other_matrix  <-  tst[rows_to_keep, , drop = FALSE]
  #CHECK: nrow(other_matrix) - (trainSample + validSample)
  num_rows = nrow(other_matrix) # Calculate the number of rows
  current_index = sample(1:num_rows, size = 1) # randomly select a start row
  indices_to_sample <- c()
  while (length(indices_to_sample) < trainSample) {
    indices_to_sample <- c(indices_to_sample, current_index)
    current_index <- current_index + sampleTest_step
    if (current_index > num_rows) {
      current_index <- current_index - num_rows
    }
  }
  train_matrix  <-  other_matrix[indices_to_sample, ]
  #CHECK: nrow(train_matrix) - length(unique(train_matrix$dateTime)) 
  TRAIN  = rbind(TRAIN, train_matrix)
  
  # VALID data
  rows_to_keep <- rep(TRUE, num_rows)
  rows_to_keep[indices_to_sample] <- FALSE
  valid_matrix  <-  other_matrix[rows_to_keep, , drop = FALSE]
  #CHECK: nrow(valid_matrix) - length(unique(valid_matrix$dateTime)) 
  VALID  = rbind(VALID, valid_matrix)
  
  cat( basename( inFiles[f] ), 
      "test=", nrow(keep_matrix), " ",
      "train", nrow(train_matrix)," ",
      "valid= ", nrow(valid_matrix)," ",
      " \n" )
}

rm( keep_matrix, other_matrix, sampled_rows, train_matrix,tst, valid_matrix) 
  
# FORMATTING FOR NNET (.mat format) ####
TRAIN$CategoryN = ifelse(TRAIN$Category== "Ambient", 1, 
                         ifelse(TRAIN$Category == "Bio", 2, 
                                ifelse(TRAIN$Category == "Bio+Anthro", 3, 
                                       ifelse(TRAIN$Category == "Anthro", 4 , TRAIN$Category) ) ))
TRAIN$CategoryN = as.numeric (as.character( TRAIN$CategoryN) )

KEEP$CategoryN = ifelse(KEEP$Category== "Ambient", 1, 
                        ifelse(KEEP$Category == "Bio", 2, 
                               ifelse(KEEP$Category == "Bio+Anthro", 3, 
                                      ifelse(KEEP$Category == "Anthro", 4 , KEEP$Category) ) ))

KEEP$CategoryN = as.numeric (as.character( KEEP$CategoryN) )

binTF = 1
clickTF = 0
inDir = inDir
outDir= inDir
saveName = "SS_AcousticScene_2023"
trainPerc = 60
validPerc = 10
trainSize = 4000
boutGab = 15
binWild = NULL
detWIld = NULL
validationTF =1

# CREATE DATA FRAMES ####
trainTestSetInfo = NULL                  # structure with 22 fields-- same
typeNames = unique(TRAIN$Category)       # cell array x by 1-- same

# "SS_AcousticScene_2023_bin_test.mat" 
testDataAll   = KEEP[1:1000,2:999]       # matrix: col = freq, sample = y
testLabelsAll = KEEP$CategoryN[1:1000]   # matrix: col = 1, sample = y

# "SS_AcousticScene_2023_bin_train.mat"
trainDataAll  = TRAIN[1:1000,2:999]      # matrix: col = freq, sample = y
trainLabelAll = TRAIN$CategoryN [1:1000] # matrix: col = 1, sample = y

# "SS_AcousticScene_2023_bin_validation.mat"
validDataAll = NULL
validLabelsAll = NULL




output_file <- paste0(inDir,"\\output.mat")
writeMat(output_file, A=typeNames, B = testDataAll)

# SAVE OUT- R version
write.csv(TRAIN , paste0(inDir,"\\TRAIN_LFAS.csv" ) )
write.csv(KEEP , paste0(inDir,"\\KEEP_LFAS.csv" ) )


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





