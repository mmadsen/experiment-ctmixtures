library(mmadsenr)

# In the simulations, I forgot to change the model class labels from equifinality-3-4.  Thus, we should recode them here
# before we go too much further.

new_labels <- function(df) {
  levels(df$model_class_label)[levels(df$model_class_label)=="allneutral"] <- "neutral"
  levels(df$model_class_label)[levels(df$model_class_label)=="mixantidom"] <- "anticonformist"
  levels(df$model_class_label)[levels(df$model_class_label)=="mixconfdom"] <- "conformist"
  df
}



if(!exists("eq5_pop_df")) {
  
  csv_file <- "equifinality-5-population-data.csv"
  binary_file <- "equifinality-5-population-data.rda"
  
  csv_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/rawdata", filename = csv_file)
  binary_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq5_pop_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    eq5_pop_df <- new_labels(eq5_pop_df)
    save(eq5_pop_df, file=binary_path)
  }     
}


if(!exists("eq5_ta_sampled_df")) {
  
  csv_file <- "equifinality-5-tasampled-data.csv"
  binary_file <- "equifinality-5-tasampled-data.rda"
  
  csv_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5/rawdata", filename = csv_file)
  binary_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-5", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq5_ta_sampled_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    eq5_ta_sampled_df <- new_labels(eq5_ta_sampled_df)
    save(eq5_ta_sampled_df, file=binary_path)
  }     
}

