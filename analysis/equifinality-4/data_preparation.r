library(mmadsenr)



if(!exists("eq4_pop_df")) {
  
  csv_file <- "equifinality-3-4-population-data.csv"
  binary_file <- "equifinality-3-4-population-data.rda"
  
  csv_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/rawdata", filename = csv_file)
  binary_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq4_pop_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    save(eq4_pop_df, file=binary_path)
  }     
}


if(!exists("eq4_ta_sampled_df")) {
  
  csv_file <- "equifinality-3-4-tasampled-data.csv"
  binary_file <- "equifinality-3-4-tasampled-data.rda"
  
  csv_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4/rawdata", filename = csv_file)
  binary_path <- get_data_path(suffix = "experiment-ctmixtures/equifinality-4", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq4_ta_sampled_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    save(eq4_ta_sampled_df, file=binary_path)
  }     
}

