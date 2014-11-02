library(mmadsenr)



if(!exists("eq3_pop_df")) {
  
  csv_file <- "equifinality-3-population-data.csv"
  binary_file <- "equifinality-3-population-data.rda"
  
  csv_path <- get_data_path(suffix = "equifinality-3", filename = csv_file)
  binary_path <- get_data_path(suffix = "equifinality-3", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq3_pop_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    save(eq3_pop_df, file=binary_path)
  }     
}

if(!exists("eq3_sampled_df")) {
  
  csv_file <- "equifinality-3-sampled-data.csv"
  binary_file <- "equifinality-3-sampled-data.rda"
  
  csv_path <- get_data_path(suffix = "equifinality-3", filename = csv_file)
  binary_path <- get_data_path(suffix = "equifinality-3", filename = binary_file)
  
  if(file.exists(binary_path)) {
    load(binary_path)  
  } else {
    eq3_sampled_df <- read.csv(csv_path,row.names=NULL,header=TRUE) 
    save(eq3_sampled_df, file=binary_path)
  }     
}


