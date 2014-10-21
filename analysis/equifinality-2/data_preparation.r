


library(dplyr)

# # population statistics
# if(!exists("pop_df")) {
#   
#   if(file.exists("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-population-data.rda")) {
#     load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-population-data.rda")  
#   } else {
#     pop_df <- read.csv("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-population-data.csv",row.names=NULL,header=TRUE) 
#     save(pop_df, file="~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-population-data.rda")
#   }     
# }
# 
# 
# if(!exists("sim_df")) {
#   
#   if(file.exists("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-simulation-data.rda")) {
#     load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-simulation-data.rda")  
#   } else {
#     sim_df <- read.csv("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-simulation-data.csv",row.names=NULL,header=TRUE) 
#     save(sim_df, file="~/local-research/diss/experiments/experiment-ctmixtures/equifinality-1-simulation-data.rda")
#   }     
# }

if(!exists("eq2_pop_df")) {
  
  if(file.exists("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-2-small-population-data.rda")) {
    load("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-2-small-population-data.rda")  
  } else {
    eq2_pop_df <- read.csv("~/local-research/diss/experiments/experiment-ctmixtures/equifinality-2-small-population-data.csv",row.names=NULL,header=TRUE) 
    save(eq2_pop_df, file="~/local-research/diss/experiments/experiment-ctmixtures/equifinality-2-small-population-data.rda")
  }     
}



