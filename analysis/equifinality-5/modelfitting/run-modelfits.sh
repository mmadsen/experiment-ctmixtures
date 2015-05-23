#!/bin/sh
export R_BASE_DATADIR=/sim/src/experiment-ctmixtures/analysis/data/equifinality-5; Rscript ./combined-tasampled-duration-hidden.r 
export R_BASE_DATADIR=/sim/src/experiment-ctmixtures/analysis/data/equifinality-5; Rscript ./perlocus-analysis.r 
export R_BASE_DATADIR=/sim/src/experiment-ctmixtures/analysis/data/equifinality-5; Rscript ./perlocus-tasampled-three-models-refactored.r 
export R_BASE_DATADIR=/sim/src/experiment-ctmixtures/analysis/data/equifinality-5; Rscript ./population-classification.r 
export R_BASE_DATADIR=/sim/src/experiment-ctmixtures/analysis/data/equifinality-5; Rscript ./tasampled-three-models-refactored.r 
