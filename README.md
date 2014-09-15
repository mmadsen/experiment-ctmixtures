Experiment:  Heterogeneous Mixtures of Cultural Transmission Rules
==============================



## Simulation Code ##

This set of experiments is being conducted using the [CTMixtures](https://github.com/mmadsen/ctmixtures) simulation code, and specifically the following release:

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.11741.png)](http://dx.doi.org/10.5281/zenodo.11741)  CTMixtures Version 2.2 


## Subdirectories ##

* paper:  contains Rmarkdown manuscript file and supporting LaTeX infrastructure
* explorations:  contains IPython notebooks, sample graphs, Mathematica notebooks, and other tests that led to the analysis performed here.
* analysis:  contains R project file and R code for analyses of simulation data
* presentation:  any conference presentations or talks that arise from the experiment.  
* outline:  paper outline
* simulations:  configurations and snapshots of job scripts for all simulation runs, including random seeds to exactly reproduce the results

## Simulations ##

Simulations are being run on AWS EC2 instances, using StarCluster to run a 10 node compute cluster of `c3.xlarge` instances.  


## Paper Manuscript ##

The paper manuscript is currently aimed at _Journal of Archaeological Science_ or possibly _Journal of Archaeological Method and Theory_, and employs the Elsevier article template given the former.  

## Data Location ##

Data are stored in transient MongoDB instances on the nodes of the compute cluster, and then exported to CSV for further processing.  The raw CSV files will be archived on Amazon S3, with links here when the simulation runs and uploads are complete.  

Final data for analysis will be archived on S3 as well, and may be included in this repository if file size limits permit.    

