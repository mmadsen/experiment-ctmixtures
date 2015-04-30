# equifinality-5 simulations #

The fifth batch of simulation runs is designed to get samples across prior distributions for "pure" models comparable to 

This batch is identical in configuration to `equifinality-1`, but uses `ctmixtures-2.4`, which corrects the way innovation rates are handled.  

I will also begin by running a sub-batch of 1000 runs per model, analyze the results, and then proceed with a fuller sample.  

## Models ##

All simulations are performed for 4MM steps in a Moran population dynamic with 100 individuals.  

* 100% anticonformism with strength fixed
* 100% anticonformism with strength drawn from prior distribution
* 100% conformism with strength fixed
* 100% conformism with strength drawn from prior distribution
* 100% purely neutral copying

All models share a uniform prior distribution on innovation in the range $[0.1, 5.0]$, in scaled units.  

All models allow the population to reach quasi-stationary equilibrium, and take a synchronic snapshot sample of the population of two sizes:  10 and 20 individuals from the total of 100.  That synchronic snapshot occurs in time step 4MM, at the conclusion of the simulation run.  

All models also perform time-averaged observations over a set of durations $[10,25,50,100]$ generations long (nb.  1 generation = 100 time steps in this model given the conversion between WF and Moran dynamics).  

All models also calculate the Kandler-Shennan trait survival over a duration of 50 generations, both with synchronic point observations at the beginning and end of the 50 generation block, and with observations on each end which are time averaged for $[10,25,50,100]$ generations.  This will allow analysis of the effect of time averaged observations on Kandler and Shennan's non-equilibrium survival method.  



