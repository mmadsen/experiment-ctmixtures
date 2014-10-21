# Equifinality Batch 2 #

The second batch of simulation runs is designed to get samples across prior distributions for four models, three of them variants on a mixture of conformist and anti-conformist transmission.  The fourth model is the control, and is an identically sized population with the same prior distribution on innovation but purely neutral copying.  

This batch is identical in configuration to `equifinality-1`, but uses `ctmixtures-2.4`, which corrects the way innovation rates are handled.  

I will also begin by running a sub-batch of 1000 runs per model, analyze the results, and then proceed with a fuller sample.  

## Models ##

All simulations are performed for 1MM steps in a Moran population dynamic with 100 individuals, so 10K generations.  

* Equal proportions of anti- and conformist transmission
* 70% conformism, 30% anticonformism
* 30% conformism, 70% anticonformism
* 100% purely neutral copying

All models share a uniform prior distribution on innovation in the range $[0.1, 5.0]$, in scaled units.  Conformist mixtures all share uniform prior distributions on the strength of the bias from the range $[0.05, 0.25]$.  

All models allow the population to reach quasi-stationary equilibrium, and take a synchronic snapshot sample of the population of two sizes:  10 and 20 individuals from the total of 100.  That synchronic snapshot occurs in time step 1MM, at the conclusion of the simulation run.  

All models also perform time-averaged observations over a set of durations $[10,25,50,100]$ generations long (nb.  1 generation = 100 time steps in this model given the conversion between WF and Moran dynamics).  

All models also calculate the Kandler-Shennan trait survival over a duration of 50 generations, both with synchronic point observations at the beginning and end of the 50 generation block, and with observations on each end which are time averaged for $[10,25,50,100]$ generations.  This will allow analysis of the effect of time averaged observations on Kandler and Shennan's non-equilibrium survival method.  

## Sample Sizes ##

For each of the 4 models listed above, Batch 1 takes 10,000 samples distributed across the joint priors of its parameters.  Thus, Batch 1 will entail 40,000 total simulation runs.  

If additional data are needed to provide good coverage of the prior space, a second batch will be added with identical experiment and model configurations, but new draws from priors.  


