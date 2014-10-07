# experiments nv4test-alllocus (all locus copying) vs. evtest4m (random locus copying) at 4MM steps rather than 1MM steps
# otherwise all priors and parameters the same.  

library(dplyr)
library(mmadsenr)
library(ggplot2)

# look at richness per locus first
evtest4m.rich <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/simulations/excessvariation-nonstationary-test/evtest4m-pop-richness-locus-data.csv")
nv4test.rich <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/simulations/excessvariation-nonstationary-test/nv4test-alllocus-pop-richness-locus-data.csv")
nv8test.rich <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/simulations/excessvariation-nonstationary-test/nv8test-alllocus-pop-richness-locus-data.csv")
nv8test.rand.rich <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/simulations/excessvariation-nonstationary-test/nv8test-randlocus-pop-richness-locus-data.csv")
nv1test.rich <- read.csv("~/Dropbox/Research/projects/coarsegraining/experiment-ctmixtures/simulations/excessvariation-nonstationary-test/nv1test-alllocus-pop-richness-locus-data.csv")






# pull in the 1MM richness data from equifinality-2-small for comparison
# pull in the per-locus richness data, this can be compared to Ewens 1972, equation 4
eq2s.rich <- read.csv(file="~/local-research/diss/experiments/experiment-ctmixtures/eq2s-pop-richness-locus-data.csv")
eq2s.rich.neutral <- filter(eq2s.rich, model_class_label == 'allneutral')

# mark the experiment with a label

nv1test.rich$exp <- 'allloci-1mm'
nv4test.rich$exp <- 'allloci-4mm'
nv8test.rich$exp <- 'allloci-8mm'

eq2s.rich.neutral$exp <- 'randomlocus-1mm'
evtest4m.rich$exp <- 'randomlocus-4mm'
nv8test.rand.rich$exp <- 'randomlocus-8mm'

# now, take a random sample of the 1MM richness data, so we have equal samples
# there are 4K samples, we want 100, so we want 0.025 as the sample fraction
eq2s.split <- random_split_dataset(eq2s.rich.neutral, 0.025)

# combine these into one dataset
combined.rich <- rbind(evtest4m.rich, nv4test.rich, nv8test.rich, eq2s.split$test, nv8test.rand.rich, nv1test.rich)

popsize <- 100
innov_rates_for_abline <- c(0.1, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0)
# expected K for innovation rates
expected_k <- numeric(length(innov_rates_for_abline))
for (i in 1:length(innov_rates_for_abline)) { expected_k[i] <- expected_k_pop(popsize, innov_rates_for_abline[i])}
expected_k_abline <- data.frame(innov_rates_for_abline, expected_k)


plt <- ggplot(data=combined.rich, aes(x=innovation_rate, y=richness_locus_value, color=factor(exp))) + stat_smooth(method="lm") + geom_point() 
plt + geom_abline(data=expected_k_abline, color="black")
ggsave("figure/combined-excess-richness.png", scale=0.5)