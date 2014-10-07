# is there too much variation in the neutral models of equifinality-2-small?
library(mmadsenr)
library(ggplot2)
library(dplyr)

# pull in the per-locus richness data, this can be compared to Ewens 1972, equation 4
eq2s.rich <- read.csv(file="~/local-research/diss/experiments/experiment-ctmixtures/eq2s-pop-richness-locus-data.csv")
eq2s.rich.neutral <- filter(eq2.rich, model_class_label == 'allneutral')

popsize <- 100
innov_rates_for_abline <- c(0.1, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0)
# expected K for innovation rates
expected_k <- numeric(length(innov_rates_for_abline))
for (i in 1:length(innov_rates_for_abline)) { expected_k[i] <- expected_k_pop(popsize, innov_rates_for_abline[i])}
expected_k_abline <- data.frame(innov_rates_for_abline, expected_k)

# plot
plt <- ggplot(data=eq2s.rich.neutral, aes(x=innovation_rate, y=richness_locus_value)) + geom_point(alpha=1/2,size=1) + geom_abline(data=expected_k_abline, color="red")
plt + xlab("Scaled Innovation Rate") + ylab("Number of Traits at a Dimension/Locus")
ggsave(filename="figure/eq2s-richness-neutral-expectedk.png",scale=0.5)