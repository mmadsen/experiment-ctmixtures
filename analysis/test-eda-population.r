library(ggplot2)
library(dplyr)

source('data_preparation.r')

plt <- ggplot(pop_df, aes(x = num_trait_configurations, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

plt <- ggplot(pop_df, aes(x = configuration_slatkin, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

neutral_df <- filter(pop_df, model_class_label == 'allneutral')
plt <- ggplot(neutral_df, aes(x = configuration_slatkin, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

plt <- ggplot(pop_df, aes(x = richness_locus_min, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

# should use histogram, takes integer values
plt <- ggplot(pop_df, aes(x = kandler_locus_min, y = ..density..)) + geom_histogram(aes(fill=model_class_label, position="stack"))
plt

# should use histogram, takes integer values
plt <- ggplot(nv.pop, aes(x = num_trait_configurations, y = ..density..)) + geom_histogram(aes(fill=model_class_label, position="stack"))
plt