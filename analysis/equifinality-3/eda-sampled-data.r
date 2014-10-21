library(ggplot2)
library(dplyr)

source('data_preparation.r')

plt <- ggplot(eq3_sampled_df, aes(x = num_configurations_ssize, y = ..density..)) + geom_histogram(aes(fill=model_class_label, position="stack")) + facet_grid(sample_size ~ .)
plt

plt <- ggplot(eq3_sampled_df, aes(x = configuration_slatkin, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

neutral_df <- filter(eq3_sampled_df, model_class_label == 'allneutral')
plt <- ggplot(neutral_df, aes(x = configuration_slatkin, y = ..density..)) + geom_density(aes(fill=model_class_label, position="stack"))
plt

plt <- ggplot(eq3_sampled_df, aes(x = richness_locus_min)) + geom_density(fill="red") + facet_grid(. ~ model_class_label)
plt

# should use histogram, takes integer values
plt <- ggplot(eq3_sampled_df, aes(x = kandler_locus_min)) + geom_histogram() + facet_grid(. ~ model_class_label)
plt

# should use histogram, takes integer values
plt <- ggplot(eq3_sampled_df, aes(x = num_trait_configurations)) + geom_histogram() + facet_grid(. ~ model_class_label)
plt