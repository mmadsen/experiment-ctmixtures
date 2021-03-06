#!/usr/bin/env python

# Copyright (c) 2013.  Mark E. Madsen <mark@madsenlab.org>
#
# This work is licensed under the terms of the Apache Software License, Version 2.0.  See the file LICENSE for details.

import ming
import csv
import logging as log
import argparse
import ctmixtures.data as data
import pytransmission.popgen as pg
import math as m


############################################################################
def setup():
    global args, config, simconfig
    parser = argparse.ArgumentParser()
    parser.add_argument("--experiment", help="provide name for experiment, to be used as prefix for database collections")
    parser.add_argument("--debug", help="turn on debugging output")
    parser.add_argument("--dbhost", help="database hostname, defaults to localhost", default="localhost")
    parser.add_argument("--dbport", help="database port, defaults to 27017", default="27017")
    parser.add_argument("--configuration", help="Path to configuration file")
    parser.add_argument("--filename", help="path and base filename for exports (DO NOT include *.csv extension)", required=True)

    args = parser.parse_args()

    if args.debug == 1:
        log.basicConfig(level=log.DEBUG, format='%(asctime)s %(levelname)s: %(message)s')
    else:
        log.basicConfig(level=log.INFO, format='%(asctime)s %(levelname)s: %(message)s')

    #### main program ####
    log.info("EXPORT DATA TO CSV - Experiment: %s", args.experiment)
    data.set_experiment_name(args.experiment)
    data.set_database_hostname(args.dbhost)
    data.set_database_port(args.dbport)
    config = data.getMingConfiguration(data.modules)
    ming.configure(**config)



############################################################################
def export_simulation_record():
    # ## Export a simulation record file, with all params and classes used, random
    ### seed, whatever is needed to replicate the simulations
    full_filename = ''
    full_filename += args.filename
    full_filename += "-simulation-data.csv"
    sim_fields = data.mixture_model_stats.sim_record_columns_to_export()
    ofile = open(full_filename, "wb")
    writer = csv.DictWriter(ofile, fieldnames=sim_fields, quotechar='"', quoting=csv.QUOTE_ALL)
    headers = dict((n, n) for n in sim_fields)
    writer.writerow(headers)
    cursor = data.MixtureModelStats.m.find(dict(), dict(timeout=False))
    for sample in cursor:
        row = dict()
        for field in sim_fields:
            row[field] = sample[field]

        # correct kandler_interval from timesteps to generations
        row['kandler_interval'] = int(row['kandler_interval']) / int(row['population_size'])

        #log.info("sim data row: %s", row)
        writer.writerow(row)
    ofile.close()


############################################################################
# # whole population statistics
# slatkin_exact = Field([float])
# shannon_entropy = Field([float])
# iqv_diversity = Field([float])
# num_trait_configurations = Field(int)
# trait_configuration_counts = Field([])
# configuration_slatkin = Field(float)
# unlabeled_frequencies = Field([])
# unlabeled_counts = Field([])
# pop_richness = Field([int])

def export_population_stats():
    # ## Export a full population census statistics file ###
    full_filename = ''
    full_filename += args.filename
    full_filename += "-richness-data.csv"
    pop_fields = data.mixture_model_stats.pop_columns_to_export()

    # adjust the fields for the new summary statistics
    pop_fields.append('innovation_rate')
    pop_fields.append('locus-richness')
    pop_fields.append('locus-slatkin')

    ofile = open(full_filename, "wb")
    writer = csv.DictWriter(ofile, fieldnames=pop_fields, quotechar='"', quoting=csv.QUOTE_ALL)
    headers = dict((n, n) for n in pop_fields)
    writer.writerow(headers)

    cursor = data.MixtureModelStats.m.find(dict(), dict(timeout=False))
    for sample in cursor:
        richness_list = sample['pop_richness']
        slatkin_list = sample['slatkin_exact']
        numloci = int(sample['num_features'])
        for i in xrange(0, numloci):
            row = dict()
            row['simulation_run_id'] = sample['simulation_run_id']
            row['model_class_label'] = sample['model_class_label']
            row['innovation_rate'] = sample['innovation_rate']
            row['locus-richness'] = richness_list[i]
            row['locus-slatkin'] = slatkin_list[i]

            #log.info("sim data row: %s", row)
            writer.writerow(row)
    ofile.close()

############################################################################
# # results by sample size
# unlabeled_freq_ssize = Field(schema.Anything)
# unlabeled_counts_ssize = Field(schema.Anything)
# unlabeled_config_counts_ssize = Field(schema.Anything)
# num_configurations_ssize = Field(schema.Anything)
# config_slatkin_ssize = Field(schema.Anything)
# entropy_ssize = Field(schema.Anything)
# iqv_ssize = Field(schema.Anything)
# richness_ssize = Field(schema.Anything)
# slatkin_ssize = Field(schema.Anything)
# kandler_remaining_count = Field([int])

def export_sampled_stats():
    ## export a file with sampled statistics
    full_filename = ''
    full_filename += args.filename
    full_filename += "-pop-sampled-richness.csv"

    ssize_fields = data.mixture_model_stats.ssize_columns_to_export()

    # adjust the fields for the new summary statistics
    ssize_fields.append('innovation_rate')
    ssize_fields.append('pop_richness')
    ssize_fields.append('ssize_25')
    ssize_fields.append('expected_25')
    ssize_fields.append('sd_25')
    ssize_fields.append('ssize_50')
    ssize_fields.append('expected_50')
    ssize_fields.append('sd_50')

    ofile = open(full_filename, "wb")
    writer = csv.DictWriter(ofile, fieldnames=ssize_fields, quotechar='"', quoting=csv.QUOTE_ALL)
    headers = dict((n, n) for n in ssize_fields)
    writer.writerow(headers)

    cursor = data.MixtureModelStats.m.find(dict(), dict(timeout=False))
    for sample in cursor:
        richness_map = sample['richness_ssize']
        pop_richness_list = sample['pop_richness']
        slatkin_list = sample['slatkin_exact']
        numloci = int(sample['num_features'])
        rich25 = richness_map['25']
        rich50 = richness_map['50']
        rich100 = richness_map['100']
        for i in xrange(0, numloci):
            row = dict()
            row['simulation_run_id'] = sample['simulation_run_id']
            row['model_class_label'] = sample['model_class_label']
            row['innovation_rate'] = sample['innovation_rate']
            row['pop_richness'] = pop_richness_list[i]

            res25 = pg.moran_expected_traits_at_locus(float(sample['innovation_rate']), 25)
            res50 = pg.moran_expected_traits_at_locus(float(sample['innovation_rate']), 50)

            row['ssize_25'] = rich25[i]
            row['expected_25'] = res25[0]
            row['sd_25'] = m.sqrt(res25[1])
            row['ssize_50'] = rich50[i]
            row['expected_50'] = res50[0]
            row['sd_50'] = m.sqrt(res50[1])



            #log.info("sim data row: %s", row)
            writer.writerow(row)
    ofile.close()

############################################################################
# # results for TA intervals over all sample sizes
# unlabeled_freq_ta_ssize = Field(schema.Anything)
# richness_ta_ssize = Field(schema.Anything)
# slatkin_ta_ssize = Field(schema.Anything)
# entropy_ta_ssize = Field(schema.Anything)
# iqv_ta_ssize = Field(schema.Anything)
# unlabeled_config_counts_ta_ssize = Field(schema.Anything)
# num_configurations_ta_ssize = Field(schema.Anything)
# config_slatkin_ta_ssize = Field(schema.Anything)
# config_entropy_ta_ssize = Field(schema.Anything)
# config_iqv_ta_ssize = Field(schema.Anything)
# kandler_remaining_tassize = Field(schema.Anything)

def export_ta_sampled_stats():
    ## export a file with sampled statistics
    full_filename = ''
    full_filename += args.filename
    full_filename += "-tasampled-data.csv"

    sim_fields = data.mixture_model_stats.tassize_columns_to_export()

    ofile = open(full_filename, "wb")
    writer = csv.DictWriter(ofile, fieldnames=sim_fields, quotechar='"', quoting=csv.QUOTE_ALL)
    headers = dict((n, n) for n in sim_fields)
    writer.writerow(headers)
    cursor = data.MixtureModelStats.m.find(dict(), dict(timeout=False))

    for sample in cursor:
        pass

    ofile.close()


############################################################################

if __name__ == "__main__":
    setup()
    export_sampled_stats()


















