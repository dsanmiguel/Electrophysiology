# Electrophysiology Clampfit Data Extraction

This is an R script that collects various electrophysiology data from CSV files that were converted from Axon Binary File (ABF) format. 
Please see https://github.com/swharden/AbfConvert for this functionality which can be used with Windows

The purpose of this is to serve as a less time-consuming alternative to manually extracting the data using Clampfit.

It is recommended you verify the action potential peaks as accurate peak detection is not guaranteed.

*There is a for loop version to be ran on multiple CSVs or an individual version if you only need to be ran on one CSV.

The output data is in CSV format as well.
