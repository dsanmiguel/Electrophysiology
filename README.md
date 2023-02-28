# Electrophysiology Clampfit Data Extraction

This is an R script that collects various electrophysiology data from CSV files that were converted from Axon Binary File (ABF) format. 
Please see https://github.com/swharden/AbfConvert for this functionality which can be used with Windows.

The purpose of these scripts are to serve as a less time-consuming alternative to manually extracting the data using Clampfit.

It is recommended you verify the action potential peaks as accurate peak detection is not guaranteed as your data may have different features.

*There is a "Multiple Files" version to be ran on a folder of CSVs converted from ABFs or an "Single File" version if you only need analysis ran on one CSV at a time.*

The output data is in CSV format as well in the folder you specify.
