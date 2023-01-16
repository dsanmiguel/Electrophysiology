# Electrophysiology Clampfit Data Extraction

This is an R script that collects various electrophysiology data from CSV files that were converted from Axon Binary File (ABF) format. 
Please see https://github.com/swharden/AbfConvert for this functionality which can be used with Windows.

A more versatile Python-based alternative is available at https://github.com/Haptein/ABFConvert but some dependencies are required.

The purpose of these scripts are to serve as a less time-consuming alternative to manually extracting the data using Clampfit.

It is recommended you verify the action potential peaks as accurate peak detection is not guaranteed.

*There is a for loop version to be ran on multiple CSVs or an individual version if you only need analysis ran on one CSV at a time.*

The output data is in CSV format as well.


### Note\*
The "Analyze Single CSV.R" only uses preestablished voltage cutoff set by voltage_cutoff variable in beginning of script. This is likely the best and most accurate way to count peaks by loading each individual file into Clampfit and manually determining cutoff.

The "Analyze Multiple CSVs.R" attempts to guess lower bound cutoff using sweeps_AP_count variable but it is recommended that accuracy of Sweep counts for Sweeps 10 through 20 be verified if this script is used. 
