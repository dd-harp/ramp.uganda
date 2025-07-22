library(ramp.xds)
library(ramp.library)
library(ramp.control)
library(ramp.work)
library(ramp.uganda)

wd = "/Users/smitdave/Library/CloudStorage/Box-Box/RAMP/models/"
args <- commandArgs(trailingOnly=TRUE)
run_ix <- c(args[1]:args[2])
for(i in run_ix) pipeline(i, "sip_si", wd)

