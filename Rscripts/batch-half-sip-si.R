library(ramp.xds)
library(ramp.library)
library(ramp.control)
library(ramp.work)
library(ramp.uganda)

wd = "/Users/smitdave/Library/CloudStorage/Box-Box/RAMP/models/"
args <- commandArgs(trailingOnly=TRUE)

ix = c(3, 13, 33, 45, 69, 71, 91, 113, 128)
for(i in ix) half_pipe(i, "sip_si", wd)
