# ramp.uganda

To install the software:

```
library(devtools)
devtools::install_github("dd-harp/ramp.uganda")
```

The software also depends on `ramptools,` `ramp.xds` and [SimBA](https://faculty.washington.edu/smitdave/simba/).

## Locations

If you want to work with `ramp.uganda,` please configure a file, called `my_paths.yaml.` It lives in the same directory as this file, `README.md.`
It holds the *local* paths to `Box/RAMP` and the `ramp.uganda` github repository. It also defines the location of the website 
[Malaria in Uganda](https://faculty.washington.edu/smitdave/uganda_intelligence/). 

## Data 

Several data elements have been imported into `ramp.uganda`

+ `district_dir` - a table with the names of all Uganda districts and a directory name for each one 
+ `region_dir`- a table with the names of all Uganda regions and a directory name for each one 
+ `uga_itn` - a table describing mass ITN distributions 
+ `uga_irs` - a table describing IRS 
+ `district_adjacency` - an adjacency matrix for all Uganda districts
+ `pfpr_by_district` - the *Pf*PR time series, from Box, imported in a standard form

## Functions

Several functions have been written to make it easier to use these data objects. This is also the home for 
the functions that support [Malaria in Uganda](https://faculty.washington.edu/smitdave/uganda_intelligence/), 
the website that disseminates information. All functions and data objects are listed in the [reference index](https://dd-harp.github.io/ramp.uganda/docs/reference/index.html), 
and the documentation can be accessed in R. For example:

```
?get_district_pfpr
?get_one_district
```

