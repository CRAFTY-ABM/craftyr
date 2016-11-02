craftyr
=============

The package craftyr provides routines to input, convert, analyse, visualise and output CRAFTY
generated model data. It facilitates the pre- and post-processing of model data since it considers
CRAFTY conventions regarding input and output folder and filename patterns.

# Features

 * conversion from CSV to raster data
 * aggregation of data
 * calculations of >10 land use metrics
 * visualisation of input data
 * visualisation of raster data
 * visualisation of input data
 * visualisation of take overs / LU transitions
 * visualisation of potential actions
 * production of LaTeX tables of input and output data
 
# Installation

In case you have not set up an R environment, useful information can be accessed [here](https://www.wiki.ed.ac.uk/display/CRAFTY/Post-Processing).

To install *craftyr* you first need to install the *devtools* package:   
``install.packages("devtools")``

Second, *craftyr* has a dependency which currently cannot be installed automatically:  
``devtools::install_bitbucket("S-Holzhauer/shbasic")``

Finally, install *craftyr*:  
``devtools::install_bitbucket("geoslurg/craftyr@default")``


# First Steps

Start by reading the introduction vignette:  
``vignette("intro", package = "craftyr")``