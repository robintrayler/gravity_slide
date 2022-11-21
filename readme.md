# Readme
This repository contains the age-modeling code for: 
* Holliday M. E., Rivera, T., Jicha, B., Trayler, R. B., Biek R. F., Braunagel, M. J., Griffith, W. A., Hacker, D. B., Malone, D. H., Mayback, D. F. (2022), *Emplacement age of the Markagunt Gravity Slide in Southwestern Utah, USA*, [Terra Nova](https://onlinelibrary.wiley.com/doi/10.1111/ter.12630)

The repository is structured into two directories. 
```
.
├── data      # contains the geochronology data and model outputs
├── R         # scripts to generate the age-model from data
└── README.md
```

The `./data/` directory contains the geochronology data and model outputs. 

The `./R/` directory contains 3 scripts. 
* `00_required_functions.R` contains the functions for an adaptive MCMC sampler and for calculating the probability density of summed normal distributions. 
* `01_model.R` runs our Bayesian model. 
* `02_summarize_results.R` will calculate the summary statistics of the model posterior and generate a figure. 