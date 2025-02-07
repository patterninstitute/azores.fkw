
<!-- README.md is generated from README.Rmd. Please edit that file -->

# azores.fkw <img src="man/figures/logo.svg" align="right" height="139" />

<!-- badges: start -->

![](https://img.shields.io/badge/R-Compendium-green.svg) [![License: CC
BY
4.0](https://img.shields.io/badge/license-CC%20BY%204.0-blue.svg)](https://cran.r-project.org/web/licenses/CC%20BY%204.0)
<!-- badges: end -->

`{azores.fkw}` is an R compendium for Peres dos Santos, R., Rodríguez,
Y., Sears, R., Magno, R. and Castilho, R. *Tour Operators as a Tool to
Improve Information on Data-Deficient Cetacean Species*. Aquatic
Conservation: Marine and Freshwater Ecosystems 32:2, pages e700067
(2025). DOI: [10.1002/aqc.70067](https://doi.org/10.1002/aqc.70067).

## Installation

``` r
# install.packages("pak")
pak::pak("patterninstitute/azores.fkw")
```

## Data

The `{azores.fkw}` bundles the main data sets needed to replicate the
analysis in this R compendium. Data are provided as exported R objects:

- `individuals`: Photo-identified false killer whale individuals.
- `sightings`: False killer whale sighting events.
- `boat_trips`: Track data of boat while performing sightseeing in the
  Azores’ study region.
- `boat_trips_by_min`: An aggregated version of `boat_trips` by minute.
- `ethology`: Observed whale behaviors during each sighting.
- `pod_age_composition`: Pod age composition for each sighting.
- `reaction_to_boat`: Pod overall reactions to boat presence for each
  sighting.
- `azores_islands`: Azores islands as a simple feature (sf) object.
- `azores_bathymetry`: Depth values for the Azores region provided as a
  tibble.

## Code

The whole analysis code used in the paper can be found at:
<https://www.github.com/patterninstitute/azores.fkw/tree/main/inst/opencr_analysis>.

The R script `opencr_analysis` contains the analysis pipeline. This code
itself either depends on published CRAN packages or on code from this R
compendium itself.

The results have been cached as an RDS file and can be found in
`inst/opencr_analysis/opencr_results.rds`.

## Figures

The figures that result from the analysis can be found at
<https://www.github.com/patterninstitute/azores.fkw/tree/main/inst/opencr_analysis/figures>.
The code that generates them can be found in `figures/figures.R`.
