# Spatial Graph Segementation on german corona Data

<img label="teaser_image" src="attic\teaser-image.jpeg">

## Project structure

    .
    ├── attic
    ├── data                  # actual data is not uploaded
    │   ├── data.RDS          # data without geometry
    │   └── geom_mapping.RDS  # data with columns 'name_rki' and 'geometry'
    ├── R
    │   └── 01.R
    ├── LICENSE
    └── README.md
    
## :warning: Remark

This project requires installation of the R package [`graphseg`](https://github.com/goepp/graphseg) which caused errors when using `solve` functionality.<br>
Therefore the [source code](https://github.com/goepp/graphseg/blob/master/R/spatial.R) was copied into a dedicated script and sourced after loading `Matrix` package (see `segmentation.R`).
    
## Getting started

The original dataset will be splitted into a data.table containing covariates and another one containing assignents of districts to its geometry object (since geometry objects are relatively memory intensive necessary data wraningling is typically done first on the data without geometry objects and afterwards joined with data containing district-geometry mappings). This is done in the script `preprocess.R` which is *not* sourced in other scripts and must be excecuted seperately once at the beginning.<br>
The actual graph segmentation is done by running the scipt `segmentation.R`. Graphical plots are created by running `plots.R` which are saved into `/assets/plots`. Further the script `functions.R` provides helper functions used in the aforementioned scripts.

## :wave: Attribution

This project is an application of the paper by Goepp & van de Kassteele (2022) \[1\] applied to [german corona data](https://diviexchange.blob.core.windows.net/%24web/zeitreihe-tagesdaten.csv) provided by RKI. <br>

## References

\[1\] Goepp, Vivien and Jan van de Kassteele. “Graph-Based Spatial Segmentation of Health-Related Areal Data.” (2022).

