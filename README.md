# Coloss - Austria honey bee colony winter mortality evaluation

Code pieces, mostly R to help with the yearly bee colony winter mortality evaluation. This evaluation is done since 2008, by the Institute of Biology of the University of Graz.

Most years (folder) have the same build up:

* ***Test_Coords.R***: Tests if given lat/long is in given district and throws map and file with wrong IDs

* ***Get_altitude.R***: Generates altitudes from lat/long via geonames API

* ***Partial_XXX Files***: Loading Libraries, Functions and main Ressources for all Files

* ***Plot_XXX Files***: Ploting files used for analysis and visualization of the data

## Version Control

Starting with project 2020 we are using renv for version control. 

## MIT Licence 
Copyright (c) 2019 Hannes Oberreiter

***Shapefiles 2019 Austria Originally:*** CC BY 4.0, Flooh Perlot (https://creativecommons.org/licenses/by/4.0/)

***Shapefiles 2020:*** We are using a modified version from [data.gv.at](https://www.data.gv.at/katalog/dataset/bev_verwaltungsgrenzenstichtagsdaten150000), CC BY 4.0, map date 01.10.2019

## Publications

### Folder 2019

#### Github Release

[![DOI](https://zenodo.org/badge/181077729.svg)](https://zenodo.org/badge/latestdoi/181077729)

#### Paper

> Oberreiter, H.; Brodschneider, R. Austrian COLOSS Survey of Honey Bee Colony Winter Losses 2018/19 and Analysis of Hive Management Practices. Diversity 2020, 12, 99.

```bibtex
@article{oberreiter2020austrian,
  title={Austrian COLOSS Survey of Honey Bee Colony Winter Losses 2018/19 and Analysis of Hive Management Practices},
  author={Oberreiter, Hannes and Brodschneider, Robert},
  journal={Diversity},
  volume={12},
  number={3},
  pages={99},
  year={2020},
  publisher={Multidisciplinary Digital Publishing Institute}
}
```


