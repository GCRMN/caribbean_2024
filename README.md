
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **Status and Trends of Caribbean Coral Reefs: 1970-2024**

## 1. Introduction

## 2. Code

### Functions

- `data_descriptors.R` Get number of sites, surveys, datasets, first and
  last year of monitoring.
- `graphical_par.R` Graphical parameters, including colors and fonts.
- `plot_region.R` Regional background map.
- `theme_graph.R` Main ggplot theme for the plots of the reports.
- `theme_map.R` Main ggplot theme for the maps of the reports.

### Cleaning and selection (`a_`)

- `a01_select_benthic-data.R`
- `a02_benthic-data_sources.R`
- `a03_clean_shp.R`
- `a07_reef-buffer.js`
- `a08_clean_cyclones.R`
- `a09_extract_sst-anom-year.R`

### Extract indicators (`b_`)

- `b05_extract_indicator_cyclones`

### Models (benthic cover) (`c_`)

- `c01_explo_benthic-data.R` Exploratory analyses of benthic cover data.

### Models (fish biomass) (`d_`)

### Figures and tables (`e_`)

- `e02_region_map.R`
- `e03_territories_map.R`
- `e04_region_sst.R`
- `e05_territories_sst.R`
- `e06_region_cyclones.R`
- `e07_territories_cyclones.R`
- `e08_region_spatio-temporal.R`
- `e09_territories_spatio-temporal.R`
- `e11_other-indicators.R`

## 3. Reproducibility parameters

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.4.1 (2024-06-14 ucrt)
     os       Windows 10 x64 (build 18363)
     system   x86_64, mingw32
     ui       RTerm
     language (EN)
     collate  French_France.utf8
     ctype    French_France.utf8
     tz       Europe/Paris
     date     2024-09-25
     pandoc   3.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package       * version    date (UTC) lib source
     askpass         1.2.0      2023-09-03 [1] CRAN (R 4.4.1)
     backports       1.5.0      2024-05-23 [1] CRAN (R 4.4.0)
     base64enc       0.1-3      2015-07-28 [1] CRAN (R 4.4.0)
     bit             4.0.5      2022-11-15 [1] CRAN (R 4.4.1)
     bit64           4.0.5      2020-08-30 [1] CRAN (R 4.4.1)
     blob            1.2.4      2023-03-17 [1] CRAN (R 4.4.1)
     broom           1.0.6      2024-05-17 [1] CRAN (R 4.4.1)
     bslib           0.8.0      2024-07-29 [1] CRAN (R 4.4.1)
     cachem          1.1.0      2024-05-16 [1] CRAN (R 4.4.1)
     callr           3.7.6      2024-03-25 [1] CRAN (R 4.4.1)
     cellranger      1.1.0      2016-07-27 [1] CRAN (R 4.4.1)
     class           7.3-22     2023-05-03 [1] CRAN (R 4.4.1)
     classInt        0.4-10     2023-09-05 [1] CRAN (R 4.4.1)
     cli             3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
     clipr           0.8.0      2022-02-22 [1] CRAN (R 4.4.1)
     clock           0.7.1      2024-07-18 [1] CRAN (R 4.4.1)
     codetools       0.2-20     2024-03-31 [1] CRAN (R 4.4.1)
     colorspace      2.1-1      2024-07-26 [1] CRAN (R 4.4.1)
     conflicted      1.2.0      2023-02-01 [1] CRAN (R 4.4.1)
     cpp11           0.5.0      2024-08-27 [1] CRAN (R 4.4.1)
     crayon          1.5.3      2024-06-20 [1] CRAN (R 4.4.1)
     curl            5.2.2      2024-08-26 [1] CRAN (R 4.4.1)
     data.table      1.16.0     2024-08-27 [1] CRAN (R 4.4.1)
     DBI             1.2.3      2024-06-02 [1] CRAN (R 4.4.1)
     dbplyr          2.5.0      2024-03-19 [1] CRAN (R 4.4.1)
     diagram         1.6.5      2020-09-30 [1] CRAN (R 4.4.0)
     dials           1.3.0      2024-07-30 [1] CRAN (R 4.4.1)
     DiceDesign      1.10       2023-12-07 [1] CRAN (R 4.4.1)
     digest          0.6.36     2024-06-23 [1] CRAN (R 4.4.1)
     doFuture        1.0.1      2023-12-20 [1] CRAN (R 4.4.1)
     dplyr           1.1.4      2023-11-17 [1] CRAN (R 4.4.1)
     dtplyr          1.3.1      2023-03-22 [1] CRAN (R 4.4.1)
     e1071           1.7-14     2023-12-06 [1] CRAN (R 4.4.1)
     evaluate        0.24.0     2024-06-10 [1] CRAN (R 4.4.1)
     fansi           1.0.6      2023-12-08 [1] CRAN (R 4.4.1)
     farver          2.1.2      2024-05-13 [1] CRAN (R 4.4.1)
     fastmap         1.2.0      2024-05-15 [1] CRAN (R 4.4.1)
     fontawesome     0.5.2      2023-08-19 [1] CRAN (R 4.4.1)
     forcats         1.0.0      2023-01-29 [1] CRAN (R 4.4.1)
     foreach         1.5.2      2022-02-02 [1] CRAN (R 4.4.1)
     fs              1.6.4      2024-04-25 [1] CRAN (R 4.4.1)
     furrr           0.3.1      2022-08-15 [1] CRAN (R 4.4.1)
     future          1.34.0     2024-07-29 [1] CRAN (R 4.4.1)
     future.apply    1.11.2     2024-03-28 [1] CRAN (R 4.4.1)
     gargle          1.5.2      2023-07-20 [1] CRAN (R 4.4.1)
     generics        0.1.3      2022-07-05 [1] CRAN (R 4.4.1)
     ggplot2         3.5.1      2024-04-23 [1] CRAN (R 4.4.1)
     globals         0.16.3     2024-03-08 [1] CRAN (R 4.4.0)
     glue            1.7.0      2024-01-09 [1] CRAN (R 4.4.1)
     googledrive     2.1.1      2023-06-11 [1] CRAN (R 4.4.1)
     googlesheets4   1.1.1      2023-06-11 [1] CRAN (R 4.4.1)
     gower           1.0.1      2022-12-22 [1] CRAN (R 4.4.0)
     GPfit           1.0-8      2019-02-08 [1] CRAN (R 4.4.1)
     gtable          0.3.5      2024-04-22 [1] CRAN (R 4.4.1)
     hardhat         1.4.0      2024-06-02 [1] CRAN (R 4.4.1)
     haven           2.5.4      2023-11-30 [1] CRAN (R 4.4.1)
     highr           0.11       2024-05-26 [1] CRAN (R 4.4.1)
     hms             1.1.3      2023-03-21 [1] CRAN (R 4.4.1)
     htmltools       0.5.8.1    2024-04-04 [1] CRAN (R 4.4.1)
     httr            1.4.7      2023-08-15 [1] CRAN (R 4.4.1)
     ids             1.0.1      2017-05-31 [1] CRAN (R 4.4.1)
     infer           1.0.7      2024-03-25 [1] CRAN (R 4.4.1)
     ipred           0.9-15     2024-07-18 [1] CRAN (R 4.4.1)
     isoband         0.2.7      2022-12-20 [1] CRAN (R 4.4.1)
     iterators       1.0.14     2022-02-05 [1] CRAN (R 4.4.1)
     jquerylib       0.1.4      2021-04-26 [1] CRAN (R 4.4.1)
     jsonlite        1.8.8      2023-12-04 [1] CRAN (R 4.4.1)
     KernSmooth      2.23-24    2024-05-17 [1] CRAN (R 4.4.1)
     knitr           1.48       2024-07-07 [1] CRAN (R 4.4.1)
     labeling        0.4.3      2023-08-29 [1] CRAN (R 4.4.0)
     lattice         0.22-6     2024-03-20 [1] CRAN (R 4.4.1)
     lava            1.8.0      2024-03-05 [1] CRAN (R 4.4.1)
     lhs             1.2.0      2024-06-30 [1] CRAN (R 4.4.1)
     lifecycle       1.0.4      2023-11-07 [1] CRAN (R 4.4.1)
     listenv         0.9.1      2024-01-29 [1] CRAN (R 4.4.1)
     lubridate       1.9.3      2023-09-27 [1] CRAN (R 4.4.1)
     magrittr        2.0.3      2022-03-30 [1] CRAN (R 4.4.1)
     MASS            7.3-61     2024-06-13 [1] CRAN (R 4.4.1)
     Matrix          1.7-0      2024-04-26 [1] CRAN (R 4.4.1)
     memoise         2.0.1      2021-11-26 [1] CRAN (R 4.4.1)
     mgcv            1.9-1      2023-12-21 [1] CRAN (R 4.4.1)
     mime            0.12       2021-09-28 [1] CRAN (R 4.4.0)
     modeldata       1.4.0      2024-06-19 [1] CRAN (R 4.4.1)
     modelenv        0.1.1      2023-03-08 [1] CRAN (R 4.4.1)
     modelr          0.1.11     2023-03-22 [1] CRAN (R 4.4.1)
     munsell         0.5.1      2024-04-01 [1] CRAN (R 4.4.1)
     nlme            3.1-166    2024-08-14 [1] CRAN (R 4.4.1)
     nnet            7.3-19     2023-05-03 [1] CRAN (R 4.4.1)
     numDeriv        2016.8-1.1 2019-06-06 [1] CRAN (R 4.4.0)
     openssl         2.2.1      2024-08-16 [1] CRAN (R 4.4.1)
     parallelly      1.38.0     2024-07-27 [1] CRAN (R 4.4.1)
     parsnip         1.2.1      2024-03-22 [1] CRAN (R 4.4.1)
     patchwork       1.2.0      2024-01-08 [1] CRAN (R 4.4.1)
     pillar          1.9.0      2023-03-22 [1] CRAN (R 4.4.1)
     pkgconfig       2.0.3      2019-09-22 [1] CRAN (R 4.4.1)
     prettyunits     1.2.0      2023-09-24 [1] CRAN (R 4.4.1)
     processx        3.8.4      2024-03-16 [1] CRAN (R 4.4.1)
     prodlim         2024.06.25 2024-06-24 [1] CRAN (R 4.4.1)
     progress        1.2.3      2023-12-06 [1] CRAN (R 4.4.1)
     progressr       0.14.0     2023-08-10 [1] CRAN (R 4.4.1)
     proxy           0.4-27     2022-06-09 [1] CRAN (R 4.4.1)
     ps              1.7.7      2024-07-02 [1] CRAN (R 4.4.1)
     purrr           1.0.2      2023-08-10 [1] CRAN (R 4.4.1)
     R6              2.5.1      2021-08-19 [1] CRAN (R 4.4.1)
     ragg            1.3.2      2024-05-15 [1] CRAN (R 4.4.1)
     rappdirs        0.3.3      2021-01-31 [1] CRAN (R 4.4.1)
     RColorBrewer    1.1-3      2022-04-03 [1] CRAN (R 4.4.0)
     Rcpp            1.0.13     2024-07-17 [1] CRAN (R 4.4.1)
     readr           2.1.5      2024-01-10 [1] CRAN (R 4.4.1)
     readxl          1.4.3      2023-07-06 [1] CRAN (R 4.4.1)
     recipes         1.1.0      2024-07-04 [1] CRAN (R 4.4.1)
     rematch         2.0.0      2023-08-30 [1] CRAN (R 4.4.1)
     rematch2        2.1.2      2020-05-01 [1] CRAN (R 4.4.1)
     reprex          2.1.1      2024-07-06 [1] CRAN (R 4.4.1)
     rlang           1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
     rmarkdown       2.28       2024-08-17 [1] CRAN (R 4.4.1)
     rpart           4.1.23     2023-12-05 [1] CRAN (R 4.4.1)
     rsample         1.2.1      2024-03-25 [1] CRAN (R 4.4.1)
     rstudioapi      0.16.0     2024-03-24 [1] CRAN (R 4.4.1)
     rvest           1.0.4      2024-02-12 [1] CRAN (R 4.4.1)
     s2              1.1.7      2024-07-17 [1] CRAN (R 4.4.1)
     sass            0.4.9      2024-03-15 [1] CRAN (R 4.4.1)
     scales          1.3.0      2023-11-28 [1] CRAN (R 4.4.1)
     selectr         0.4-2      2019-11-20 [1] CRAN (R 4.4.1)
     sf              1.0-16     2024-03-24 [1] CRAN (R 4.4.1)
     sfd             0.1.0      2024-01-08 [1] CRAN (R 4.4.1)
     shape           1.4.6.1    2024-02-23 [1] CRAN (R 4.4.0)
     slider          0.3.1      2023-10-12 [1] CRAN (R 4.4.1)
     SQUAREM         2021.1     2021-01-13 [1] CRAN (R 4.4.0)
     stringi         1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
     stringr         1.5.1      2023-11-14 [1] CRAN (R 4.4.1)
     survival        3.7-0      2024-06-05 [1] CRAN (R 4.4.1)
     sys             3.4.2      2023-05-23 [1] CRAN (R 4.4.1)
     systemfonts     1.1.0      2024-05-15 [1] CRAN (R 4.4.1)
     terra           1.7-78     2024-05-22 [1] CRAN (R 4.4.1)
     textshaping     0.4.0      2024-05-24 [1] CRAN (R 4.4.1)
     tibble          3.2.1      2023-03-20 [1] CRAN (R 4.4.1)
     tidymodels      1.2.0      2024-03-25 [1] CRAN (R 4.4.1)
     tidyr           1.3.1      2024-01-24 [1] CRAN (R 4.4.1)
     tidyselect      1.2.1      2024-03-11 [1] CRAN (R 4.4.1)
     tidyverse       2.0.0      2023-02-22 [1] CRAN (R 4.4.1)
     timechange      0.3.0      2024-01-18 [1] CRAN (R 4.4.1)
     timeDate        4032.109   2023-12-14 [1] CRAN (R 4.4.1)
     tinytex         0.52       2024-07-18 [1] CRAN (R 4.4.1)
     tune            1.2.1      2024-04-18 [1] CRAN (R 4.4.1)
     tzdb            0.4.0      2023-05-12 [1] CRAN (R 4.4.1)
     units           0.8-5      2023-11-28 [1] CRAN (R 4.4.1)
     utf8            1.2.4      2023-10-22 [1] CRAN (R 4.4.1)
     uuid            1.2-1      2024-07-29 [1] CRAN (R 4.4.1)
     vctrs           0.6.5      2023-12-01 [1] CRAN (R 4.4.1)
     viridisLite     0.4.2      2023-05-02 [1] CRAN (R 4.4.1)
     vroom           1.6.5      2023-12-05 [1] CRAN (R 4.4.1)
     warp            0.2.1      2023-11-02 [1] CRAN (R 4.4.1)
     withr           3.0.1      2024-07-31 [1] CRAN (R 4.4.1)
     wk              0.9.3      2024-09-06 [1] CRAN (R 4.4.1)
     workflows       1.1.4      2024-02-19 [1] CRAN (R 4.4.1)
     workflowsets    1.1.0      2024-03-21 [1] CRAN (R 4.4.1)
     xfun            0.47       2024-08-17 [1] CRAN (R 4.4.1)
     xml2            1.3.6      2023-12-04 [1] CRAN (R 4.4.1)
     yaml            2.3.10     2024-07-26 [1] CRAN (R 4.4.1)
     yardstick       1.3.1      2024-03-21 [1] CRAN (R 4.4.1)

     [1] C:/Users/jwicquart/AppData/Local/Programs/R/R-4.4.1/library

    ──────────────────────────────────────────────────────────────────────────────
