
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **Status and Trends of Caribbean Coral Reefs: 1970-2024**

## 1. Introduction [<img src='misc/2025-10-20_report-page-cover.jpg' align="right" height="300" />](https://gcrmn.net/pacific-report-2025-v1/)

Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed non risus.
Suspendisse lectus tortor, dignissim sit amet, adipiscing nec, ultricies
sed, dolor. Cras elementum ultrices diam. Maecenas ligula massa, varius
a, semper congue, euismod non, mi. Proin porttitor, orci nec nonummy
molestie, enim est eleifend mi, non fermentum diam nisl sit amet erat.
Duis semper. Duis arcu massa, scelerisque vitae, consequat in, pretium
a, enim. Pellentesque congue. Ut in risus volutpat libero pharetra
tempor. Cras vestibulum bibendum augue. Praesent egestas leo in pede.
Praesent blandit odio eu enim. Pellentesque sed dui ut augue blandit
sodales. Vestibulum ante ipsum primis in faucibus orci luctus et
ultrices posuere cubilia Curae; Aliquam nibh. Mauris ac mauris sed pede
pellentesque fermentum. Maecenas adipiscing ante non diam sodales
hendrerit.

Ut velit mauris, egestas sed, gravida nec, ornare ut, mi. Aenean ut orci
vel massa suscipit pulvinar. Nulla sollicitudin. Fusce varius, ligula
non tempus aliquam, nunc turpis ullamcorper nibh, in tempus sapien eros
vitae ligula. Pellentesque rhoncus nunc et augue. Integer id felis.
Curabitur aliquet pellentesque diam. Integer quis metus vitae elit
lobortis egestas. Lorem ipsum dolor sit amet, consectetuer adipiscing
elit. Morbi vel erat non mauris convallis vehicula. Nulla et sapien.
Integer tortor tellus, aliquam faucibus, convallis id, congue eu, quam.
Mauris ullamcorper felis vitae erat. Proin feugiat, augue non elementum
posuere, metus purus iaculis lectus, et tristique ligula justo vitae
magna.

Aliquam convallis sollicitudin purus. Praesent aliquam, enim at
fermentum mollis, ligula massa adipiscing nisl, ac euismod nibh nisl eu
lectus. Fusce vulputate sem at sapien. Vivamus leo. Aliquam euismod
libero eu enim. Nulla nec felis sed leo placerat imperdiet. Aenean
suscipit nulla in justo. Suspendisse cursus rutrum augue. Nulla
tincidunt tincidunt mi. Curabitur iaculis, lorem vel rhoncus faucibus,
felis magna fermentum augue, et ultricies lacus lorem varius purus.
Curabitur eu amet.

## 2. Code

### Functions

- `combine_model_data.R` Combine benthic cover model results.
- `combine_plot_trends.R` Combine temporal trend plots produced by the
  function *plot_trends.R*.
- `create_chapter_doc.qmd` Countries and territories chapter template.
- `data_descriptors.R` Get number of sites, surveys, datasets, first and
  last year of monitoring.
- `download_predictors.R` Download predictors extracted through GEE and
  stored on Google Drive.
- `extract_coeff.R` Extract linear models *a* and *b* coefficients.
- `extract_mankendall.R` Calculate temporal trends using Man Kendall
  test.
- `graphical_par.R` Graphical parameters, including colors and fonts.
- `plot_pred_obs.R` Plot predicted *vs* observed values (model
  evaluation).
- `plot_region.R` Regional background map.
- `plot_residuals.R` Plot residuals (model evaluation).
- `plot_trends.R` Plot temporal trends.
- `plot_vimp.R` Plot Variable Importance Plot (VIMP).
- `prepare_benthic_data.R` Prepare benthic cover data.
- `plot_pdp.R` Plot Partial Dependence Plot (PDP).
- `plot_prediction_map.R` Plot the map of the Caribbean with predicted
  values.
- `render_qmd.R` Render script *create_chapter_doc.qmd*.
- `theme_graph.R` Main ggplot theme for plots.
- `theme_map.R` Main ggplot theme for maps.
- `theme_map_area.R` ggplot theme for countries and territories maps.

### Cleaning and selection (`a_`)

- `a01_select_topo-bathy.js` Extract topography and bathymetry using
  [Google Earth Engine](https://earthengine.google.com/) (GEE).
- `a02_clean_shp.R` Clean shapefiles from different sources.
- `a03_select_benthic-data.R` Extract benthic cover data from
  [gcrmndb_benthos](https://github.com/GCRMN/gcrmndb_benthos).
- `a04_benthic-data_sources.R` Extract lists of datasetID and
  contributors details.
- `a05_reef-buffer.js` Create coral reef buffer polygons at 20, 50, and
  100 km using [GEE](https://earthengine.google.com/).
- `a06_reef-buffer-area.R` Join reef buffer with areas’ boundaries.
- `a07_clean_cyclones.R` Clean cyclones data.
- `a08_download_crw-year.R` Extract netCDF file aggregated per year from
  Coral Reef Watch (CRW).

### Indicators’ extraction (`b_`)

- `b01_extract_indicator_population.js` Extract population indicators
  using [GEE](https://earthengine.google.com/).
- `b02_extract_indicator_sst.R` Extract SST indicators.
- `b03_extract_indicator_cyclones.R` Extract cyclones indicators.

### Models (benthic cover) (`c_`)

- `c01_explo_benthic-data.qmd` Exploratory analyses of benthic cover
  data.
- `c02_select_pred-sites.js` A
- `c03_extract_predictor_gee.js` A
- `c04_extract_predictor_gravity.R` A
- `c05_extract_predictor_enso.R` A
- `c06_extract_predictor_cyclones.R` A
- `c07_extract_predictor_crw.R` A
- `c08_model_data-preparation.R` Combine predictors and prepare cover
  benthic data.
- `c09_xgboost-model.R` Run XGBoost models.

### Figures and tables (`d_`)

- `d01_materials-and-methods.R` Produce figures for the Materials and
  Methods.
- `d02_region_map.R` Produce map of the region.
- `d03_areas_map.R` Produce contextual maps for areas.
- `d04_region_sst.R` Produce figures for SST for the region.
- `d05_areas_sst.R` Produce figures for SST for areas.
- `d06_region_cyclones.R` Produce figures for cyclones for the region.
- `d07_areas_cyclones.R` Produce figures for cyclones for areas.
- `d08_region_spatio-temporal.R` Produce figures for spatio-temporal
  distribution of monitoring for the region.
- `d09_areas_spatio-temporal.R` Produce figures for spatio-temporal
  distribution of monitoring for areas.
- `d10_benthic-cover_trends.R` Produce figures for benthic cover
  temporal trends.
- `d11_other-indicators.R` Produce figures, tables, and numbers for
  other indicators.
- `d12_fish-biomass.R` Produce figure for the fish biomass box.
- `d13_case-study.R` Produce figures for the case studies.

### Writing and sharing (`e_`)

- `e01_create-chapters.R` Generate .docx chapters for areas.
- `e02_authors-contribution.R` Export author’s contributions .xlsx file.
- `e03_push-google-drive.R` Export figures to the Google Drive folder.

## 3. Reproducibility parameters

    Warning in system2("quarto", "-V", stdout = TRUE, env = paste0("TMPDIR=", :
    l'exécution de la commande '"quarto"
    TMPDIR=C:/Users/jerem/AppData/Local/Temp/Rtmpc3gYnV/file44c67e3e4a -V' renvoie
    un statut 1
    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.5.1 (2025-06-13 ucrt)
     os       Windows 11 x64 (build 26200)
     system   x86_64, mingw32
     ui       RTerm
     language (EN)
     collate  French_France.utf8
     ctype    French_France.utf8
     tz       Europe/Paris
     date     2025-10-21
     pandoc   3.6.3 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
     quarto   NA @ C:\\PROGRA~1\\RStudio\\RESOUR~1\\app\\bin\\quarto\\bin\\quarto.exe

    ─ Packages ───────────────────────────────────────────────────────────────────
     package       * version    date (UTC) lib source
     askpass         1.2.1      2024-10-04 [1] CRAN (R 4.5.0)
     backports       1.5.0      2024-05-23 [1] CRAN (R 4.5.0)
     base64enc       0.1-3      2015-07-28 [1] CRAN (R 4.5.0)
     bit             4.6.0      2025-03-06 [1] CRAN (R 4.5.0)
     bit64           4.6.0-1    2025-01-16 [1] CRAN (R 4.5.0)
     blob            1.2.4      2023-03-17 [1] CRAN (R 4.5.0)
     broom           1.0.10     2025-09-13 [1] CRAN (R 4.5.1)
     bslib           0.9.0      2025-01-30 [1] CRAN (R 4.5.0)
     cachem          1.1.0      2024-05-16 [1] CRAN (R 4.5.0)
     callr           3.7.6      2024-03-25 [1] CRAN (R 4.5.0)
     cellranger      1.1.0      2016-07-27 [1] CRAN (R 4.5.0)
     class           7.3-23     2025-01-01 [2] CRAN (R 4.5.1)
     classInt        0.4-11     2025-01-08 [1] CRAN (R 4.5.0)
     cli             3.6.5      2025-04-23 [1] CRAN (R 4.5.0)
     clipr           0.8.0      2022-02-22 [1] CRAN (R 4.5.0)
     clock           0.7.3      2025-03-21 [1] CRAN (R 4.5.0)
     codetools       0.2-20     2024-03-31 [2] CRAN (R 4.5.1)
     conflicted      1.2.0      2023-02-01 [1] CRAN (R 4.5.0)
     cpp11           0.5.2      2025-03-03 [1] CRAN (R 4.5.0)
     crayon          1.5.3      2024-06-20 [1] CRAN (R 4.5.0)
     curl            7.0.0      2025-08-19 [1] CRAN (R 4.5.1)
     data.table      1.17.8     2025-07-10 [1] CRAN (R 4.5.1)
     DBI             1.2.3      2024-06-02 [1] CRAN (R 4.5.0)
     dbplyr          2.5.1      2025-09-10 [1] CRAN (R 4.5.1)
     diagram         1.6.5      2020-09-30 [1] CRAN (R 4.5.0)
     dials           1.4.2      2025-09-04 [1] CRAN (R 4.5.1)
     DiceDesign      1.10       2023-12-07 [1] CRAN (R 4.5.0)
     digest          0.6.37     2024-08-19 [1] CRAN (R 4.5.0)
     dplyr           1.1.4      2023-11-17 [1] CRAN (R 4.5.0)
     dtplyr          1.3.2      2025-09-10 [1] CRAN (R 4.5.1)
     e1071           1.7-16     2024-09-16 [1] CRAN (R 4.5.0)
     evaluate        1.0.5      2025-08-27 [1] CRAN (R 4.5.1)
     farver          2.1.2      2024-05-13 [1] CRAN (R 4.5.0)
     fastmap         1.2.0      2024-05-15 [1] CRAN (R 4.5.0)
     fontawesome     0.5.3      2024-11-16 [1] CRAN (R 4.5.0)
     forcats         1.0.1      2025-09-25 [1] CRAN (R 4.5.1)
     fs              1.6.6      2025-04-12 [1] CRAN (R 4.5.0)
     furrr           0.3.1      2022-08-15 [1] CRAN (R 4.5.0)
     future          1.67.0     2025-07-29 [1] CRAN (R 4.5.1)
     future.apply    1.20.0     2025-06-06 [1] CRAN (R 4.5.0)
     gargle          1.6.0      2025-09-03 [1] CRAN (R 4.5.1)
     generics        0.1.4      2025-05-09 [1] CRAN (R 4.5.0)
     ggplot2         4.0.0      2025-09-11 [1] CRAN (R 4.5.1)
     globals         0.18.0     2025-05-08 [1] CRAN (R 4.5.0)
     glue            1.8.0      2024-09-30 [1] CRAN (R 4.5.0)
     googledrive     2.1.2      2025-09-10 [1] CRAN (R 4.5.1)
     googlesheets4   1.1.2      2025-09-03 [1] CRAN (R 4.5.1)
     gower           1.0.2      2024-12-17 [1] CRAN (R 4.5.0)
     GPfit           1.0-9      2025-04-12 [1] CRAN (R 4.5.0)
     gtable          0.3.6      2024-10-25 [1] CRAN (R 4.5.0)
     hardhat         1.4.2      2025-08-20 [1] CRAN (R 4.5.1)
     haven           2.5.5      2025-05-30 [1] CRAN (R 4.5.0)
     highr           0.11       2024-05-26 [1] CRAN (R 4.5.0)
     hms             1.1.3      2023-03-21 [1] CRAN (R 4.5.0)
     htmltools       0.5.8.1    2024-04-04 [1] CRAN (R 4.5.0)
     httr            1.4.7      2023-08-15 [1] CRAN (R 4.5.0)
     ids             1.0.1      2017-05-31 [1] CRAN (R 4.5.0)
     infer           1.0.9      2025-06-26 [1] CRAN (R 4.5.0)
     ipred           0.9-15     2024-07-18 [1] CRAN (R 4.5.0)
     isoband         0.2.7      2022-12-20 [1] CRAN (R 4.5.0)
     jquerylib       0.1.4      2021-04-26 [1] CRAN (R 4.5.0)
     jsonlite        2.0.0      2025-03-27 [1] CRAN (R 4.5.0)
     KernSmooth      2.23-26    2025-01-01 [2] CRAN (R 4.5.1)
     knitr           1.50       2025-03-16 [1] CRAN (R 4.5.0)
     labeling        0.4.3      2023-08-29 [1] CRAN (R 4.5.0)
     lattice         0.22-7     2025-04-02 [2] CRAN (R 4.5.1)
     lava            1.8.1      2025-01-12 [1] CRAN (R 4.5.0)
     lhs             1.2.0      2024-06-30 [1] CRAN (R 4.5.0)
     lifecycle       1.0.4      2023-11-07 [1] CRAN (R 4.5.0)
     listenv         0.9.1      2024-01-29 [1] CRAN (R 4.5.0)
     lubridate       1.9.4      2024-12-08 [1] CRAN (R 4.5.0)
     magrittr        2.0.3      2022-03-30 [1] CRAN (R 4.5.0)
     MASS            7.3-65     2025-02-28 [2] CRAN (R 4.5.1)
     Matrix          1.7-4      2025-08-28 [1] CRAN (R 4.5.1)
     memoise         2.0.1      2021-11-26 [1] CRAN (R 4.5.0)
     mime            0.13       2025-03-17 [1] CRAN (R 4.5.0)
     modeldata       1.5.1      2025-08-22 [1] CRAN (R 4.5.1)
     modelenv        0.2.0      2024-10-14 [1] CRAN (R 4.5.0)
     modelr          0.1.11     2023-03-22 [1] CRAN (R 4.5.0)
     nnet            7.3-20     2025-01-01 [2] CRAN (R 4.5.1)
     numDeriv        2016.8-1.1 2019-06-06 [1] CRAN (R 4.5.0)
     openssl         2.3.4      2025-09-30 [1] CRAN (R 4.5.1)
     parallelly      1.45.1     2025-07-24 [1] CRAN (R 4.5.1)
     parsnip         1.3.3      2025-08-31 [1] CRAN (R 4.5.1)
     patchwork       1.3.2      2025-08-25 [1] CRAN (R 4.5.1)
     pillar          1.11.1     2025-09-17 [1] CRAN (R 4.5.1)
     pkgconfig       2.0.3      2019-09-22 [1] CRAN (R 4.5.0)
     prettyunits     1.2.0      2023-09-24 [1] CRAN (R 4.5.0)
     processx        3.8.6      2025-02-21 [1] CRAN (R 4.5.0)
     prodlim         2025.04.28 2025-04-28 [1] CRAN (R 4.5.0)
     progress        1.2.3      2023-12-06 [1] CRAN (R 4.5.0)
     progressr       0.17.0     2025-10-15 [1] CRAN (R 4.5.1)
     proxy           0.4-27     2022-06-09 [1] CRAN (R 4.5.0)
     ps              1.9.1      2025-04-12 [1] CRAN (R 4.5.0)
     purrr           1.1.0      2025-07-10 [1] CRAN (R 4.5.1)
     R6              2.6.1      2025-02-15 [1] CRAN (R 4.5.0)
     ragg            1.5.0      2025-09-02 [1] CRAN (R 4.5.1)
     rappdirs        0.3.3      2021-01-31 [1] CRAN (R 4.5.0)
     RColorBrewer    1.1-3      2022-04-03 [1] CRAN (R 4.5.0)
     Rcpp            1.1.0      2025-07-02 [1] CRAN (R 4.5.1)
     readr           2.1.5      2024-01-10 [1] CRAN (R 4.5.0)
     readxl          1.4.5      2025-03-07 [1] CRAN (R 4.5.0)
     recipes         1.3.1      2025-05-21 [1] CRAN (R 4.5.0)
     rematch         2.0.0      2023-08-30 [1] CRAN (R 4.5.0)
     rematch2        2.1.2      2020-05-01 [1] CRAN (R 4.5.0)
     reprex          2.1.1      2024-07-06 [1] CRAN (R 4.5.0)
     rlang           1.1.6      2025-04-11 [1] CRAN (R 4.5.0)
     rmarkdown       2.30       2025-09-28 [1] CRAN (R 4.5.1)
     rpart           4.1.24     2025-01-07 [2] CRAN (R 4.5.1)
     rsample         1.3.1      2025-07-29 [1] CRAN (R 4.5.1)
     rstudioapi      0.17.1     2024-10-22 [1] CRAN (R 4.5.0)
     rvest           1.0.5      2025-08-29 [1] CRAN (R 4.5.1)
     s2              1.1.9      2025-05-23 [1] CRAN (R 4.5.0)
     S7              0.2.0      2024-11-07 [1] CRAN (R 4.5.1)
     sass            0.4.10     2025-04-11 [1] CRAN (R 4.5.0)
     scales          1.4.0      2025-04-24 [1] CRAN (R 4.5.0)
     selectr         0.4-2      2019-11-20 [1] CRAN (R 4.5.0)
     sf              1.0-21     2025-05-15 [1] CRAN (R 4.5.0)
     sfd             0.1.0      2024-01-08 [1] CRAN (R 4.5.0)
     shape           1.4.6.1    2024-02-23 [1] CRAN (R 4.5.0)
     slider          0.3.2      2024-10-25 [1] CRAN (R 4.5.0)
     sparsevctrs     0.3.4      2025-05-25 [1] CRAN (R 4.5.0)
     SQUAREM         2021.1     2021-01-13 [1] CRAN (R 4.5.0)
     stringi         1.8.7      2025-03-27 [1] CRAN (R 4.5.0)
     stringr         1.5.2      2025-09-08 [1] CRAN (R 4.5.1)
     survival        3.8-3      2024-12-17 [2] CRAN (R 4.5.1)
     sys             3.4.3      2024-10-04 [1] CRAN (R 4.5.0)
     systemfonts     1.3.1      2025-10-01 [1] CRAN (R 4.5.1)
     tailor          0.1.0      2025-08-25 [1] CRAN (R 4.5.1)
     terra           1.8-70     2025-09-27 [1] CRAN (R 4.5.1)
     textshaping     1.0.3      2025-09-02 [1] CRAN (R 4.5.1)
     tibble          3.3.0      2025-06-08 [1] CRAN (R 4.5.0)
     tidymodels      1.4.1      2025-09-08 [1] CRAN (R 4.5.1)
     tidyr           1.3.1      2024-01-24 [1] CRAN (R 4.5.0)
     tidyselect      1.2.1      2024-03-11 [1] CRAN (R 4.5.0)
     tidyverse       2.0.0      2023-02-22 [1] CRAN (R 4.5.0)
     timechange      0.3.0      2024-01-18 [1] CRAN (R 4.5.0)
     timeDate        4041.110   2024-09-22 [1] CRAN (R 4.5.0)
     tinytex         0.57       2025-04-15 [1] CRAN (R 4.5.0)
     tune            2.0.0      2025-09-01 [1] CRAN (R 4.5.1)
     tzdb            0.5.0      2025-03-15 [1] CRAN (R 4.5.0)
     units           0.8-7      2025-03-11 [1] CRAN (R 4.5.0)
     utf8            1.2.6      2025-06-08 [1] CRAN (R 4.5.0)
     uuid            1.2-1      2024-07-29 [1] CRAN (R 4.5.0)
     vctrs           0.6.5      2023-12-01 [1] CRAN (R 4.5.0)
     viridisLite     0.4.2      2023-05-02 [1] CRAN (R 4.5.0)
     vroom           1.6.6      2025-09-19 [1] CRAN (R 4.5.1)
     warp            0.2.1      2023-11-02 [1] CRAN (R 4.5.0)
     withr           3.0.2      2024-10-28 [1] CRAN (R 4.5.0)
     wk              0.9.4      2024-10-11 [1] CRAN (R 4.5.0)
     workflows       1.3.0      2025-08-27 [1] CRAN (R 4.5.1)
     workflowsets    1.1.1      2025-05-27 [1] CRAN (R 4.5.0)
     xfun            0.53       2025-08-19 [1] CRAN (R 4.5.1)
     xml2            1.4.0      2025-08-20 [1] CRAN (R 4.5.1)
     yaml            2.3.10     2024-07-26 [1] CRAN (R 4.5.0)
     yardstick       1.3.2      2025-01-22 [1] CRAN (R 4.5.0)

     [1] C:/Users/jerem/AppData/Local/R/win-library/4.5
     [2] C:/Program Files/R/R-4.5.1/library

    ──────────────────────────────────────────────────────────────────────────────
