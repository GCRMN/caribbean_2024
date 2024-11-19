// 1. Combine site with observed data and sites to predict //////////////////////////////////////////////////

// 1.1 Load sites with observed data and sites to predict ----

var site_obs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords_obs");
    
var site_pred = ee.FeatureCollection("users/jeremywicquart/pacific_2023_site-coords_pred");

// 1.2 Data vizualisation ----

Map.addLayer(site_pred);
Map.addLayer(site_obs.style({color: 'red'}));

// 1.3 Merge FeatureCollections ----

var site_coords = site_pred.merge(site_obs);

// 1.4 Export the data as an SHP file ----

Export.table.toDrive({
  collection:site_coords,
  folder:"GEE",
  fileNamePrefix:"site-coords_all",
  fileFormat:"SHP",
  description:"site-coords_all"
});

// 2. Extract predictor "human population living within 5 km radius from the site" /////////////////////////

// 2.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 2.2 Apply the function (here 5 km radius) ----

var site_buffer = site_coords.map(bufferPoints(5000, false));

// 2.3 Load data population data ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
  .select('population_count');

// 2.4 Sum of population within the buffer ----

var pop_by_site = data_pop.map(function(image){
  return image.reduceRegions({
    collection:site_buffer, 
    reducer:ee.Reducer.sum().setOutputs(["pred_population"]), 
    scale: 1000
  });
})
.flatten();

// 2.5 Export the data ----

Export.table.toDrive({
  collection:pop_by_site,
  folder:"GEE",
  fileNamePrefix:"pred_human-pop",
  fileFormat:"CSV",
  description:"pred_human-pop",
  selectors:["system:index", "site_id", "type", "pred_population"]
});

// 3. Extract predictor "land extent around 10 km radius from the site" ////////////////////////////////

// 3.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 3.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 3.3 Load elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').selfMask();

// 3.4 Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 3.5 Use this layer to mask elevation data ----

var data_area = data_area.mask(elevation);

// 3.6 Extract mean elevation ----

var data_elevation = data_area.reduceRegions({
  reducer: ee.Reducer.sum().setOutputs(["pred_land"]),
  collection: site_buffer,
  scale: 90,
});

// 3.7 Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"pred_land",
  fileFormat:"CSV",
  description:"pred_land",
  selectors:["site_id", "type", "pred_land"]
});

// 4. Extract predictor "mean elevation around 10 km radius from the site" ////////////////////////////////

// 4.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 4.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 4.3 Load elevation data ----

var elevation = ee.Image('CGIAR/SRTM90_V4').select('elevation');

// 4.4 Extract mean elevation ----

var data_elevation = elevation.reduceRegions({
  reducer: ee.Reducer.mean().setOutputs(["pred_elevation"]),
  collection: site_buffer,
  scale: 90,
});

// 4.5 Export the data ----

Export.table.toDrive({
  collection:data_elevation,
  folder:"GEE",
  fileNamePrefix:"pred_elevation",
  fileFormat:"CSV",
  description:"pred_elevation",
  selectors:["site_id", "type", "pred_elevation"]
});

// 5. Extract predictor "mean chl a around 10 km radius from the site" ////////////////////////////////

// 5.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 5.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 5.3 Load chlorophyll a data ----

var data_chla = ee.ImageCollection("NASA/OCEANDATA/MODIS-Aqua/L3SMI")
                  .filter(ee.Filter.date('2002-01-01', '2022-12-31'))
                  .select('chlor_a');

// 5.4 Reduce mean between all the images ----

var pred_chla_mean = data_chla.reduce(ee.Reducer.mean());

// 5.5 Extract values for sites ----

var pred_chla_mean = pred_chla_mean.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_chla_mean"]),
  collection: site_buffer,
  scale: 10000
});

// 5.6 Export the data ----

Export.table.toDrive({
  collection:pred_chla_mean,
  folder:"GEE",
  fileNamePrefix:"pred_chla_mean",
  fileFormat:"CSV",
  description:"pred_chla_mean",
  selectors:["site_id", "type", "pred_chla_mean"]
});

// 6. Extract predictor "sd chl a around 10 km radius from the site" ////////////////////////////////////

// 6.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 6.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 6.3 Load chlorophyll a data ----

var data_chla = ee.ImageCollection("NASA/OCEANDATA/MODIS-Aqua/L3SMI")
                  .filter(ee.Filter.date('2002-01-01', '2022-12-31'))
                  .select('chlor_a');

// 6.4 Reduce mean between all the images ----

var pred_chla_sd = data_chla.reduce(ee.Reducer.stdDev());

// 6.5 Extract values for sites ----

var pred_chla_sd = pred_chla_sd.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_chla_sd"]),
  collection: site_buffer,
  scale: 10000
});

// 6.6 Export the data ----

Export.table.toDrive({
  collection:pred_chla_sd,
  folder:"GEE",
  fileNamePrefix:"pred_chla_sd",
  fileFormat:"CSV",
  description:"pred_chla_sd",
  selectors:["site_id", "type", "pred_chla_sd"]
});

// 7. Extract predictor "mean sst per year" //////////////////////////////////////////////////////

// 7.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 7.2 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 7.3 Map a function to select data within the year and apply mean reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.mean())
        .set('year', y);
    })
  );

// 7.4 Mean of SST per year for each site ----

var pred_sst_mean = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_mean"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 7.5 Export the data ----

Export.table.toDrive({
  collection:pred_sst_mean,
  folder:"GEE",
  fileNamePrefix:"pred_sst_mean",
  fileFormat:"CSV",
  description:"pred_sst_mean",
  selectors:["year", "site_id", "type", "pred_sst_mean"]
});

// 8. Extract predictor "max sst per year" //////////////////////////////////////////////////////

// 8.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 8.2 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 8.3 Map a function to select data within the year and apply max reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.max())
        .set('year', y);
    })
  );

// 8.4 Max of SST per year for each site ----

var pred_sst_max = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_max"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 8.5 Export the data ----

Export.table.toDrive({
  collection:pred_sst_max,
  folder:"GEE",
  fileNamePrefix:"pred_sst_max",
  fileFormat:"CSV",
  description:"pred_sst_max",
  selectors:["year", "site_id", "type", "pred_sst_max"]
});

// 9. Extract predictor "min sst per year" //////////////////////////////////////////////////////

// 9.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 9.2 List of years to aggregate ----

var years = ee.List.sequence(1981, 2023);

// 9.3 Map a function to select data within the year and apply min reducer ----

var data_sst_year = ee.ImageCollection.fromImages(
    years.map(function(y) {
      return data_sst
        .filter(ee.Filter.calendarRange(y, y, 'year'))
        .reduce(ee.Reducer.min())
        .set('year', y);
    })
  );

// 9.4 Min of SST per year for each site ----

var pred_sst_min = data_sst_year.map(function(image) {
  return image.reduceRegions({
    collection: site_coords,
    reducer:ee.Reducer.first().setOutputs(["pred_sst_min"])
  }).map(function (featureWithReduction) {
    return featureWithReduction.copyProperties(image);
  });
}).flatten();

// 9.5 Export the data ----

Export.table.toDrive({
  collection:pred_sst_min,
  folder:"GEE",
  fileNamePrefix:"pred_sst_min",
  fileFormat:"CSV",
  description:"pred_sst_min",
  selectors:["year", "site_id", "type", "pred_sst_min"]
});

// 10. Extract predictor "sst sd" ////////////////////////////////////////////////////////////

// 10.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');
                  
// 10.2 SD between the dates ----

var data_sst_sd = data_sst.reduce(ee.Reducer.stdDev());

// 10.3 Extract SST SD for each site ----

var result_sst_sd = data_sst_sd.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_sst_sd"]),
  collection: site_coords,
  scale: 10000
});

// 10.4 Export the data ----

Export.table.toDrive({
  collection:result_sst_sd,
  folder:"GEE",
  fileNamePrefix:"pred_sst_sd",
  fileFormat:"CSV",
  description:"pred_sst_sd",
  selectors:["site_id", "type", "pred_sst_sd"]
});

// 11. Extract predictor "sst skewness" /////////////////////////////////////////////////////////

// 11.1 Import data ----

var data_sst = ee.ImageCollection('NOAA/CDR/OISST/V2_1')
                  .filter(ee.Filter.date('1981-01-01', '2023-12-31'))
                  .select('sst');

// 11.2 Skewness between the dates ----

var data_sst_skew = data_sst.reduce(ee.Reducer.skew());

// 11.3 Extract SST skewness for each site ----

var result_sst_skew = data_sst_skew.reduceRegions({
  reducer: ee.Reducer.first().setOutputs(["pred_sst_skewness"]),
  collection: site_coords,
  scale: 10000
});

// 11.4 Export the data ----

Export.table.toDrive({
  collection:result_sst_skew,
  folder:"GEE",
  fileNamePrefix:"pred_sst_skewness",
  fileFormat:"CSV",
  description:"pred_sst_skewness",
  selectors:["site_id", "type", "pred_sst_skewness"]
});

// 12. Extract predictor "reef extent" ///////////////////////////////////////////////////////////

// 12.1 Create a function to create a buffer around a point ----

function bufferPoints(radius, bounds) {
  return function(pt) {
    pt = ee.Feature(pt);
    return bounds ? pt.buffer(radius).bounds() : pt.buffer(radius);
  };
}

// 12.2 Apply the function (here 10 km radius) ----

var site_buffer = site_coords.map(bufferPoints(10000, false));

// 12.3 Load and Allen Coral Atlas (ACA) data ----

var aca_benthic = ee.Image("ACA/reef_habitat/v2_0").select('benthic').selfMask();

// 12.4 Create a layer of surface by pixel (in km2) ----

var data_area = ee.Image.pixelArea().divide(1000000);

// 12.5 Use this layer to mask ACA data ----

var aca_area = data_area.mask(aca_benthic);

// 12.6 Calculate reef area within each buffer ----

var reef_extent = aca_area.reduceRegions({
  collection: site_buffer,
  reducer: ee.Reducer.sum().setOutputs(["pred_reefextent"]), 
  scale:5
});

// 12.7 Export the data ----

Export.table.toDrive({
  collection:reef_extent,
  folder:"GEE",
  fileNamePrefix:"pred_reef-extent",
  fileFormat:"CSV",
  description:"pred_reef-extent",
  selectors:["site_id", "type", "pred_reefextent"]
});
