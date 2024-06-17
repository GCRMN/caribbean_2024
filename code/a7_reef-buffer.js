// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/caribbean_2024_reefs");

// 2. Create 100 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(100000); // 100 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 3. Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 4. Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reefs_buffer",
  fileFormat:"SHP",
  description:"reefs_buffer"
});
