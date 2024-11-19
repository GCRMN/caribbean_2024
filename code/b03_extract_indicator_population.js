// A. Population within 5 km from coral reefs ---------------------------------------------------

// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/pacific_2023_reefs");

// 2. Create 5 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(5000); // 5 km  
};

var buffer_reef = data_reefs.map(reef_buffer).union();

//Map.addLayer(buffer_reef);

// 3. Join with EEZ ----

var data_eez = ee.FeatureCollection("users/jeremywicquart/pacific_2023_eez");

var vectorList = data_eez.toList(data_eez.size());

var data_eez_reef = buffer_reef.iterate(function(feature, list){
  list = ee.List(list);
  feature = ee.Feature(feature);

  var intersection = vectorList.map(function(feat) {
    feat = ee.Feature(feat);
    var intersection = feat.intersection(feature, ee.ErrorMargin(1));
    return ee.Feature(intersection).set({'Intersect': intersection.area().divide(1000 * 1000)});
  });

  return list.add(intersection);
}, ee.List([]));

var data_eez_reef = ee.FeatureCollection(ee.List(data_eez_reef).flatten());

//Map.addLayer(data_eez_reef);

// 4. Import GPW count ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
                  .select('population_count');

// 5. Empty Collection to fill ----

var ft = ee.FeatureCollection(ee.List([]));

// 6. Create function to extract population ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions({
    reducer: ee.Reducer.sum(),
    collection: data_eez_reef,
    scale: 930,
    });
  
  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 7. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 8. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_5km",
  fileFormat:"CSV",
  description:"ind_human-pop_5km",
  selectors:["TERRITORY1", "date", "sum"],
});

// B. Population per EEZ -----------------------------------------------------------------

// 1. Import data ----

var data_eez = ee.FeatureCollection("users/jeremywicquart/pacific_2023_eez");

// 2. Import GPW count ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
                  .select('population_count');

// 3. Empty Collection to fill ----

var ft = ee.FeatureCollection(ee.List([]));

// 4. Create function to extract SST ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions({
    reducer: ee.Reducer.sum(),
    collection: data_eez,
    scale: 930,
    });
  
  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 5. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 6. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_eez",
  fileFormat:"CSV",
  description:"ind_human-pop_eez",
  selectors:["TERRITORY1", "date", "sum"],
});
