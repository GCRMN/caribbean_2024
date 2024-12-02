// 1. Define the rectangle for the Caribbean ----

var rectangleBounds = ee.Geometry.Rectangle(
  [-105, 6, -50, 38]
);

// 2. Topography data ----

var dataset_topo = ee.Image('USGS/SRTMGL1_003');

Export.image.toDrive({
  image: dataset_topo,
  folder:"GEE",
  fileNamePrefix:"topography",
  description: "topography",
  region: rectangleBounds,
  maxPixels:20e10,
  scale: 200,
  fileFormat: 'GeoTIFF',
  formatOptions: {
    cloudOptimized: true
  }
});

// 3. Bathymetry data ----

// 3.1 Load data ----

var dataset_bathy = ee.Image('NOAA/NGDC/ETOPO1')
  .select("bedrock")
  .clip(rectangleBounds);

// 3.2 Increase spatial resolution ----

var dataset_bathy = dataset_bathy.resample('bilinear').reproject({'crs': 'EPSG:4326','scale': 300});

// 3.3 Define the thresholds ----

var zones = dataset_bathy
  .lt(0)
  .add(dataset_bathy.lt(-0.1))
  .add(dataset_bathy.lt(-5))
  .add(dataset_bathy.lt(-10))
  .add(dataset_bathy.lt(-20))
  .add(dataset_bathy.lt(-50))
  .add(dataset_bathy.lt(-100))
  .add(dataset_bathy.lt(-200))
  .add(dataset_bathy.lt(-300))
  .add(dataset_bathy.lt(-500))
  .add(dataset_bathy.lt(-1000))
  .add(dataset_bathy.lt(-2000))
  .add(dataset_bathy.lt(-3000))
  .add(dataset_bathy.lt(-4000))
  .add(dataset_bathy.lt(-5000));
  
zones = zones.updateMask(zones.neq(0));

// 3.4 Convert the zones of the thresholded vectors ----

var vectors = zones.addBands(dataset_bathy).reduceToVectors({
  geometry: rectangleBounds,
  crs: 'EPSG:4326',
  scale: 300,
  maxPixels: 1e30,
  geometryType: 'polygon',
  eightConnected: false,
  labelProperty: 'zone',
  reducer: ee.Reducer.mean()
});

// 3.5 Export the data ----

Export.table.toDrive({
  collection:vectors,
  folder:"GEE",
  fileNamePrefix:"bathymetry",
  fileFormat:"SHP",
  description:"bathymetry"
});
