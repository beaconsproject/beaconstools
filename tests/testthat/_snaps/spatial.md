# benchmarks build as expected

    Code
      catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c("PB_0001",
        "PB_0002"))
    Output
      Simple feature collection with 2 features and 1 field
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: 686200.9 ymin: -1540493 xmax: 752950.9 ymax: -1418750
      Projected CRS: NAD_1983_Albers
        network                       geometry
      1 PB_0001 POLYGON ((747821.7 -1460714...
      2 PB_0002 POLYGON ((703339.8 -1531127...

# networks build as expected

    Code
      catchments_to_benchmarks(benchmark_table_sample, catchments_sample, c(
        "PB_0001__PB_0002"))
    Output
      Simple feature collection with 1 feature and 1 field
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: 686200.9 ymin: -1540493 xmax: 752950.9 ymax: -1418750
      Projected CRS: NAD_1983_Albers
                 network                       geometry
      1 PB_0001__PB_0002 MULTIPOLYGON (((705280 -153...

# reserve is added as expected

    Code
      append_reserve(benchmarks, pa_1, "PA_1")
    Output
      Simple feature collection with 3 features and 1 field
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: 478494.8 ymin: -1540493 xmax: 752950.9 ymax: -1305868
      Projected CRS: NAD_1983_Albers
        network                       geometry
      1 PB_0001 POLYGON ((747821.7 -1460714...
      2 PB_0002 POLYGON ((703339.8 -1531127...
      3    PA_1 POLYGON ((478494.8 -1382608...

# reserve is dissolved correctly

    Code
      append_reserve(benchmarks, pa_1, "PA_1")
    Output
      Simple feature collection with 2 features and 1 field
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: 686200.9 ymin: -1540493 xmax: 752950.9 ymax: -1418750
      Projected CRS: NAD_1983_Albers
        network                       geometry
      1 PB_0001 POLYGON ((747821.7 -1460714...
      2    PA_1 POLYGON ((704062 -1531639, ...

# networks made as expected

    Code
      benchmarks_to_networks(benchmarks, c("PB_0001__PB_0002", "PB_0001__PB_0003"))
    Output
      Simple feature collection with 2 features and 1 field
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: 686200.9 ymin: -1540493 xmax: 752950.9 ymax: -1418750
      Projected CRS: NAD_1983_Albers
                 network                       geometry
      1 PB_0001__PB_0002 MULTIPOLYGON (((704062 -153...
      2 PB_0001__PB_0003 POLYGON ((737410.4 -1513663...

