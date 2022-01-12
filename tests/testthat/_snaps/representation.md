# targets are as expected with 5000 reserve size

    Code
      gen_targets(ref_poly, led_sample, 5000)
    Output
      # A tibble: 15 x 5
         class_value area_km2 reserve_size class_proportion target_km2
               <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
       1           1   94684.         5000        0.727        3634.  
       2           2   11316.         5000        0.0869        434.  
       3           3    6748.         5000        0.0518        259.  
       4           4    5119.         5000        0.0393        196.  
       5           5    6219.         5000        0.0477        239.  
       6           6    2595.         5000        0.0199         99.6 
       7           7    1467.         5000        0.0113         56.3 
       8           8     846.         5000        0.00650        32.5 
       9           9     596.         5000        0.00458        22.9 
      10          10     337.         5000        0.00259        12.9 
      11          11     183.         5000        0.00140         7.02
      12          12      98          5000        0.000752        3.76
      13          13      39          5000        0.000299        1.5 
      14          14      19          5000        0.000146        0.73
      15          15      10          5000        0.0000768       0.38

# targets are as expected with 10000 reserve size

    Code
      gen_targets(ref_poly, led_sample, 10000)
    Output
      # A tibble: 15 x 5
         class_value area_km2 reserve_size class_proportion target_km2
               <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
       1           1   94684.        10000        0.727        7268.  
       2           2   11316.        10000        0.0869        869.  
       3           3    6748.        10000        0.0518        518.  
       4           4    5119.        10000        0.0393        393.  
       5           5    6219.        10000        0.0477        477.  
       6           6    2595.        10000        0.0199        199.  
       7           7    1467.        10000        0.0113        113.  
       8           8     846.        10000        0.00650        65.0 
       9           9     596.        10000        0.00458        45.8 
      10          10     337.        10000        0.00259        25.9 
      11          11     183.        10000        0.00140        14.0 
      12          12      98         10000        0.000752        7.52
      13          13      39         10000        0.000299        2.99
      14          14      19         10000        0.000146        1.46
      15          15      10         10000        0.0000768       0.77

# targets are as expected with class_vals

    Code
      gen_targets(ref_poly, led_sample, 10000, c(1:5))
    Output
      # A tibble: 5 x 5
        class_value area_km2 reserve_size class_proportion target_km2
              <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
      1           1   94684.        10000           0.763       7631.
      2           2   11316.        10000           0.0912       912.
      3           3    6748.        10000           0.0544       544.
      4           4    5119.        10000           0.0413       412.
      5           5    6219.        10000           0.0501       501.

