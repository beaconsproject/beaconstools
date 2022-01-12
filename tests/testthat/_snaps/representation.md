# targets are as expected with 5000 reserve size

    Code
      gen_targets(catchments_sample, led_sample, 5000)
    Output
      # A tibble: 13 x 5
         class_value area_km2 reserve_size class_proportion target_km2
               <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
       1           1   3145.          5000         0.610       3048.  
       2           2    421.          5000         0.0817       409.  
       3           3    267.          5000         0.0517       259.  
       4           4    334.          5000         0.0648       324.  
       5           5    522.          5000         0.101        506.  
       6           6    150.          5000         0.0291       145.  
       7           7     98.5         5000         0.0191        95.4 
       8           8     80.1         5000         0.0155        77.6 
       9           9     62.1         5000         0.0120        60.2 
      10          10     37           5000         0.00717       35.9 
      11          11     20           5000         0.00388       19.4 
      12          12     19           5000         0.00368       18.4 
      13          13      3           5000         0.000582       2.91

# targets are as expected with 10000 reserve size

    Code
      gen_targets(catchments_sample, led_sample, 10000)
    Output
      # A tibble: 13 x 5
         class_value area_km2 reserve_size class_proportion target_km2
               <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
       1           1   3145.         10000         0.610       6096.  
       2           2    421.         10000         0.0817       817.  
       3           3    267.         10000         0.0517       517.  
       4           4    334.         10000         0.0648       648.  
       5           5    522.         10000         0.101       1011.  
       6           6    150.         10000         0.0291       291.  
       7           7     98.5        10000         0.0191       191.  
       8           8     80.1        10000         0.0155       155.  
       9           9     62.1        10000         0.0120       120.  
      10          10     37          10000         0.00717       71.7 
      11          11     20          10000         0.00388       38.8 
      12          12     19          10000         0.00368       36.8 
      13          13      3          10000         0.000582       5.82

# targets are as expected with class_vals

    Code
      gen_targets(catchments_sample, led_sample, 10000, c(1:5))
    Output
      # A tibble: 5 x 5
        class_value area_km2 reserve_size class_proportion target_km2
              <dbl>    <dbl>        <dbl>            <dbl>      <dbl>
      1           1    3145.        10000           0.671       6706.
      2           2     421.        10000           0.0899       899.
      3           3     267.        10000           0.0569       569.
      4           4     334.        10000           0.0713       713.
      5           5     522.        10000           0.111       1113.

