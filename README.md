# ChangePoint
Simulate a time series with a change in the mean (changepoint) and identify it. Compile with

`gfortran kind.f90 random.f90 split_data.f90 xsplit_data.f90`

Sample results:

```
 Test 1: Constant Mean
                     true mean:    0.000000

             Optimal partition: 946
                         Means:    0.001879    0.017915   -0.273652
                           MSE:    1.041606    1.048536    0.842187
                Test Statistic:    2.275229
                       p-value:    0.524752
                Test Statistic:    2.275229
                       p-value:    0.524752

             Optimal partition: 281
                         Means:    0.002373   -0.196010    0.079521
                           MSE:    1.037042    1.048643    1.011274
                Test Statistic:    3.839547
                       p-value:    0.158416
                Test Statistic:    3.839547
                       p-value:    0.158416

             Optimal partition: 6
                         Means:   -0.026603   -1.559275   -0.018901
                           MSE:    0.997165    0.281353    0.988899
                Test Statistic:    6.437002
                       p-value:    0.039604
                Test Statistic:    6.437002
                       p-value:    0.039604


Test 2: Change Point
                    true means:    0.000000    0.200000
                true partition: 501

           Optimal partition k: 491
   means of of true partitions:    4.969542    5.401683
                         Means:    5.185612    4.953796    5.408337
                           MSE:    1.040547    0.962473    1.014321
                Test Statistic:    7.229439
                       p-value:    0.079208

           Optimal partition k: 543
   means of of true partitions:    5.072474    5.503031
                         Means:    5.287753    5.079885    5.533745
                           MSE:    1.035794    1.012404    0.951829
                Test Statistic:    7.224981
                       p-value:    0.059406

           Optimal partition k: 484
   means of of true partitions:    4.989867    5.419885
                         Means:    5.204876    4.970376    5.423954
                           MSE:    1.059612    0.999392    1.016501
                Test Statistic:    7.140279
                       p-value:    0.079208
```
                    
