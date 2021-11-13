\name{pcw}
\alias{pcw}
\docType{data}
\title{
Tree census data in a network of forest plots in Panama}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("pcw")}
\format{
  The format is:
List of 5
 $ spe   : int [1:50, 1:778] 0 0 0 0 0 0 0 0 0 0 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "P01" "P02" "P03" "P04" ...
  .. ..$ : NULL
 $ env   :'data.frame':	50 obs. of  4 variables:
  ..$ Rainfall : num [1:50] 2993 3072 3007 3000 2414 ...
  ..$ Elevation: num [1:50] 20 100 180 180 40 30 60 50 410 90 ...
  ..$ AgeCat   : Factor w/ 3 levels "1","2","3": 1 3 2 2 1 2 2 3 3 3 ...
  ..$ Geol     : Factor w/ 9 levels "pT","Tb","Tbo",..: 6 4 4 4 7 7 7 1 1 5 ...
 $ xy    : num [1:50, 1:2] -80 -80 -80 -80 -79.8 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:50] "P01" "P02" "P03" "P04" ...
  .. ..$ : chr [1:2] "x" "y"
 $ map   :Formal class 'SpatialPolygons' [package "sp"] with 4 slots
  .. ..@ polygons   :List of 1
  .. .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. .. ..@ Polygons :List of 603
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.57 8.79
  .. .. .. .. .. .. .. ..@ area   : num 6.41e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -77.6 -77.6 -77.6 -77.6 -77.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.6 8.82
  .. .. .. .. .. .. .. ..@ area   : num 1.08e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -77.6 -77.6 -77.6 -77.6 -77.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.59 8.83
  .. .. .. .. .. .. .. ..@ area   : num 4.08e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -77.6 -77.6 -77.6 -77.6 -77.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 8.9
  .. .. .. .. .. .. .. ..@ area   : num 5.91e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:91, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 8.9
  .. .. .. .. .. .. .. ..@ area   : num 2.41e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:77, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 8.9
  .. .. .. .. .. .. .. ..@ area   : num 7.95e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.69 8.91
  .. .. .. .. .. .. .. ..@ area   : num 5.02e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.69 8.91
  .. .. .. .. .. .. .. ..@ area   : num 3.93e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 8.91
  .. .. .. .. .. .. .. ..@ area   : num 9.73e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:47, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.72 8.92
  .. .. .. .. .. .. .. ..@ area   : num 5.17e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.69 8.93
  .. .. .. .. .. .. .. ..@ area   : num 1.17e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.72 8.93
  .. .. .. .. .. .. .. ..@ area   : num 0.000285
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:371, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.75 8.95
  .. .. .. .. .. .. .. ..@ area   : num 9.58e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:41, 1:2] -77.7 -77.7 -77.7 -77.7 -77.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.77 9.01
  .. .. .. .. .. .. .. ..@ area   : num 6.71e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -77.8 -77.8 -77.8 -77.8 -77.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.77 9.06
  .. .. .. .. .. .. .. ..@ area   : num 2.96e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:59, 1:2] -77.8 -77.8 -77.8 -77.8 -77.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.89 9.09
  .. .. .. .. .. .. .. ..@ area   : num 7.49e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -77.9 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.88 9.12
  .. .. .. .. .. .. .. ..@ area   : num 4.11e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:77, 1:2] -77.9 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.93 9.14
  .. .. .. .. .. .. .. ..@ area   : num 0.000119
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:199, 1:2] -77.9 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.94 9.15
  .. .. .. .. .. .. .. ..@ area   : num 1.55e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:48, 1:2] -77.9 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.97 9.19
  .. .. .. .. .. .. .. ..@ area   : num 6.71e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.97 9.19
  .. .. .. .. .. .. .. ..@ area   : num 1.01e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:42, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 9.2
  .. .. .. .. .. .. .. ..@ area   : num 1.82e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:46, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 9.22
  .. .. .. .. .. .. .. ..@ area   : num 6.6e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:26, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.03 9.23
  .. .. .. .. .. .. .. ..@ area   : num 1.02e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.03 9.24
  .. .. .. .. .. .. .. ..@ area   : num 5.26e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.03 9.25
  .. .. .. .. .. .. .. ..@ area   : num 1.51e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:62, 1:2] -78 -78 -78 -78 -78 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.13 9.28
  .. .. .. .. .. .. .. ..@ area   : num 7.33e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] -78.1 -78.1 -78.1 -78.1 -78.1 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.13 9.29
  .. .. .. .. .. .. .. ..@ area   : num 6.28e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -78.1 -78.1 -78.1 -78.1 -78.1 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.14 9.29
  .. .. .. .. .. .. .. ..@ area   : num 7.21e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] -78.1 -78.1 -78.1 -78.1 -78.1 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.15 9.29
  .. .. .. .. .. .. .. ..@ area   : num 1.01e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:60, 1:2] -78.1 -78.1 -78.1 -78.1 -78.1 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.2 9.3
  .. .. .. .. .. .. .. ..@ area   : num 7.97e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:50, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.17 9.31
  .. .. .. .. .. .. .. ..@ area   : num 2.1e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:80, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.18 9.31
  .. .. .. .. .. .. .. ..@ area   : num 8.25e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:38, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.23 9.31
  .. .. .. .. .. .. .. ..@ area   : num 7.4e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:28, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.23 9.32
  .. .. .. .. .. .. .. ..@ area   : num 6.16e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.23 9.32
  .. .. .. .. .. .. .. ..@ area   : num 2.7e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:26, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.23 9.32
  .. .. .. .. .. .. .. ..@ area   : num 1.21e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:57, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.26 9.33
  .. .. .. .. .. .. .. ..@ area   : num 8.47e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:51, 1:2] -78.3 -78.3 -78.3 -78.3 -78.3 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.25 9.33
  .. .. .. .. .. .. .. ..@ area   : num 1.37e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] -78.2 -78.2 -78.2 -78.2 -78.2 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.25 9.33
  .. .. .. .. .. .. .. ..@ area   : num 8.59e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:51, 1:2] -78.3 -78.3 -78.3 -78.3 -78.3 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.26 9.33
  .. .. .. .. .. .. .. ..@ area   : num 5.5e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -78.3 -78.3 -78.3 -78.3 -78.3 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.26 9.37
  .. .. .. .. .. .. .. ..@ area   : num 1.26e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:77, 1:2] -78.3 -78.3 -78.3 -78.3 -78.3 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.48 9.42
  .. .. .. .. .. .. .. ..@ area   : num 1.28e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:45, 1:2] -78.5 -78.5 -78.5 -78.5 -78.5 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.66 9.43
  .. .. .. .. .. .. .. ..@ area   : num 5.55e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.67 9.43
  .. .. .. .. .. .. .. ..@ area   : num 6.87e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.48 9.43
  .. .. .. .. .. .. .. ..@ area   : num 5.63e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -78.5 -78.5 -78.5 -78.5 -78.5 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.63 9.43
  .. .. .. .. .. .. .. ..@ area   : num 4.48e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.52 9.43
  .. .. .. .. .. .. .. ..@ area   : num 5.8e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -78.5 -78.5 -78.5 -78.5 -78.5 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.6 9.43
  .. .. .. .. .. .. .. ..@ area   : num 3.33e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.6 9.43
  .. .. .. .. .. .. .. ..@ area   : num 3.04e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:79, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.68 9.44
  .. .. .. .. .. .. .. ..@ area   : num 7.72e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:41, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.6 9.44
  .. .. .. .. .. .. .. ..@ area   : num 8.17e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.59 9.44
  .. .. .. .. .. .. .. ..@ area   : num 1.78e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:63, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 9.44
  .. .. .. .. .. .. .. ..@ area   : num 7.1e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.59 9.44
  .. .. .. .. .. .. .. ..@ area   : num 1.64e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:53, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.57 9.45
  .. .. .. .. .. .. .. ..@ area   : num 5.1e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.57 9.45
  .. .. .. .. .. .. .. ..@ area   : num 2.85e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:73, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.83 9.45
  .. .. .. .. .. .. .. ..@ area   : num 5.1e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.55 9.45
  .. .. .. .. .. .. .. ..@ area   : num 9.71e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.61 9.46
  .. .. .. .. .. .. .. ..@ area   : num 1.71e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:47, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.98 9.46
  .. .. .. .. .. .. .. ..@ area   : num 5.47e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -79 -79 -79 -79 -79 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.62 9.46
  .. .. .. .. .. .. .. ..@ area   : num 3.85e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.96 9.47
  .. .. .. .. .. .. .. ..@ area   : num 4.71e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -79 -79 -79 -79 -79 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.84 9.47
  .. .. .. .. .. .. .. ..@ area   : num 4.93e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.58 9.47
  .. .. .. .. .. .. .. ..@ area   : num 7.17e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.58 9.47
  .. .. .. .. .. .. .. ..@ area   : num 1.21e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.52 9.47
  .. .. .. .. .. .. .. ..@ area   : num 1.55e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:53, 1:2] -78.5 -78.5 -78.5 -78.5 -78.5 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.69 9.47
  .. .. .. .. .. .. .. ..@ area   : num 1.4e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:85, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.65 9.48
  .. .. .. .. .. .. .. ..@ area   : num 5.94e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.71 9.48
  .. .. .. .. .. .. .. ..@ area   : num 1.03e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.63 9.48
  .. .. .. .. .. .. .. ..@ area   : num 1.37e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:65, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.72 9.48
  .. .. .. .. .. .. .. ..@ area   : num 4.24e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.62 9.48
  .. .. .. .. .. .. .. ..@ area   : num 8.18e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.67 9.48
  .. .. .. .. .. .. .. ..@ area   : num 6.32e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.64 9.48
  .. .. .. .. .. .. .. ..@ area   : num 9.63e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.82 9.48
  .. .. .. .. .. .. .. ..@ area   : num 9.1e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:39, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.68 9.48
  .. .. .. .. .. .. .. ..@ area   : num 1.24e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:41, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.57 9.48
  .. .. .. .. .. .. .. ..@ area   : num 4.4e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.73 9.49
  .. .. .. .. .. .. .. ..@ area   : num 5.09e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:150, 1:2] -78.7 -78.7 -78.7 -78.7 -78.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 9.5
  .. .. .. .. .. .. .. ..@ area   : num 1.2e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:47, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 9.5
  .. .. .. .. .. .. .. ..@ area   : num 3.95e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 9.51
  .. .. .. .. .. .. .. ..@ area   : num 7.95e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 9.51
  .. .. .. .. .. .. .. ..@ area   : num 9.56e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:37, 1:2] -78.8 -78.8 -78.8 -78.8 -78.8 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 9.54
  .. .. .. .. .. .. .. ..@ area   : num 5.64e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 9.54
  .. .. .. .. .. .. .. ..@ area   : num 5.08e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.97 9.55
  .. .. .. .. .. .. .. ..@ area   : num 5.09e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -79 -79 -79 -79 -79 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.89 9.55
  .. .. .. .. .. .. .. ..@ area   : num 1.35e-05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 9.55
  .. .. .. .. .. .. .. ..@ area   : num 5.71e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.96 9.55
  .. .. .. .. .. .. .. ..@ area   : num 5.47e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -79 -79 -79 -79 -79 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.89 9.55
  .. .. .. .. .. .. .. ..@ area   : num 7.26e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.89 9.55
  .. .. .. .. .. .. .. ..@ area   : num 6.26e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:29, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.95 9.56
  .. .. .. .. .. .. .. ..@ area   : num 9.49e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:44, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.88 9.56
  .. .. .. .. .. .. .. ..@ area   : num 6.67e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.86 9.57
  .. .. .. .. .. .. .. ..@ area   : num 3.24e-06
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -78.9 -78.9 -78.9 -78.9 -78.9 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.09 8.52
  .. .. .. .. .. .. .. ..@ area   : num 6.05
  .. .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. .. ..@ coords : num [1:101706, 1:2] -79 -79 -79 -79 -79 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.58 9.25
  .. .. .. .. .. .. .. ..@ area   : num 0.00336
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:3330, 1:2] -79.6 -79.6 -79.6 -79.6 -79.6 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.9 9.17
  .. .. .. .. .. .. .. ..@ area   : num 0.0316
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:17718, 1:2] -79.7 -79.7 -79.7 -79.7 -79.7 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.09 9.03
  .. .. .. .. .. .. .. ..@ area   : num 2.31e-05
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:94, 1:2] -80.1 -80.1 -80.1 -80.1 -80.1 ...
  .. .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.77 9.27
  .. .. .. .. .. .. .. ..@ area   : num 2.76e-05
  .. .. .. .. .. .. .. ..@ hole   : logi TRUE
  .. .. .. .. .. .. .. ..@ ringDir: int -1
  .. .. .. .. .. .. .. ..@ coords : num [1:129, 1:2] -79.8 -79.8 -79.8 -79.8 -79.8 ...
  .. .. .. .. .. .. [list output truncated]
  .. .. .. .. ..@ plotOrder: int [1:603] 95 573 97 100 430 566 193 192 154 408 ...
  .. .. .. .. ..@ labpt    : num [1:2] -80.09 8.52
  .. .. .. .. ..@ ID       : chr "0"
  .. .. .. .. ..@ area     : num 6.17
  .. ..@ plotOrder  : int 1
  .. ..@ bbox       : num [1:2, 1:2] -83.05 7.2 -77.17 9.65
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "x" "y"
  .. .. .. ..$ : chr [1:2] "min" "max"
  .. ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
  .. .. .. ..@ projargs: chr NA
 $ xy.utm: num [1:50, 1:2] 614857 613985 614674 615019 637158 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : chr [1:2] "UTMx" "UTMy"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(pcw)
## maybe str(pcw) ; plot(pcw) ...
}
\keyword{datasets}
