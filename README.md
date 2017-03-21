nneo
====



[![Build Status](https://travis-ci.org/ropenscilabs/nneo.svg?branch=master)](https://travis-ci.org/ropenscilabs/nneo)
[![codecov.io](https://codecov.io/github/ropenscilabs/nneo/coverage.svg?branch=master)](https://codecov.io/github/ropenscilabs/nneo?branch=master)

`nneo` - R client for [NEON API](http://data.neonscience.org/data-api)

Routes and R methods

* `/products` - `nneo_products()`/`nneo_product()`
* `/sites` - `nneo_sites()`/`nneo_site()`
* `/locations` - `nneo_locations()`/`nneo_location()`

## installation


```r
devtools::install_github("ropenscilabs/nneo")
```


```r
library("nneo")
```

## list products


```r
nneo_products()
#> # A tibble: 183 × 13
#>    productStatus
#> *          <chr>
#> 1         FUTURE
#> 2         ACTIVE
#> 3         FUTURE
#> 4         FUTURE
#> 5         ACTIVE
#> 6         FUTURE
#> 7         ACTIVE
#> 8         FUTURE
#> 9         FUTURE
#> 10        FUTURE
#> # ... with 173 more rows, and 12 more variables: productDescription <chr>,
#> #   productCode <chr>, productCategory <chr>, themes <list>,
#> #   productScienceTeam <chr>, productName <chr>,
#> #   productCodePresentation <chr>, specs <list>,
#> #   productScienceTeamAbbr <chr>, productCodeLong <chr>,
#> #   productHasExpanded <lgl>, siteCodes <list>
```

## list sites


```r
nneo_sites()
#> # A tibble: 47 × 11
#>                               siteDescription siteLongitude    siteType
#> *                                       <chr>         <dbl>       <chr>
#> 1                                Jornada LTER    -106.84254 RELOCATABLE
#> 2               Klemme Range Research Station     -99.05878 RELOCATABLE
#> 3                  Disney Wilderness Preserve     -81.43619 RELOCATABLE
#> 4                                 Posey Creek     -78.14678        CORE
#> 5           Ordway-Swisher Biological Station     -81.99343        CORE
#> 6                                       Healy    -149.21335 RELOCATABLE
#> 7       Niwot Ridge Mountain Research Station    -105.58237        CORE
#> 8            Jones Ecological Research Center     -84.46862 RELOCATABLE
#> 9  Smithsonian Conservation Biology Institute     -78.13949        CORE
#> 10       Rocky Mountain National Park CASTNET    -105.54596 RELOCATABLE
#> # ... with 37 more rows, and 8 more variables: stateName <chr>,
#> #   stateCode <chr>, siteLatitude <dbl>, domainName <chr>,
#> #   domainCode <chr>, siteCode <chr>, dataProducts <list>, siteName <chr>
```

## list a location


```r
res <- nneo_location("HARV")
names(res)
#>  [1] "locationChildren"         "locationElevation"       
#>  [3] "locationDescription"      "locationType"            
#>  [5] "locationProperties"       "locationDecimalLatitude" 
#>  [7] "locationName"             "domainCode"              
#>  [9] "siteCode"                 "locationDecimalLongitude"
#> [11] "locationParent"
```

## data


```r
nneo_data(product_code = "DP1.00098.001", site_code = "HEAL", 
          year_month = "2016-05")
#> $status
#> [1] 200
#> 
#> $data
#> $data$files
#> # A tibble: 4 × 4
#>        crc32                                                       name
#> *      <chr>                                                      <chr>
#> 1 0xb2506dd5  NEON.D19.HEAL.DP1.00098.001.00000.003.000.001.RH_1min.csv
#> 2 0xd6e86fd9  NEON.D19.HEAL.DP1.00098.001.00000.000.040.001.RH_1min.csv
#> 3  0x560b9ba NEON.D19.HEAL.DP1.00098.001.00000.003.000.030.RH_30min.csv
#> 4 0x78a83344 NEON.D19.HEAL.DP1.00098.001.00000.000.040.030.RH_30min.csv
#> # ... with 2 more variables: url <chr>, size <chr>
#> 
#> $data$productCode
#> [1] "DP1.00098.001"
#> 
#> $data$urls
#> [1] "http://data.neonscience.org:80/api/v0/data/DP1.00098.001/HEAL/2016-05/NEON.D19.HEAL.DP1.00098.001.00000.003.000.001.RH_1min.csv?package=basic" 
#> [2] "http://data.neonscience.org:80/api/v0/data/DP1.00098.001/HEAL/2016-05/NEON.D19.HEAL.DP1.00098.001.00000.000.040.001.RH_1min.csv?package=basic" 
#> [3] "http://data.neonscience.org:80/api/v0/data/DP1.00098.001/HEAL/2016-05/NEON.D19.HEAL.DP1.00098.001.00000.003.000.030.RH_30min.csv?package=basic"
#> [4] "http://data.neonscience.org:80/api/v0/data/DP1.00098.001/HEAL/2016-05/NEON.D19.HEAL.DP1.00098.001.00000.000.040.030.RH_30min.csv?package=basic"
#> 
#> $data$month
#> [1] "2016-05"
#> 
#> $data$siteCode
#> [1] "HEAL"
```

## Meta

* Please [report any issues or bugs](https://github.com/ropenscilabs/nneo/issues).
* License: MIT
* Get citation information for `nneo` in R doing `citation(package = nneo')`
* Please note that this project is CONDUCT.md). By participating in this project you agree to abide by its terms.

[![ropensci_footer](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)