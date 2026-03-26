
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SEEEapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--03--31-yellowgreen.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.0-orange.svg?style=flat-square)](commits/master)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/SEEEcreator)](https://cran.r-project.org/package=SEEEcreator)
<!-- badges: end -->

The goal of SEEEapi is to provide an easy way to access the indicator
calculation services from the [French system for evaluating water
status](http://seee.eaufrance.fr/)

## Installation

You can install the released version of SEEEapi from
[GitHub](https://github.com) with:

``` r
#install.packages(remotes)
remotes::install_github("CedricMondy/SEEEapi")
```

## Example

### List of available indicators

You can list all available indicators :

``` r
library(SEEEapi)
get_indic()
#> # A tibble: 53 x 4
#>    Type               Indicator       Version `Input files`
#>    <chr>              <chr>           <chr>           <int>
#>  1 Outil d'évaluation EBio_CE_2015    1.0.0               5
#>  2 Outil d'évaluation EBio_CE_2018    1.0.0               5
#>  3 Outil d'évaluation EBio_CE_Reunion 1.0.0               4
#>  4 Outil d'évaluation EBio_PE_2015    1.0.0               4
#>  5 Outil d'évaluation EBio_PE_2018    1.0.0               4
#>  6 <NA>               I2M2            1.0.2               1
#>  7 Outil d'évaluation I2M2            1.0.3               1
#>  8 Outil d'évaluation I2M2            1.0.4               1
#>  9 Outil d'évaluation I2M2            1.0.5               1
#> 10 Outil d'évaluation I2M2            1.0.6               1
#> # ... with 43 more rows
```

You can also list only the indicators from a given type:

``` r
get_indic(type = "diagnostic")
#> # A tibble: 7 x 4
#>   Type                Indicator     Version `Input files`
#>   <chr>               <chr>         <chr>           <int>
#> 1 Outil de diagnostic IIR           1.0.0               3
#> 2 Outil de diagnostic IPRplus       1.0.0               2
#> 3 Outil de diagnostic IPRplus       1.0.1               2
#> 4 Outil de diagnostic IPRplus       1.0.2               2
#> 5 Outil de diagnostic IPRplus       1.0.3               2
#> 6 Outil de diagnostic ODInvertebres 1.0.1               1
#> 7 Outil de diagnostic ODInvertebres 1.0.2               1
```

### Run calculations

The calculations can be executed online on the SEEE server:

``` r
calc_indic(indic      = "IPR", 
           version    = "1.0.3", 
           file_paths = c("IPR_entree_01_env.txt", "IPR_entree_02_faun.txt"))
```

    #> $info
    #> [1] "IPR v1.0.3 31/03/2020 Temps d'execution : 2.98secs"
    #> 
    #> $result
    #> # A tibble: 900 x 6
    #>    CODE_OPERATION CODE_STATION DATE       CODE_PAR LIB_PAR   RESULTAT          
    #>    <chr>          <chr>        <chr>      <chr>    <chr>     <chr>             
    #>  1 9860695        1141000      24/09/2008 <NA>     ALT       5                 
    #>  2 9860695        1141000      24/09/2008 7744     Score NER 0.161168846721986 
    #>  3 9860695        1141000      24/09/2008 7743     Score NEL 0.0523349458608265
    #>  4 9860695        1141000      24/09/2008 7644     Score NTE 2.56594296178653  
    #>  5 9860695        1141000      24/09/2008 7786     Score DIT 0.880800094018145 
    #>  6 9860695        1141000      24/09/2008 7746     Score DIO 3.47592937201397  
    #>  7 9860695        1141000      24/09/2008 7745     Score DII 4.31585732740989  
    #>  8 9860695        1141000      24/09/2008 7787     Score DTI 5.14337534155021  
    #>  9 9860695        1141000      24/09/2008 7036     IPR       16.5954088893616  
    #> 10 9860163        2000011      11/10/2007 <NA>     ALT       220               
    #> # ... with 890 more rows

The calculation can also be executed locally by providing a directory
where the scripts will be saved (either using `get_algo` or directly by
`calc_indic`):

``` r
calc_indic(indic    = "IPR",
           version  = NULL,                     # default value, will use the most
                                                # recent version available
           data     = list(IPR_entree_01_env,   # can use data from the environment
                           IPR_entree_02_faun), # instead of file paths
           locally  = TRUE,
           dir_algo = "my/path/to/algos")
```
