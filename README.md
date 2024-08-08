## CERMBscraper

An R package with functions to extract selected content from fire-related
documents such as Incident Action Plans (PDF format) produced by the NSW 
Rural Fire Service.

Written for the Centre for Environmental Risk Management of Bushfires,
University of Wollongong, Australia. The package may be freely used 
but it probably won't be much use to you unless you are working with 
New South Wales bushfire reporting documents or something very similar.


### Installation

Note: you will need a Java runtime environment installed on your computer 
to use this package since it depends on the `tabulapdf` and `rJava` packages.
For Windows systems you can download a Microsoft build of OpenJDK from 
[here](https://www.microsoft.com/openjdk). For other operating systems try 
[here](https://adoptium.net/temurin/releases/).

``` r
# If you don't have the `remotes` package installed
# install.packages("remotes")

remotes::install_github("mbedward/CERMBscraper")

```

