Trabajo con mapas
================

# Introducción

En este documento trabajaremos para explorar la identidad de plantas de
*tacsonia* del Perú

## trabajando con los datos

Vamos a cargar las librerias

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.2.2

    ## Warning: package 'ggplot2' was built under R version 4.2.2

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.2

    ## Warning: package 'readr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.2

    ## Warning: package 'dplyr' was built under R version 4.2.2

    ## Warning: package 'stringr' was built under R version 4.2.2

    ## Warning: package 'forcats' was built under R version 4.2.2

    ## Warning: package 'lubridate' was built under R version 4.2.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(writexl)
```

    ## Warning: package 'writexl' was built under R version 4.2.3

Ahora voy a leer los datos

``` r
library(readr)
library(writexl)
library(raster)
```

    ## Warning: package 'raster' was built under R version 4.2.3

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 4.2.3

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.2.3

``` r
plants <- read_csv("Datos.csv", locale=locale(encoding="latin1"))
```

    ## Rows: 1272 Columns: 31

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (26): PAIS, DEPARTA, ACRO_DEPA, PROVIN, ACRO_PRO, DISTRIT, LOCALI, RAN_L...
    ## dbl  (4): LONGI, LATI, ELEVA, AÑO_CLAS
    ## lgl  (1): ESTA_CONS
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Ahora voy a hacer el código que voy a ejecutar. Exploraremos la base de
datos. Solo usaremos las columnas de departamento, elevación, especie,
longitud y latitud para cada una de las especies.

``` r
huamachucoensis <- plants %>% 
  dplyr::filter(ESPECIES == "P. huamachucoensis L.K. Escobar") %>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)  
  writexl::write_xlsx(huamachucoensis, "huamachucoensis.xlsx")

nueva <- plants %>% 
  dplyr::filter(ESPECIES == "P. nueva")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(nueva, "nueva.xlsx")

amazonica <- plants %>% 
  dplyr::filter(ESPECIES == "P. amazonica L. K. Escobar")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(amazonica, "amazonica.xlsx")

anastomosans <- plants %>% 
  dplyr::filter(ESPECIES == "P. anastomosans (Lamb. Ex Dc.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(anastomosans, "anastomosans.xlsx")

cumbalensis <- plants %>% 
  dplyr::filter(ESPECIES == "P. cumbalensis (H. Karst.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(cumbalensis, "cumbalensis.xlsx")

glaberrima <- plants %>% 
  dplyr::filter(ESPECIES == "P. glaberrima (Juss.) Poir")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(glaberrima, "glaberrima.xlsx")

gracilens <- plants %>% 
  dplyr::filter(ESPECIES == "P. gracilens (A. Gray) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(gracilens, "gracilens.xlsx")

kuethiana <- plants %>% 
  dplyr::filter(ESPECIES == "P. kuethiana B. Esquerre")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(kuethiana, "kuethiana.xlsx")

lanceolata <- plants %>% 
  dplyr::filter(ESPECIES == "P. lanceolata (Mast.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(lanceolata, "lanceolata.xlsx")

mandonii <- plants %>% 
  dplyr::filter(ESPECIES == "P. mandonii (Mast.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(mandonii, "mandonii.xlsx")

manicata <- plants %>% 
  dplyr::filter(ESPECIES == "P. manicata (Juss.) Pers.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(manicata, "manicata.xlsx")

mathewsii <- plants %>% 
  dplyr::filter(ESPECIES == "P. mathewsii (Mast.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(mathewsii, "mathewsii.xlsx")

mixta <- plants %>% 
  dplyr::filter(ESPECIES == "P. mixta L.f.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(mixta, "mixta.xlsx")

parvifolia <- plants %>% 
  dplyr::filter(ESPECIES == "P. parvifolia (DC.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(parvifolia, "parvifolia.xlsx")

peduncularis <- plants %>% 
  dplyr::filter(ESPECIES == "P. peduncularis Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(peduncularis, "peduncularis.xlsx")

pinnatistipula <- plants %>% 
  dplyr::filter(ESPECIES == "P. pinnatistipula Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(pinnatistipula, "pinnatistipula.xlsx")

runa <- plants %>% 
  dplyr::filter(ESPECIES == "P. runa L. K Escobar")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(runa, "runa.xlsx")

salpoense <- plants %>% 
  dplyr::filter(ESPECIES == "P. salpoense S. Leiva & Tantalean")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(salpoense, "salpoense.xlsx")

tarminiana <- plants %>% 
  dplyr::filter(ESPECIES == "P. tarminiana Coppens y V. E. Barney")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(tarminiana, "tarminiana.xlsx")

trifoliata <- plants %>% 
  dplyr::filter(ESPECIES == "P. trifoliata Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI) 
writexl::write_xlsx(trifoliata, "trifoliata.xlsx")

tripartita <- plants %>% 
  dplyr::filter(ESPECIES == "P. tripartita (Juss.) Poir.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(tripartita, "tripartita.xlsx")

trisecta <- plants %>% 
  dplyr::filter(ESPECIES == "P. trisecta Mast.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(trisecta, "trisecta.xlsx")

weberbaueri <- plants %>% 
  dplyr::filter(ESPECIES == "P. weberbaueri Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(weberbaueri, "weberbaueri.xlsx")

weigendii <- plants %>% 
  dplyr::filter(ESPECIES == "P. weigendii T. Ulmer & Schwerdtferger")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(weigendii, "weigendii.xlsx")

xrosea <- plants %>% 
  dplyr::filter(ESPECIES == "P. x rosea (H. Karst.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
writexl::write_xlsx(xrosea, "xrosea.xlsx")
```

\##Ahora creamos shapefile todas las especies por separado

``` r
huamachucoensis <- read_xlsx('huamachucoensis.xlsx')
xy <- huamachucoensis[,4:5]
huamachucoensis_shp <- SpatialPointsDataFrame(coords = xy, data = huamachucoensis)
proj4string(huamachucoensis_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(huamachucoensis_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
shapefile(huamachucoensis_shp, 'huamachucoensis.shp')

amazonica <- read_xlsx('amazonica.xlsx')
xy <- amazonica[,4:5]
amazonica_shp <- SpatialPointsDataFrame(coords = xy, data = amazonica)
proj4string(amazonica_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(amazonica_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
shapefile(amazonica_shp, 'amazonica.shp')

anastomosans <- read_xlsx('anastomosans.xlsx')
xy <- anastomosans[,4:5]
anastomosans_shp <- SpatialPointsDataFrame(coords = xy, data = anastomosans)
proj4string(anastomosans_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(anastomosans_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
shapefile(anastomosans_shp, 'anastomosans.shp')

cumbalensis <- read_xlsx('cumbalensis.xlsx')
xy <- cumbalensis[,4:5]
cumbalensis_shp <- SpatialPointsDataFrame(coords = xy, data = cumbalensis)
proj4string(cumbalensis_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(cumbalensis_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
shapefile(cumbalensis_shp, 'cumbalensis.shp')

glaberrima <- read_xlsx('glaberrima.xlsx')
xy <- glaberrima[,4:5]
glaberrima_shp <- SpatialPointsDataFrame(coords = xy, data = glaberrima)
proj4string(glaberrima_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(glaberrima_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-5.png)<!-- -->

``` r
shapefile(glaberrima_shp, 'glaberrima.shp')

gracilens <- read_xlsx('gracilens.xlsx')
xy <- gracilens[,4:5]
gracilens_shp <- SpatialPointsDataFrame(coords = xy, data = gracilens)
proj4string(gracilens_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(gracilens_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-6.png)<!-- -->

``` r
shapefile(gracilens_shp, 'gracilens.shp')

kuethiana <- read_xlsx('kuethiana.xlsx')
xy <- kuethiana[,4:5]
kuethiana_shp <- SpatialPointsDataFrame(coords = xy, data = kuethiana)
proj4string(kuethiana_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(kuethiana_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-7.png)<!-- -->

``` r
shapefile(kuethiana_shp, 'kuethiana.shp')

lanceolata <- read_xlsx('lanceolata.xlsx')
xy <- lanceolata[,4:5]
lanceolata_shp <- SpatialPointsDataFrame(coords = xy, data = lanceolata)
proj4string(lanceolata_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(lanceolata_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-8.png)<!-- -->

``` r
shapefile(lanceolata_shp, 'lanceolata.shp')

mandonii <- read_xlsx('mandonii.xlsx')
xy <- mandonii[,4:5]
mandonii_shp <- SpatialPointsDataFrame(coords = xy, data = mandonii)
proj4string(mandonii_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(mandonii_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-9.png)<!-- -->

``` r
shapefile(mandonii_shp, 'mandonii.shp')

mathewsii <- read_xlsx('mathewsii.xlsx')
xy <- mathewsii[,4:5]
mathewsii_shp <- SpatialPointsDataFrame(coords = xy, data = mathewsii)
proj4string(mathewsii_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(mathewsii_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-10.png)<!-- -->

``` r
shapefile(mathewsii_shp, 'mathewsii.shp')

mixta <- read_xlsx('mixta.xlsx')
xy <- mixta[,4:5]
mixta_shp <- SpatialPointsDataFrame(coords = xy, data = mixta)
proj4string(mixta_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(mixta_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-11.png)<!-- -->

``` r
shapefile(mixta_shp, 'mixta.shp')

nueva <- read_xlsx('nueva.xlsx')
xy <- nueva [,4:5]
nueva_shp <- SpatialPointsDataFrame(coords = xy, data = nueva)
proj4string(nueva_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(nueva_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-12.png)<!-- -->

``` r
shapefile(nueva_shp, 'nueva.shp')

parvifolia <- read_xlsx('parvifolia.xlsx')
xy <- parvifolia [,4:5]
parvifolia_shp <- SpatialPointsDataFrame(coords = xy, data = parvifolia)
proj4string(parvifolia_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(parvifolia_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-13.png)<!-- -->

``` r
shapefile(parvifolia_shp, 'parvifolia.shp')

peduncularis <- read_xlsx('peduncularis.xlsx')
xy <- peduncularis [,4:5]
peduncularis_shp <- SpatialPointsDataFrame(coords = xy, data = peduncularis)
proj4string(peduncularis_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(peduncularis_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-14.png)<!-- -->

``` r
shapefile(peduncularis_shp, 'peduncularis.shp')

pinnatistipula <- read_xlsx('pinnatistipula.xlsx')
xy <- pinnatistipula [,4:5]
pinnatistipula_shp <- SpatialPointsDataFrame(coords = xy, data = pinnatistipula)
proj4string(pinnatistipula_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(pinnatistipula_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-15.png)<!-- -->

``` r
shapefile(pinnatistipula_shp, 'pinnatistipula.shp')

runa <- read_xlsx('runa.xlsx')
xy <- runa [,4:5]
runa_shp <- SpatialPointsDataFrame(coords = xy, data = runa)
proj4string(runa_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(runa_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-16.png)<!-- -->

``` r
shapefile(runa_shp, 'runa.shp')

salpoense <- read_xlsx('salpoense.xlsx')
xy <- salpoense [,4:5]
salpoense_shp <- SpatialPointsDataFrame(coords = xy, data = salpoense)
proj4string(salpoense_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(salpoense_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-17.png)<!-- -->

``` r
shapefile(salpoense_shp, 'salpoense.shp')

tarminiana <- read_xlsx('tarminiana.xlsx')
xy <- tarminiana [,4:5]
tarminiana_shp <- SpatialPointsDataFrame(coords = xy, data = tarminiana)
proj4string(tarminiana_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(tarminiana_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-18.png)<!-- -->

``` r
shapefile(tarminiana_shp, 'tarminiana.shp')

trifoliata <- read_xlsx('trifoliata.xlsx')
xy <- trifoliata [,4:5]
trifoliata_shp <- SpatialPointsDataFrame(coords = xy, data = trifoliata)
proj4string(trifoliata_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(trifoliata_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-19.png)<!-- -->

``` r
shapefile(trifoliata_shp, 'trifoliata.shp')

tripartita <- read_xlsx('tripartita.xlsx')
xy <- tripartita [,4:5]
tripartita_shp <- SpatialPointsDataFrame(coords = xy, data = tripartita)
proj4string(tripartita_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(tripartita_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-20.png)<!-- -->

``` r
shapefile(tripartita_shp, 'tripartita.shp')

trisecta <- read_xlsx('trisecta.xlsx')
xy <- trisecta [,4:5]
trisecta_shp <- SpatialPointsDataFrame(coords = xy, data = trisecta)
proj4string(trisecta_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(trisecta_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-21.png)<!-- -->

``` r
shapefile(trisecta_shp, 'trisecta.shp')

weberbaueri <- read_xlsx('weberbaueri.xlsx')
xy <- weberbaueri [,4:5]
weberbaueri_shp <- SpatialPointsDataFrame(coords = xy, data = weberbaueri)
proj4string(weberbaueri_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(weberbaueri_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-22.png)<!-- -->

``` r
shapefile(weberbaueri_shp, 'weberbaueri.shp')

weigendii <- read_xlsx('weigendii.xlsx')
xy <- weigendii [,4:5]
weigendii_shp <- SpatialPointsDataFrame(coords = xy, data = weigendii)
proj4string(weigendii_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(weigendii_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-23.png)<!-- -->

``` r
shapefile(weigendii_shp, 'weigendii.shp')

xrosea <- read_xlsx('xrosea.xlsx')
xy <- xrosea [,4:5]
xrosea_shp <- SpatialPointsDataFrame(coords = xy, data = xrosea)
proj4string(xrosea_shp) = CRS('+proj=longlat + datum=WGS84 +no_defs')
PERU <- shapefile('PER_adm1.shp')
plot(PERU)
plot(xrosea_shp, ad=TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-24.png)<!-- -->

``` r
shapefile(xrosea_shp, 'xrosea.shp')
```




    ## GitHub Documents

    This is an R Markdown format used for publishing markdown documents to GitHub. When you click the **Knit** button all R code chunks are run and a markdown file (.md) suitable for publishing to GitHub is generated.

    ## Including Code

    You can include R code in the document as follows:


    ```r
    summary(cars)

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

![](README_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
