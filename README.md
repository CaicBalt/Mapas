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

Ahora voy a leer los datos

``` r
library(readr)
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
nueva <- plants %>% 
  dplyr::filter(ESPECIES == "P. nueva")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
amazonica <- plants %>% 
  dplyr::filter(ESPECIES == "P. amazonica L. K. Escobar")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
anastomosans <- plants %>% 
  dplyr::filter(ESPECIES == "P. anastomosans (Lamb. Ex Dc.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
cumbalensis <- plants %>% 
  dplyr::filter(ESPECIES == "P. cumbalensis (H. Karst.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
glaberrima <- plants %>% 
  dplyr::filter(ESPECIES == "P. glaberrima (Juss.) Poir")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
gracilens <- plants %>% 
  dplyr::filter(ESPECIES == "P. gracilens (A. Gray) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
kuethiana <- plants %>% 
  dplyr::filter(ESPECIES == "P. kuethiana B. Esquerre")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
lanceolata <- plants %>% 
  dplyr::filter(ESPECIES == "P. lanceolata (Mast.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
mandonii <- plants %>% 
  dplyr::filter(ESPECIES == "P. mandonii (Mast.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
manicata <- plants %>% 
  dplyr::filter(ESPECIES == "P. manicata (Juss.) Pers.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
mathewsii <- plants %>% 
  dplyr::filter(ESPECIES == "P. mathewsii (Mast.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
mixta <- plants %>% 
  dplyr::filter(ESPECIES == "P. mixta L.f.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
parvifolia <- plants %>% 
  dplyr::filter(ESPECIES == "P. parvifolia (DC.) Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
pedunculatis <- plants %>% 
  dplyr::filter(ESPECIES == "P. peduncularis Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
pinnatistipula <- plants %>% 
  dplyr::filter(ESPECIES == "P. pinnatistipula Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
runa <- plants %>% 
  dplyr::filter(ESPECIES == "P. runa L. K Escobar")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
salpoense <- plants %>% 
  dplyr::filter(ESPECIES == "P. salpoense S. Leiva & Tantalean")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
tarminiana <- plants %>% 
  dplyr::filter(ESPECIES == "P. tarminiana Coppens y V. E. Barney")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
trifoliata <- plants %>% 
  dplyr::filter(ESPECIES == "P. trifoliata Cav.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
tripartita <- plants %>% 
  dplyr::filter(ESPECIES == "P. tripartita (Juss.) Poir.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
trisecta <- plants %>% 
  dplyr::filter(ESPECIES == "P. trisecta Mast.")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
weberbaueri <- plants %>% 
  dplyr::filter(ESPECIES == "P. weberbaueri Harms")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
weigendii <- plants %>% 
  dplyr::filter(ESPECIES == "P. weigendii T. Ulmer & Schwerdtferger")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
xrosea <- plants %>% 
  dplyr::filter(ESPECIES == "P. x rosea (H. Karst.) Killip")%>% 
  dplyr::select(ESPECIES, DEPARTA, ELEVA, LONGI, LATI)
```

## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

``` r
summary(cars)
```

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
