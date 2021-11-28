R Notebook
================

Se realiza un análisis exploratorio

``` r
rm(list=ls())

# Librer?a
library(tidymodels)
```

    ## Warning: package 'tidymodels' was built under R version 4.0.5

    ## Registered S3 method overwritten by 'tune':
    ##   method                   from   
    ##   required_pkgs.model_spec parsnip

    ## -- Attaching packages -------------------------------------- tidymodels 0.1.4 --

    ## v broom        0.7.10     v recipes      0.1.17
    ## v dials        0.0.10     v rsample      0.1.1 
    ## v dplyr        1.0.7      v tibble       3.1.5 
    ## v ggplot2      3.3.5      v tidyr        1.1.4 
    ## v infer        1.0.0      v tune         0.1.6 
    ## v modeldata    0.1.1      v workflows    0.2.4 
    ## v parsnip      0.1.7      v workflowsets 0.1.0 
    ## v purrr        0.3.4      v yardstick    0.0.9

    ## Warning: package 'broom' was built under R version 4.0.5

    ## Warning: package 'dials' was built under R version 4.0.5

    ## Warning: package 'dplyr' was built under R version 4.0.5

    ## Warning: package 'ggplot2' was built under R version 4.0.5

    ## Warning: package 'infer' was built under R version 4.0.5

    ## Warning: package 'modeldata' was built under R version 4.0.5

    ## Warning: package 'parsnip' was built under R version 4.0.5

    ## Warning: package 'purrr' was built under R version 4.0.5

    ## Warning: package 'recipes' was built under R version 4.0.5

    ## Warning: package 'rsample' was built under R version 4.0.5

    ## Warning: package 'tibble' was built under R version 4.0.5

    ## Warning: package 'tidyr' was built under R version 4.0.5

    ## Warning: package 'tune' was built under R version 4.0.5

    ## Warning: package 'workflows' was built under R version 4.0.5

    ## Warning: package 'workflowsets' was built under R version 4.0.5

    ## Warning: package 'yardstick' was built under R version 4.0.5

    ## -- Conflicts ----------------------------------------- tidymodels_conflicts() --
    ## x purrr::discard() masks scales::discard()
    ## x dplyr::filter()  masks stats::filter()
    ## x dplyr::lag()     masks stats::lag()
    ## x recipes::step()  masks stats::step()
    ## * Use suppressPackageStartupMessages() to eliminate package startup messages

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 4.0.5

``` r
library(fastDummies)
```

    ## Warning: package 'fastDummies' was built under R version 4.0.5

``` r
library(brms)
```

    ## Warning: package 'brms' was built under R version 4.0.5

    ## Loading required package: Rcpp

    ## Warning: package 'Rcpp' was built under R version 4.0.5

    ## 
    ## Attaching package: 'Rcpp'

    ## The following object is masked from 'package:rsample':
    ## 
    ##     populate

    ## Loading 'brms' package (version 2.16.1). Useful instructions
    ## can be found by typing help('brms'). A more detailed introduction
    ## to the package is available through vignette('brms_overview').

    ## 
    ## Attaching package: 'brms'

    ## The following object is masked from 'package:dials':
    ## 
    ##     mixture

    ## The following object is masked from 'package:stats':
    ## 
    ##     ar

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.0.5

``` r
library(DataExplorer)
```

    ## Warning: package 'DataExplorer' was built under R version 4.0.5

``` r
datos <- read_excel("C:/Users/peter/OneDrive - ITESO/Documents/School Work/Maestría Data Science/Optimización Convexa/Dataset entradas/datos_cuchareados.xlsx")

introduce(datos)
```

    ## # A tibble: 1 x 9
    ##    rows columns discrete_columns continuous_columns all_missing_columns
    ##   <int>   <int>            <int>              <int>               <int>
    ## 1  3300       5                3                  2                   0
    ## # ... with 4 more variables: total_missing_values <int>, complete_rows <int>,
    ## #   total_observations <int>, memory_usage <dbl>

``` r
plot_intro(datos)
```

![](Proyecto_1_markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Librer?a fastDummies para convertir variables a dummy y quitar 1 categor?a de cada variable.
datos_dummy <- dummy_cols(datos,  select_columns = c("Marca", "Diseño"),remove_first_dummy = TRUE)

#Limpiar datos para quitar variables repetidas o que no se van a usar 
datos_dummy1 <- subset(datos_dummy,select=-c(RES,Marca,Diseño))

#Renombrar la primera columna
names(datos_dummy1)[1] <- "Y"

# Normalizamos
#datos_dummy1$Precio <- datos_dummy1$Precio/100

datos_dummy1 <- data.frame(datos_dummy1)



# modelo bernoulli
fit <- brm(Y ~ Diseño_Del_mundial+
             Precio+
             Marca_Gaser+
             Diseño_Fondo_blanco+
             Marca_Molten+
             Marca_Voit +
             Diseño_Colorido+
             Diseño_Con_temática,
           data = datos_dummy1, 
           family = bernoulli(link = "logit"), 
           silent=TRUE, 
           refresh = -1,
           prior = c(set_prior("normal(-1,1)", class="b", coef="Marca_Gaser"), 
                     set_prior("normal(1, 1)", class="b", coef="Marca_Voit"), 
                     set_prior("normal(0, 1)", class="b", coef="Marca_Molten"), 
                     set_prior("normal(1, 1)", class="b", coef="Precio")), 
)
```

    ## Compiling Stan program...

    ## Start sampling

    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 18.248 seconds (Warm-up)
    ## Chain 1:                5.649 seconds (Sampling)
    ## Chain 1:                23.897 seconds (Total)
    ## Chain 1: 
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 20.405 seconds (Warm-up)
    ## Chain 2:                6.33 seconds (Sampling)
    ## Chain 2:                26.735 seconds (Total)
    ## Chain 2: 
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 17.723 seconds (Warm-up)
    ## Chain 3:                6.752 seconds (Sampling)
    ## Chain 3:                24.475 seconds (Total)
    ## Chain 3: 
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 17.692 seconds (Warm-up)
    ## Chain 4:                6.663 seconds (Sampling)
    ## Chain 4:                24.355 seconds (Total)
    ## Chain 4:

``` r
summary(fit)
```

    ##  Family: bernoulli 
    ##   Links: mu = logit 
    ## Formula: Y ~ Diseño_Del_mundial + Precio + Marca_Gaser + Diseño_Fondo_blanco + Marca_Molten + Marca_Voit + Diseño_Colorido + Diseño_Con_temática 
    ##    Data: datos_dummy1 (Number of observations: 3300) 
    ##   Draws: 4 chains, each with iter = 2000; warmup = 1000; thin = 1;
    ##          total post-warmup draws = 4000
    ## 
    ## Population-Level Effects: 
    ##                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept               0.80      0.14     0.53     1.07 1.00     2191     2088
    ## Diseño_Del_mundial     -0.13      0.12    -0.37     0.12 1.00     2164     2512
    ## Precio                 -0.00      0.00    -0.00    -0.00 1.00     3413     2547
    ## Marca_Gaser            -1.11      0.11    -1.32    -0.88 1.00     2450     2255
    ## Diseño_Fondo_blanco    -0.14      0.12    -0.38     0.09 1.00     2014     2105
    ## Marca_Molten           -0.86      0.11    -1.07    -0.65 1.00     2569     2550
    ## Marca_Voit             -0.51      0.10    -0.71    -0.30 1.00     2349     2449
    ## Diseño_Colorido        -0.06      0.12    -0.30     0.17 1.00     2117     2282
    ## Diseño_Con_temática    -0.10      0.12    -0.34     0.14 1.00     1949     2522
    ## 
    ## Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
    ## and Tail_ESS are effective sample size measures, and Rhat is the potential
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

Magnolia

``` r
loss <- function(price){
  # First Create the input data to predict with out model - we want to predict whether or not our phone will sell
  our.ball <- data.frame(Diseño_Del_mundial=0,
                            Precio=price,
                            Marca_Gaser=0,
                            Diseño_Fondo_blanco=1,
                            Marca_Molten=0,
                            Marca_Voit=1,
                            Diseño_Colorido=0,
                            Diseño_Con_temática=0
                            ) 
  
  # Next, for each posterior sample from out model, predict whether or not our phone would sell at the given price. This will give a vector of 0's and 1's, did the phone sell in each posterior sample. Think of each posterior sample as a simulation. 
  pp <- posterior_predict(fit, newdata=our.ball)
  
  # Next calculate the expected return for each of these posterior simulations
  mean(pp*price)
}

(op <- optim(500,function(x)-loss(x)))
```

    ## Warning in optim(500, function(x) -loss(x)): one-dimensional optimization by Nelder-Mead is unreliable:
    ## use "Brent" or optimize() directly

    ## $par
    ## [1] 500.0002
    ## 
    ## $value
    ## [1] -117.0001
    ## 
    ## $counts
    ## function gradient 
    ##      159       NA 
    ## 
    ## $convergence
    ## [1] 10
    ## 
    ## $message
    ## NULL

Hola

``` r
x <- 90:1000 # Listing prices to evaluate
l <- sapply(x, loss) 
plot(x, l, xlab = "Listing Price", ylab = "Expected Return")
```

![](Proyecto_1_markdown_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
