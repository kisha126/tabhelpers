
# tabhelpers

The goal of `tabhelpers` is to provide a collection of helper functions
that assist in printing and formatting data frames, enhancing table
display with customizable styles and alignment. This is especially
useful in displaying statistical results in R console/command line.

> **Note**: The package is currently **under construction** and not yet
> available on CRAN.

## Installation

You can install the development version of `tabhelpers` from GitHub
with:

``` r
# install.packages("devtools")
devtools::install_github("kisha126/tabhelpers")
```

## Example

Here’s a basic example that demonstrates how to use `tabhelpers` to
format and style a table:

``` r
library(tabhelpers)

mtcars |> 
    tibble::rownames_to_column() |> 
    table_default(
        justify_cols = list("1" = "right"),
        style_colnames = list(
            "mpg" = function(ctx) cli::col_red(cli::style_bold(ctx$formatted_value)),
            "cyl" = "blue_italic"
        ),
        style_columns = list(
            "mpg" = function(ctx) {
                val <- as.numeric(ctx$value)
                if (val > 20) cli::col_green(ctx$formatted_value)
                else if (val > 15) cli::col_yellow(ctx$formatted_value)
                else cli::col_red(ctx$formatted_value)
            }
        ),
        digits = 1,
        center_table = TRUE,
        n_space = 2
    )
```

<pre class="r-output"><code>───────────────────────────────────────────────────────────────────────────────────
              rowname  <span style='color: #BB0000; font-weight: bold;'>mpg </span>  <span style='color: #0000BB; font-style: italic;'>cyl</span>  disp   hp   drat  wt   qsec  vs  am  gear  carb  
───────────────────────────────────────────────────────────────────────────────────
            Mazda RX4  <span style='color: #00BB00;'>21.0</span>   6   160.0  110  3.9   2.6  16.5  0   1    4     4    
        Mazda RX4 Wag  <span style='color: #00BB00;'>21.0</span>   6   160.0  110  3.9   2.9  17.0  0   1    4     4    
           Datsun 710  <span style='color: #00BB00;'>22.8</span>   4   108.0  93   3.9   2.3  18.6  1   1    4     1    
       Hornet 4 Drive  <span style='color: #00BB00;'>21.4</span>   6   258.0  110  3.1   3.2  19.4  1   0    3     1    
    Hornet Sportabout  <span style='color: #BBBB00;'>18.7</span>   8   360.0  175  3.1   3.4  17.0  0   0    3     2    
              Valiant  <span style='color: #BBBB00;'>18.1</span>   6   225.0  105  2.8   3.5  20.2  1   0    3     1    
           Duster 360  <span style='color: #BB0000;'>14.3</span>   8   360.0  245  3.2   3.6  15.8  0   0    3     4    
            Merc 240D  <span style='color: #00BB00;'>24.4</span>   4   146.7  62   3.7   3.2  20.0  1   0    4     2    
             Merc 230  <span style='color: #00BB00;'>22.8</span>   4   140.8  95   3.9   3.1  22.9  1   0    4     2    
             Merc 280  <span style='color: #BBBB00;'>19.2</span>   6   167.6  123  3.9   3.4  18.3  1   0    4     4    
            Merc 280C  <span style='color: #BBBB00;'>17.8</span>   6   167.6  123  3.9   3.4  18.9  1   0    4     4    
           Merc 450SE  <span style='color: #BBBB00;'>16.4</span>   8   275.8  180  3.1   4.1  17.4  0   0    3     3    
           Merc 450SL  <span style='color: #BBBB00;'>17.3</span>   8   275.8  180  3.1   3.7  17.6  0   0    3     3    
          Merc 450SLC  <span style='color: #BBBB00;'>15.2</span>   8   275.8  180  3.1   3.8  18.0  0   0    3     3    
   Cadillac Fleetwood  <span style='color: #BB0000;'>10.4</span>   8   472.0  205  2.9   5.2  18.0  0   0    3     4    
  Lincoln Continental  <span style='color: #BB0000;'>10.4</span>   8   460.0  215  3.0   5.4  17.8  0   0    3     4    
    Chrysler Imperial  <span style='color: #BB0000;'>14.7</span>   8   440.0  230  3.2   5.3  17.4  0   0    3     4    
             Fiat 128  <span style='color: #00BB00;'>32.4</span>   4    78.7  66   4.1   2.2  19.5  1   1    4     1    
          Honda Civic  <span style='color: #00BB00;'>30.4</span>   4    75.7  52   4.9   1.6  18.5  1   1    4     2    
       Toyota Corolla  <span style='color: #00BB00;'>33.9</span>   4    71.1  65   4.2   1.8  19.9  1   1    4     1    
        Toyota Corona  <span style='color: #00BB00;'>21.5</span>   4   120.1  97   3.7   2.5  20.0  1   0    3     1    
     Dodge Challenger  <span style='color: #BBBB00;'>15.5</span>   8   318.0  150  2.8   3.5  16.9  0   0    3     2    
          AMC Javelin  <span style='color: #BBBB00;'>15.2</span>   8   304.0  150  3.1   3.4  17.3  0   0    3     2    
           Camaro Z28  <span style='color: #BB0000;'>13.3</span>   8   350.0  245  3.7   3.8  15.4  0   0    3     4    
     Pontiac Firebird  <span style='color: #BBBB00;'>19.2</span>   8   400.0  175  3.1   3.8  17.0  0   0    3     2    
            Fiat X1-9  <span style='color: #00BB00;'>27.3</span>   4    79.0  66   4.1   1.9  18.9  1   1    4     1    
        Porsche 914-2  <span style='color: #00BB00;'>26.0</span>   4   120.3  91   4.4   2.1  16.7  0   1    5     2    
         Lotus Europa  <span style='color: #00BB00;'>30.4</span>   4    95.1  113  3.8   1.5  16.9  1   1    5     2    
       Ford Pantera L  <span style='color: #BBBB00;'>15.8</span>   8   351.0  264  4.2   3.2  14.5  0   1    5     4    
         Ferrari Dino  <span style='color: #BBBB00;'>19.7</span>   6   145.0  175  3.6   2.8  15.5  0   1    5     6    
        Maserati Bora  <span style='color: #BB0000;'>15.0</span>   8   301.0  335  3.5   3.6  14.6  0   1    5     8    
           Volvo 142E  <span style='color: #00BB00;'>21.4</span>   4   121.0  109  4.1   2.8  18.6  1   1    4     2    
───────────────────────────────────────────────────────────────────────────────────
</code></pre>

Changing the default `border_char` value:

``` r
mtcars |> 
    tibble::rownames_to_column() |> 
    table_default(
        justify_cols = list("1" = "right"),
        style_colnames = list(
            "mpg" = function(ctx) cli::col_red(cli::style_bold(ctx$formatted_value)),
            "cyl" = "blue_italic"
        ),
        style_columns = list(
            "mpg" = function(ctx) {
                val <- as.numeric(ctx$value)
                if (val > 20) cli::col_green(ctx$formatted_value)
                else if (val > 15) cli::col_yellow(ctx$formatted_value)
                else cli::col_red(ctx$formatted_value)
            }
        ),
        digits = 1,
        border_char = "=",
        center_table = TRUE,
        n_space = 2
    )
```

<pre class="r-output"><code>===================================================================================
              rowname  <span style='color: #BB0000; font-weight: bold;'>mpg </span>  <span style='color: #0000BB; font-style: italic;'>cyl</span>  disp   hp   drat  wt   qsec  vs  am  gear  carb  
===================================================================================
            Mazda RX4  <span style='color: #00BB00;'>21.0</span>   6   160.0  110  3.9   2.6  16.5  0   1    4     4    
        Mazda RX4 Wag  <span style='color: #00BB00;'>21.0</span>   6   160.0  110  3.9   2.9  17.0  0   1    4     4    
           Datsun 710  <span style='color: #00BB00;'>22.8</span>   4   108.0  93   3.9   2.3  18.6  1   1    4     1    
       Hornet 4 Drive  <span style='color: #00BB00;'>21.4</span>   6   258.0  110  3.1   3.2  19.4  1   0    3     1    
    Hornet Sportabout  <span style='color: #BBBB00;'>18.7</span>   8   360.0  175  3.1   3.4  17.0  0   0    3     2    
              Valiant  <span style='color: #BBBB00;'>18.1</span>   6   225.0  105  2.8   3.5  20.2  1   0    3     1    
           Duster 360  <span style='color: #BB0000;'>14.3</span>   8   360.0  245  3.2   3.6  15.8  0   0    3     4    
            Merc 240D  <span style='color: #00BB00;'>24.4</span>   4   146.7  62   3.7   3.2  20.0  1   0    4     2    
             Merc 230  <span style='color: #00BB00;'>22.8</span>   4   140.8  95   3.9   3.1  22.9  1   0    4     2    
             Merc 280  <span style='color: #BBBB00;'>19.2</span>   6   167.6  123  3.9   3.4  18.3  1   0    4     4    
            Merc 280C  <span style='color: #BBBB00;'>17.8</span>   6   167.6  123  3.9   3.4  18.9  1   0    4     4    
           Merc 450SE  <span style='color: #BBBB00;'>16.4</span>   8   275.8  180  3.1   4.1  17.4  0   0    3     3    
           Merc 450SL  <span style='color: #BBBB00;'>17.3</span>   8   275.8  180  3.1   3.7  17.6  0   0    3     3    
          Merc 450SLC  <span style='color: #BBBB00;'>15.2</span>   8   275.8  180  3.1   3.8  18.0  0   0    3     3    
   Cadillac Fleetwood  <span style='color: #BB0000;'>10.4</span>   8   472.0  205  2.9   5.2  18.0  0   0    3     4    
  Lincoln Continental  <span style='color: #BB0000;'>10.4</span>   8   460.0  215  3.0   5.4  17.8  0   0    3     4    
    Chrysler Imperial  <span style='color: #BB0000;'>14.7</span>   8   440.0  230  3.2   5.3  17.4  0   0    3     4    
             Fiat 128  <span style='color: #00BB00;'>32.4</span>   4    78.7  66   4.1   2.2  19.5  1   1    4     1    
          Honda Civic  <span style='color: #00BB00;'>30.4</span>   4    75.7  52   4.9   1.6  18.5  1   1    4     2    
       Toyota Corolla  <span style='color: #00BB00;'>33.9</span>   4    71.1  65   4.2   1.8  19.9  1   1    4     1    
        Toyota Corona  <span style='color: #00BB00;'>21.5</span>   4   120.1  97   3.7   2.5  20.0  1   0    3     1    
     Dodge Challenger  <span style='color: #BBBB00;'>15.5</span>   8   318.0  150  2.8   3.5  16.9  0   0    3     2    
          AMC Javelin  <span style='color: #BBBB00;'>15.2</span>   8   304.0  150  3.1   3.4  17.3  0   0    3     2    
           Camaro Z28  <span style='color: #BB0000;'>13.3</span>   8   350.0  245  3.7   3.8  15.4  0   0    3     4    
     Pontiac Firebird  <span style='color: #BBBB00;'>19.2</span>   8   400.0  175  3.1   3.8  17.0  0   0    3     2    
            Fiat X1-9  <span style='color: #00BB00;'>27.3</span>   4    79.0  66   4.1   1.9  18.9  1   1    4     1    
        Porsche 914-2  <span style='color: #00BB00;'>26.0</span>   4   120.3  91   4.4   2.1  16.7  0   1    5     2    
         Lotus Europa  <span style='color: #00BB00;'>30.4</span>   4    95.1  113  3.8   1.5  16.9  1   1    5     2    
       Ford Pantera L  <span style='color: #BBBB00;'>15.8</span>   8   351.0  264  4.2   3.2  14.5  0   1    5     4    
         Ferrari Dino  <span style='color: #BBBB00;'>19.7</span>   6   145.0  175  3.6   2.8  15.5  0   1    5     6    
        Maserati Bora  <span style='color: #BB0000;'>15.0</span>   8   301.0  335  3.5   3.6  14.6  0   1    5     8    
           Volvo 142E  <span style='color: #00BB00;'>21.4</span>   4   121.0  109  4.1   2.8  18.6  1   1    4     2    
===================================================================================
</code></pre>

This package allows users to apply advanced styling and alignment to
tables, such as color coding, column alignment, and borders, making it
easier to view large datasets directly in the command line.
