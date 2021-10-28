Analysising CPUE
================
Georgina Shaw
27/10/2021

## Comparing the median CPUE to the mean CPUE, incuding a time series

Firstly load all the packages needed

``` r
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyverse)
library(ggplot2)
```

Next read in the data

``` r
landings_data <- read_csv("sample_landings_data_raw.txt")
```

Renaming the columns and turning the date column into a date format that
R recognises

``` r
landings_data <- landings_data %>%
  rename(Year = yy,
         Date = dat,
         Trip_ID = trip,
         Effort_Hours = effort,
         Gear = gr,
         Species = sp,
         Length_cm = l_cm,
         Weight_g = w_cm) %>%
   mutate(Date = mdy(Date)) 
```

I cleaned the data and removed any values with NA so it would be easier
to calculate mean and medians

``` r
landings_data[!complete.cases(landings_data),]
```

    ## # A tibble: 3 x 8
    ##    Year Date       Trip_ID Effort_Hours Gear     Species      Length_cm Weight_g
    ##   <dbl> <date>       <dbl>        <dbl> <chr>    <chr>            <dbl>    <dbl>
    ## 1  2003 2003-05-01      10           10 <NA>     Caesio cuni~      19       157.
    ## 2  2003 2003-05-01      10           10 Handline Caesio cuni~      19        NA 
    ## 3  2004 2004-12-18      NA            9 Trap     Caesio cuni~      20.1     186.

``` r
landings_data <- na.omit(landings_data)
```

Computing the CPUE

``` r
cpue_data <- landings_data %>% 
    mutate(Weight_kg = Weight_g / 1000) %>%
    group_by(Year,Trip_ID) %>% 
    summarize(Trip_CPUE = sum(Weight_kg) /           mean(Effort_Hours)) %>% 
    group_by(Year) %>% 
    summarize(Median_CPUE_kg_hour =                  median(Trip_CPUE), Mean_CPUE_kg_hour =           mean(Trip_CPUE), Trip_CPUE = Trip_CPUE) 
```

``` r
cpue_data
```

    ## # A tibble: 1,880 x 4
    ## # Groups:   Year [9]
    ##     Year Median_CPUE_kg_hour Mean_CPUE_kg_hour Trip_CPUE
    ##    <dbl>               <dbl>             <dbl>     <dbl>
    ##  1  2003               0.318             0.493    1.25  
    ##  2  2003               0.318             0.493    0.315 
    ##  3  2003               0.318             0.493    0.186 
    ##  4  2003               0.318             0.493    0.251 
    ##  5  2003               0.318             0.493    0.0565
    ##  6  2003               0.318             0.493    0.0837
    ##  7  2003               0.318             0.493    0.0455
    ##  8  2003               0.318             0.493    0.318 
    ##  9  2003               0.318             0.493    0.177 
    ## 10  2003               0.318             0.493    0.100 
    ## # ... with 1,870 more rows

Computing a graph comparing median CPUE to the mean CPUE with indication
of the year they are from

``` r
  ggplot(data = cpue_data, mapping =  aes(x=Median_CPUE_kg_hour,y=Mean_CPUE_kg_hour)) +
  geom_point(mapping = aes(x = Median_CPUE_kg_hour, y = Mean_CPUE_kg_hour, color = Year)) +
  geom_line() + 
  ylab("Mean CPUE [kg/hour]") + xlab("MEdian CPUE [kg/houe]") +
  ggtitle("Comparing Median CPUE against Mean CPUE") 
```

![](Analysing_CPUE_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Boxplot of CPUE

``` r
boxplot(cpue_data$Trip_CPUE)
```

![](Analysing_CPUE_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
