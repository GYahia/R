Analyze YouTube Trends with a Dashboard
---------------------------------------

YouTubers are faced with the pressure to create content that can
potentially become viral and help pave their way to be the Internet’s
next sensation.

What, then, gets more views nowadays? Are certain categories more
popular than others in general, or does title length matter too?

Setting Up the Environment
--------------------------

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(ggplot2)
```

Reading Data from the CSV File
------------------------------

``` r
setwd("/home/gyahia/Documents/Portfolio/RStudio/CognitiveClass/Youtube_Project/")
video_stats <- read.csv("videos-stats.csv")
```

Information about the dataset
-----------------------------

### First glance at the dataset

``` r
#  video_stats
```

### Checking Dimensions

It will give the number of OBSERVATIONS first (rows) and the number of
VARIABLES after (columns)

``` r
dim(video_stats)
```

    ## [1] 1881    8

### Two different ways of summarizing the dataset

``` r
str(video_stats)
```

    ## 'data.frame':    1881 obs. of  8 variables:
    ##  $ X           : int  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ Title       : chr  "Apple Pay Is Killing the Physical Wallet After Only Eight Years | Tech News Briefing Podcast | WSJ" "The most EXPENSIVE thing I own." "My New House Gaming Setup is SICK!" "Petrol Vs Liquid Nitrogen | Freezing Experiment | പെട്രോളിനെ ഐസ് ആകാൻ പറ്റുമോ | M4 Tech |" ...
    ##  $ Video.ID    : chr  "wAZZ-UWGVHI" "b3x28s61q3c" "4mgePWWCAmA" "kXiYSI7H2b0" ...
    ##  $ Published.At: chr  "2022-08-23" "2022-08-24" "2022-08-23" "2022-08-23" ...
    ##  $ Keyword     : chr  "tech" "tech" "tech" "tech" ...
    ##  $ Likes       : num  3407 76779 63825 71566 96513 ...
    ##  $ Comments    : num  672 4306 3338 1426 5155 ...
    ##  $ Views       : num  135612 1758063 1564007 922918 1855644 ...

``` r
summary(video_stats)
```

    ##        X           Title             Video.ID         Published.At      
    ##  Min.   :   0   Length:1881        Length:1881        Length:1881       
    ##  1st Qu.: 470   Class :character   Class :character   Class :character  
    ##  Median : 940   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   : 940                                                           
    ##  3rd Qu.:1410                                                           
    ##  Max.   :1880                                                           
    ##                                                                         
    ##    Keyword              Likes             Comments          Views          
    ##  Length:1881        Min.   :      -1   Min.   :    -1   Min.   :2.500e+01  
    ##  Class :character   1st Qu.:    2672   1st Qu.:   199   1st Qu.:8.452e+04  
    ##  Mode  :character   Median :   14787   Median :   814   Median :5.917e+05  
    ##                     Mean   :  170061   Mean   :  7863   Mean   :1.161e+07  
    ##                     3rd Qu.:   60906   3rd Qu.:  3378   3rd Qu.:2.805e+06  
    ##                     Max.   :16445558   Max.   :732818   Max.   :4.034e+09  
    ##                     NA's   :2          NA's   :2        NA's   :2

### Checking for any N/A values

``` r
video_stats %>%
    summarise_all(~ sum(is.na(.)))
```

    ##   X Title Video.ID Published.At Keyword Likes Comments Views
    ## 1 0     0        0            0       0     2        2     2

There are two N/A Values for variables Likes, Comments an views.

### Dropping these values from the dataset.

``` r
video_stats <- video_stats %>%
    drop_na()
```

### Checking for duplicates

``` r
any(duplicated(video_stats, ))
```

    ## [1] FALSE

``` r
video_stats[duplicated(video_stats), ]
```

    ## [1] X            Title        Video.ID     Published.At Keyword     
    ## [6] Likes        Comments     Views       
    ## <0 rows> (or 0-length row.names)

No duplicate rows found in the dataset.

If we want to look for duplicates depending on one or multiple variable
we can look ccheck it this way.

``` r
# any(duplicated(video_stats$Keyword))
# video_stats[duplicated(video_stats$Keyword),]
```

Data Manipulation
-----------------

### Creating new variables more pertinent to the job at hand

### Checking performance of videos per 1K views in terms of Likes and Comments

``` r
video_stats <- video_stats %>%
    mutate(
        LikesPer1k = round(Likes / (Views / 1000), 2),
        CommentsPer1k = round(Comments / (Views / 1000), 2)
    )
video_stats <- video_stats %>%
    mutate(PubYear = as.factor(substr(Published.At, 1, 4)))

glimpse(video_stats)
```

    ## Rows: 1,879
    ## Columns: 11
    ## $ X             <int> 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16…
    ## $ Title         <chr> "Apple Pay Is Killing the Physical Wallet After Only Eig…
    ## $ Video.ID      <chr> "wAZZ-UWGVHI", "b3x28s61q3c", "4mgePWWCAmA", "kXiYSI7H2b…
    ## $ Published.At  <chr> "2022-08-23", "2022-08-24", "2022-08-23", "2022-08-23", …
    ## $ Keyword       <chr> "tech", "tech", "tech", "tech", "tech", "tech", "tech", …
    ## $ Likes         <dbl> 3407, 76779, 63825, 71566, 96513, 33570, 135047, 216935,…
    ## $ Comments      <dbl> 672, 4306, 3338, 1426, 5155, 1643, 9367, 12605, 2882, 10…
    ## $ Views         <dbl> 135612, 1758063, 1564007, 922918, 1855644, 943119, 59377…
    ## $ LikesPer1k    <dbl> 25.12, 43.67, 40.81, 77.54, 52.01, 35.59, 22.74, 45.36, …
    ## $ CommentsPer1k <dbl> 4.96, 2.45, 2.13, 1.55, 2.78, 1.74, 1.58, 2.64, 0.41, 1.…
    ## $ PubYear       <fct> 2022, 2022, 2022, 2022, 2022, 2021, 2022, 2022, 2021, 20…

Creating Plots
--------------

In this case we want to see the numbeer of published videos per year. We
will be using one variable that is discrete for the plotting PubYear. In
this scenarion geom\_bar is the best way to go.

``` r
video_stats %>%
    ggplot(aes(x = PubYear)) +
    geom_bar(fill = "blue") +
    theme_minimal() +
    labs(
        title = "Number of videos per year",
        x = "Publication year", y = "Count of videos"
    )
```

![](https://raw.githubusercontent.com/GYahia/R/main/unnamed-chunk-11-1.png)

``` r
group_keyword_avg_likes_views <- video_stats %>%
    group_by(Keyword) %>%
    summarise(
        avg_likes_in_K = mean(Likes, na.rm = FALSE) / 1000,
        avg_views_in_K = mean(Views, na.rm = FALSE) / 1000, count = n()
    )
print(
    arrange(group_keyword_avg_likes_views, desc(avg_likes_in_K)),
    n = 25
)
```

    ## # A tibble: 41 × 4
    ##    Keyword       avg_likes_in_K avg_views_in_K count
    ##    <chr>                  <dbl>          <dbl> <int>
    ##  1 mrbeast               2106.          66764.    50
    ##  2 animals                761.          94724.    38
    ##  3 bed                    474.          53893.    44
    ##  4 google                 471.         103365.    45
    ##  5 music                  314.          29365.    46
    ##  6 cubes                  303.          15039.    50
    ##  7 history                273.          15047.    50
    ##  8 marvel                 210.           6614.    50
    ##  9 tutorial               168.           6761.    50
    ## 10 how-to                 159.           7809.    48
    ## 11 mukbang                141.          10905.    45
    ## 12 apple                  118.          10747.    42
    ## 13 physics                 98.7          3692.    50
    ## 14 food                    95.0          5252.    48
    ## 15 mathchemistry           93.8          3328.    15
    ## 16 sports                  90.5          8601.    49
    ## 17 reaction                74.0           623.    50
    ## 18 lofi                    73.4          3990.    41
    ## 19 interview               73.3          2966.    50
    ## 20 business                72.0          7236.    48
    ## 21 biology                 66.5          4122.    47
    ## 22 education               62.3          2684.    24
    ## 23 trolling                61.4          1420.    50
    ## 24 minecraft               61.2          1813.    50
    ## 25 tech                    60.5          1919.    48
    ## # … with 16 more rows

Mr Beast is the one getting the most number of likes even though hehas
not the biggest number of average views.

``` r
group_keyword_avg_likes_views_year <- video_stats %>%
    group_by(Keyword, PubYear) %>%
    summarise(
        avg_likes = mean(Likes, na.rm = FALSE),
        avg_views = mean(Views, na.rm = FALSE), count = n()
    )
```

    ## `summarise()` has grouped output by 'Keyword'. You can override using the
    ## `.groups` argument.

``` r
print(
    arrange(group_keyword_avg_likes_views_year, desc(avg_likes)),
    n = 25
)
```

    ## # A tibble: 221 × 5
    ## # Groups:   Keyword [41]
    ##    Keyword PubYear avg_likes   avg_views count
    ##    <chr>   <fct>       <dbl>       <dbl> <int>
    ##  1 animals 2013    11025176  1582262997      1
    ##  2 mrbeast 2021     5712064.  155041850.     6
    ##  3 google  2018     5501680. 1351287503.     3
    ##  4 history 2017     5471653   147339243      1
    ##  5 history 2016     5400589   434352213      1
    ##  6 apple   2016     4144389   425478119      1
    ##  7 mrbeast 2020     4013403   129851855.     4
    ##  8 animals 2014     3381630   510054522      2
    ##  9 bed     2020     2469151.  170552912      4
    ## 10 cubes   2009     2307773   168546247      1
    ## 11 mrbeast 2019     2003151    90017911.    12
    ## 12 music   2021     1824084.  243465852.     4
    ## 13 lofi    2019     1638369    84747957      1
    ## 14 animals 2009     1357197   153478497      1
    ## 15 music   2020     1257132.  132379942.     2
    ## 16 mrbeast 2022     1104711.   28868813.    28
    ## 17 animals 2019     1103713   106151957      1
    ## 18 cubes   2016      954935    94900199      1
    ## 19 bed     2009      887882.  223451481.     3
    ## 20 sports  2017      867074   106014469      1
    ## 21 animals 2020      769652.   48428226.     9
    ## 22 bed     2011      766414   179429906.     2
    ## 23 how-to  2013      723998    31858250      1
    ## 24 sat     2011      712273    18116954      1
    ## 25 food    2017      672749    48018833      1
    ## # … with 196 more rows

Depending on the year, some Keywords got more likes than MrBeast videos
on average.

``` r
summary(group_keyword_avg_likes_views)
```

    ##    Keyword          avg_likes_in_K     avg_views_in_K         count      
    ##  Length:41          Min.   :   3.013   Min.   :   247.5   Min.   :15.00  
    ##  Class :character   1st Qu.:  33.663   1st Qu.:  1192.0   1st Qu.:45.00  
    ##  Mode  :character   Median :  66.484   Median :  2966.1   Median :48.00  
    ##                     Mean   : 167.996   Mean   : 11902.2   Mean   :45.83  
    ##                     3rd Qu.: 141.407   3rd Qu.:  7809.3   3rd Qu.:50.00  
    ##                     Max.   :2105.914   Max.   :103365.0   Max.   :50.00

Maximum count is 50 and Max Avg Views is :103365 (in Thousands).

``` r
coeff_avg_views_vs_count <- max(group_keyword_avg_likes_views$count) /
    max(group_keyword_avg_likes_views$avg_views_in_K)
```

``` r
legend_colors <- c("blue", "red")
legend_labels <- c("Count", "AVG Views (in Thousands)")

# Plot count and average views by keyword
ggplot(group_keyword_avg_likes_views, aes(x = Keyword)) +
    # Add first y-axis with count
    geom_bar(aes(y = count, fill = legend_colors[1]),
        stat = "identity",
        width = 0.4, position = position_nudge(x = 0.15)
    ) +
    # Add second y-axis with average views
    geom_bar(aes(y = avg_views_in_K / 1000, fill = legend_colors[2]),
        stat = "identity",
        width = 0.4, position = position_nudge(x = -0.15)
    ) +
    # Add a legend
    scale_fill_manual(values = legend_colors, labels = legend_labels) +
    # Add a title and adjust theme
    ggtitle("\n Count and Average Views by Keyword\n") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 15),
         text = element_text(size = 20)) +
    scale_y_continuous(
        name = "Count\n",
        sec.axis = sec_axis(~ . / coeff_avg_views_vs_count,
         name = "AVG Views (in Thounsands)\n")
    )
```

![](https://raw.githubusercontent.com/GYahia/R/main/unnamed-chunk-16-1.png)

Even though some categories have the same number of videos available,
the number of likes is way too divergent for 5 of them.

Keywords having more than 10000000 AVG Views.

``` r
most_viewed_channels <- group_keyword_avg_likes_views %>%
    filter(avg_views_in_K > 10000) %>%
    arrange(desc(avg_views_in_K))

most_viewed_channels
```

    ## # A tibble: 9 × 4
    ##   Keyword avg_likes_in_K avg_views_in_K count
    ##   <chr>            <dbl>          <dbl> <int>
    ## 1 google            471.        103365.    45
    ## 2 animals           761.         94724.    38
    ## 3 mrbeast          2106.         66764.    50
    ## 4 bed               474.         53893.    44
    ## 5 music             314.         29365.    46
    ## 6 history           273.         15047.    50
    ## 7 cubes             303.         15039.    50
    ## 8 mukbang           141.         10905.    45
    ## 9 apple             118.         10747.    42

``` r
legend_labels2 <- c("AVG Likes (in Thousands)", "AVG Views (in Thousands)")


ggplot(most_viewed_channels, aes(x = Keyword)) +
    # Add first y-axis with count
    geom_bar(aes(y = avg_likes_in_K, fill = legend_colors[1]),
        stat = "identity",
        width = 0.4, position = position_nudge(x = 0.2)
    ) +
    # Add second y-axis with average views
    geom_bar(aes(y = avg_views_in_K / 1000, fill = legend_colors[2]),
        stat = "identity",
        width = 0.4, position = position_nudge(x = -0.2)
    ) +
    # Add a legend
    scale_fill_manual(values = legend_colors, labels = legend_labels2) +
    # Add a title and adjust theme
    ggtitle("\n Average Views/Likes by Keyword\n") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90),
         text = element_text(size = 10)) +
    scale_y_continuous(
        name = "AVG Views (in Thounsands)\n",
        sec.axis = sec_axis(~ . / 50, name = "AVG Likes (in Thounsands)\n")
    )
```

![](https://raw.githubusercontent.com/GYahia/R/main/unnamed-chunk-17-1.png)
