---
title: "Mini Project 3"
author: "Elise THibodeau - ethibodeau0825@floridapoly.edu"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise you will explore methods to create different types of data visualizations (such as plotting text data, or exploring the distributions of continuous variables).


## PART 1: Density Plots

Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) for 2022, attempt to recreate the charts shown below which were generated using data from 2016. You can read the 2022 dataset using the code below: 


``` r
library(tidyverse)
weather_tpa <- read_csv("../data/tpa_weather_2022.csv")
# random sample 
sample_n(weather_tpa, 4)
```

```
## # A tibble: 4 × 7
##    year month   day precipitation max_temp min_temp ave_temp
##   <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>    <dbl>
## 1  2022     4    16          0          88       73     80.5
## 2  2022     5     4          0          88       73     80.5
## 3  2022     3    11          0          86       72     79  
## 4  2022     6    24          0.19       95       79     87
```

See Slides from Week 4 of Visualizing Relationships and Models (slide 10) for a reminder on how to use this type of dataset with the `lubridate` package for dates and times (example included in the slides uses data from 2016).

Using the 2022 data: 

(a) Create a plot like the one below:

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-2-1.png" alt="Histogram panels showing the distribution of daily maximum temperatures for each month in Tampa during 2022. Temperatures are coolest in winter and warmest in summer, with most summer days reaching 85°F to 95°F" width="80%" style="display: block; margin: auto;" />

Hint: the option `binwidth = 3` was used with the `geom_histogram()` function.

(b) Create a plot like the one below:

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-3-1.png" alt="Density plot of daily maximum temperatures in Tampa during 2022. Most temperatures fall between 85°F and 95°F, while temperatures below 70°F occur relatively infrequently." width="80%" style="display: block; margin: auto;" />

Hint: check the `kernel` parameter of the `geom_density()` function, and use `bw = 0.5`.

(c) Create a plot like the one below:

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-4-1.png" alt="Density plots of daily maximum temperatures by month in Tampa during 2022. Temperatures increase from winter into summer, peak between June and September, and then decrease through fall and winter. Summer months show temperatures concentrated around 85°F to 95°F." width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 

(d) Generate a plot like the chart below:



```
## Warning: package 'ggridges' was built under R version 4.4.3
```

```
## Warning: package 'viridis' was built under R version 4.4.2
```

```
## Loading required package: viridisLite
```

```
## Picking joint bandwidth of 1.93
```

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-5-1.png" alt="Ridgeline density plots of monthly maximum temperatures in Tampa during 2022. Winter months show cooler temperatures, while summer months are concentrated between 85°F and 95°F. Median temperatures are indicated by vertical lines within each ridge." width="80%" style="display: block; margin: auto;" />

Hint: use the`{ggridges}` package, and the `geom_density_ridges()` function paying close attention to the `quantile_lines` and `quantiles` parameters. The plot above uses the `plasma` option (color scale) for the _viridis_ palette.


(e) Create a plot of your choice that uses the attribute for precipitation _(values of -99.9 for temperature or -99.99 for precipitation represent missing data)_.


``` r
library(tidyverse)
library(lubridate)

monthly_rain <- weather_tpa %>%
  filter(precipitation != -99.99) %>%
  group_by(month) %>%
  summarize(avg_precip = mean(precipitation))

ggplot(monthly_rain,
       aes(x = factor(month),
           y = avg_precip,
           fill = avg_precip)) +
  geom_col() +
  scale_fill_viridis_c(option = "viridis") +
  labs(
    title = "Average Daily Precipitation by Month",
    x = "Month",
    y = "Average Precipitation"
  ) +
  theme_bw()
```

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-6-1.png" alt="Bar chart displaying average daily precipitation for each month in Tampa during 2022. Precipitation increases during late spring and summer, peaks in July and September at approximately 0.4 inches per day on average, and decreases during the winter months. The chart highlights Tampa's wetter summer rainy season and drier winter season."  />


## PART 2 

> **You can choose to work on either Option (A) or Option (B)**. Remove from this template the option you decided not to work on. 


### Option (A): Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: Week 6 PowerPoint slides of Visualizing Text Data. 

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/data/BB_top100_2015.csv)

- [RateMyProfessors comments](https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/data/rmp_wit_comments.csv)

- [FL Poly News Articles](https://raw.githubusercontent.com/aalhamadani/dataviz_final_project/main/data/flpoly_news_SP23.csv)


(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)


``` r
library("tidytext")
```

```
## Warning: package 'tidytext' was built under R version 4.4.3
```

``` r
# Load data
songs <- read_csv("../data/BB_top100_2015.csv")
```

```
## Rows: 100 Columns: 6
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (3): Song, Artist, Lyrics
## dbl (3): Rank, Year, Source
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
head(songs)
```

```
## # A tibble: 6 × 6
##    Rank Song              Artist                              Year Lyrics Source
##   <dbl> <chr>             <chr>                              <dbl> <chr>   <dbl>
## 1     1 uptown funk       mark ronson featuring bruno mars    2015 this …      1
## 2     2 thinking out loud ed sheeran                          2015 when …      1
## 3     3 see you again     wiz khalifa featuring charlie puth  2015 its b…      1
## 4     4 trap queen        fetty wap                           2015 im li…      1
## 5     5 sugar             maroon 5                            2015 im hu…      1
## 6     6 shut up and dance walk the moon                       2015 oh do…      1
```

``` r
# Create word frequency data
word_counts <- songs %>%
  select(Song, Artist, Lyrics) %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20)

# Plot top 20 words
ggplot(word_counts, aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(
    title = "Most Common Words in Billboard Top 100 Lyrics",
    subtitle = "Based on Billboard Top 100 songs from 2015",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none"
  )
```

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-7-1.png" alt="Bar chart displaying the 20 most frequently used words in Billboard Top 100 song lyrics from 2015. The most common words are 'im', 'dont', and 'love', followed by words such as 'baby', 'wanna', and 'youre'."  />

### Interactive Plot


``` r
#Maximum Temperatures by month - dropdown interactive chart
# Create empty plot
library(plotly)
```

```
## Warning: package 'plotly' was built under R version 4.4.3
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

``` r
p <- plot_ly()

# Add one trace for each month
for(i in 1:12){

  month_data <- weather_tpa %>%
    filter(month == i)

  p <- p %>%
    add_lines(
      data = month_data,
      x = ~date,
      y = ~max_temp,
      name = unique(as.character(month_data$month_name)),
      visible = ifelse(i == 1, TRUE, FALSE),
      hovertemplate =
        paste(
          "<b>Date:</b> %{x}<br>",
          "<b>Max Temp:</b> %{y}°F<extra></extra>"
        )
    )
}

# Create dropdown buttons
buttons <- list()

for(i in 1:12){

  visible <- rep(FALSE, 12)
  visible[i] <- TRUE

  buttons[[i]] <- list(
    method = "update",
    args = list(
      list(visible = visible),
      list(title = paste("Maximum Temperatures -",
                         month.name[i]))
    ),
    label = month.name[i]
  )
}

# Add dropdown menu
p <- p %>%
  layout(
    title = "Maximum Temperatures - January",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Maximum Temperature (°F)"),
    updatemenus = list(
      list(
        type = "dropdown",
        active = 0,
        buttons = buttons
      )
    )
  )

p
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-6d04d30cddb8fed97196" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-6d04d30cddb8fed97196">{"x":{"visdat":{"586c5c24515f":["function () ","plotlyVisDat"],"586c4099141b":["function () ","data"],"586c4c36971":["function () ","data"],"586c68c76ea6":["function () ","data"],"586c769a427a":["function () ","data"],"586c62562c3c":["function () ","data"],"586c764a7454":["function () ","data"],"586c2b446239":["function () ","data"],"586c1d3f32e9":["function () ","data"],"586c2a683462":["function () ","data"],"586c374235ae":["function () ","data"],"586c28f13880":["function () ","data"],"586c6eff786e":["function () ","data"]},"cur_data":"586c6eff786e","attrs":{"586c4099141b":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"January","visible":true,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c4c36971":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"February","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c68c76ea6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"March","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c769a427a":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"April","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c62562c3c":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"May","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c764a7454":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"June","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c2b446239":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"July","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c1d3f32e9":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"August","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c2a683462":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"September","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c374235ae":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"October","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c28f13880":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"November","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true},"586c6eff786e":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","name":"December","visible":false,"hovertemplate":"<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Maximum Temperatures - January","xaxis":{"domain":[0,1],"automargin":true,"title":"Date"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Maximum Temperature (°F)"},"updatemenus":[{"type":"dropdown","active":0,"buttons":[{"method":"update","args":[{"visible":[true,false,false,false,false,false,false,false,false,false,false,false]},{"title":"Maximum Temperatures - January"}],"label":"January"},{"method":"update","args":[{"visible":[false,true,false,false,false,false,false,false,false,false,false,false]},{"title":"Maximum Temperatures - February"}],"label":"February"},{"method":"update","args":[{"visible":[false,false,true,false,false,false,false,false,false,false,false,false]},{"title":"Maximum Temperatures - March"}],"label":"March"},{"method":"update","args":[{"visible":[false,false,false,true,false,false,false,false,false,false,false,false]},{"title":"Maximum Temperatures - April"}],"label":"April"},{"method":"update","args":[{"visible":[false,false,false,false,true,false,false,false,false,false,false,false]},{"title":"Maximum Temperatures - May"}],"label":"May"},{"method":"update","args":[{"visible":[false,false,false,false,false,true,false,false,false,false,false,false]},{"title":"Maximum Temperatures - June"}],"label":"June"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,true,false,false,false,false,false]},{"title":"Maximum Temperatures - July"}],"label":"July"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,false,true,false,false,false,false]},{"title":"Maximum Temperatures - August"}],"label":"August"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,false,false,true,false,false,false]},{"title":"Maximum Temperatures - September"}],"label":"September"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,false,false,false,true,false,false]},{"title":"Maximum Temperatures - October"}],"label":"October"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,false,false,false,false,true,false]},{"title":"Maximum Temperatures - November"}],"label":"November"},{"method":"update","args":[{"visible":[false,false,false,false,false,false,false,false,false,false,false,true]},{"title":"Maximum Temperatures - December"}],"label":"December"}]}],"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["2022-01-01","2022-01-02","2022-01-03","2022-01-04","2022-01-05","2022-01-06","2022-01-07","2022-01-08","2022-01-09","2022-01-10","2022-01-11","2022-01-12","2022-01-13","2022-01-14","2022-01-15","2022-01-16","2022-01-17","2022-01-18","2022-01-19","2022-01-20","2022-01-21","2022-01-22","2022-01-23","2022-01-24","2022-01-25","2022-01-26","2022-01-27","2022-01-28","2022-01-29","2022-01-30","2022-01-31"],"y":[82,82,75,76,75,74,81,81,84,81,73,77,74,72,75,71,66,64,75,78,76,62,57,63,57,63,74,67,55,58,69],"type":"scatter","mode":"lines","name":"January","visible":true,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-02-01","2022-02-02","2022-02-03","2022-02-04","2022-02-05","2022-02-06","2022-02-07","2022-02-08","2022-02-09","2022-02-10","2022-02-11","2022-02-12","2022-02-13","2022-02-14","2022-02-15","2022-02-16","2022-02-17","2022-02-18","2022-02-19","2022-02-20","2022-02-21","2022-02-22","2022-02-23","2022-02-24","2022-02-25","2022-02-26","2022-02-27","2022-02-28"],"y":[76,82,85,83,71,64,70,56,69,72,78,80,76,68,75,84,86,81,77,82,86,87,86,86,84,85,82,80],"type":"scatter","mode":"lines","name":"February","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-03-01","2022-03-02","2022-03-03","2022-03-04","2022-03-05","2022-03-06","2022-03-07","2022-03-08","2022-03-09","2022-03-10","2022-03-11","2022-03-12","2022-03-13","2022-03-14","2022-03-15","2022-03-16","2022-03-17","2022-03-18","2022-03-19","2022-03-20","2022-03-21","2022-03-22","2022-03-23","2022-03-24","2022-03-25","2022-03-26","2022-03-27","2022-03-28","2022-03-29","2022-03-30","2022-03-31"],"y":[79,78,83,89,88,89,88,86,86,84,86,79,70,81,84,80,79,87,86,84,85,88,87,77,80,81,81,83,86,88,86],"type":"scatter","mode":"lines","name":"March","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"line":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-04-01","2022-04-02","2022-04-03","2022-04-04","2022-04-05","2022-04-06","2022-04-07","2022-04-08","2022-04-09","2022-04-10","2022-04-11","2022-04-12","2022-04-13","2022-04-14","2022-04-15","2022-04-16","2022-04-17","2022-04-18","2022-04-19","2022-04-20","2022-04-21","2022-04-22","2022-04-23","2022-04-24","2022-04-25","2022-04-26","2022-04-27","2022-04-28","2022-04-29","2022-04-30"],"y":[83,83,80,85,88,87,81,76,76,77,85,88,86,86,87,88,88,91,84,85,87,87,88,90,90,87,88,88,89,87],"type":"scatter","mode":"lines","name":"April","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(214,39,40,1)","line":{"color":"rgba(214,39,40,1)"}},"error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"line":{"color":"rgba(214,39,40,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-05-01","2022-05-02","2022-05-03","2022-05-04","2022-05-05","2022-05-06","2022-05-07","2022-05-08","2022-05-09","2022-05-10","2022-05-11","2022-05-12","2022-05-13","2022-05-14","2022-05-15","2022-05-16","2022-05-17","2022-05-18","2022-05-19","2022-05-20","2022-05-21","2022-05-22","2022-05-23","2022-05-24","2022-05-25","2022-05-26","2022-05-27","2022-05-28","2022-05-29","2022-05-30","2022-05-31"],"y":[86,87,88,88,90,89,87,89,90,87,86,87,86,88,88,87,89,91,90,88,90,96,92,94,95,92,89,92,92,96,93],"type":"scatter","mode":"lines","name":"May","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(148,103,189,1)","line":{"color":"rgba(148,103,189,1)"}},"error_y":{"color":"rgba(148,103,189,1)"},"error_x":{"color":"rgba(148,103,189,1)"},"line":{"color":"rgba(148,103,189,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-06-01","2022-06-02","2022-06-03","2022-06-04","2022-06-05","2022-06-06","2022-06-07","2022-06-08","2022-06-09","2022-06-10","2022-06-11","2022-06-12","2022-06-13","2022-06-14","2022-06-15","2022-06-16","2022-06-17","2022-06-18","2022-06-19","2022-06-20","2022-06-21","2022-06-22","2022-06-23","2022-06-24","2022-06-25","2022-06-26","2022-06-27","2022-06-28","2022-06-29","2022-06-30"],"y":[89,91,86,88,91,91,92,92,88,91,90,90,92,94,95,95,97,98,94,95,94,92,93,95,92,96,93,95,94,93],"type":"scatter","mode":"lines","name":"June","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(140,86,75,1)","line":{"color":"rgba(140,86,75,1)"}},"error_y":{"color":"rgba(140,86,75,1)"},"error_x":{"color":"rgba(140,86,75,1)"},"line":{"color":"rgba(140,86,75,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-07-01","2022-07-02","2022-07-03","2022-07-04","2022-07-05","2022-07-06","2022-07-07","2022-07-08","2022-07-09","2022-07-10","2022-07-11","2022-07-12","2022-07-13","2022-07-14","2022-07-15","2022-07-16","2022-07-17","2022-07-18","2022-07-19","2022-07-20","2022-07-21","2022-07-22","2022-07-23","2022-07-24","2022-07-25","2022-07-26","2022-07-27","2022-07-28","2022-07-29","2022-07-30","2022-07-31"],"y":[91,91,91,94,94,96,93,94,92,93,89,93,94,92,93,93,90,93,93,93,93,95,94,95,94,94,93,95,97,97,97],"type":"scatter","mode":"lines","name":"July","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(227,119,194,1)","line":{"color":"rgba(227,119,194,1)"}},"error_y":{"color":"rgba(227,119,194,1)"},"error_x":{"color":"rgba(227,119,194,1)"},"line":{"color":"rgba(227,119,194,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-08-01","2022-08-02","2022-08-03","2022-08-04","2022-08-05","2022-08-06","2022-08-07","2022-08-08","2022-08-09","2022-08-10","2022-08-11","2022-08-12","2022-08-13","2022-08-14","2022-08-15","2022-08-16","2022-08-17","2022-08-18","2022-08-19","2022-08-20","2022-08-21","2022-08-22","2022-08-23","2022-08-24","2022-08-25","2022-08-26","2022-08-27","2022-08-28","2022-08-29","2022-08-30","2022-08-31"],"y":[94,96,92,95,96,95,96,94,94,93,90,92,92,87,87,92,92,93,92,96,94,92,92,95,93,91,92,91,90,91,93],"type":"scatter","mode":"lines","name":"August","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(127,127,127,1)","line":{"color":"rgba(127,127,127,1)"}},"error_y":{"color":"rgba(127,127,127,1)"},"error_x":{"color":"rgba(127,127,127,1)"},"line":{"color":"rgba(127,127,127,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-09-01","2022-09-02","2022-09-03","2022-09-04","2022-09-05","2022-09-06","2022-09-07","2022-09-08","2022-09-09","2022-09-10","2022-09-11","2022-09-12","2022-09-13","2022-09-14","2022-09-15","2022-09-16","2022-09-17","2022-09-18","2022-09-19","2022-09-20","2022-09-21","2022-09-22","2022-09-23","2022-09-24","2022-09-25","2022-09-26","2022-09-27","2022-09-28","2022-09-29","2022-09-30"],"y":[94,91,93,93,94,94,91,87,88,87,90,91,90,87,91,88,92,88,90,92,93,94,93,92,92,93,84,76,77,77],"type":"scatter","mode":"lines","name":"September","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(188,189,34,1)","line":{"color":"rgba(188,189,34,1)"}},"error_y":{"color":"rgba(188,189,34,1)"},"error_x":{"color":"rgba(188,189,34,1)"},"line":{"color":"rgba(188,189,34,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-10-01","2022-10-02","2022-10-03","2022-10-04","2022-10-05","2022-10-06","2022-10-07","2022-10-08","2022-10-09","2022-10-10","2022-10-11","2022-10-12","2022-10-13","2022-10-14","2022-10-15","2022-10-16","2022-10-17","2022-10-18","2022-10-19","2022-10-20","2022-10-21","2022-10-22","2022-10-23","2022-10-24","2022-10-25","2022-10-26","2022-10-27","2022-10-28","2022-10-29","2022-10-30","2022-10-31"],"y":[82,86,84,83,83,84,87,88,90,90,89,85,82,88,88,90,86,81,68,73,77,82,86,86,83,83,85,87,87,87,86],"type":"scatter","mode":"lines","name":"October","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(23,190,207,1)","line":{"color":"rgba(23,190,207,1)"}},"error_y":{"color":"rgba(23,190,207,1)"},"error_x":{"color":"rgba(23,190,207,1)"},"line":{"color":"rgba(23,190,207,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-11-01","2022-11-02","2022-11-03","2022-11-04","2022-11-05","2022-11-06","2022-11-07","2022-11-08","2022-11-09","2022-11-10","2022-11-11","2022-11-12","2022-11-13","2022-11-14","2022-11-15","2022-11-16","2022-11-17","2022-11-18","2022-11-19","2022-11-20","2022-11-21","2022-11-22","2022-11-23","2022-11-24","2022-11-25","2022-11-26","2022-11-27","2022-11-28","2022-11-29","2022-11-30"],"y":[88,90,88,86,86,89,87,85,76,74,78,81,78,79,81,79,69,69,75,61,79,78,79,79,82,82,81,78,83,82],"type":"scatter","mode":"lines","name":"November","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-12-01","2022-12-02","2022-12-03","2022-12-04","2022-12-05","2022-12-06","2022-12-07","2022-12-08","2022-12-09","2022-12-10","2022-12-11","2022-12-12","2022-12-13","2022-12-14","2022-12-15","2022-12-16","2022-12-17","2022-12-18","2022-12-19","2022-12-20","2022-12-21","2022-12-22","2022-12-23","2022-12-24","2022-12-25","2022-12-26","2022-12-27","2022-12-28","2022-12-29","2022-12-30","2022-12-31"],"y":[77,81,82,80,81,81,84,84,79,80,70,76,80,81,76,68,70,69,74,67,69,69,70,45,46,55,68,75,80,81,71],"type":"scatter","mode":"lines","name":"December","visible":false,"hovertemplate":["<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>","<b>Date:<\/b> %{x}<br> <b>Max Temp:<\/b> %{y}°F<extra><\/extra>"],"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"line":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

### Before Vs After

## Before


``` r
top_artists <- songs %>%
  count(Artist, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top_artists,
       aes(x = "", y = n, fill = Artist)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = rainbow(10)) +
  labs(
    title = "Top Artists in Billboard Top 100 Songs"
  ) +
  theme_void()
```

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-9-1.png" alt="Pie chart showing the top 10 artists with songs in the Billboard Top 100. Slice sizes represent the number of songs contributed by each artist."  />
## Improved Chart


``` r
top_artists <- songs %>%
  count(Artist, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(Artist = reorder(Artist, n))

ggplot(top_artists,
       aes(x = n,
           y = Artist,
           fill = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    hjust = -0.2
  ) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Artists with the Most Billboard Top 100 Songs",
    subtitle = "Billboard Top 100 Songs (2015)",
    x = "Number of Songs",
    y = "Artist"
  ) +
  coord_cartesian(
    xlim = c(0, max(top_artists$n) + 1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none"
  )
```

<img src="Thibodeau_project_03_files/figure-html/unnamed-chunk-10-1.png" alt="Horizontal bar chart showing the 10 artists with the most songs in the Billboard Top 100. Bars are ordered from lowest to highest count and include labels showing the exact number of songs."  />

The original chart used a rainbow-colored pie chart, which made comparisons difficult because viewers had to estimate differences in slice sizes and rely on a separate legend. The redesigned bar chart places all artists on a common scale, sorts them by frequency, and uses a more accessible color palette. These changes reduce visual clutter and make the ranking and differences between artists much easier to understand.

