---
title: "Mini Project 2"
author: "Elise Thibodeau - ethibodeau0825@floridapoly.edu"
date: "2026-06-18"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---




# Introduction
For this project, I explored the *Kodiak Bear GPS Collar Fix Location* dataset obtained from Data.gov. The dataset contains GPS collar observations collected from Kodiak bears on Kodiak Island, Alaska between 2008 and 2015. In addition to geographic coordinates, the dataset includes information such as **activity levels, temperature, date and time of observation, and GPS accuracy measurements**.
The objective of this analysis was to explore patterns in bear movement and activity through data visualization and modeling. Specifically, I examined which bears traveled the farthest distances, where bears were most active across Kodiak Island, and how their activity levels varied throughout the day.

# Data Preparation and Summary
The dataset contained GPS collar observations for **57 individual bears and over 170,000 location records**. Prior to analysis, observations with missing latitude, longitude, or activity values were removed. The date-time field was used to create additional variables such as hour of day for further analysis.
Several summary statistics were generated using dplyr functions such as **filter(), group_by(), summarize(), and mutate()**. These summaries included the number of observations per year, activity distributions, and estimated distances traveled by each bear. Distance travel was estimated by calculating the distance between consecutive GPS locations for each bear and summing these distances across the observation windows.
The summaries revealed substantial variation in movement patterns among bears, with some traveling considerably greater distances than others.



``` r
library(dplyr)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(broom)

bears <- read_csv("../data/beargps_08-15.csv")
head(bears)
```

```
## # A tibble: 6 × 10
##    ...1 bearid  year bear_year posdtAK               lat  long  PDOP activity
##   <dbl>  <dbl> <dbl> <chr>     <dttm>              <dbl> <dbl> <dbl>    <dbl>
## 1     1    515  2008 515_2008  2008-05-31 15:00:43  57.3 -154.  6.51     30.5
## 2     2    515  2008 515_2008  2008-05-31 17:00:37  57.3 -154.  3.77      0.5
## 3     3    515  2008 515_2008  2008-05-31 19:00:36  57.3 -154.  2.65     46  
## 4     4    515  2008 515_2008  2008-05-31 21:00:36  57.3 -154.  3.39      0.5
## 5     5    515  2008 515_2008  2008-05-31 23:00:36  57.3 -154.  6.9       0.5
## 6     6    515  2008 515_2008  2008-06-01 01:00:49  57.3 -154.  4.59      1.5
## # ℹ 1 more variable: tempC <dbl>
```




``` r
# Basic summaries
nrow(bears)
```

```
## [1] 172545
```

``` r
n_distinct(bears$bearid)
```

```
## [1] 57
```

``` r
summary(bears$activity)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     1.0    20.5   160.4    61.0  2802.0
```

``` r
summary(bears$tempC)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    -7.0    15.0    19.0    20.3    25.0    47.0
```

``` r
# GPS fixes by year
bears %>%
  group_by(year) %>%
  summarize(
    gps_fixes = n(),
    bears_tracked = n_distinct(bearid)
  )
```

```
## # A tibble: 7 × 3
##    year gps_fixes bears_tracked
##   <dbl>     <int>         <int>
## 1  2008      8167             3
## 2  2009      4797             3
## 3  2010     57452            20
## 4  2011     46439            21
## 5  2012     12543            19
## 6  2014     23994            12
## 7  2015     19153            15
```

``` r
# Observations per bear
bears %>%
  group_by(bearid) %>%
  summarize(
    observations = n()
  ) %>%
  summary()
```

```
##      bearid       observations 
##  Min.   :  6.0   Min.   : 173  
##  1st Qu.:535.0   1st Qu.:1487  
##  Median :556.0   Median :2862  
##  Mean   :530.5   Mean   :3027  
##  3rd Qu.:574.0   3rd Qu.:3822  
##  Max.   :594.0   Max.   :8502
```

# Interactive Visualization: Top Bears by Distance Traveled
The first visualization was an interactive bar chart showing the bears with the greatest estimated travel distances. Distance was calculated using sequential GPS coordinates for each bear and aggregated across the study period.
This visualization allows users to interactively explore which bears exhibited the largest movement ranges. The results indicate that a small number of bears accounted for substantially greater travel distances than the rest of the population, suggesting differences in movement behavior and habitat use among the bears.



``` r
#1 - Interactive plot for top bears by distance traveled

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(geosphere)
library(lubridate)
library(forcats)


bears_clean <- bears %>%
  mutate(
    posdtAK = ymd_hms(posdtAK),
    bearid = as.factor(bearid)
  ) %>%
  filter(!is.na(bearid), !is.na(posdtAK), !is.na(lat), !is.na(long))

bear_distances <- bears_clean %>%
  arrange(bearid, posdtAK) %>%
  group_by(bearid) %>%
  mutate(
    previous_long = lag(long),
    previous_lat = lag(lat)
  ) %>%
  filter(!is.na(previous_long), !is.na(previous_lat)) %>%
  rowwise() %>%
  mutate(
    distance_meters = distHaversine(
      c(previous_long, previous_lat),
      c(long, lat)
    )
  ) %>%
  ungroup() %>%
  group_by(bearid) %>%
  summarize(
    total_distance_km = sum(distance_meters, na.rm = TRUE) / 1000,
    gps_points = n(),
    first_date = min(posdtAK),
    last_date = max(posdtAK),
    .groups = "drop"
  ) %>%
  arrange(desc(total_distance_km))

top_bears_distance <- bear_distances %>%
  slice_max(total_distance_km, n = 15) %>%
  mutate(
    bearid = fct_reorder(as.factor(bearid), total_distance_km),
    total_distance_km = round(total_distance_km, 1)
  )

p_distance <- plot_ly(
  data = top_bears_distance,
  x = ~total_distance_km,
  y = ~bearid,
  type = "bar",
  orientation = "h",
  marker = list(
    color = ~total_distance_km,
    colorscale = "Viridis",
    showscale = TRUE,
    colorbar = list(title = "Distance<br>(km)")
  ),
  text = ~paste0(total_distance_km, " km"),
  textposition = "outside",
  hovertext = ~paste(
    "<b>Bear ID:</b>", bearid,
    "<br><b>Total Distance:</b>", total_distance_km, "km",
    "<br><b>GPS Points:</b>", gps_points,
    "<br><b>First Date:</b>", format(first_date, "%B %d, %Y"),
    "<br><b>Last Date:</b>", format(last_date, "%B %d, %Y")
  ),
  hoverinfo = "text"
) %>%
  layout(
    title = list(
      text = "<b>Top Bears by Estimated Distance Traveled</b>",
      x = 0.05
    ),
    xaxis = list(
      title = "Estimated Distance Traveled (km)",
      showgrid = TRUE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = "Bear ID",
      automargin = TRUE
    ),
    margin = list(l = 90, r = 100, t = 70, b = 60),
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )

p_distance
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-e290f624a207a19cccdb" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-e290f624a207a19cccdb">{"x":{"visdat":{"349c64f860e5":["function () ","plotlyVisDat"]},"cur_data":"349c64f860e5","attrs":{"349c64f860e5":{"x":{},"y":{},"orientation":"h","marker":{"color":{},"colorscale":"Viridis","showscale":true,"colorbar":{"title":"Distance<br>(km)"}},"text":{},"textposition":"outside","hovertext":{},"hoverinfo":"text","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":60,"l":90,"t":70,"r":100},"title":{"text":"<b>Top Bears by Estimated Distance Traveled<\/b>","x":0.050000000000000003},"xaxis":{"domain":[0,1],"automargin":true,"title":"Estimated Distance Traveled (km)","showgrid":true,"zeroline":false},"yaxis":{"domain":[0,1],"automargin":true,"title":"Bear ID","type":"category","categoryorder":"array","categoryarray":["580","584","554","577","560","527","535","558","517","534","548","532","520","545","530","6","55","106","515","516","529","531","533","536","537","538","540","543","546","547","550","553","555","556","559","561","563","564","565","566","567","569","570","571","573","574","575","578","581","582","583","586","589","590","591","592","594"]},"plot_bgcolor":"white","paper_bgcolor":"white","hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[2311.1999999999998,2101.5999999999999,2050.6999999999998,2005.0999999999999,1886.5999999999999,1877.5,1627.8,1612,1602.0999999999999,1319.2,1289.5999999999999,1274.5,1222.0999999999999,1081,1065],"y":["530","545","520","532","548","534","517","558","535","527","560","577","554","584","580"],"orientation":"h","marker":{"color":[2311.1999999999998,2101.5999999999999,2050.6999999999998,2005.0999999999999,1886.5999999999999,1877.5,1627.8,1612,1602.0999999999999,1319.2,1289.5999999999999,1274.5,1222.0999999999999,1081,1065],"colorscale":"Viridis","showscale":true,"colorbar":{"title":"Distance<br>(km)"},"line":{"color":"rgba(31,119,180,1)"}},"text":["2311.2 km","2101.6 km","2050.7 km","2005.1 km","1886.6 km","1877.5 km","1627.8 km","1612 km","1602.1 km","1319.2 km","1289.6 km","1274.5 km","1222.1 km","1081 km","1065 km"],"textposition":["outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside","outside"],"hovertext":["<b>Bear ID:<\/b> 530 <br><b>Total Distance:<\/b> 2311.2 km <br><b>GPS Points:<\/b> 6243 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> August 21, 2012","<b>Bear ID:<\/b> 545 <br><b>Total Distance:<\/b> 2101.6 km <br><b>GPS Points:<\/b> 8461 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> October 05, 2012","<b>Bear ID:<\/b> 520 <br><b>Total Distance:<\/b> 2050.7 km <br><b>GPS Points:<\/b> 4225 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> June 11, 2012","<b>Bear ID:<\/b> 532 <br><b>Total Distance:<\/b> 2005.1 km <br><b>GPS Points:<\/b> 7889 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> July 22, 2015","<b>Bear ID:<\/b> 548 <br><b>Total Distance:<\/b> 1886.6 km <br><b>GPS Points:<\/b> 5902 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> August 03, 2012","<b>Bear ID:<\/b> 534 <br><b>Total Distance:<\/b> 1877.5 km <br><b>GPS Points:<\/b> 6196 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> July 29, 2012","<b>Bear ID:<\/b> 517 <br><b>Total Distance:<\/b> 1627.8 km <br><b>GPS Points:<\/b> 6433 <br><b>First Date:<\/b> May 28, 2008 <br><b>Last Date:<\/b> July 31, 2012","<b>Bear ID:<\/b> 558 <br><b>Total Distance:<\/b> 1612 km <br><b>GPS Points:<\/b> 3921 <br><b>First Date:<\/b> June 10, 2011 <br><b>Last Date:<\/b> October 12, 2012","<b>Bear ID:<\/b> 535 <br><b>Total Distance:<\/b> 1602.1 km <br><b>GPS Points:<\/b> 7509 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> June 04, 2012","<b>Bear ID:<\/b> 527 <br><b>Total Distance:<\/b> 1319.2 km <br><b>GPS Points:<\/b> 4933 <br><b>First Date:<\/b> June 07, 2010 <br><b>Last Date:<\/b> October 12, 2011","<b>Bear ID:<\/b> 560 <br><b>Total Distance:<\/b> 1289.6 km <br><b>GPS Points:<\/b> 4588 <br><b>First Date:<\/b> June 10, 2011 <br><b>Last Date:<\/b> October 03, 2012","<b>Bear ID:<\/b> 577 <br><b>Total Distance:<\/b> 1274.5 km <br><b>GPS Points:<\/b> 3708 <br><b>First Date:<\/b> June 10, 2014 <br><b>Last Date:<\/b> September 08, 2015","<b>Bear ID:<\/b> 554 <br><b>Total Distance:<\/b> 1222.1 km <br><b>GPS Points:<\/b> 4713 <br><b>First Date:<\/b> June 10, 2011 <br><b>Last Date:<\/b> August 11, 2015","<b>Bear ID:<\/b> 584 <br><b>Total Distance:<\/b> 1081 km <br><b>GPS Points:<\/b> 3666 <br><b>First Date:<\/b> June 10, 2014 <br><b>Last Date:<\/b> October 05, 2015","<b>Bear ID:<\/b> 580 <br><b>Total Distance:<\/b> 1065 km <br><b>GPS Points:<\/b> 3225 <br><b>First Date:<\/b> June 10, 2014 <br><b>Last Date:<\/b> August 11, 2015"],"hoverinfo":["text","text","text","text","text","text","text","text","text","text","text","text","text","text","text"],"type":"bar","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


# Spatial Visualization: Kodiak Bear Activity Locations
The second visualization was a spatial map of GPS collar observations displayed across Kodiak Island. Geographic coordinates were plotted on a map background and colored according to activity level.
Activity values represent the activity level recorded by the GPS collar of time a bear was active during the collection window as measured by the collar's onboard mercury switch. Brighter colors indicate periods of higher activity, while darker colors represent lower activity levels.
The map reveals clusters of observations concentrated in specific regions of Kodiak Island. Areas with higher activity levels may represent important feeding, travel, or habitat locations. This visualization highlights the spatial distribution of bear behavior and provides insight into how activity varies across the island.



``` r
#2 - Spatial Vizualization

library(tidyverse)
library(ggplot2)
library(viridis)
library(maps)


# Clean data
bears_clean <- bears %>%
  filter(
    !is.na(lat),
    !is.na(long),
    !is.na(activity)
  )

#Get world map data
world_map <- map_data("world")

ggplot() +

  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray92",
    color = "gray65",
    linewidth = 0.3
  ) +
  
  geom_density_2d(
  data = bears_clean,
  aes(
    x = long,
    y = lat
  ),
  color = "pink",
  alpha = 0.5
) +

  geom_point(
    data = bears_clean %>% slice_sample(n = 12000),
    aes(x = long, y = lat, color = activity),
    alpha = 0.45,
    size = 0.55
  ) +

  scale_color_viridis_c(
    name = "Activity\nLevel"
  ) +

  coord_quickmap(
    xlim = c(-154.75, -153.25),
    ylim = c(56.9, 57.65),
    expand = FALSE
  ) +

  labs(
    title = "Kodiak Bear GPS Activity Locations",
    subtitle = "GPS collar observations across Kodiak Island colored by activity level",
    caption = "Source: Kodiak bear GPS collar data, 2008–2015",
    x = "Longitude",
    y = "Latitude"
  ) +

  theme_minimal(base_size = 14) +

  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 9, color = "gray40"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )
```

<img src="Thibodeau_project_02_files/figure-html/unnamed-chunk-4-1.png" alt="Map of Kodiak Island showing GPS collar locations for bears collected between 2008 and 2015. Each point represents a recorded bear location and is colored by activity level, with darker purple indicating lower activity and brighter yellow indicating higher activity. Most observations are concentrated in the central and eastern portions of the island, with the highest activity levels clustered in a few localized areas rather than being evenly distributed across the landscape. The map highlights that bears tend to use specific regions of Kodiak Island more intensively than others."  />



# Model Visualization: Daily Activity Cycle
To investigate temporal patterns in bear behavior, a model was created examining bear activity by hour of day. Hour values were extracted from the timestamp associated with each GPS observation, and average activity levels were calculated for each hour.
The resulting visualization illustrates how activity levels change throughout the day. The model identifies periods of increased and decreased activity and provides insight into daily behavioral patterns. The hour of peak activity is **9 PM** while the lowest activity time is **4 AM**. Understanding when bears are most active can contribute to broader ecological interpretations regarding feeding, movement, and habitat use.


``` r
#3 - Bears Daily Activity Cycle

library(tidyverse)
library(lubridate)
library(broom)


bears_hour <- bears %>%
  filter(!is.na(posdtAK), !is.na(activity)) %>%
  mutate(
    hour = hour(posdtAK)
  )

# Activity by hour of day
hour_model <- lm(activity ~ factor(hour), data = bears_hour)

summary(hour_model)
```

```
## 
## Call:
## lm(formula = activity ~ factor(hour), data = bears_hour)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -242.58 -175.08 -131.80  -48.25 2630.62 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     103.472      4.321  23.945  < 2e-16 ***
## factor(hour)1   -31.500      6.320  -4.984 6.22e-07 ***
## factor(hour)2   -50.009      6.164  -8.113 4.99e-16 ***
## factor(hour)3   -50.581      6.353  -7.961 1.71e-15 ***
## factor(hour)4   -53.720      6.151  -8.733  < 2e-16 ***
## factor(hour)5   -35.071      6.296  -5.570 2.55e-08 ***
## factor(hour)6     8.726      6.077   1.436    0.151    
## factor(hour)7    98.441      6.219  15.830  < 2e-16 ***
## factor(hour)8   111.688      6.051  18.458  < 2e-16 ***
## factor(hour)9   132.422      6.228  21.262  < 2e-16 ***
## factor(hour)10  101.961      6.100  16.716  < 2e-16 ***
## factor(hour)11  101.419      6.307  16.080  < 2e-16 ***
## factor(hour)12   67.904      6.166  11.013  < 2e-16 ***
## factor(hour)13   62.430      6.379   9.787  < 2e-16 ***
## factor(hour)14   57.770      6.175   9.355  < 2e-16 ***
## factor(hour)15   72.113      6.354  11.350  < 2e-16 ***
## factor(hour)16   70.550      6.131  11.506  < 2e-16 ***
## factor(hour)17   88.721      6.294  14.096  < 2e-16 ***
## factor(hour)18   75.504      6.098  12.381  < 2e-16 ***
## factor(hour)19  118.870      6.227  19.090  < 2e-16 ***
## factor(hour)20  109.324      6.031  18.128  < 2e-16 ***
## factor(hour)21  139.104      6.203  22.425  < 2e-16 ***
## factor(hour)22   91.019      6.046  15.054  < 2e-16 ***
## factor(hour)23   61.791      6.250   9.887  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 376 on 172521 degrees of freedom
## Multiple R-squared:  0.025,	Adjusted R-squared:  0.02487 
## F-statistic: 192.3 on 23 and 172521 DF,  p-value: < 2.2e-16
```

``` r
hour_summary <- bears_hour %>%
  group_by(hour) %>%
  summarize(
    avg_activity = mean(activity, na.rm = TRUE),
    .groups = "drop"
  )


peak_hour <- hour_summary %>%
  filter(avg_activity == max(avg_activity))

# Plot daily activity cycle
ggplot(hour_summary, aes(x = hour, y = avg_activity)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_point(size = 3, color = "#0072B2") +
  geom_point(
    data = peak_hour,
    aes(x = hour, y = avg_activity),
    size = 5,
    color = "#E69F00"
  ) +
  annotate(
    "text",
    x = peak_hour$hour,
    y = peak_hour$avg_activity + 10,
    label = "Peak activity",
    fontface = "bold",
    color = "#E69F00",
    size = 4
  ) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Daily Activity Cycle of Kodiak Bears",
    subtitle = "Average activity level by hour of day",
    x = "Hour of Day",
    y = "Average Activity Level"
  ) +
  theme_minimal(base_size = 14)
```

<img src="Thibodeau_project_02_files/figure-html/unnamed-chunk-5-1.png" alt="Line chart showing the average activity level of Kodiak bears by hour of the day. Activity is lowest during the early morning hours, reaching its minimum around 4:00 a.m. Activity then rises sharply after 6:00 a.m., remains relatively high throughout the daytime, and reaches its highest level at approximately 9:00 p.m. before declining again later in the evening. The pattern suggests that Kodiak bears are least active before dawn and most active during the morning and evening hours."  />

## Overall Key Findings

Together, the visualizations tell a story about how Kodiak bears move and behave across both space and time. The interactive distance-traveled plot revealed that some bears traveled substantially farther than others, suggesting differences in movement behavior and habitat use. The spatial visualization showed that bear observations and higher activity levels were concentrated in specific areas of Kodiak Island rather than being evenly distributed, indicating preferred habitats or wilderness parks. The daily activity cycle visualization demonstrated that bear activity changes throughout the day, suggesting that activity follows predictable behavioral patterns rather than occurring randomly.

One of the main challenges encountered during the project was creating an accurate spatial visualization. Because the dataset was provided as a CSV file containing latitude and longitude coordinates, additional work was required to identify an appropriate map layer and align the GPS observations with the island boundaries. Another challenge was calculating estimated travel distances from sequential GPS locations and ensuring that observations were correctly grouped by bear. Additional analyses that could further explore this dataset include examining seasonal activity patterns, comparing movement behavior across years, identifying activity hotspots using density-based mapping techniques, or developing more advanced regression models to investigate factors influencing bear activity.

Several design and visualization principles discussed in class were applied throughout this project. First, principles of perception were considered by selecting visual encodings that allow patterns to stand out quickly, such as using color intensity to represent activity levels and geographic position to display movement patterns. Additionally, Gestalt principles proximity and similarity helps recognize areas where bear activity is concentrated and identify groups of observations that may represent commonly used habitats or travel routes.

The visualizations also incorporated the CRAP design principles. Contrast was created through the use of the viridis color palette to distinguish low and high activity levels. Repetition was maintained through consistent themes, fonts, and formatting across all visualizations. Alignment was used to create a clean and organized layout, while proximity helped group related information such as titles, legends, and plot elements. Clear labels, descriptive titles, and minimal visual clutter were used to improve readability and ensure that the visualizations effectively communicated the projects findings.

# Conclusion
This analysis demonstrates how GPS collar data can be used to examine wildlife movement and behavior through multiple visualization techniques. The results showed considerable variation in the distances traveled by individual bears, suggesting differences in movement behavior and habitat use. Spatial visualizations indicated that GPS observations and higher activity levels were concentrated in specific areas of the island rather than being evenly distributed, suggesting that bears may preferentially use certain habitats, feeding areas, or travel corridors. Overall, the project demonstrates how GPS collar data can be used to better understand wildlife behavior through data visualization, spatial analysis, and statistical modeling.



