---
title: "Data Visualization - Mini-Project 1"
author: "Elise Thibodeau `ethibodeau0825@floridapoly.edu`"
output:
  html_document:
    df_print: paged
    keep_md: true
---





``` r
library(tidyverse)
library(dplyr)
library(ggplot2)
```

I chose the **Cereals.csv** dataset because of  its rich collection of nutritional information that allows for meaningful analysis and the creation of a variety of interesting visualizations. After exploring the cereal dataset, I noticed that many of the observations shared common characteristics, such as manufacturer and cereal type. These categorical variables presented opportunities for meaningful comparisons. A combination of bar charts, a boxplot, and a scatterplot was used to highlight different features of the dataset, including category distributions, variability among manufacturers, and relationships between nutritional variables. Together, these visualizations provide a more complete understanding of the patterns and trends present within the cereal data.


``` r
#Using the cereal dataset
cereal <- read_csv("../data/Cereals.csv")
head(cereal)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["mfr"],"name":[2],"type":["chr"],"align":["left"]},{"label":["type"],"name":[3],"type":["chr"],"align":["left"]},{"label":["calories"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["protein"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["fat"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["sodium"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["fiber"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["carbo"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["sugars"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["potass"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["vitamins"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["shelf"],"name":[13],"type":["dbl"],"align":["right"]},{"label":["weight"],"name":[14],"type":["dbl"],"align":["right"]},{"label":["cups"],"name":[15],"type":["dbl"],"align":["right"]},{"label":["rating"],"name":[16],"type":["dbl"],"align":["right"]}],"data":[{"1":"100%_Bran","2":"N","3":"C","4":"70","5":"4","6":"1","7":"130","8":"10.0","9":"5.0","10":"6","11":"280","12":"25","13":"3","14":"1","15":"0.33","16":"68.40297"},{"1":"100%_Natural_Bran","2":"Q","3":"C","4":"120","5":"3","6":"5","7":"15","8":"2.0","9":"8.0","10":"8","11":"135","12":"0","13":"3","14":"1","15":"1.00","16":"33.98368"},{"1":"All-Bran","2":"K","3":"C","4":"70","5":"4","6":"1","7":"260","8":"9.0","9":"7.0","10":"5","11":"320","12":"25","13":"3","14":"1","15":"0.33","16":"59.42551"},{"1":"All-Bran_with_Extra_Fiber","2":"K","3":"C","4":"50","5":"4","6":"0","7":"140","8":"14.0","9":"8.0","10":"0","11":"330","12":"25","13":"3","14":"1","15":"0.50","16":"93.70491"},{"1":"Almond_Delight","2":"R","3":"C","4":"110","5":"2","6":"2","7":"200","8":"1.0","9":"14.0","10":"8","11":"NA","12":"25","13":"3","14":"1","15":"0.75","16":"34.38484"},{"1":"Apple_Cinnamon_Cheerios","2":"G","3":"C","4":"110","5":"2","6":"2","7":"180","8":"1.5","9":"10.5","10":"10","11":"70","12":"25","13":"1","14":"1","15":"0.75","16":"29.50954"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
summary(cereal)
```

```
##      name               mfr                type              calories    
##  Length:77          Length:77          Length:77          Min.   : 50.0  
##  Class :character   Class :character   Class :character   1st Qu.:100.0  
##  Mode  :character   Mode  :character   Mode  :character   Median :110.0  
##                                                           Mean   :106.9  
##                                                           3rd Qu.:110.0  
##                                                           Max.   :160.0  
##                                                                          
##     protein           fat            sodium          fiber       
##  Min.   :1.000   Min.   :0.000   Min.   :  0.0   Min.   : 0.000  
##  1st Qu.:2.000   1st Qu.:0.000   1st Qu.:130.0   1st Qu.: 1.000  
##  Median :3.000   Median :1.000   Median :180.0   Median : 2.000  
##  Mean   :2.545   Mean   :1.013   Mean   :159.7   Mean   : 2.152  
##  3rd Qu.:3.000   3rd Qu.:2.000   3rd Qu.:210.0   3rd Qu.: 3.000  
##  Max.   :6.000   Max.   :5.000   Max.   :320.0   Max.   :14.000  
##                                                                  
##      carbo          sugars           potass          vitamins     
##  Min.   : 5.0   Min.   : 0.000   Min.   : 15.00   Min.   :  0.00  
##  1st Qu.:12.0   1st Qu.: 3.000   1st Qu.: 42.50   1st Qu.: 25.00  
##  Median :14.5   Median : 7.000   Median : 90.00   Median : 25.00  
##  Mean   :14.8   Mean   : 7.026   Mean   : 98.67   Mean   : 28.25  
##  3rd Qu.:17.0   3rd Qu.:11.000   3rd Qu.:120.00   3rd Qu.: 25.00  
##  Max.   :23.0   Max.   :15.000   Max.   :330.00   Max.   :100.00  
##  NA's   :1      NA's   :1        NA's   :2                        
##      shelf           weight          cups           rating     
##  Min.   :1.000   Min.   :0.50   Min.   :0.250   Min.   :18.04  
##  1st Qu.:1.000   1st Qu.:1.00   1st Qu.:0.670   1st Qu.:33.17  
##  Median :2.000   Median :1.00   Median :0.750   Median :40.40  
##  Mean   :2.208   Mean   :1.03   Mean   :0.821   Mean   :42.67  
##  3rd Qu.:3.000   3rd Qu.:1.00   3rd Qu.:1.000   3rd Qu.:50.83  
##  Max.   :3.000   Max.   :1.50   Max.   :1.500   Max.   :93.70  
## 
```


``` r
#Grouping cerealys by type and counting the amount of each type
cereal %>%
  group_by(type) %>%
  summarise(count = n())
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["type"],"name":[1],"type":["chr"],"align":["left"]},{"label":["count"],"name":[2],"type":["int"],"align":["right"]}],"data":[{"1":"C","2":"74"},{"1":"H","2":"3"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>
### Figure 1
**Figure 1** is the distribution of cereal by calories. Most cereals fall within the 101–125 calorie range, making it the most common category in the dataset. In contrast, very few cereals contain more than 150 calories or fewer than 75 calories per serving. This suggests that cereal manufacturers tend to produce products with moderate calorie content rather than extremely low or high caloric options.


``` r
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

plot_data <- cereal %>%
  mutate(
    calorie_range = cut(
      calories,
      breaks = c(0, 75, 100, 125, 150, 200),
      labels = c("0-75", "76-100", "101-125", "126-150", "151+")
    )
  ) %>%
  count(calorie_range)

p <- ggplot(
  plot_data,
  aes(
    x = calorie_range,
    y = n,
    fill = calorie_range,
    text = paste(
      "Calorie Range:", calorie_range,
      "<br>Number of Cereals:", n
    )
  )
) +
  geom_col() +
  labs(
    title = "Distribution of Cereal by Calories",
    x = "Calories per Serving",
    y = "Number of Cereals",
    fill = "Calorie Range"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

interactive_plot <- ggplotly(p, tooltip = "text")

interactive_plot
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-33b84d6751e40fe90d4a" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-33b84d6751e40fe90d4a">{"x":{"data":[{"orientation":"v","width":0.89999999999999991,"base":0,"x":[1],"y":[5],"text":"Calorie Range: 0-75 <br>Number of Cereals: 5","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(13,8,135,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"0-75","legendgroup":"0-75","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000013,"base":0,"x":[2],"y":[25],"text":"Calorie Range: 76-100 <br>Number of Cereals: 25","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(126,3,168,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"76-100","legendgroup":"76-100","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[3],"y":[39],"text":"Calorie Range: 101-125 <br>Number of Cereals: 39","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(204,70,120,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"101-125","legendgroup":"101-125","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[4],"y":[7],"text":"Calorie Range: 126-150 <br>Number of Cereals: 7","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,148,65,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"126-150","legendgroup":"126-150","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"v","width":0.90000000000000036,"base":0,"x":[5],"y":[1],"text":"Calorie Range: 151+ <br>Number of Cereals: 1","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(240,249,33,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"151+","legendgroup":"151+","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":51.334163553341632,"r":9.2984640929846396,"b":51.141552511415526,"l":47.422166874221674},"font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279},"title":{"text":"<b> Distribution of Cereal by Calories <\/b>","font":{"color":"rgba(0,0,0,1)","family":"","size":22.316313823163139},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,5.5999999999999996],"tickmode":"array","ticktext":["0-75","76-100","101-125","126-150","151+"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["0-75","76-100","101-125","126-150","151+"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.877542548775427},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"y","title":{"text":"Calories per Serving","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-1.9500000000000002,40.950000000000003],"tickmode":"array","ticktext":["0","10","20","30","40"],"tickvals":[2.2204460492503131e-16,10,20,30.000000000000004,40],"categoryorder":"array","categoryarray":["0","10","20","30","40"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":14.87754254877543},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.8453149175440583,"zeroline":false,"anchor":"x","title":{"text":"Number of Cereals","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.877542548775427},"title":{"text":"Calorie Range","font":{"color":"rgba(0,0,0,1)","family":"","size":18.596928185969279}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"3b303cb698b":{"x":{},"y":{},"fill":{},"text":{},"type":"bar"}},"cur_data":"3b303cb698b","visdat":{"3b303cb698b":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

### Figure 2

**Figure 2** is a boxplot that compares the distribution of calories per serving across cereal manufacturers. Most manufacturers have similar median calorie levels, but manufacturers Q, P, and R exhibit greater variability in calorie content. Several outliers are visible, particularly for manufacturer K, indicating the presence of cereals with unusually high or low-calorie counts. The visualization reveals differences not only in typical calorie levels but also in the consistency of products offered by each manufacturer.


``` r
#Distribution of calories per serving by manufacturer
ggplot(cereal,
       aes(x = reorder(mfr, calories, median),
           y = calories,
           fill = mfr)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Calories per Serving by Manufacturer",
    x = "Manufacturer",
    y = "Distribution of Calories per Serving",
    fill = "Manufacturer"
  ) +
  scale_fill_viridis_d(
    option = "plasma",
    begin = 0.15,
    end = 0.9
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )
```

<img src="Thibodeau_project_01_files/figure-html/unnamed-chunk-5-1.png" alt="Boxplot showing the distribution of cereal calories per serving across manufacturers. Most manufacturers have median calorie values around 100–110 calories per serving. Manufacturer N has the lowest median and the smallest spread, while manufacturers K, P, and R have higher medians with greater variability. Several manufacturers, particularly K and G, have high-calorie outliers reaching up to 160 calories per serving, indicating that although most cereals cluster around 100–120 calories, a few products contain substantially more calories."  />

### Figure 3

**Figure 3** is a scatterplot that examines the relationship between sugar content and calories in cereals. Each point represents a cereal, while the trend line indicates the overall relationship between the two variables. The upward slope of the trend line suggests a positive association, meaning that cereals with higher sugar content tend to contain more calories per serving


``` r
#Sugar vs calories in each cereal scatterplot
ggplot(cereal, aes(x = sugars, y = calories)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  labs(
    title = "Sugar vs Calories",
  )
```

<img src="Thibodeau_project_01_files/figure-html/unnamed-chunk-6-1.png" alt="Scatterplot showing the relationship between sugar content and calories per serving for cereals, with a fitted linear trend line. Each point represents one cereal. The trend line slopes upward, indicating a positive relationship: cereals with more sugar generally contain more calories. Although there is considerable variation, most cereals fall between 90 and 120 calories per serving and contain 5 to 12 grams of sugar, with a few higher-calorie outliers reaching up to 160 calories."  />

### Interpretation

The story I would tell with these plots is to *show how calorie count varies throughout the cereal market*. Most cereals contain a moderate number of calories per serving, but manufacturers differ in the average calorie content of their products. The visuals lead to findings which provide insight into *how nutritional characteristics vary among cereal products and manufacturers*.

I applied multiple principles of design and data visualization throughout this assignment. **Contrast** was used by selecting visualization types that best highlighted different aspects of the data. A bar chart was used to compare the distribution of cereals across calorie ranges, a boxplot emphasized differences in calorie distributions among manufacturers and identified potential outliers, and a scatterplot revealed the relationship between sugar content and calories. **Repetition was** maintained through the consistent use of formatting, labeling, and overall chart structure across all visualizations, creating a cohesive presentation. **Alignment** was carefully considered to ensure that titles, axes, labels, and graphical elements were organized and easy to follow. **Proximity** was used by grouping related information together within each visualization, helping viewers quickly understand the relationships being presented. Additionally, **clear titles, descriptive labels, and simplified formatting** were used to improve readability and reduce visual clutter. 

Together, these design principles help the visualizations emphasize comparisons, reveal patterns in cereal nutrition, and create a cohesive narrative that guides the viewer from understanding calorie distributions to exploring how manufacturers and sugar content contribute to differences among cereals.


