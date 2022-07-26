# Visualizations

## Social expenditure comparison among regions (Latin America, Easter Europe and East Asia)

I will plot the historical development of average social expenditure in percentage of GDP by region, particularly in Latin America, Easter Europe and East Asia. The goal is to compare how these regions have promoted public spending on social policies over the years.

Loading packpages
```
pacman::p_load(tidyverse, dplyr, tidyr, ggplot2, ggpubr, gghighlight)
```

Loading data set

```
load("data_geral.RData")
```

Creating a new variable called *region* in order to distinguish each country by region. Those are ISO-3 country codes.


```
data_geral <- data_geral %>% 
  mutate(region = case_when(ccode %in% c("CHN", "SGP", "HKG",
                            "JPN", "KOR","IDN",
                            "MYS", "PHL", "THA",
                            "VNM") ~ "Leste Asiático", # East Asia
                            ccode %in% c("CZE", "POL",
                            "SVK", "SVN", "LVA", "LTU", "MYS", "EST", "HUN") ~ "Europa Oriental", # Eastern Europe
                             ccode %in% c("ARG", "BRA", "BOL", "CHL", "COL",
                            "CRI", "ECU", "PER", "HND",
                            "SLV", "MEX", "DOM", "URY", "VEN")
                            ~ "América Latina")) # Latin America
```

Selecting variables and then calculating the average social spending grouped by region.


```
g <- data_geral %>% 
  select(year, ccode, region, gasto_social)

g <- g %>% 
  group_by(year, region) %>% 
  mutate(gasto_m = mean(gasto_social, na.rm = TRUE))

#gasto_social = social expedinture (% GDP)
```

Filtering years between 1995 and 2015. More data is available for this time frame.


```
g1 <- g %>%
 filter(year >= 1995L & year <= 2015L)

```

Loading new theme for using in the chart. I used the theme developed by Claus O. Wilke and used in his book *Fundamentals of Data Visualization*. Wilke shared his codes [here](https://github.com/clauswilke/dataviz).


```
library(pkgsimon)
library(tidytext)

```

 Plotting with ``ggplot``, ``gghighlight`` and ``pkgsimon``. ``gghighlight`` has a lot of useful functions, I used it in order to directly label the region names at the end of the lines. It could have done through other packpages, like ``directlabels``, ``ggrepel`` or ``ggtext``.


```
g1 %>% 
ggplot() +
 aes(x = year, y = gasto_m, fill = region, colour = region) +
 geom_line(size = 1) +
 scale_fill_hue(direction = 1) +
 scale_color_hue(direction = 1) +
 theme_dviz_half_open()+
  scale_y_continuous(labels = function(x) paste0(x, "%"), #using % for Y labels
                     limits = c(0,26)) +
  xlim(1995, 2017) +
  xlab("") +
  ylab("Gasto social (% PIB)") +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"), #choosing colors manually
                     name = NULL) +
  scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                    name = NULL) +
 gghighlight((gasto_m) > 0, use_direct_label = T)  +
  theme(axis.line.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_text(margin = margin(0, 0, 0, 0)),
        plot.margin = margin(14, 7, 3, 1.5)) 
```
 
 
 
