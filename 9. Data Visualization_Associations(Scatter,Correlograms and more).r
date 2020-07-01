# This section covers Scatter plot, Bubble Plot, Correlogram,
# dimension reduction and 
# Dumbbell charts and slopegraph

combinedmpg_df <-
select(vehiclemodels,
       'Fuel.Type1', 'Year','ATV.Type','Cylinders',
       'Combined.Mpg.For.Fuel.Type1',
       'Engine.displacement') %>%
filter(Year==2020,
       ATV.Type %in% c('Plug-in Hybrid','Hybrid',"")) %>%
mutate(VehicleType = factor(case_when(
    ATV.Type =="" ~ "Non-Hybrids",
    ATV.Type =="Plug-in Hybrid" ~ "Plug-in Hybrids",
    ATV.Type =="Hybrid" ~ "Hybrids"),
    levels = c("Non-Hybrids","Plug-in Hybrids","Hybrids"))) %>%
rename(mpg = Combined.Mpg.For.Fuel.Type1,disp =Engine.displacement )

# Scatter Plot
ggplot(combinedmpg_df,
       aes(x=disp,
           y=mpg,
           fill=VehicleType,
           color = VehicleType)) +
geom_point(pch = 21, color = "white",size = 2.5) +
scale_x_continuous(name = "Displacement (in liters)") +
scale_y_continuous(name = "Combined Fuel Economy (mpg)") +
theme(
legend.position = c(0.95, 0.5),
legend.justification = c(1, 0),
legend.spacing.x = unit(2, "pt"),
legend.spacing.y = unit(2, "pt"),
legend.key.width = unit(10, "pt"),
strip.text = element_text(size = 12, margin = margin(2, 0, 2, 0)),
strip.background  = element_rect(
    fill = "grey85", colour = "grey85",
    linetype = 1, size = 0.25),
plot.title = element_text(hjust=0.5)) +
ggtitle("Scatterplot of Fuel Economy of Vehicles vs Displacement")


# Bubble Plot
# Number of Cylinders in vehicle is mapped to Circle Size
vehicles_bubbleplot <-
ggplot(combinedmpg_df,
       aes(x=disp,
           y=mpg,
           fill=VehicleType,
           color = VehicleType,
           size = Cylinders)) +
geom_point(pch = 21, color = "white") +
scale_x_continuous(name = "Displacement (in liters)") +
scale_y_continuous(name = "Combined Fuel Economy (mpg)") +
scale_radius(
name = "Cylinders",
range = c(3, 12),
limits = c(3, 12),
breaks = c(3,4,6,8,10,12),
labels =  c(3,4,6,8,10,12),
guide = guide_legend(
    direction = "horizontal",
    title.position = "top",
    title.hjust = 0.5,
    label.position = "right",
    override.aes = list(fill = "gray40"))) +
theme(
legend.position = c(0.95, 0.5),
legend.justification = c(1, 0),
legend.spacing.x = unit(2, "pt"),
legend.spacing.y = unit(2, "pt"),
legend.key.width = unit(10, "pt"),
strip.text = element_text(size = 12, margin = margin(2, 0, 2, 0)),
strip.background  = element_rect(
    fill = "grey85", colour = "grey85",
    linetype = 1, size = 0.25),
plot.title = element_text(hjust=0.5)) +
ggtitle("Bubble plot of Fuel Economy of Vehicles vs Displacement depicting Cylinders ")

#################################################################################
########################### Correlogram ###########################
#################################################################################
"""
Correlograms can be visualized through a scatterplot, or 
a symbol that represents the correlation(bubble,square,line,number)
"""
# Lets consider the exchange rates dataset to see correlation between currencies

################ Method 1 : Using Hierarchial Clustering

# Step 1 :  Spread the data ; Easy to eliminate NA
exchangerates_df <-
spread(euroexrates,Currency,Rate) %>%
filter(!is.na(AUD),!is.na(CAD),!is.na(GBP),
       !is.na(MXN),!is.na(SEK),!is.na(USD))

# Set Date as row names ; Only currencies left in columns
row.names(exchangerates_df) <- exchangerates_df$Date
exchangerates_df <- exchangerates_df[!names(exchangerates_df)=='Date']

# Step 2 : Correlation Matrix and convert into long form
corrmatrx <- cor(exchangerates_df,method="pearson")
stacked_corrmat <- stack(as.data.frame(corrmatrx))
names(stacked_corrmat) <- c("Coefficient","Curr1")

# Step 3 : Create 2 Columns to make plotting easier
stacked_corrmat <- cbind(stacked_corrmat,
                         Curr2 = rep(names(exchangerates_df)))
                         #length(names(exchangerates_df)))

# Step 4 : Hierarchial clustering 
clust <- hclust(as.dist(1-corrmatrx), method="average") 
levels <- clust$labels[clust$order]

stacked_corrmat$Curr1 <- factor(stacked_corrmat$Curr1, 
                                levels = levels)
stacked_corrmat$Curr2 <- factor(stacked_corrmat$Curr2, 
                                levels = levels)

# Plots the correlogram as a square using geom_tile()
ggplot(filter(stacked_corrmat, 
              as.integer(Curr1) < as.integer(Curr2)),
       aes(Curr1, Curr2, fill=Coefficient)) + 
  geom_tile(color = "white", size = 1) + 
  scale_x_discrete(position = "top", name = NULL, expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0)) +
  coord_fixed() +
   scale_fill_distiller(type = "seq", 
                       palette = 1, 
                       direction = -1,
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("–1.0", "0", "1.0"),
                       name = "Correlation",
                       values = NULL, 
                       space = "Lab", 
                       #na.value = "grey50",
                       guide = "colourbar", 
                       aesthetics = "fill")  +
  ggtitle("Correlogram of Exchange Rates") +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(3, "pt"),
    legend.position = c(.97, .0),
    legend.justification = c(1, 0),
    legend.title.align = 0.5,
    plot.title=element_text(hjust=0.5)
  )


################ Method 2 : Visualized as bubbles using geom_point()
# Visualization of Correlogram using bubble, line and number
ggplot(filter(stacked_corrmat, as.integer(Curr1) < as.integer(Curr2)),
       aes(Curr1, Curr2, fill=Coefficient, size = abs(Coefficient))) +
  geom_point(shape = 21, stroke = 0) + 
  scale_x_discrete(position = "top", 
                   name = NULL, 
                   expand = c(0, 0.5)) +
  scale_y_discrete(name = NULL, 
                   expand = c(0, 0.5)) +
  scale_size_area(limits = c(-1,1),
                  max_size = 20, 
                  guide = "none") +
  coord_fixed() +
  scale_fill_distiller(type = "div", 
                       palette = 8, 
                       direction = -1,
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("–1.0", "0", "1.0"),
                       name = "Correlation",
                       values = NULL, 
                       space = "Lab", 
                       #na.value = "grey50",
                       guide = "colourbar", 
                       aesthetics = "fill") + 
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = grid::unit(3, "pt"),
    legend.position = c(.97, .0),
    legend.justification = c(1, 0),
    legend.title.align = 0.5
  )

################ Method 3 : Visualized as squares Using ggcorr()
# Plots the correlogram as a square using ggcorr()
ggcorr(corrmatrx,method = c("everything", "pearson")) + 
  scale_fill_distiller(type = "seq", 
                       palette = 1, 
                       direction = -1,
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1),
                       labels = c("–1.0", "0", "1.0"),
                       name = "Correlation",
                       values = NULL, 
                       space = "Lab", 
                       #na.value = "grey50",
                       guide = "colourbar", 
                       aesthetics = "fill")

################ Method 4 : Visualized as Scatterplots using ggpairs()
# Check correlations (as scatterplots) with corrleation coefficient 
ggpairs(as.data.frame(corrmatrx), 
        title="Correlogram with ggpairs()") +
theme(plot.title=element_text(hjust=0.5))

# Dumbbell Charts

    y2009models <-
    select(vehiclemodels,
         'Make','Model','Fuel.Type',
         'Drive','Cylinders','Year',
         'Combined.Mpg.For.Fuel.Type1') %>%
    filter(Year==1990,
        !is.na(Year)) %>%
    rename(y2009 = Year,
        mpg_y2009=Combined.Mpg.For.Fuel.Type1) %>%
    unite(y2009model,Make,Model,Fuel.Type,Drive,Cylinders)

    select(filter(vehiclemodels,
                Year==2019,
                !is.na(Year)),
         'Make','Model','Fuel.Type',
         'Drive','Cylinders','Year',
         'Combined.Mpg.For.Fuel.Type1') %>%
    unite(y2019model,Make,Model,Fuel.Type,Drive,Cylinders) %>%
    left_join(y2009models,
            by=c("y2019model"="y2009model")) %>%
    filter(!is.na(y2009)) %>%
    unique()

# create dumbbell plot
ggplot(plotdata_wide, 
       aes(y = reorder(country, y1952),
           x = y1952,
           xend = y2007)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red") +
  theme_minimal() + 
  labs(title = "Change in Life Expectancy",
       subtitle = "1952 to 2007",
       x = "Life Expectancy (years)",
       y = "")

# Slopegraph
"""
Lets consider the airlinedelay dataset to check reason behind delay
over the years
"""
airlinedelay_df <-
airlinedelay %>%
filter(year>2014,
       !is.na(X.arr_delay),!is.na(X.carrier_delay),
       !is.na(weather_delay),!is.na(nas_delay),
       !is.na(security_delay),!is.na(late_aircraft_delay)
       ) %>%
select(year,X.arr_delay,X.carrier_delay,
       weather_delay,nas_delay,
       security_delay,late_aircraft_delay) %>%
group_by(year) %>%
summarize(totaldelay = sum(X.arr_delay),
          totalcarrier = sum(X.carrier_delay),
          totalweather = sum(weather_delay),
          totalnas = sum(nas_delay),
          totalsecurity = sum(security_delay),
          totallateaircraft = sum(late_aircraft_delay)) %>%
mutate(carrierpct = totalcarrier/totaldelay,
       weatherpct = totalweather/totaldelay,
       naspct = totalnas/totaldelay,
       securitypct = totalsecurity/totaldelay,
       lateaircraftpct = totallateaircraft/totaldelay) %>%
select(year,carrierpct,weatherpct,naspct,securitypct,lateaircraftpct) %>%
gather("delay","percentage",2:6)

labels_df <- filter(airlinedelay_df, 
                    year == 2019) %>%
             left_join(
                tibble(
                delaytype = c("Air Carrier\nDelay",
                            "Extreme\nWeather",
                            "National Aviation\nSystem Delay",
                            "Security Delay", 
                            "Aircraft\nArriving Late"),
                delay = c("carrierpct","weatherpct",
                          "naspct","securitypct",
                          "lateaircraftpct"))) %>%
                rename(label_y=percentage)

ggplot(airlinedelay_df,aes(x=year,y=percentage)) +
geom_line(aes(group = delay), color = "gray60") +
geom_point(color = "white", size = 4) +
geom_point(color = "#0072B2", size = 2) +
geom_text(data = labels_df,
          aes(x = year + 0.1, 
              y = label_y , 
              label = delaytype),
          size = 10/.pt,
          hjust = 0) +
scale_x_continuous(limits = c(2015, 2020), 
                   breaks = seq(2015,2020,1),
                   labels = c("2015","2016","2017","2018","2019",""),
                   expand = expand_scale(add = c(0.8, 0)),
                   name = NULL,
                   position = "top") +
scale_y_continuous(expand = c(0, 0.05),
                   name = "Percentage of total delay minutes") +
coord_cartesian(clip = "off") 


# Dimension Reduction
