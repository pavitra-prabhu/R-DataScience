# Lets consider the alternative fuels dataset to check 
afstations_pies_df <- 
    select(afstations,fuel_type_code, st_prv_code) %>%
    table() %>%
    reshape2::melt() %>%
    left_join(US_states,by=c('st_prv_code'='State.Code')) %>%
    group_by(fuel_type_code,Region.Name) %>%
    summarize(station_count = sum(value)) %>%
    filter(!is.na(Region.Name),station_count>0) 

# Total Count of alternative fueling stations
total_stationcount <- sum(afstations_df$station_count)

# Labels for alternative fuel types
labels_pie <- select(afstations_pies_df,
                    'fuel_type_code','station_count') %>% 
              group_by(fuel_type_code) %>%
              summarize(fuelstations = sum(station_count)) %>%
              arrange(-as.numeric(fuel_type_code)) %>%
    mutate( 
        cumsumcnt = cumsum(fuelstations),
        end_angle = 2*pi*cumsum(fuelstations)/total_stationcount, 
        start_angle = lag(end_angle, default = 0),   
        mid_angle = 0.5*(start_angle + end_angle),   
        hjust = ifelse(mid_angle>pi, 1, 0),
        vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1)) %>%
    mutate (
        fueltype = case_when(
            fuel_type_code == "BD" ~ "Biodiesel",
            fuel_type_code == "LNG" ~ "Liquified Natural Gas",
            fuel_type_code == "LPG" ~ "Propane",
            fuel_type_code == "HY" ~ "Hydrogen",
            fuel_type_code == "CNG" ~ "CNG",
            fuel_type_code == "E85" ~ "Ethanol",
            fuel_type_code == "ELEC" ~ "Electric"))

# Radius
rpie = 0.8
rlabel = 1.05 * rpie

# Pie Charts
pieplot <- 
ggplot(labels_pie) +
theme_void() +
geom_arc_bar(
    data =labels_pie,
    aes(
        x0 = 0, 
        y0 = 0, 
        r0 = 0, 
        r = rpie,
        start = start_angle, 
        end = end_angle, 
        fill = fueltype),
      color ="white",
      size = 0.3) +
geom_text_repel(
    data =labels_pie,
    aes(
        x = rlabel*sin(mid_angle),
        y = rlabel*cos(mid_angle),
        label = paste(fueltype,'\n','(',fuelstations,')',sep=""),
        hjust = hjust,
        vjust = (1.2+vjust)), 
        nudge_y = 0.1,
        size = 3) +    
coord_fixed(clip = "off") +
scale_x_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_y_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_fill_manual(values = rev(mycolortriplet[c(3,6,9,12,18,21,24)])) +
theme(legend.position="none") + 
ggtitle("Breakdown of alternative fueling station by number of stations")


# Doughnut Charts
rpie1 <- 0.7
rpie2 <- 1
rlabel <- 1.02 * rpie

doughnut_plot <-
ggplot(labels_pie) +
theme_void() +
geom_arc_bar(
    aes(
        x0 = 0, 
        y0 = 0, 
        r0 = rpie1, 
        r = rpie2,
        start = start_angle, 
        end = end_angle, 
        fill = fuel_type_code),
      color ="white",
      size = 0.2) +
geom_text_repel(
    data =labels_pie,
    aes(
        x = rlabel*sin(mid_angle),
        y = rlabel*cos(mid_angle),
        label = paste(fueltype,'\n','(',fuelstations,')',sep=""),
        hjust = hjust,
        vjust = (1.2+vjust)), 
        nudge_y = 0.1,
        size = 3) +   
coord_fixed(clip = "off") +
scale_x_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_y_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_fill_manual(values = rev(mycolortriplet[c(3,6,9,12,18,21,24)])) +
theme(plot.margin = margin(3.5, 1.5, 3.5, 1.5),
      legend.position="none") +
annotate(geom = 'text', 
         x = 0, 
         y = 0, 
         label ="Alternative Fuel stations")



# Sunburst Plot
sunburst_df <- afstations_pies_df %>%
               arrange(-as.numeric(fuel_type_code)) %>%
               ungroup() %>%
    mutate( 
        cumsumcnt = cumsum(station_count),
        end_angle = 2*pi*cumsum(station_count)/total_stationcount, 
        start_angle = lag(end_angle, default = 0),   
        mid_angle = 0.5*(start_angle + end_angle),   
        hjust = ifelse(mid_angle>pi, 1, 0),
        vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1)) 

rpie1 <- 0.4
rpie2 <- 0.7         
rpie3 <- 1

sunburst_plot <-
ggplot() + 
theme_void() +
geom_arc_bar(data=labels_pie,
    aes(
        x0 = 0, 
        y0 = 0, 
        r0 = rpie1, 
        r = rpie2,
        start = start_angle, 
        end = end_angle, 
        fill = fuel_type_code),
      color ="white",
      size = 0.2) +
geom_arc_bar(data=sunburst_df,
    aes(
        x0 = 0, 
        y0 = 0, 
        r0 = rpie2, 
        r = rpie3,
        start = start_angle, 
        end = end_angle, 
        fill = fuel_type_code),
      color ="white",
      alpha = 0.8,
      size = 0.2) +
geom_text_repel(
    data =labels_pie,
    aes(
        x = rlabel*sin(mid_angle),
        y = rlabel*cos(mid_angle),
        label = paste(fueltype,'\n','(',fuelstations,')',sep=""),
        hjust = hjust,
        vjust = (1.2+vjust)), 
        nudge_y = 0.1,
        size = 3) +   
coord_fixed(clip = "off") +
scale_x_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_y_continuous(
    limits = c(-1.2, 1.2),
    expand = c(0, 0),
    name = "",
    breaks = NULL,
    labels = NULL) +
scale_fill_manual(values = rev(mycolortriplet[c(3,6,9,12,18,21,24)])) +
theme(plot.margin = margin(3.5, 1.5, 3.5, 1.5),
      legend.position="none") +
annotate(geom = 'text', 
         x = 0, 
         y = 0, 
         label ="Alternative \n Fuel stations \n by Type & Region ")