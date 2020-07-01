# Mosaic Plots  
diesel_cars <- select(filter(vehiclemodels,
                               vehiclemodels$Fuel.Type=="Diesel"),
                               'Make','Model','Year','Drive','Cylinders')

# Summary of diesel cars by drivetrain and cylinders
select(diesel_cars, Drive, Cylinders) %>%
  table() %>% 
  reshape2::melt() %>%
  filter(Drive!="",!is.na(Drive)) %>%     
  mutate(
    Drive = case_when(
      Drive %in% c("4-Wheel or All-Wheel Drive",
                   "4-Wheel Drive",
                   "All-Wheel Drive") ~ "4-Wheel / All Wheel Drive",
      Drive == "2-Wheel Drive" ~ "2-Wheel Drive",
      Drive == "Front-Wheel Drive" ~ "Front-Wheel Drive",
      Drive == "Rear-Wheel Drive" ~ "Rear-Wheel Drive",
      Drive == "Part-time 4-Wheel Drive" ~ "Part-time 4-Wheel Drive",
      TRUE ~ as.character(Drive)
    )) %>%
  group_by(Drive,Cylinders) %>% 
  summarize(carcount=sum(value)) %>%
  group_by(Cylinders) %>%
  mutate(group_count = sum(carcount)) -> summ_dieselcars

# Labels for each tile in the mosaic plot
labels_dieselcars <- 
group_by(summ_dieselcars, Cylinders) %>%
filter(carcount != 0) %>%
arrange(desc(Drive)) %>%
mutate(labels_y = (cumsum(carcount) - 0.5*carcount)/group_count)

# Mosaic plot of diesel cars by drivetrain and Cylinders
dieselcars_mosaic <-
ggplot(summ_dieselcars,
       aes(x=factor(Cylinders),
           y=carcount,
           width=group_count,
           fill=Drive)) +
geom_bar(stat = "identity", 
         position = "fill", 
         colour = "white", 
         size = 0.75) +
geom_text(data = labels_dieselcars,
          aes(y = labels_y, 
              label = carcount, 
              color = Drive),
          na.rm = TRUE,
          size = 10/.pt,
          color="black" ) +
facet_grid(~ Cylinders, 
           scales = "free_x", 
           space = "free_x") +
scale_y_continuous(name = NULL,
                   expand = c(0, 0)) +
scale_x_discrete(name = NULL) +
scale_fill_manual(values = mycolorpalgradient[1:5]) +
coord_cartesian(clip = "off") +
ggtitle("Mosaic plot of Diesel cars by drivetrain and Cylinders") +
theme(
    line = element_blank(),
    strip.text = element_blank(),
    axis.ticks.length = unit(0, "pt"),
    axis.ticks.y = element_blank(),
    axis.text.y =element_blank(),
    panel.spacing.x = unit(0, "pt"),
    legend.position="bottom",
    plot.title = element_text(hjust=0.5))