                            
################################################################################################
################################ MULTIVARIATE PLOTS : Proportions ##############################
################################################################################################

"""
Consider Diesel cars manufactured from 1984 till date, lets create multivariate 
faceted plots for this distribution. Before diving into Multivariate plot for this 
distribution, lets evaluate univariate plot
"""
# Diesel cars from 1984-2020
dieselcars <- select(filter(vehiclemodels,
                               vehiclemodels$Fuel.Type=="Diesel",
                               !is.na(Drive),Drive!=""),
                               'Make','Model','Year','Drive','Cylinders') 

dieselcars_n <-  dieselcars  %>%  count(Drive)
                 
# Case 1: Univariate plot of Distribution of diesel cars by Drivetrain (by number)
dieselcarsplot_n <-
ggplot(dieselcars_n, 
       aes(x = factor(Drive),
           y = n, 
           fill = Drive)) + 
  geom_col() + 
  geom_text(aes(label = n), 
            size = small_size, 
            hjust = -0.5, 
            color = "black") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(limits=c(0,400),expand=c(0,0)) +
  scale_fill_manual(values = mycolortriplet[c(3,6,9,12,15,18,21,24)], 
                    guide = "none") + 
  coord_cartesian(clip = "off") +
  coord_flip() +
  ggtitle('Diesel cars distribution by Drivetrain\n(visualized as side-by-side bars)')
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()) 

# Case 2: Univariate plot of Distribution of diesel cars by Drivetrain (by percent)
dieselcars_pct <- dieselcars  %>%            
                      group_by(Drive) %>%
                      summarize(n = n()) %>% 
                      mutate(pct = n/sum(n),
                             lbl = scales::percent(pct))

dieselcarsplot_pct <-
ggplot(dieselcars_pct,
       aes(x=factor(Drive,levels=rev(dieselcars_pct$Drive)),
           y=pct,
           fill=Drive)) +
    geom_col() +
    geom_text(aes(label = lbl), 
              size = small_size, 
              hjust = -0.5, 
              color = "black") +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(limits=c(0,0.5),expand=c(0,0)) +
    scale_fill_manual(values = mycolortriplet[c(3,6,9,12,15,18,21,24)], 
                    guide = "none") + 
    coord_cartesian(clip = "off") +
    coord_flip() +
    ggtitle('Diesel cars distribution by Drivetrain\n(as percentage)') +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank()) 

# Multivariate plot of Distribution of diesel cars by Drivetrain and cylinders                             
# Faceting Side-by-Side bars

dieselcars <- select(filter(vehiclemodels,
                               vehiclemodels$Fuel.Type=="Diesel",
                               !is.na(Drive),Drive!=""),
                               'Make','Model','Year','Drive','Cylinders') 

quartz.options(width=10, height=5)
dieselcars_cyl <- dieselcars %>% 
                  count(Drive,Cylinders)

dieselcars_bars <- 
ggplot(dieselcars_cyl, 
       aes(x = Drive, 
           y = n,
           fill = Drive)) + 
  geom_col() + 
  facet_grid(~Cylinders) +
  geom_text(aes(label = n), 
            size = xsmall_size, 
            hjust = -0.5, 
            color = "black") +
  theme(strip.background = element_blank(),
        legend.position="none",
        plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(name="Number of Cars",limits=c(0,250),breaks=c(0,100,200)) +
  scale_x_discrete(labels=unique(dieselcars_cyl$Drive)) +
  scale_fill_manual(values=mycolortriplet[c(3,6,9,12,15,18,21,24)])+
  coord_flip() +
  ggtitle("Distribution Diesel Cars by Drivetrain and Cylinders") 

"""
Now, lets create multivariate stacked bars for the dieselcars dataset.
"""
# Stacked Bars
dieselcars_stacked <- dieselcars  %>%            
                         group_by(Drive,Cylinders) %>%
                         summarize(n = n()) %>% 
                         mutate(pct = n/sum(n),
                                lbl = scales::percent(pct),
                                label_y =1- (cumsum(pct) - 0.5*pct))

plot_dieselcars_stacked <-
ggplot(dieselcars_stacked, 
       aes(x = Drive, 
           y = pct,
           fill = factor(Cylinders))) + 
  geom_col(position = "stack", color = "white") + 
  geom_text(aes(x = Drive, 
                y = label_y,
                label = lbl), 
            size=xsmall_size, 
            color = c("black")) +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(name="Percent",limits=c(0,1),expand=c(0,0)) +
  scale_x_discrete(name="Drivetrain",labels=unique(dieselcars_stacked$Drive)) +
  scale_fill_manual(name="Cylinders",
                    values=mycolortriplet[c(6,9,12,18,24)])+
  ggtitle("Distribution Diesel Cars by Drivetrain and Cylinders") 


# Stacked Densities
# Lets consider mpg data of regular fuel cars
"""
Although stacked density plots give an idea of distribution, its hard to gauge 
the proportion of the specific drivetrain as a part of whole
Eg. 2 Wheel Drive cars 
"""

df_regular <- select(filter(vehiclemodels,
                            Fuel.Type=="Regular",
                            Drive!="",!is.na(Drive)),
                    'Drive','City.Mpg.For.Fuel.Type1') 

# Stacked density plot of Regular fuel cars by drivetrain.
 ggplot(df_regular,
       aes(x=df_regular$City.Mpg.For.Fuel.Type1,
           y=..count..,
           color=df_regular$Drive,
           fill=df_regular$Drive)) +
  geom_density(position="fill")  +
  scale_x_continuous(name = "City mpg", expand = c(0, 0)) +
  scale_y_continuous(
    expand = c(0, 0), name = "relative proportion",
    labels = scales::percent) + 
  scale_fill_manual(values=mycolortriplet[c(3,6,9,12,15,18,21)],
                    name = "Drivetrain") +
  scale_color_manual(values=mycolortriplet[c(3,6,9,12,15,18,21)],
                    name = "Drivetrain") +
  coord_cartesian(clip = "off") +
  ggtitle("Stacked Kernel density estimate of \n regular fuel vehicle models by drivetrain(1984-Present)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="right")

# Partial Density plots 
# Limitation : Hard to gauge the proportion relative to other drive trains
partial_density_drivetrain<-
ggplot(df_regular, 
       aes(x = City.Mpg.For.Fuel.Type1, 
           y = ..count..)) +
  geom_density_line(data = select(df_regular, -Drive), 
                    aes(fill = "All drivetrain types"), 
                    color = "transparent") +
  geom_density_line(aes(fill = "highlighted group"), 
                    color = "transparent") +
  facet_wrap(~Drive, nrow = 1) +
  scale_x_continuous(name = "City mileage(mpg)", 
                     limits = c(0, 60), 
                     expand = c(0, 0)) +
  scale_y_continuous(name = "count", 
                     expand = c(0, 0)) +
  scale_fill_manual(
    values = c("#b3b3b3a0", "steelblue"),
    name = NULL,
    guide = guide_legend(direction = "horizontal")
  ) +
  coord_cartesian(clip = "off") +
  theme(
    axis.line.x = element_blank(),
    strip.text = element_text(size = 8, margin = margin(0, 0, 0.2, 0, "cm")),
    legend.position = "bottom",
    legend.justification = "right",
    legend.margin = margin(4.5, 0, 1.5, 0, "pt"),
    legend.spacing.x = grid::unit(4.5, "pt"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.box.spacing = grid::unit(0, "cm"))

# Relative Density plots
"""
From thr previous plot, we can see that the most common drivetrains are
4-Wheel or All-Wheel Drive, Front-Wheel Drive and Rear wheel drive.
Lets evaluate these by relative density plots
"""

df_relativedrivetrain <-
rbind(
  mutate(df_regular,
         drivetrain = as.character(fct_collapse(df_selecteddrivetrain$Drive, 
                                           `Front-wheel Drive` = "Front-Wheel Drive", 
                                           aother = c("Rear-Wheel Drive","4-Wheel or All-Wheel Drive","4-Wheel Drive","All-Wheel Drive","Part-time 4-Wheel Drive","2-Wheel Drive"))),
         highlight = "Front-Wheel Drive"),
  mutate(df_regular,
         drivetrain = as.character(fct_collapse(df_selecteddrivetrain$Drive, 
                                           `Rear-Wheel Drive` = "Rear-Wheel Drive", 
                                           aother = c("Front-Wheel Drive","4-Wheel or All-Wheel Drive","4-Wheel Drive","All-Wheel Drive","Part-time 4-Wheel Drive","2-Wheel Drive"))),
         highlight = "Rear-Wheel Drive"),
  mutate(df_regular,
         drivetrain = as.character(fct_collapse(df_selecteddrivetrain$Drive, 
                                           `Part-time Four-Wheel Drive` = "Part-time 4-Wheel Drive", 
                                           aother = c("Front-Wheel Drive","Rear-Wheel Drive","4-Wheel or All-Wheel Drive","4-Wheel Drive","All-Wheel Drive","2-Wheel Drive"))),
         highlight = "Part-time Four-Wheel Drive"),
  mutate(df_regular,
         drivetrain = as.character(fct_collapse(df_selecteddrivetrain$Drive, 
                                           `Two-Wheel Drive` = "2-Wheel Drive", 
                                           aother = c("Front-Wheel Drive","Rear-Wheel Drive","4-Wheel or All-Wheel Drive","4-Wheel Drive","All-Wheel Drive","Part-time 4-Wheel Drive"))),
         highlight = "Two-Wheel Drive"),
  mutate(df_regular,
         drivetrain = as.character(fct_collapse(df_selecteddrivetrain$Drive, 
                                           `Four-Wheel or All-Wheel Drive` = c("4-Wheel or All-Wheel Drive","4-Wheel Drive","All-Wheel Drive"), 
                                           aother = c("Front-Wheel Drive","Rear-Wheel Drive","Part-time 4-Wheel Drive","2-Wheel Drive"))),
         highlight = "Four-Wheel or All-Wheel Drive")) %>%
  mutate(
    highlight = factor(highlight, 
                       levels = c("Front-Wheel Drive",
                                  "Rear-Wheel Drive",
                                  "Four-Wheel or All-Wheel Drive",
                                  "Part-time Four-Wheel Drive",
                                  "Two-Wheel Drive"))) 

quartz.options(width=10,height=5)
relative_density_drivetrain<-
ggplot(df_relativedrivetrain, 
    aes(City.Mpg.For.Fuel.Type1)) +
annotate(geom = "rect", 
        xmin = -Inf, 
        xmax = Inf, 
        ymin = -Inf, 
        ymax = Inf, 
        fill = "grey70", 
        color = NA) +
geom_density_line(
    aes(y =stat(count), 
        fill = drivetrain), 
        color = "transparent",
        position = "fill") +
facet_wrap(~highlight,nrow=1) +
scale_x_continuous(name = "City Mileage(mpg)",
                    limits = c(7, 60),
                    expand = c(0, 0)) +
scale_y_continuous(name = "relative proportion", 
                    labels = scales::percent,
                    expand = c(0, 0)) +
scale_fill_manual(
    values = c("transparent",rep("steelblue",times=5)),
    name = NULL,
    breaks = c("aother","Four-Wheel or All-Wheel Drive"),
    labels = c("All vehicle models(1984-2020)","highlighted group"),
    guide = guide_legend(
                        direction = "horizontal",
                        override.aes = list(fill = c("#bebebe","steelblue")))) +
coord_cartesian(clip = "off") +
theme(
    axis.line.x = element_blank(),
    strip.text = element_text(size = 8, 
                            margin = margin(0, 0, 0.2, 0, "cm")),
    legend.position = "bottom",
    legend.justification = "right",
    legend.margin = margin(4.5, 0, 1.5, 0, "pt"),
    legend.spacing.x = grid::unit(4.5, "pt"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.box.spacing = grid::unit(0, "cm"))    