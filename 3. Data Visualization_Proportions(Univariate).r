"""
This file includes Univariate plots
Univariate graphs plot the distribution of data from a single 
variable. The variable can be categorical (e.g., race, sex) 
or quantitative (e.g., age, weight).
The plots covered here are
>> Pie Chart
>> Horizontal Bars
>> Vertical Bars
>> Side-by-Side Bars
"""

# Visualization of Distribution of electric cars by drivetrain visualized
################################ Pie Chart ################################
vehicles_allelectric <- select(filter(vehiclemodels,
                               vehiclemodels$Fuel.Type=="Electricity",
                               !is.na(Drive),Drive!=""),
                               'Make','Model','Year','Drive')

vehicles_pie <- vehicles_allelectric %>%
                    count(Drive) %>%
                    mutate(
                            end_angle = 2*pi*cumsum(n)/nrow(vehicles_allelectric),   # ending angle for each pie slice
                            start_angle = lag(end_angle,default = 0),   # starting angle for each pie slice
                            mid_angle = 0.5*(start_angle + end_angle),
                            hjust = ifelse(mid_angle>pi, 1, 0),
                            vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))   # middle of each pie slice, for the text label
rpie = 1
rlabel = 1.05 * rpie

electriccars_pie <- ggplot(vehicles_pie) + theme_void() +
  geom_arc_bar(
    aes(
      x0 = 0, 
      y0 = 0, 
      r0 = 0, 
      r = rpie,
      start = start_angle, 
      end = end_angle, 
      fill = vehicles_pie$Drive),color ="white",size = 0.5,alpha=0.8) +
  geom_text(
    aes(
      x = rlabel*sin(mid_angle),
      y = rlabel*cos(mid_angle),
      label = c("2WD","4WD","AWD","FWD","RWD"),
      hjust=hjust,
      vjust=vjust,
)) +
  geom_text(
    aes(
      x = 0.6*sin(mid_angle),
      y = 0.6*cos(mid_angle),
      label = vehicles_pie$n),
    color = "black",
    size=small_size) +
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
  scale_fill_manual(values = rev(mycolorpalgradient[1:5])) +
  theme(
  legend.position = "none",
  plot.margin = margin(3.5, 1.5, 3.5, 1.5))


################################ Stacked Bars ################################
# (Vertical and horizontal in a plot grid)
electriccars <- vehicles_allelectric %>%
                    count(Drive) %>%
                    mutate(label_y = cumsum(n) - n/2)

# Plots the vertical stacked bar
ec_bars_stacked_base <-
ggplot(electriccars,
       aes(x = 1,
           y = n, 
           fill = factor(Drive, levels = rev(Drive)))) + 
  geom_col(position = "stack", color = "white",alpha=0.8) + 
  geom_text(
    aes(x = 1., 
        y = label_y, 
        label = n), 
    size=small_size, 
    color = c("black")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0),breaks = NULL, name = NULL) +
  scale_fill_manual(values = mycolorpalgradient[1:5], guide = "none") 
# mycolortriplet[c(3,6,12,15,18)]
# Adds the labels to the vertical stacked bar
ec_bars_yax <- 
axis_canvas(ec_bars_stacked_base, 
            axis = "y") +
  geom_text(
    data = electriccars,
    aes(x = 0.04, 
        y = label_y, 
        label = c("2WD","4WD","AWD","FWD","RWD")),
        hjust = 0, 
        vjust = 0.5, 
        size = small_size) +
  scale_x_continuous(limits = c(0, 1))

# Setting the plot, grob and width of vertical stacked bar
ec_bars_stacked <- insert_yaxis_grob(
  ec_bars_stacked_base + 
    theme(
      axis.ticks = element_line(color = "gray70"),
      plot.margin = margin(7, 1.5, 7, 1.5)),
    ec_bars_yax, grid::unit(0.9, "null"))

# Adds labels to the horizontal stacked bar
ec_bars_xax <- 
axis_canvas(ec_bars_stacked_base,axis = "y") +
  geom_text(
    data = electriccars,
    aes(x = 0., 
        y = label_y, 
        label = c("2WD","4WD","AWD","FWD","RWD"), 
        hjust = 0.5, vjust = 0),
        size=small_size) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  coord_flip()

# Setting the plot, grob and width of horizontal stacked bar
ec_bars_hstacked <- insert_xaxis_grob(
  ec_bars_stacked_base + coord_flip() + 
    scale_y_continuous(expand = c(0, 0), position = "right") +
    theme(
      axis.ticks = element_line(color = "gray70"),
      plot.margin = margin(3, 1.5, 3, 3)
    ),
  ec_bars_xax, grid::unit(10, "pt"), position = "bottom")

# Plots the nested grid with a pie chart, horizontal and vertical stacked bars
electriccars_dist <- plot_grid(
  ec_bars_stacked,
  plot_grid(
    ec_bars_hstacked, electriccars_pie,
    ncol = 1, 
    rel_heights = c(6,9)),
  rel_widths = c(5,9))

################################ Side-by-Side bars ################################
# Univariate Side-by-side bar visualization of electric cars by drivetrain
ec_bars <- 
ggplot(electriccars, 
       aes(x = factor(Drive, levels = electriccars$Drive), 
           y = n, 
           fill = Drive)) + 
  geom_col(alpha=0.9,color=mycolorpalgradient[c(3)]) + 
  geom_text(aes(label = n), 
            size = small_size, 
            vjust = 2, 
            color = "black") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = mycolorpalgradient[1:5], 
                    guide = "none") + 
  coord_cartesian(clip = "off") +
  ggtitle('Electric cars distribution by Drivetrain\n(visualized as side-by-side bars)')
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()) 
