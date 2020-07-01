library(ggplot2)

########################## Single Distribution ############################

# Histogram using geom_histogram()
ggplot(vehiclemodels, aes(x = vehiclemodels$Year)) + 
  geom_histogram(breaks=seq(1980,2020,by=1),col="#56B4E9",fill="#0072B2")  + 
  labs(x="Years (1984-Present) ",y="Count") +
  ggtitle("Histogram of all the vehicle models (1984-Present)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

# Kernel density estimaction using geom_density()
  ggplot(vehiclemodels, aes(x = vehiclemodels$Year)) + 
  geom_density(fill="#56B4E950",alpha=0.3,adjust=1/2) +
  labs(x="Years (1984-Present) ",y="Count") +
  ggtitle("Kernel density estimate of all the vehicle models (1984-Present)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

########################## Multiple Distributions ############################
"""
 Density plots work well for multiple distributions as they are somewhat 
 distinct and contiguous ; 
 For instance, we can visualize the distribution of vehicle models over the years
 based on the type of drivetrain ;
"""
# Kernel density estimation for multiple distributions
# Making a subset of the original dataset
vehiclemodels_drivetrain <- select(vehiclemodels,'Year','Make','Model','Drive')
# Creating a table with drivetrain counts
data.frame(
YearMfg = seq(1981,2020,by=1),
fwd = hist(filter(vehiclemodels_drivetrain,Drive=="Front-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
rwd = hist(filter(vehiclemodels_drivetrain,Drive=="Rear-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
awd = hist(filter(vehiclemodels_drivetrain,Drive=="All-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
fourwd = hist(filter(vehiclemodels_drivetrain,Drive=="4-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
twowd = hist(filter(vehiclemodels_drivetrain,Drive=="2-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
part4wd = hist(filter(vehiclemodels_drivetrain,Drive=="Part-time 4-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
all4wd = hist(filter(vehiclemodels_drivetrain,Drive=="4-Wheel or All-Wheel Drive")$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts,
alldrivetrain = hist(vehiclemodels_drivetrain$Year,breaks=seq(1980,2020,by=1),plot=FALSE)$counts
) %>% 
gather(drivetrain, count,-YearMfg) -> drivetrain_count

# Set the levels for the column drivetrain
drivetrain_count$drivetrain <- factor(drivetrain_count$drivetrain , 
                                      levels = c("fwd", "rwd","awd","fourwd","twowd","part4wd","all4wd","alldrivetrain"))

# Plots the density plots  distinct and contiguous
ggplot(drivetrain_count,aes(x = drivetrain_count$YearMfg,y=drivetrain_count$count,color=drivetrain_count$drivetrain,fill=drivetrain_count$drivetrain)) + 
  geom_density(stat = "identity",alpha=0.2)  +
  scale_x_continuous(limits = c(1980, 2021), name = "Years (1984-Present)", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1050), name = "Number of vehicle models", expand = c(0, 0)) +
  scale_fill_manual(values=mycolortriplet[c(3,6,9,12,15,18,21,24)],
                    breaks=c("fwd", "rwd","awd","fourwd","twowd","part4wd","all4wd","alldrivetrain"),
                    labels = c("Front-Wheel Drive","Rear-Wheel Drive","All-Wheel Drive","4-Wheel Drive","2-Wheel Drive","Part-time 4-Wheel Drive","4-Wheel or All-Wheel Drive","All Drivetrain types"),
                    name = "Drivetrain") +
  scale_color_manual(values=mycolortriplet[c(3,6,9,12,15,18,21,24)],
                     breaks=c("fwd", "rwd","awd","fourwd","twowd","part4wd","all4wd","alldrivetrain"),
                     labels = c("Front-Wheel Drive","Rear-Wheel Drive","All-Wheel Drive","4-Wheel Drive","2-Wheel Drive","Part-time 4-Wheel Drive","4-Wheel or All-Wheel Drive","All Drivetrain types"),
                     name="Drivetrain") +
  coord_cartesian(clip = "off") +
  ggtitle("Kernel density estimate of all the vehicle models (1984-Present)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") 

# Visualizing distributions along the vertical axis

# Visualizing distributions along the horizontal axis

################ Empirical cumulative distribution functions  ###################
"""Other examples are VAR and student grades"""
# Evaluating the ecdf of Ford-81, Toyota-55, Chevrolet-50 and Honda-37 for the year 2018
# Dataset : City mileage of Ford models for the year 2018

regularvehiclemodels <- select(filter(vehiclemodels,
                              !is.na(vehiclemodels$City.Mpg.For.Fuel.Type1),
                              vehiclemodels$Fuel.Type=="Regular",
                              vehiclemodels$Make=="Ford",
                              vehiclemodels$Year==2018),
                              'Year','Make','Model','City.Mpg.For.Fuel.Type1')

vehicle_data <- data.frame(regularvehiclemodels,
                           rank = rank(regularvehiclemodels$City.Mpg.For.Fuel.Type1,ties.method = "random"))

# Empirical cumulative distribution function in ascending order
# ecdf using geom_point() and stat_ecdf()
ggplot(vehicle_data, aes(x = regularvehiclemodels$City.Mpg.For.Fuel.Type1, 
                         y = 81*..y..)) + 
  stat_ecdf(geom = "step", color = "#0072B2") +
  geom_point(aes(y = rank), color = "#0072B2") +
  scale_x_continuous(limits = c(10, 31), 
                     expand = c(0, 0),
                     name="City mpg") +
  scale_y_continuous(limits = c(-.5, 82), 
                     expand = c(0, 0), 
                     name = "Model rank of Ford(ascending)") +
  coord_cartesian(clip = "off") +
  ggtitle("ECDF of City mileage of 2018 Ford models ") +
  theme(axis.line.x = element_blank(),plot.title=element_text(hjust = 0.5))

# Empirical cumulative distribution function in descending order
ggplot(vehicle_data, aes(x = regularvehiclemodels$City.Mpg.For.Fuel.Type1, 
                         y = 82-81*..y..)) + 
  stat_ecdf(geom = "step", color = "#0072B2") +
  geom_point(aes(y = 82-rank), color = "#0072B2") +
  scale_x_continuous(limits = c(10, 31), 
                     expand = c(0, 0),
                     name="City mpg") +
  scale_y_continuous(limits = c(-.5, 82), 
                     expand = c(0, 0), 
                     name = "Model rank of Ford(descending)") +
  coord_cartesian(clip = "off") +
  ggtitle("ECDF of City mileage of 2018 Ford models ") +
  theme(axis.line.x = element_blank(),plot.title=element_text(hjust = 0.5))

# Normalized ecdf 
ggplot(vehicle_data, aes(x = regularvehiclemodels$City.Mpg.For.Fuel.Type1, 
                         y =..y..)) + 
  stat_ecdf(geom = "step", color = "#0072B2") +
  scale_x_continuous(limits = c(10, 31), 
                     expand = c(0, 0),
                     name="City mpg") +
  scale_y_continuous(limits = c(-.01, 1.01), 
                     expand = c(0, 0), 
                     name = "Cumulative Frequency") +
  coord_cartesian(clip = "off") +
  ggtitle("Normalized ECDF of City mileage of 2018 Ford models ") +
  theme(axis.line.x = element_blank(),plot.title=element_text(hjust = 0.5))

###################### Highly skewed distributions ############################
### Examples : County population, word distribution in a book
# Dataset : City mileage of vehicle models (2002-Present)

citympg <- select(filter(vehiclemodels,!is.na(vehiclemodels$City.Mpg.For.Fuel.Type1),vehiclemodels$Year>2002),'Year','Make','Model','City.Mpg.For.Fuel.Type1')

mpg_data <- data.frame(citympg,
                       rank = rank(citympg$City.Mpg.For.Fuel.Type1,
                                   ties.method = "random"))

# Density plot of a Highly skewed distribution (Mileage data)
ggplot(mpg_data, aes(x = mpg_data$City.Mpg.For.Fuel.Type1)) + 
  geom_density(fill="#56B4E950",alpha=0.3,adjust=1/2) +
  theme_minimal() +
  labs(x="City Mileage (2003-Present) ",y="Count") +
  ggtitle("Kernel density estimate of mileage all the vehicle models (2002-Present)") +
  theme(plot.title = element_text(hjust = 0.5))  

# Normalized ECDF of a Highly skewed distribution (Mileage data)
ggplot(mpg_data, aes(x = mpg_data$City.Mpg.For.Fuel.Type1, y =..y..)) + 
  stat_ecdf(geom = "step", 
            color = "#0072B2", 
            size = 0.75, 
            pad = FALSE) +
  theme_minimal() +
  scale_x_continuous(limits = c(10, 58), 
                     expand = c(0, 0),
                     name="City mpg") +
  scale_y_continuous(limits = c(-.01, 1.01), 
                     expand = c(0, 0), 
                     name = "Cumulative Frequency") +
  coord_cartesian(clip = "off") +
  ggtitle("Normalized ECDF of City mileage of all vehicle models(2002-Present) ") +
  theme(axis.line.x = element_blank(),plot.title=element_text(hjust = 0.5))

# Density plot of logarithm of a Highly skewed distribution (Mileage data)
ggplot(mpg_data, aes(x = log10(mpg_data$City.Mpg.For.Fuel.Type1))) + 
  geom_density(fill="#56B4E950",alpha=0.3) +
  scale_x_continuous(
    expand = c(0.01, 0),
    name = expression(paste("log"["10"], "(number of models)"))) +
  scale_y_continuous(expand = c(0, 0), name = "density") +
  theme_minimal() +
  ggtitle("Kernel density estimate of logarithm of mileage of all the vehicle models (2002-Present)") +
  theme(plot.title = element_text(hjust = 0.5))  

# ECDF of logarithm of a Highly skewed distribution (Mileage data)
ggplot(mpg_data, aes(x = log10(mpg_data$City.Mpg.For.Fuel.Type1))) + 
  stat_ecdf(geom = "step", 
            color = "#0072B2", 
            pad = FALSE) +
  scale_x_continuous(
    expand = c(0.01, 0),
    name = expression(paste("log"["10"], "(number of Models)"))) +
  scale_y_continuous(expand = c(0.01, 0), name = "cumulative frequency") +
  coord_cartesian(clip = "off") +
  ggtitle("Normalized ECDF of logarithm of City mileage of all vehicle models(2002-Present) ") +
  theme(axis.line.x = element_blank(),plot.title=element_text(hjust = 0.5))


# Descending ecdf with logarithmic x and y axes ; Power law does not hold
ggplot(mpg_data, aes(x=mpg_data$City.Mpg.For.Fuel.Type1, y = 1-..y..)) + 
  stat_ecdf(geom = "step", 
            color = "#0072B2", 
            size = 0.75, 
            pad = FALSE) +
  scale_x_log10(expand = c(0.01, 0),
                breaks = c(1e2, 1e3, 1e4, 1e5, 1e6, 1e7),
                labels = c(expression(10^2), expression(10^3), expression(10^4),
                           expression(10^5), expression(10^6), expression(10^7)),
                name = "number of models") +
  scale_y_log10(expand = c(0.01, 0), breaks = c(1e-3, 1e-2, 1e-1, 1), name = "relative frequency") 

# When Power law holds, the plot will look like thi

###################### Quantile–quantile plots ######################
# Dataset : City mileage of Ford models for the year 2018

regularvehiclemodels <- select(filter(vehiclemodels,
                              !is.na(vehiclemodels$City.Mpg.For.Fuel.Type1),
                              vehiclemodels$Fuel.Type=="Regular",
                              vehiclemodels$Make=="Ford",
                              vehiclemodels$Year==2018),
                              'Year','Make','Model','City.Mpg.For.Fuel.Type1')

vehicle_data <- data.frame(regularvehiclemodels,
                           rank = rank(regularvehiclemodels$City.Mpg.For.Fuel.Type1,ties.method = "random"))

params <- as.list(MASS::fitdistr(regularvehiclemodels$City.Mpg.For.Fuel.Type1, 
                                 "normal")$estimate)
# axis line segments
df_segment <- data.frame(
  x = c(0, 0), 
  xend = c(50, 0),
  y = c(0, 0),
  yend = c(0, 50)
)

# Q-Q plot using stat_qq() and geom_abline()
ggplot(vehicle_data, aes(sample = City.Mpg.For.Fuel.Type1)) + 
  geom_abline(slope = 1, intercept = 0, color = "grey70") +
  stat_qq(dparams = params, color = "#0072B2") +
  geom_segment(
    data = df_segment,
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.5, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0, 50), 
                     expand = c(0, 0), 
                     breaks = 10*(5:10)) +
  scale_y_continuous(limits = c(0,50), 
                     expand = c(0, 0), 
                     breaks = 10*(5:10), 
                     name = "observed") +
  coord_fixed(clip = "off") +
  ggtitle("q-q plot of City mileage of Ford Models for the year 2018 ") + 
  theme(axis.line = element_blank(),plot.title=element_text(hjust = 0.5))

###################### Multiple Distributions in one plot ######################
# Dataset : Airline delay data

# List of Largest Airlines in USA
airline_list = c('DL', # Delta Air Lines Inc.
                 'B6', # JetBlue Airways
                 'WN', # Southwest Airlines Co.
                 'AS', # Alaska Airlines Inc.
                 'HA', # Hawaiian Airlines Inc.
                 'UA', # United Air Lines Inc.
                 'NK', # Spirit Air Lines
                 'AA', # American Airlines Inc.
                 'F9') # Frontier Airlines Inc.


# Top 4 airlines by fleet size
airlinefleets = c('AA','UA','DL','WN')
airlinedelay_short <- select(filter(airlinedelay,
                                    airlinedelay$carrier %in% airlinefleets,
                                    year==2018),
                             'carrier','carrier_name','X.carrier_delay')

""" Lets evaluate the mean delays in 2018 across these 15 airports
using Boxplot, Violin plot, Strip chart and Sina plot
"""

# Boxplot of carrier delays of largest air carriers in the Unites States
ggplot(data=airlinedelay_short,
       aes(droplevels(carrier_name),X.carrier_delay)) +
       ylim(0,75000) +
       geom_boxplot(outlier.colour="red",outlier.shape=1) +
       theme_bw() + 
       ggtitle("Box plot for carrier delay by Airlines") +
       labs(x="Largest airlines in the US",y="Carrier Delay in minutes") +
       coord_flip() +
       theme(plot.title=element_text(hjust=0.5))    

# Violin plot of carrier delays of largest air carriers in the Unites States
ggplot(data=airlinedelay_short,
       aes(droplevels(carrier_name),X.carrier_delay)) +
       ylim(0,75000) +
       geom_violin() +
       theme_bw() + 
       ggtitle("Violin plot for carrier delay by Airlines") +
       labs(x="Largest airlines in the US",y="Carrier Delay in minutes") +
       coord_flip() +
       theme(plot.title=element_text(hjust=0.5))    

# Strip chart of carrier delays of largest air carriers in the Unites States
# Using geom_point()
ggplot(data=airlinedelay_short,
       aes(droplevels(carrier),X.carrier_delay)) +
       ylim(0,75000) +
       geom_point(size = 0.75) + 
       theme_bw() + 
       ggtitle("Strip chart for delay in flights by Airlines") +
       labs(x="Largest airlines in the US",y="Carrier Delay in minutes") +
       coord_flip() +
       theme(plot.title=element_text(hjust=0.5))   

# Jitter plot(Strip chart with jitters) of carrier delays of largest air carriers in the Unites States
ggplot(data=airlinedelay_short,
       aes(droplevels(carrier_name),X.carrier_delay)) +
       ylim(0,40000) +
       geom_point(position = position_jitter(width = .15, height = 0, seed = 320), size = 0.05) +
       theme_bw() + 
       ggtitle("Jitter plot for delay in flights by Airlines") +
       labs(x="Largest airlines in the US",y="Carrier Delay in minutes") +
       coord_flip() +
       theme(plot.title=element_text(hjust=0.5))   


# Sina plot of carrier delays of largest air carriers in the Unites States
ggplot(data=airlinedelay_short,
       aes(droplevels(carrier_name),X.carrier_delay)) +
       ylim(0,10000) +
       geom_violin(fill = "gray90") + 
       ggforce::geom_sina(size = 0.05) + 
       theme_bw() + 
       ggtitle("Sina plot for delay in flights by Airlines") +
       labs(x="Largest airlines in the US",y="Carrier Delay in minutes") +
       coord_flip() +
       theme(plot.title=element_text(hjust=0.5))    

# Ridgeline plots to view carrier delay over the years
# Subset of vehiclemodels
citympg <- select(filter(vehiclemodels,
                         !is.na(vehiclemodels$City.Mpg.For.Fuel.Type1),
                         vehiclemodels$Make=="Ford"),
                         'Year','City.Mpg.For.Fuel.Type1')

# To convert Year as character
citympg_ridge <- transform(citympg,Year_char= as.character(Year))

# Ridgeline plot of city mileage of vehicle models over the years (1985-Present)
ggplot(citympg_ridge, 
       aes(x=citympg_ridge$City.Mpg.For.Fuel.Type1,y=citympg_ridge$Year_char)) +
geom_density_ridges(bandwidth=0.9,
                    scale=2,
                    fill="#c2a5cf",
                    color="black") +
xlim(5,40) +
labs(x="City Mileage (mpg)") +
# scale_x_continuous(name = "City Mileage (mpg)",breaks=c(0,15,30,45)) +
scale_y_discrete(name="Year",
                 breaks = c(1985,1990,1995,2000,2005,2010,2015,2020),
                 expand = c(0,0)) + 
coord_cartesian(clip = "off") +
ggtitle("Ridgeline plot of city mileage of vehicle models over the years") +
theme(plot.title = element_text(hjust=0.5))

# Trend comparison using Ridgeline plots
# Task is to find a dataset with polarized data
