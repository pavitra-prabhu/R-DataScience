library(ggplot2)

################ Bar Plot, Dot plot and Heatmaps #######################
# Alternative Fuel Vehicles
barplot_vehicles_df <- filter(vehiclemodels,ATV.Type!="") %>%
                       select('ATV.Type')

# Bar plot using geom_bar() depicting the distribution of Alternative fuel vehicles
barplot_vehicles <-
ggplot(barplot_vehicles_df, 
       aes(x=factor(ATV.Type)))+
  geom_bar(stat="count", 
           width=0.7, 
           fill="steelblue")+
  ggtitle("Distribution of Alternative fueled vehicles") +
  labs(x="Type of Alternative Fuel",y="Count") +
  theme_pprabhu_amounts() +
  theme(plot.title = element_text(hjust = 0.5))

# Stacked bar plot using geom_bar()
autobrand_list <- c('Chevrolet','Ford','Dodge','GMC','Toyota','BMW','Mercedes-Benz','Nissan','Mitsubishi','Volkswagen')
vehiclemodels_short <- filter(vehiclemodels,
                              Make %in% autobrand_list,
                              !is.na(Drive),Drive!="")
# Plot the stacked barplot
stacked_barplot <-
ggplot(data=vehiclemodels_short, aes(x=factor(Make),fill=Drive)) +
       geom_bar(stat="count",color="white",alpha=0.6) +
       labs(x="Car Manufacturers",y="Count") +
       ggtitle("Distribution of Car Models by Drivetrain") +
       theme_minimal() + 
       theme(plot.title = element_text(hjust = 0.5)) + 
       scale_fill_manual(values=rev(mycolortriplet[c(3,6,9,12,15,18,21)])) 
       #scale_fill_brewer(palette="Blues") + 

# Grouped Bar plot using geom_bar()
autobrand_list <- c('Chevrolet','Ford','Dodge','GMC','Toyota','BMW','Mercedes-Benz','Nissan','Mitsubishi','Volkswagen')
vehiclemodels_short <- filter(vehiclemodels,
                              Make %in% autobrand_list,
                              !is.na(Drive),Drive!="")
grouped_barplot <-
ggplot(data=vehiclemodels_short, aes(x=factor(Drive),fill=Make)) +
       geom_bar(stat="count", position=position_dodge(),alpha=0.9) +
       labs(x="Car Manufacturers",y="Count") +
       ggtitle("Distribution of Car Models by Drivetrain") +
       theme_minimal() + 
       theme(plot.title = element_text(hjust = 0.5)) + 
       scale_fill_brewer(palette="Spectral") + 
       coord_flip()

# Dot plot using geom_point()
ggplot(data=select(crudeoilannual,'Year','Spot.Price'), 
       aes(x = as.character(Year),y=Spot.Price)) +
       geom_point(color = "#0072B2", size = 3) +
       labs(x="Year",y="Spot price of Crude oil (WTI)") +
       ggtitle("Dot plot of spot price of WTI Crude oil from 1986-2018") +
       theme_minimal() + 
       theme(plot.title = element_text(hjust = 0.5)) + 
       coord_flip()       

# Heatmap using geom_tile()
airline_list = c('ATL','LAX','ORD','DFW','DEN','JFK','SFO','LAS','SEA','CLT','MCO','MIA','PHX','EWR','IAH')
airlinedelay_short <- select(filter(airlinedelay,airport %in% airline_list) %>% mutate(weather_delay = ifelse(is.na(weather_delay), 0, weather_delay)),'year','airport','weather_delay')
airlinedelay_summary <- airlinedelay_short %>% group_by(year,airport) %>% summarize(delay_weather = mean(weather_delay))
ggplot(data=airlinedelay_summary,aes(x=year,y=airport,fill= delay_weather)) +
       geom_tile(size = 0.25) +    
       labs(x="Year",y="Airports") +
       ggtitle("Heatmap of Airline delays of most popular airports in the US") +
       theme_minimal() + 
       scale_fill_gradient(low = "white", high = "#08519C") +
       theme(plot.title = element_text(hjust = 0.5))

