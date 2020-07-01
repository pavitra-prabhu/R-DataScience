################################################################################################
################################ Movement/Flows : Proportions ##############################
################################################################################################

# Load libraries
library(GGally) # Includes ggparcoord
library(ggforce) # Includes geom_parallel_sets
library(networkD3) # Includes sankeyNetwork function
library(ggalluvial) # Includes geom_alluvium

# Parallel Coordinate plot
# Lets consider light duty vehicles for flex-fuel vehicles and check mpg 
mpgdata<-
select(filter(lightdutyvehicles,
            Fuel.ID==11,
            !is.na(Conventional.Fuel.Economy.City),
    !is.na(Alternative.Fuel.Economy.City),
    !is.na(Conventional.Fuel.Economy.Highway),
    !is.na(Alternative.Fuel.Economy.Highway),
    !is.na(Conventional.Fuel.Economy.Combined),
    !is.na(Alternative.Fuel.Economy.Combined)),
    "Category",
    "Category.ID",
    "Conventional.Fuel.Economy.City",
    "Alternative.Fuel.Economy.City",
    "Conventional.Fuel.Economy.Highway",
    "Alternative.Fuel.Economy.Highway",
    "Conventional.Fuel.Economy.Combined",
    "Alternative.Fuel.Economy.Combined")

mpgparcoord_plot <-
ggparcoord(data = mpgdata,
           columns = 2:8, 
           groupColumn = 1 , 
           title = "Parallel Coordinate plot of Fuel Economy of FFVs",
           showPoints = TRUE,
           scale ="globalminmax",
           alphaLines = 0.5) + 
theme_minimal() +
theme(plot.title = element_text(size=10,hjust=0.5),
      legend.position="none") +
scale_color_manual(values=c("steelblue","#E8E8E8","#E8E8E8","#E8E8E8")) +
scale_x_discrete(labels = c("Vehicle Category",
                            "Gasoline city",
                            "E85 city",
                            "Gasoline hwy",
                            "E85 hwy",
                            "Gasoline city/hwy",
                            "E85 city/hwy"),
                 expand = c(0.12,0.12)) +
xlab("Fuel Economy of Gasoline vs E85 in Flex-Fuel Vehicles") +
geom_text(data = mpgdata %>%
            select(Category,Category.ID) %>% 
            unique() %>% 
            arrange(-Category.ID) %>%
            mutate(x = 1,
                   y = Category.ID),
        aes(x = x, y = y, label = Category),
        hjust = 1.1,size = 2.5,
        inherit.aes = FALSE) 


# Parallel Sets with FuelGroup,FuelCategory,FuelType
# Lets use the vehiclemodels dataset, for a parallel sets plot on the fuel types.

# Assigns fuel group and subgroup to fuel type
fuelgrouping <- select(vehiclemodels,'Fuel.Type') %>% 
                unique() %>%
mutate(FuelSubGroup = factor(case_when(
    Fuel.Type %in% c('Regular','Midgrade','Premium') ~ "Conventional Gasoline",
    Fuel.Type %in% c('Diesel') ~ "Diesel",
    Fuel.Type %in% c('CNG') ~ "Compressed Natural Gas",
    Fuel.Type %in% c('Electricity') ~ "All-Electric",
    Fuel.Type %in% c('Regular Gas or Electricity',  
                     'Premium Gas or Electricity', 
                     'Regular Gas and Electricity',
                     'Premium and Electricity') ~ "Plug-In Hybrids",
    Fuel.Type %in% c('Gasoline or natural gas') ~ "Bifuel (CNG)",
    Fuel.Type %in% c('Gasoline or propane') ~ "Bifuel (LPG)",
    Fuel.Type %in% c('Gasoline or E85',
                    'Premium or E85') ~ "Flex-Fuel"),
    levels = c("Conventional Gasoline","Diesel","Compressed Natural Gas",
              "All-Electric","Plug-In Hybrids","Bifuel (CNG)",
              "Bifuel (LPG)","Flex-Fuel"))) %>%
mutate(
FuelGroup = factor(case_when(
    FuelSubGroup %in% c("Conventional Gasoline",
                        "Diesel",
                        "Compressed Natural Gas") ~ "Fossil Fuels",
    FuelSubGroup %in% c('All-Electric',
                        'Plug-In Hybrids',
                        'Flex-Fuel',
                        'Bifuel (CNG)',
                        'Bifuel (LPG)') ~ "Alternative Fuels"),
    levels = c("Fossil Fuels","Alternative Fuels")))

fuelvehicles <-
table(select(filter(vehiclemodels,
       !is.na(Fuel.Type),
       !Fuel.Type==""),'Fuel.Type')) %>%
reshape2::melt() %>%
rename(FuelType = Var1,countcars=value) %>%
left_join(fuelgrouping,by = c('FuelType'='Fuel.Type'))

# Creating x and y variables for Parallel sets
fuelvehicles <- gather_set_data(fuelvehicles,c("FuelType","FuelSubGroup","FuelGroup"))
# Specifies the x axis order
fuelvehicles$x <- factor(fuelvehicles$x,levels=c("FuelGroup","FuelSubGroup","FuelType"))

# Parallel sets plot
vehicles_parallelsets <-
ggplot(fuelvehicles, 
       aes(x, 
           id = id, 
           split = y, 
           value = countcars)) +
  geom_parallel_sets(aes(fill = FuelGroup), 
                     alpha = 0.6, 
                     axis.width = 0.13) +
  geom_parallel_sets_axes(axis.width = 0.1, 
                          fill = "grey80", 
                          color = "grey80") +
  geom_parallel_sets_labels(
    color = 'black',
    size = 6/.pt,
    angle = 0) +
  scale_x_discrete(
    name = NULL,
    expand = c(0, 0.2)) + 
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(14, 1.5, 2, 1.5),
    legend.position="none")


# Sankey Plot
"""
Sankey diagrams are a specific type of flow diagram, 
in which the width of the arrows is shown proportionally 
to the flow quantity. They are typically used to visualize energy 
or material or cost transfers between processes.
Packages that can be used to plot a sankey diagram : 
riverplot, networkD3, alluvial
"""
fuelstations <- 
    select(afstations,fuel_type_code, st_prv_code) %>%
    table() %>%
    reshape2::melt() %>%
    left_join(US_states,by=c('st_prv_code'='State.Code')) %>%
    group_by(fuel_type_code,Region.Name) %>%
    summarize(station_count = sum(value)) %>%
    filter(!is.na(Region.Name),station_count>0)# %>%
    #spread(Region.Name,station_count)

# sankeyNetwork uses 2 dataframes (i.e. nodes and links)
fueltypes <- unique(as.character(fuelstations$fuel_type_code))
regions <- c("Midwest","Northeast","South","West")
# Nodes include 7 fuel types and 4 regions 
nodes <- data.frame(node=c(0:10),name=c(fueltypes,regions))

# Links having a source target and value for each flow
fuelstations <- merge(fuelstations,nodes,
                      by.x="fuel_type_code",
                      by.y="name")

fuelstations <- merge(fuelstations,nodes,
                      by.x="Region.Name",
                      by.y="name")

links <- fuelstations[ , c("node.x", "node.y", "station_count")]
colnames(links) <- c("source", "target", "value")
# Interactive Sankey plot 
networkD3::sankeyNetwork(Links = links,
                         Nodes = nodes,
                         Source = 'source',
                         Target = 'target',
                         Value = 'value',
                         NodeID = 'name',
                         units = 'stations',
                         fontSize = 12)


# Alluvial Diagram
"""
Alluvial diagram is a variant of the Parallel Sets but for 
categorical variables and often to display trends over time and phases.
"""

is_alluvia_form(as.data.frame(fuelstations), axes = 1:3, silent = TRUE)

# Plots the alluvial diagram (Alluvia or wide Format)
alluvial_wide <-
ggplot(as.data.frame(fuelstations),
       aes(y = station_count, 
           axis1 = Region.Name, 
           axis2 = fuel_type_code)) +
  geom_alluvium(aes(fill = fuel_type_code), 
                width = 1/12) +
  geom_stratum(width = 1/12, 
               fill = "black", 
               color = "grey") +
  geom_label(stat = "stratum", 
             label.strata = TRUE) +
  scale_x_discrete(limits = c("Region","Fuel"),expand=c(0.05,0.05)) +
  scale_fill_manual(values = mycolortriplet[c(3,6,9,12,15,18,21)]) +
  ggtitle("Alternative Fueling stations by type and region")


# Time series alluvia for the flight delay data

airline_df <- as.data.frame(
select(filter(airlinedelay,carrier %in% mainlinepass_airlines,
                           year>2000,
                           !is.na(arr_flights),
                           !is.na(arr_del15),
                           !is.na(arr_cancelled),
                           !is.na(arr_diverted)),
       year,arr_flights,arr_del15,arr_cancelled,arr_diverted) %>%
mutate(
    ontime_flights = arr_flights - arr_del15 - arr_cancelled - arr_diverted
      ) %>%
group_by(year) %>%
summarize(ontime_flights = sum(ontime_flights),
          delayed_flights = sum(arr_del15),
          cancelled_flights = sum(arr_cancelled),
          diverted_flights = sum(arr_diverted)) %>%
gather(FlightType,flightcount,-year))

alluvial_timeseries <-
ggplot(airline_df,
       aes(x = year, 
           alluvium = FlightType,
           y = flightcount)) +
  geom_flow(aes(fill = FlightType, colour = FlightType), 
            width = 0)


# Plots the alluvial diagram (Lode or long Format)
altfuel_df <-
select(filter(lightdutyvehicles,Model.Year>1999),
       'Fuel','Category','Model.Year') %>%
table() %>%
reshape2::melt()

lightdutyvehicles_lode <-
to_lodes_form(altfuel_df,axes=3:23)

lightdutyvehicles_lode$x <- factor(lightdutyvehicles_lode$x,
                                            levels = c(2000,2001,2002,2003,2004,
                                                       2005,2006,2007,2008,2009,
                                                       2010,2011,2012,2013,2014,
                                                       2015,2016,2017,2018,2019,
                                                       2020))
###########
# LDV dataset

altfuel_df <- 
select(filter(lightdutyvehicles,Model.Year>1999),
       'Fuel','Category','Model.Year') %>%
table() %>%
reshape2::melt() %>%
group_by(Model.Year,Fuel) %>%
summarize(carcount = sum(value)) %>%
mutate(alluvium  = seq(1,12,1))


ggplot(altfuel_df,
       aes(x = Model.Year, 
           y = carcount,
           stratum = Fuel, 
           alluvium = alluvium,
           fill = Fuel,
           color = Fuel)) +
  geom_flow() +
  geom_stratum() +
  #geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("Alternative Fuel Vehicle Production by Fuel and Category")


