# Set the Working directory for the file
setwd("/Users/pavitragajanana/development/2. Data Files/")
quartz.options(width=6, height=6)
# Loading the datasets
# crudeoil <- read.csv('CrudeOilData.csv')
creditratings <-read.csv('Fitch-ratings-history.csv',sep=';') # 607824 records
airlinedelay <- read.csv('airline_delay.csv') # 264962 rows
aviationaccidents <-read.csv('ntsb-aviation-accident-dataset.csv',sep=';') # 167367 records
euroexrates <- read.csv('euro-exchange-rates.csv',sep=';') # 45024 records
vehiclemodels <-read.csv('all-vehicles-model.csv',sep=';') # 41443 records
# vehicles <-read.csv('vehicles.csv') # 41836 records
vehiclemodels <-read.csv('vehicles.csv') # 41836 records
afstations <-read.csv('Alternative Fuel Stations.csv')
crudeoildaily <- read.csv('CrudeOil_Daily_Cushing_OK_WTI_Spot_Price_FOB.csv') # 8488 records
bankdata <- read.csv('bank-data.csv') # 1889 records
lightdutyvehicles <- read.csv('LightDutyVehicles.csv')
alternativefuelvehicles <- read.csv('Alternative Fuel Vehicles.csv')
US_states <- read.csv('US-States.csv') # 59 records
crudeoilannual <- read.csv('CrudeOil_Annual_Cushing.csv') # 33 records

install.packages(c('matrixStats','corrplot','robust'))
install.packages(c('dplyr','tidyr'))
install.packages(c('ggplot2','ggforce','ggridges','RColorBrewer','cowplot','forcats','treemapify','ggrepel','GGally','networkD3','ggalluvial'))
############## Import Libraries #################
# Statistics libraries
library(matrixStats) # Includes weightedMedian
library(corrplot) # Includes corrplot function
# library(robust) # Includes covRob function

# Data manipulation libraries
library(dplyr) # includes filter function
library(tidyr) # includes spread function


# Visualization libraries
library(ggplot2) # Includes ggplot function
#library(ggforce) # Includes geom_sina & geom_parallel_sets
library(ggridges) # Includes geom_
library(RColorBrewer) # For nice looking color palettes
library(cowplot) # Includes axis_canvas for generating A Canvas Onto Which One Can Draw Axis-Like Objects
library(forcats) # Includes fct_rev
library(treemapify) # Includes geom_treemap
library(ggrepel) # Includes geom_text_repel
library(GGally) # Includes ggparcoord
library(networkD3) # Includes sankeyNetwork function
library(ggalluvial) # Includes geom_alluvium

################ Data visualisation essentials ################
# View Color palettes
# Used below in Stacked Bar plot is "Paired"
brewer.pal(n = 8, name = "Dark2")
display.brewer.all()

mycolorpalgradient <- c("#40004b", # Dark Purple 1
                        "#762a83", # Dark Purple 2  
                        "#9970ab", # Medium Purple 
                        "#c2a5cf", # Lighter Purple 2
                        "#e7d4e8", # Lightest Purple 1  
                        "#f7f7f7", # White
                        "#d9f0d3", # Lightest Green 1
                        "#a6dba0", # Medium Green
                        "#5aae61", # Dark Green 2
                        "#1b7837") # Dark Green 1

mycolortriplet <- c("#f6e8c3","#dfc27d","#bf812d", # Brown  
                    "#c7eae5","#80cdc1","#35978f", # Green
                    "#fde0ef","#f1b6da","#de77ae", # Pink
                    "#e7d4e8","#c2a5cf","#9970ab", # Purple
                    "#e0e0e0","#bababa","#878787", # Grey
                    "#e0f3f8","#abd9e9","#74add1", # Blue
                    "#e6f5d0","#b8e186","#7fbc41", # Light Green
                    "#fddbc7","#f4a582","#d6604d") # Red


# Color palette and size used in this file
mycolorpal <- c("#810F7C",
                "#E41A1C",
                "#FD8D3C",
                "#08519C",
                "#33A02C")
# Size 
small_size <- 8/.pt
main_size <- 12/.pt
xsmall_size <- 6/.pt
