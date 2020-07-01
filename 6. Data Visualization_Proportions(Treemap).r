
# Tree maps

afstations_df <- 
    select(afstations,fuel_type_code, st_prv_code) %>%
    table() %>%
    reshape2::melt() %>%
    left_join(US_states,by=c('st_prv_code'='State.Code')) %>%
    group_by(fuel_type_code,Region.Name,State.Name) %>%
    summarize(station_count = sum(value)) %>%
    filter(!is.na(State.Name),!is.na(Region.Name),station_count>0)

hues <- c(0,30,60,90,120,240,300)
# blue (240), brown (60), green (120), red (0), 
# purple (275), turquoise (180), rust (30), olive (90), 
# aqua (210), mulberry (330), emerald (150), and violet (300).

# Creating a dataset for color coding the dataset
afstations_density <-
    afstations_df %>%
    group_by(fuel_type_code,Region.Name) %>%
    summarize(mincount = min(station_count),
              maxcount = max(station_count)) %>%
    mutate(index = as.numeric(fuel_type_code)) %>%
    left_join(afstations_df,
              by = c("fuel_type_code", "Region.Name")) %>%
    group_by(index) %>%
    mutate(
    colvalue = (station_count-mincount)/(maxcount-mincount),
    fill = scales::gradient_n_pal(
      colorspace::sequential_hcl(
        7,
        h = hues[index[1]],
        c = 80,
        l = c(35,95),
        power = 1))(1-colvalue))

# Treemap Illustrating the Alternative Fuels data by Regions
afstations_treemap <- 
ggplot(afstations_density,
       aes(area = station_count,
           fill = fill,
           label = State.Name,
           subgroup = fuel_type_code)) +
geom_treemap() + 
geom_treemap_subgroup_text(
    colour = "white",
    place = "centre", 
    alpha = 0.7,
    grow = TRUE) +
facet_wrap(~Region.Name) +
geom_treemap_subgroup_border(color = "white") +
geom_treemap_text(
    aes(label = State.Name),
    color = "black",
    place = "centre",
    grow = FALSE) +
scale_fill_identity() +
coord_cartesian(clip = "off") +
guides(colour = "none", fill = "none") +
ggtitle("Treemap of Alternate Fuel - Fueling Stations") +
theme(plot.title = element_text(hjust = 0.5))

# Interactive Treemap using d3treer package
# 