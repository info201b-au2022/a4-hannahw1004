library(tidyverse)
library("ggplot2")
# The functions might be useful for A4
source("../source/a4-helpers.R")

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(incarceration)
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  #first call datasets
  total_pop <- incarceration %>%
    #pipe and do group by year 
    group_by(year) %>%
    # pipe again and then summarise the total jail population by each year 
    summarise(total_jail_population = sum(total_jail_pop, na.rm = TRUE))
return(total_pop)   
}
#View the the returned database to check if everything is queried 
View(get_year_jail_pop())
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  #store ggplot in a variable to return 
  bar_pop <- ggplot(get_year_jail_pop()) +
    #use geom_col function to graph bar graph
    geom_col(
      #use aesthetic function to plot bar graph by setting x-axis equal year and y-axis equal to jail population
      mapping = aes(x = year, 
                    y = total_jail_population)
      ) +
    #add caption and title of the graph 
    labs(
      title = "Growth of US Prison Population",
      caption = "portrays growth of us prisoners between 1970 to 2010"
    ) +
    #make the scale of the graph numbers 
    scale_y_continuous(labels = scales::comma) 
  return(bar_pop)   
} 
#call function to view bar graph 
plot_jail_pop_for_us()

## Section 4  ---- 

#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>

#create vector of states to use as an input 
statess <- c("WA", "AL", "CA", "OR")

get_year_jail_pop_by_states <- function(states) {
  state_total <- incarceration %>%
    #pipe from incarceration to filter states this is to make sure the function can accept the vector 
    filter(state %in% states) %>%
    #group by year and state
    group_by(year, state) %>%
    #summarise total jail population by grouped year and state 
    summarise(total_jail_state_population = sum(total_jail_pop, na.rm = TRUE))
  return(state_total)
}
#View to check if database quried all the needed information
View(get_year_jail_pop_by_states(statess))

plot_jail_pop_by_states <- function(states) {
  #create a variable to store the ggplot and call the previous function to start plotting
  line_pop <- ggplot(get_year_jail_pop_by_states(statess)) +
    #geom_line creates a line graph 
    geom_line(
      #use aesthetic function to plot line graph by setting x axis to year
      mapping = aes(x = year,
                    #set y axis to total_jail_state_population
                    y= total_jail_state_population,
                    #distinguish different state line graph by color 
                    color = state)
    ) +
    #create labels for the graph 
    labs(
      title = "Growth of Prison Population by State",
      caption = "portrays growth trends of prison population of Washington, Alabama, California, and Organ states"
    )+
    #make sure the scales are readable in numbers 
    scale_y_continuous(labels = scales::comma)
  return(line_pop)
}
#call function to check if the line grpah is plotted 
plot_jail_pop_by_states()
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
get_gender_jail_pop <- function() {
  df <- incarceration %>%
    #group by year in from piped incarceration 
    group_by(year) %>%
    # select year, female_jail_pop, and male_jail_pop column to create sepearte dataframe
    select(year, female_jail_pop, male_jail_pop)
  return(df)
}

plot_gender_jail_pop <- function() {
  #create variable to store ggplot and call previous function to get values
  scatter_pop <- ggplot(get_gender_jail_pop(),  
                        #use aesthetic function to plot by making x axis equal to female_jail_pop
                        aes(x = female_jail_pop,
                            #set y axis equal to male_jail_pop
                            y = male_jail_pop, 
                            #group the values into different years
                            group = year)) +
    #geom_point plots scappter plot 
    geom_point(
      #to differentiate different year, set different color for each year
      aes(color = year)
    )+
    #add labels to the graph 
    labs(
      title = "Different Gender Population Trends in Prison",
      caption = "portrays comparison of male and female population from 1970-2010"
    ) +
    #make the scale readable by turning it into numbers
    scale_y_continuous(labels = scales::comma)
    return(scatter_pop)
}
#call function to check if it is plotted correctly 
plot_gender_jail_pop()
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 

get_female_state_jail_pop <- function() {
  #make the state column into lowercase and full name otherwise it won't match up with the map
  incarceration$state <- tolower(state.name[match(incarceration$state, state.abb)])
  
  df<- incarceration %>%
    #group by state that is piped from incarceration
    group_by(state) %>%
    #summarise the sum of female_jail_pop 
    summarise(sum_female_pop = sum(female_jail_pop, na.rm = TRUE))
  
#get rid of any NA values in state column 
df_na <- df %>% drop_na(state)
  return(df_na)
}
#check to see if the dataframe is returned correctly
View(get_female_state_jail_pop())

plot_female_state_jail_pop <- function(){
  #use map_data provided to create a map 
  state_shape <- map_data("state") %>%
    #rename so it can be joined 
    rename(state = region) %>%
    #join previous data of female jail state population by state 
    left_join(get_female_state_jail_pop(), by="state")

#draw the map by setting fill of each female prison population 
 map_plot <- ggplot(state_shape) +
   #used to plot shapes 
    geom_polygon(
      #set x axis to longitude and y axis to latitude to draw the map 
      mapping = aes(x = long, y = lat, group = group, fill = sum_female_pop), 
      #the states will be seperated with white lines
      color = "white", 
      #the size of the line is thin
      size = .1
    )+ 
   #use map-based coordination system
    coord_map() +
   #decide a color for the high and low spectrum of female prison population in each state
    scale_fill_continuous(labels = scales :: comma, low = "#132B43", high = "Red") +
   #add labels to the graph 
   labs(
      title = "Female jail population by state",
      caption = "illustrates spectrum of number of female prisoner by state"
    ) +
   #get ride of x axis label to avoid confusion
   xlab(" ") +
   #get ride of y axis label to avoid confusion
   ylab(" ") + 
   #make the scale readable by turning it into numbers
   scale_y_continuous(labels = scales::comma) 
 return(map_plot)
}
#call the function to  check if the map is plotted correctly 
plot_female_state_jail_pop()

get_male_state_jail_pop <- function() {
  #make the state column into lowercase and full name otherwise it won't match up with the map
  incarceration$state <- tolower(state.name[match(incarceration$state, state.abb)])
  
  df<- incarceration %>%
    #group by state that is piped from incarceration
    group_by(state) %>%
    #summarise the sum of male_jail_pop 
    summarise(sum_male_pop = sum(male_jail_pop, na.rm = TRUE))
  #get rid of any NA values in state column 
  df_na <- df %>% drop_na(state)
  return(df_na)
}
#check to see if the dataframe is returned correctly
View(get_male_state_jail_pop())

plot_male_state_jail_pop <- function(){
  #use map_data provided to create a map
  state_shape <- map_data("state") %>%
    #rename so it can be joined 
    rename(state = region) %>%
    #join previous data of male prison state population by state 
    left_join(get_male_state_jail_pop(), by="state")
  
  #draw the map by setting fill of each male prison population 
  map_plot <- ggplot(state_shape) +
    #used to plot shapes 
    geom_polygon(
      #set x axis to longitude and y axis to latitude to draw the map 
      mapping = aes(x = long, y = lat, group = group, fill = sum_male_pop), 
      #the states will be seperated with white lines
      color = "white", 
      #the size of the line is thin
      size = .1
    )+ 
    #use map-based coordination system
    coord_map() +
    #decide a color for the high and low spectrum of male prison population in each state
    scale_fill_continuous(labels = scales :: comma, low = "#132B43", high = "Red") +
    #add labels to the graph 
    labs(
      title = "Male jail population by state",
      caption = "illustrates spectrum of number of male prisoner by state"
    ) +
    #get ride of x axis label to avoid confusion
    xlab(" ") +
    #get ride of y axis label to avoid confusion
    ylab(" ") + 
    #make the scale readable by turning it into numbers
    scale_y_continuous(labels = scales::comma) 
  return(map_plot)
}
#call the function to  check if the map is plotted correctly 
plot_male_state_jail_pop()



#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


