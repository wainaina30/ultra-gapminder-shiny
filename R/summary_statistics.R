#summary statistics for variables

library(gapminder)
library(tidyverse)
library(rlang)

#what is the typical life expectancy
##by continent, year, country
###filter/group by var and calculate mean, median
#takes 
summary_stats <- function(data, grp_by){
  
  data |> 
    group_by({{grp_by}}) |> 
    summarise(
      across(where(is.numeric),
             list(
               mean = ~mean(.x, na.rm = T),
               median = median
             ))
    )
}
