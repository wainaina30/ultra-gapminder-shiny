#summary statistics for variables

library(gapminder)
library(tidyverse)
library(rlang)

#what is the typical life expectancy
##by continent, year, country
###filter/group by var and calculate mean, median
#Returns combined summary stats for all listed cols in single table, might 
#be good foi single function summary
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

#takes function arg but cant access function arguments
summary_stats_fun <- function(data, grp_by,FUN){
  fun_sym<- rlang::ensym(FUN)
  data |> 
    group_by({{grp_by}}) |> 
    summarise(
      across(where(is.numeric),
             .fns = !!fun_sym
             ))
}

