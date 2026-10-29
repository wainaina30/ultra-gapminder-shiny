#summary statistics for variables

library(gapminder)
library(tidyverse)
library(rlang)

#what is the typical life expectancy
##by continent, year, country
###filter/group by var and calculate mean, median
#Returns combined summary stats for all listed cols in single table, might 
#be good foi single function summary
summary_stats <- function(data, grp_by, ...){
  
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

#
#tidy eval args collection
group_one_var_one <- function(data, grp_by, var){
  data |> 
    group_by({{grp_by}}) |> 
    summarise(
      mean = mean({{var}}, na.rm = T),
      .groups = "drop"
      )
}

#collect vars by c() 
#pass them as unquoted strings ie as symbols
group_many_var_many <- function(data, grp_by, vars){
  data |> 
    group_by(pick({{grp_by}})) |> 
    summarise(
      across({{vars}}, ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#pass them as character strings
group_many_all_of <- function(data, grp_by, vars){
  data |> 
    group_by(across(all_of(grp_by))) |> 
    summarise(
      across(all_of(vars), ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#rlang args collection
#syms capture multiple grouping variables and inject them !!!
rlang_group_many_var_many <- function(data, grp_by, var){
  #convert character vector to symbols
  sym_grp <- rlang::syms(grp_by)
  data |> 
    group_by(!!!sym_grp) |> 
    summarise(
      across(all_of(var),
             ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#use both rlang for grouping and varible 
rlang_group_many_var_many2 <- function(data, grp_by, var){
  grp_syms <- rlang::syms(grp_by)
  var_syms <- rlang::syms(var)
  
  data |> 
    group_by(!!!grp_syms) |> 
    summarise(
      across(
        #inject list of quosures using !!!
        c(!!!var_syms), #capture injected symbols in a tidyselect using c()
        ~mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

#dynamic dots in rlang and tidy eval
#dym dots by itself does not evaluate strings as column names or select 
#columns

#tidy evaluation: use of ... for multiple groupings without the user using 
#c() to enclose columns
#after dots use arg names ie tidy_dots_group_many_var_many(gapminder, year, 
#continent, var = c(lifeExp, pop)) check var = c(...  )
tidy_dots_group_many_var_many <- function(data, ..., var){
  data |> 
    group_by(...) |> 
    summarise(
      across({{var}},
             ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

#rlang explicit capture 
rlang_dots_group_many_var_many <- function(data, ..., var){
  grps <- enquos(...)
  vars <- enquo(var)
  
  data |> 
    group_by(!!!grps) |> 
    summarise(
      across(
        !!vars,
        ~mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

#MEASURE OF CENTRAL TENDANCY FUNCTIONS
#how can you use a function that accepts both group_by's and no groups 
#at the same time, if else???


#pre analysis check list 
#missingness, the amont of missing data and the reason the data is missing
#mean, sensitive to missing obs that disproportionately exlude very high
#or very low values
#median, robust to extreme values but can still be biased if missingness
#changes the distribution around the middle
#mode,  can change when missing obs disproportionately remove the most 
#frequent category or value


check_num_calc_moct <- function(x){
  if(all(is.numeric(x))){
    mean(x[which(is.finite(x))])
  }else{
  print("not all")
  }
}

#what if the user doesnt want groups
#function that does summarise for vars without groups 
calc_moct <- function(data, var){
  data |> 
    summarise(
      across(
        where(is.numeric),
        ~check_num_calc_moct(.x)
      )
    )
}

# HOW TO CHECK IF FUNCTION IS DATA MASK OR TIDYSELECT PROGRAMMATICALLY
# how to pass quaotation to non data mask or non tidyselect functions.
prop_missing <- function(data, var){
  #var_ <- enquo(var)
  #sum(is.na(subset(data, {{var}}))) / dim(data)[1] * 100
  
  sum(is.na(data[[var]])) / dim(data)[1] * 100
}  

prop_missing_datamask <- function(data, var){
  data |> 
    summarise(
      prop = sum(is.na({{var}})) / n()
    ) |> 
    pull(prop)
}

#check constant variables, all numeric variables
##variance and SD is zero 0
is_constant <- function(data){
  data |> 
    summarise(
      across(
        where(is.numeric),
        #whats the point of unique in df??
        ~length(unique(.x)) <= 1 
      )
    )
}

#function where grouping is optional 
optional_grouping_dots <- function(data, var, ...){
  data |> 
    group_by(..., .add = TRUE) |> 
    summarise(
      across({{var}},
             ~mean(.x, na.rm = T)
             )
    )
}
#cant bve used in shiny, character groupings cant be passed to dots 
optional_grouping_dots_allof <- function(data, var, ...){
  data |> 
    group_by(..., .add = TRUE) |> 
    summarise(
      across(all_of(var),
             ~mean(.x, na.rm = T)
             )
    )
}
#can be used in shiny, works with or without groups 
optional_grouping_dots_allofshiny <- function(data, var, groups = character()){
  data |> 
    group_by(across(all_of(groups))) |> 
    summarise(
      across(all_of(var),
             ~mean(.x, na.rm = T)
             )
    )
}


#low variation columns 
##calculate variance and SD and dont use either of theme alone 
##to label a column as low variation.
##variance is very sensitive to scale
##use of coefficient of variation (SD relative to mean)

####this function doesnt show the names of the variables
check_variation <- function(data, cv_cutoff = 0.05){
  data |> 
    select(where(is.numeric)) |> 
    purrr::map_dfr(function(x){
      x <- x[is.finite(x)]
      n <- length(x)
    
      variance <- if(n >= 2) var(x) else NA_real_
      sd_value <- if(n >= 2) sd(x) else NA_real_
      mean_value <- if(n > 0) mean(x) else NA_real_
      
      #cov
      cv <- if(!is.na(sd_value) && is.finite(mean_value) &&
               abs(mean_value) > 0){
        sd_value / abs(mean_value)
      }else{
        NA_real_
      }
      
      tibble(
         n = n,
         sd = sd_value,
         variance  = variance,
         cv = cv,
         cv_percent = cv * 100,
         low_variation =  cv < cv_cutoff
      )
    })
}

#shows variables names
#most important for ratio-scale variables with meaningful zero
#can be misleading when mean is close to zero or when the varible 
#takes negative values

check_variation <- function(data, cv_cutoff = 0.05) {
  
  data |>
    select(where(is.numeric)) |>
    imap_dfr(function(x, variable) {
      
      # Keep finite observations
      x <- x[is.finite(x)]
      
      n <- length(x)
      
      # Calculate variation measures
      variance <- if (n >= 2) var(x) else NA_real_
      sd_value <- if (n >= 2) sd(x) else NA_real_
      mean_value <- if (n > 0) mean(x) else NA_real_
      
      # Coefficient of variation
      cv <- if (!is.na(sd_value) &&
                is.finite(mean_value) &&
                abs(mean_value) > 0) {
        sd_value / abs(mean_value)
      } else {
        NA_real_
      }
      
      tibble(
        variable = variable,
        n = n,
        variance = variance,
        sd = sd_value,
        cv = cv,
        variation_status = case_when(
          n == 0 ~ "No finite observations",
          n < 2 ~ "Insufficient observations",
          sd_value == 0 ~ "Constant",
          is.na(cv) ~ "CV undefined",
          cv < cv_cutoff ~ "Low relative variation",
          TRUE ~ "Not flagged as low variation"
        )
      )
    })
}

#the output of this function is not useful for diagnosing cols  
check_variation_across <- function(data) {
  
  data |>
    summarise(
      across(
        where(is.numeric),
        list(
          variance = ~ var(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE),
          cv = ~ {
            avg <- mean(.x, na.rm = TRUE)
            s <- sd(.x, na.rm = TRUE)
            
            if (is.finite(avg) && avg != 0) {
              s / abs(avg)
            } else {
              NA_real_
            }
          }
        ),
        .names = "{.col}_{.fn}"
      )
    )
}

#outlier nad plausibility check 
#plausibility check, could the value reasonably occur, given what 
# the variable measures and its units 
check_moct_outliers <- function(data, fence = 1.5){
  #take numeric cols
  data |> 
    select(where(is.numeric)) |> 
    imap_dfr(function(x, variable){
      #work with finite values/cols/exclude inf,-inf
      x <- x[is.finite(x)]
      #handle insufficient obs
      if(length(x) < 2){
        return(tibble(
          variable = variable,
          n = length(x),
          n_outliers = NA_integer_,
          lower_fence = NA_integer_,
          uppper_fence = NA_integer_
        ))
      }
      #calc quartiles and IQR = Q1 = Q3
      q <- quantile(x ,probs  = c(0.25, 0.75), names = FALSE)
      iqr <- q[2] - q[1]
      #calc outlier fences
      lower <- q[1] - fence * iqr
      upper <- q[2] + fence * iqr
      #count potential outliers 
      n_outliers <- sum(x < lower | x > upper )
      
      tibble(
        variable = variable,
        n = length(x),
        n_outliers = n_outliers,
        lower_fence = lower,
        upper_fence = upper 
      )
    })
}

#check skewness
##the effect of scale to skewness, ratio and interval data??
##extreme values affect skewness
##most meaningful for quantitative variables with meaningful distances
check_moct_skewnes <- function(data){
  data |> 
    select(where(is.numeric)) |> 
    imap_dfr(function(x, variable){
      x <- x[is.finite(x)]
      s <- if(length(x) > 3 && sd(x) > 0){
        moments::skewness(x)
      }else{
        NA_real_ # numeric stored as doubles
      }
      tibble(
        variable = variable,
        n = n_distinct(x),
        skewness = s
      )
    })
}

#check dataPreparation 
#check for constants, included/redundacy doubles, bijections columns

#check potential low cardianality numeric variables
check_low_cardinality <- function(data){
  data |> 
    select(where(is.numeric)&
             where(~ n_distinct(.x,na.rm = TRUE) <= 5)
             )
}

#check proportion of missingness 
prop_missing_.x <- function(data){
  data |> 
    summarise(
      across(
        where(is.numeric),
        ~sum(is.na(.x) / nrow(data) * 100)
      )
    )
}


#perform measure of central tendancy on numeric cols or all
  #missingness: check proportion of missingness, cut off = .95,
  #can it issue a warning if prop is > 50%

perform_moct_allnum <- function(data, groups = character()){
  data |> 
    group_by(all_of(groups)) |> 
    summarise(
      across(where(is.numeric),
      { #exclude constants and infinite values
        data_ <- data[which(as.logical(!is_constant(data)))]
        mean(data_[which(is.finite(data_))])
      }
    )
    )
    
}


perform_moct_allnum2 <- function(data, groups = character(), exclude = "year"){
  data |> 
    group_by(across(all_of(groups))) |> 
    summarise(
      across(
      where(is.numeric) & !any_of(exclude),
      ~test3(.x)
    )
  )
}



test3 <- function(x){
  if(!is.numeric(x) | is_constant_vector(x)){
    stop("conditions to calculate not met!")
  }else{
    
    mean(x[which(is.finite(x))])
  }
}

perform_moct_vars <- function(data, vars, groups = character()
                          ,prop_cut_off = .95){
  data |> 
    group_by(across(all_of(groups))) |> 
    summarise(
      across(
        all_of(vars),
        {
          if()
        }
    )
  )
}

perform_moct_test <- function(x){
  if(!is.numeric(x)){
    stop("Numeric variables required!")
  }
  if(){
     mean(x[which(is.finite(x))])
  }
}

test <- function(x){
  if(!is.numeric(x)){
    stop("all values maust be numeric")
  }
  
  if(!all(is.finite(x))){
    stop("all values must be finite!")
  }
  
  if(var(x) == 0){
    stop("x must have non zero variance")
  }
  mean(x)
}

test2 <- function(x){
  if(!is.numeric(x) | !all(is.finite(x)) | var(x) == 0){
    stop("conditions to calculate not met!")
  }
  mean(x)
}




is_constant_vector <- function(x){
  length(unique(x)) <= 1
}
