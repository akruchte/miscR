library(tidyverse)
library(tidycensus)
library(assertr)
library(mipfp)


## load census variables for interactively finding correct variables
variables <- load_variables(year = 2010, 'acs5')
which_vars <- filter(variables, str_detect(name, 'B01001')) %>%
    slice(1:111) %>% pull(name) # filter down to black, white, and total

#county wide demographics data
data <- get_acs('county', which_vars, year = 2010, state = 'pa')

cleaned <- data %>% left_join(variables, by = c('variable' = 'name')) %>%
    mutate(sex = case_when(
               str_detect(label, 'Male') ~ 'Male',
               str_detect(label, 'Female') ~ 'Female',
               TRUE ~ 'Total'
           ),
           race = case_when(
               str_detect(concept, 'BLACK') ~ 'Black',
               str_detect(concept, 'WHITE') ~ 'White',
               TRUE ~ 'Total'
           ),
           age = str_extract(label, '(?<=(Male|Female)!!).+')
           ) %>%
    filter(sex != 'Total', !is.na(age)) %>%
    select(-label, -moe, -variable, -concept)

relabel_age <- function(age) {
    case_when(
        age %in% c('21 years', '20 years', '22 to 24 years') ~ '20 to 24 years',
        age %in% c('35 to 39 years', '40 to 44 years') ~ '35 to 44 years',
        age %in% c('45 to 49 years', '50 to 54 years') ~ '45 to 54 years',
        age %in% c('55 to 59 years', '60 to 64 years', '60 and 61 years', '62 to 64 years') ~ '55 to 64 years',
        age %in% c('65 and 66 years', '67 to 69 years', '70 to 74 years') ~ '65 to 74 years',
        age %in% c('75 to 79 years', '80 to 84 years') ~ '75 to 84 years',
        TRUE ~ age
    )
}

cleaned <- cleaned %>% mutate(age = relabel_age(age)) %>%
    group_by(GEOID, NAME, sex, race, age) %>%
    summarize(estimate = sum(estimate)) %>%
    ungroup() %>%
    spread(race, estimate) %>%
    mutate(Other = Total - (Black + White), Total = NULL) %>%
    verify(Other >= 0) %>%
    gather(race, estimate, Black, White, Other) %>%
    group_nest(GEOID, NAME) %>%
    rename(age_sex_race = data)


#tract by tract within county population estimates
tract_data <- get_acs(geography = 'tract', 'B01001_001', state = 'PA')

tracts <- tract_data %>%
    mutate(GEOID = str_sub(GEOID, 1, 5),
           variable = NULL) %>%
    select(-moe) %>%
    group_nest(GEOID) %>%
    rename(tract = data)

counties <- get_acs(geography = 'county', 'B01001_001', state = 'PA')

constraints <- full_join(tracts, cleaned)




int_trs <- function(xv){
    x <- as.vector(xv)
    xint <- floor(x)
    r <- x - xint
    def <- round(sum(r)) # the deficit population
                                        # the weights be 'topped up' (+ 1 applied)
    topup <- sample(length(x), size = def, prob = r)
    xint[topup] <- xint[topup] + 1
    dim(xint) <- dim(xv)
    dimnames(xint) <- dimnames(xv)
    xint
}


counties <- counties %>% mutate(ind_data = rep(list(NA), nrow(.)) )

for (i in seq_len(nrow(constraints))) {
    age_sex_race_names <- names(constraints[[i, 'age_sex_race']]) %>% discard(~.x == 'estimate')
    age_sex_race <- constraints[[i, 'age_sex_race']] %>% unite(age_sex_race, -estimate) %>% deframe

    tract_names <- names(constraints[[i, 'tract']]) %>% discard(~.x == 'estimate')
    tract <- constraints[[i, 'tract']] %>% unite(tract, -estimate) %>% deframe

    target <- list(age_sex_race, tract)
    seed <- array(1, map(target, length), map(target, names))
    weights <- suppressWarnings(Ipfp(seed, seq_along(target), target))
    weight_table <- as.data.frame.table(weights[[1]])
    expansion_counts <- int_trs(weight_table$Freq * counties[[i,'estimate']])
    indices <- rep(1:nrow(weight_table), expansion_counts)

    ind_data <- as_tibble(weight_table[indices, ]) %>%
        separate(Var1, into = age_sex_race_names, sep = '_') %>%
        separate(Var2, into = tract_names, sep = '_')

    counties$ind_data[i] <- list(ind_data)
}

dat_final <- counties %>% unnest() %>% select(sex, age, race, NAME, NAME1)






