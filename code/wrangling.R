#Population Data:
#2020, 2021 data comming from:
#https://api.census.gov/data/2021/pep/population

#2022,2023 data download from:
#https://www.census.gov/data/tables/time-series/demo/popest/2020s-national-total.html

#regions data comming from:
#https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json

library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)
library(jsonlite)
library(purrr)
library(httr2)
library(tidyverse)
library(lubridate)
library(readr)
library(knitr)


pop_2022_2023 <- read_excel("/Users/zhangjingxi/bst260/260_final_project/data/pop.xlsx", col_names = FALSE)
colnames(pop_2022_2023) <- as.character(pop_2022_2023[2, ]) # Set the first row as headers
# remove twice the first row since there are some header when download the data.
pop_2022_2023 <- pop_2022_2023[-1, ]  
pop_2022_2023 <- pop_2022_2023[-1, ]
# remove all the '.' in front of the state name
pop_2022_2023$state_name <- gsub("^\\.", "", pop_2022_2023$state_name)

# Make the data same as pset-4
long_pop<- pop_2022_2023 %>%
  pivot_longer(
    cols = starts_with("20"), 
    names_to = "year",
    values_to = "population"
  )

# Download 2020, 2021 and regions data:
census_key <- "e772e43cd3ddc77cead5d930d1da491dede1a107"
saveRDS(census_key, file = "census-key.R")
url <- "https://api.census.gov/data/2021/pep/population"

request <- request (url) |>
  req_url_query(get = I("POP_2020,POP_2021,NAME"), 
                `for` = I("state:*"), 
                key = census_key)
response <- request |> req_perform()
population <-  response |>
  resp_body_json(simplifyVector = TRUE)


population <- population |> 
  row_to_names(1) |>
  as.tibble()|> # convert to tibble
  select(-state)|> # remove stat column
  rename(state_name = NAME) |> # rename state column to state_name
  pivot_longer (-state_name, names_to = "year", values_to = "population")|>
  mutate(year= str_remove(year, "POP_"))|> # remove POP_ from year
  mutate(across(-state_name,as.numeric)) # parese all relevant colunns to numeric

population <- rbind(population, long_pop)

population <- population |> 
  mutate(state = state.abb[match(state_name, state.name)])|>
  mutate(state = case_when ( 
    state_name == "Puerto Rico" ~ "PR",
    state_name == "District of Columbia" ~ "DC",
    TRUE ~ state)) # use case_when to add abbreviations for DC and PR



url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url,simplifyDataFrame = FALSE)
regions <- map_df(regions, function(x) 
  data.frame(region = x$region, region_name = x$region_name, state_name = x$states)|>
    mutate(region_name = case_when ( 
      region_name == "New York and New Jersey, Puerto Rico, Virgin Islands" ~ 
        "NY, NJ, PR, VI",
      TRUE ~ region_name)))
population <- left_join(population, regions, by = "state_name")
population <- population %>%
  mutate(year = as.numeric(year))

write.csv(population, "/Users/zhangjingxi/bst260/260_final_project/data/population_data.csv", row.names = FALSE)



# Cases data comming from:
# https://data.cdc.gov/resource/pwn4-m3yp.json

# Death data comming from:
# https://data.cdc.gov/resource/r8kw-7aab.json

get_cdc_data<- function(url, limit = 1000000000) {
  ret <- request (url) |>
    req_url_query("$limit" = limit) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  return(ret)
}
cases_raw <- get_cdc_data("https://data.cdc.gov/resource/pwn4-m3yp.json")
deaths_raw<- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json")
cases <-cases_raw |>
  mutate(cases = parse_number(new_cases),
         date = as_date(ymd_hms(end_date)))|>
  filter(state %in% population$state) |>
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date))|>
  select(state,mmwr_year,mmwr_week,cases) |>
  arrange(state, mmwr_year , mmwr_week)

deaths <- deaths_raw |>
  mutate(deaths= parse_number(covid_19_deaths),
         date = as_date(ymd_hms(end_date))) |>
  filter(state %in% population$state_name) |>
  mutate(State = state.abb[match(state,state.name)]) |>
  mutate(State = case_when ( 
    state == "Puerto Rico" ~ "PR",
    state == "District of Columbia" ~ "DC",
    TRUE ~ State)) |> 
  mutate(mmwr_year = epiyear(date),mmwr_week= as.numeric(mmwr_week))|>
  select(mmwr_week, mmwr_year, deaths, State)|>
  arrange(State, mmwr_year, mmwr_week)

write.csv(cases, "/Users/zhangjingxi/bst260/260_final_project/data/cases.csv", row.names = FALSE)
write.csv(deaths, "/Users/zhangjingxi/bst260/260_final_project/data/death.csv", row.names = FALSE)







