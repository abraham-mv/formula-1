library(tidyverse)
library(here)

years <- 1950:1955
races <- list()
qual <- list()
entries <- list()
events <- list()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (year in years){
  races[[year]] <- read.csv(paste0("../data/race/race_",year,".csv"))
  qual[[year]] <- read.csv(paste0("../data/qual/qual_",year,".csv"))
  entries[[year]] <- read.csv(paste0("../data/entries/entries_",year,".csv"))
  events[[year]] <- read.csv(paste0("../data/events/event_",year,".csv"))
}
races <- do.call(rbind, races)
qual <- do.call(rbind, qual)
entries <- do.call(rbind, entries)
events <- do.call(rbind, events)

# Let's do some data manipulation. First we will modify the date in the events 
# dataframe so we have something useful to work with. 
events_new <- events |> 
  # separate the years
  separate(Date, into = c("Day", "Month", "year"), sep = " ", remove = F) |> 
  mutate(gp_name = tolower(gsub(" ","-",Event)),
         Date = dmy(Date),
         circuit = Circuit.Name,
         year = as.numeric(year)) |> 
  select(-c(X, Event, Circuit.Name, Day, Month))

# Obtain the engine and car manufacturers for each driver/team on each evetn
new_entries <- entries |> select(-c(X, X., Nat, Team))

# Obtain the race positions
race_positions <- races |> 
  # Account for drivers who shared one car in the grand prix
  separate_rows(Driver, sep = "/") |> 
  filter(Driver != "") |> 
  # Account for duplicated drivers in the grand prix
  group_by(gp_name, year, Driver) |> 
  slice_max(Laps) |> 
  ungroup() |> 
  # get the race positions
  mutate(race_pos = Pos) |>
  # Change the best lap variable into seconds
  mutate(Best = sapply(strsplit(Best, ":"), 
                       function(x)(as.numeric(x[1])*60 +
                                     as.numeric(x[2])))) |> 
  # get the fastest lap for each race
  group_by(year, gp_name) |> 
  # create a binary variable to see if a given driver got the fastest lap
  mutate(best_lap = as.integer(Best == min(Best, na.rm = T))) |> 
  mutate(best_lap = ifelse(is.na(best_lap), 0, best_lap)) |> 
  select(race_pos, best_lap, Driver, gp_name, year) 

qual_df <- qual |> 
  select(-c(X, Best, Lap, Laps)) |> 
  filter(gp_name != "indianapolis-500") |> 
  left_join(events_new, by = c("gp_name", "year")) |>
  left_join(new_entries, 
            by = c( "Driver", "gp_name", "year")) |> 
  right_join(race_positions, by = c("Driver","gp_name", "year")) |>
  distinct(Date, Driver, .keep_all = T) |> 
  mutate(Time = sapply(strsplit(Time, ":"), 
                       function(x)as.numeric(x[1])*60 + 
                         as.numeric(x[2])),
         gp_name = factor(gp_name, levels = unique(gp_name)), 
         Nat = as.factor(Nat),
         Team = as.factor(Team),
         Gap = as.numeric(gsub("\\+ ", "", Gap)),
         Interval = as.numeric(gsub("\\+ ", "", Interval)),
         Engine = as.factor(gsub("\\d", "", Engine)),
         Car = as.factor(gsub("\\d", "", Car))
  ) |> 
  mutate(race_pos = ifelse(race_pos == "DNF", 99, as.numeric(race_pos))) |> 
  mutate(points = ifelse(race_pos < 6, 1, 0))

# Get the experience of the drivers in terms of number of races
exp <- qual_df |> 
  group_by(Driver, Date) |> 
  summarise(count = n()) |> 
  mutate(exp = cumsum(count)) |> 
  select(-count)

qual_df <- qual_df |> left_join(exp, by = c("Driver", "Date"))

qual_df$ENGINE <- fct_collapse(qual_df$Engine, 
                               "Ferrari" = c("Ferrari . L", "Ferrari ", "Ferrari -", "Ferrari V", "Ferrari H"),
                               "Maserati" = c("Maserati AGCM", "Maserati AGCM-", "Maserati CL","Maserati F"),
                               "Vanwall" = c("Vanwall .", "Vanwall . L" ),
                               "Simca-Gordini" = c("Simca-Gordini ", "Simca-Gordini C", "Simca S"),
                               "Bristol" = c("Bristol ", "Bristol BS"),
                               "BRM" = c("BRM . VC"),
                               "Mercedes" = c("Mercedes M"))

circuits <- qual_df |> 
  select(Date, circuit, Time, Kph) |> 
  mutate(circuit_length = Time*Kph/3600) |> # Estimate the length for each driver
  group_by(circuit) |> 
  summarise(circuit_length = mean(circuit_length, na.rm = T)) |>  # Get the length of each circuit
  filter(!is.na(circuit))

circuits[circuits$circuit == "Pedralbes Circuit",2] <- 6.333
circuits[circuits$circuit == "Aintree Motor Racing Circuit",2] <- 4.828
circuits[circuits$circuit == "Rouen-Les-Essarts",2] <- 5.543

qual_df_circ <- qual_df |> 
  left_join(circuits, by = "circuit") |> 
  mutate(Kph = ifelse(is.na(Kph), 3600*circuit_length/Time, Kph),
         Time = ifelse(is.na(Time), circuit_length/Kph*3600, Time)) |> 
  filter(!is.na(Pos) & !is.na(ENGINE))

qual_df_circ$Team <- fct_collapse(qual_df_circ$Team, 
                                  "Ferrari" = c("Scuderia Ferrari", "Ferrari"),
                                  "Enrico Plate" = c("Enrico Plate", "Scuderia Enrico Platé"),
                                  "Vandervell" = c("G A Vandervell", "Vandervell Products Ltd", 
                                                   "Vandervell Products"))

qual_df_circ <- qual_df_circ |> 
  mutate(ENGINE = factor(ifelse(ENGINE == "" & Team == "Escuderia Bandeirantes", 
                                "Maserati", as.character(ENGINE)))) |> 
  mutate(ENGINE = factor(ifelse(ENGINE == "" & Team == "Enrico Plate", 
                                "Plat&eacute", as.character(ENGINE)))) 

data_for_model <- qual_df_circ |> 
  select(Date, year, gp_name, circuit, Pos, Driver, Team, ENGINE, Time, Kph, race_pos,
         exp, circuit_length, best_lap, points) |> 
  mutate(points = ifelse(is.na(points), 0, points)) |> 
  mutate(race_pos = ifelse(is.na(race_pos), 99, race_pos)) |>
  drop_na()

data_for_model$Team <- factor(data_for_model$Team, levels = unique(data_for_model$Team))
data_for_model$ENGINE <- factor(data_for_model$ENGINE, levels = unique(data_for_model$ENGINE))
data_for_model$Driver <- factor(data_for_model$Driver, levels = unique(data_for_model$Driver))

# Adding world drivers championship points
data_for_model <- data_for_model |> 
  mutate(race_points = case_when(race_pos == 1 ~ 8,
                                 race_pos == 2 ~ 6,
                                 race_pos == 3 ~ 4, 
                                 race_pos == 4 ~ 3,
                                 race_pos == 5 ~ 2, 
                                 race_pos > 5 ~ 0)) |> 
  mutate(race_points = ifelse(best_lap == 1, race_points + 1, race_points)) |> 
  group_by(Driver, year) |> 
  mutate(wdc_points = cumsum(race_points)) |> 
  mutate(wdc_points_shift = lag(wdc_points)) |> 
  mutate(wdc_points_shift = ifelse(is.na(wdc_points_shift), 0, wdc_points_shift))

# Changing the team variable
data_for_model <- data_for_model |> 
  group_by(Team) |> 
  mutate(team_appear = n()) |> 
  group_by(Team) |> 
  mutate(count_drivers = n_distinct(Driver)) |> # Call it solo if a team only has one driver
  mutate(new_team = factor(case_when(count_drivers == 1 ~ "Solo", 
                                     team_appear < 10 ~ "Other", # Other if the team appears less than 10
                                     TRUE ~ as.character(Team)))) |> 
  select(-c(team_appear, count_drivers))

# Changing the engine variable (call it other when it appears less than 10 times)
data_for_model <- data_for_model |> 
  group_by(ENGINE) |> 
  mutate(eng_appear = n()) |> 
  mutate(new_engine = factor(ifelse(eng_appear < 10, "other", as.character(ENGINE)))) |> 
  select(-eng_appear)

data_for_model <- data_for_model |> 
  mutate(team_index = as.integer(new_team), 
         driver_index = as.integer(Driver),
         engine_index = as.integer(new_engine),
         race_index = as.integer(factor(Date))) |> 
  arrange(race_index) |> 
  select(-c(Team, ENGINE, best_lap))

write.csv(data_for_model, "../data/data_for_model.csv")