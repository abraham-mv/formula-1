# Author: Abraham Morales

# This code is for scrapping the FIA website to obtain the data that will be used in the project. 
#It's still a work in progress, don't run it lightly it might download a bunch of old F1 datasets!

library(tidyverse)
library(rvest)
library(janitor)
library(xml2)
library(stringr)

link_fia <- "https://fiaresultsandstatistics.motorsportstats.com"
link_seasons <- "https://fiaresultsandstatistics.motorsportstats.com/series/formula-one/season/"

tibble_builder <- function(link, year, cols, column_names, include_date = F){
  # Function to create a tibble out of an html table from formula 1
  page <- read_html(link)
  table_text <- page |> 
    html_nodes("td") |> 
    html_text()
  
  tibble_built <- as_tibble(matrix(table_text, ncol = cols, byrow = T))
  colnames(tibble_built) <- column_names
  
  # Extracting the name of the grand prix out of the link 
  gp_name <- str_extract(link, paste0("(?<=/results/",year,"-).*"))
  gp_name <- gsub("\\/classific.*", "", gp_name)
  #gp_name <- word_remove(gp_name, c("/classification*"))
  
  # Extracting the date the grand prix was held on
  if (include_date){
    link_new <- word_remove(link, c("classification"))
    date <- get_date(link_new)
    # Extracting the circuit the gp was held on
    circuit <- get_circuit(link_new)  
    tibble_built$date <- rep(date, dim(tibble_built)[1])
    tibble_built$circuit <- rep(circuit, dim(tibble_built)[1])
  }
  
  
  tibble_built$gp_name <- rep(gp_name, dim(tibble_built)[1])
  tibble_built$year <- rep(year, dim(tibble_built)[1])
  return(tibble_built)
}

get_gp_links <- function(link){
  page <- read_html(link)
  nodes <- page |> 
    html_nodes(xpath = "//td/a") 
  hrefs <- nodes |> 
    html_attr("href")
  
  gps <- hrefs[grep("/results/", hrefs)]
  
  link_list <- array()
  for (i in 1:length(gps)){
    link_list[i] <- paste0(link_fia, gps[i])
  }
  return(unique(link_list))
}

get_link <- function(link, str){
  page <- read_html(link)
  hrefs <- page |> 
    html_nodes(xpath = paste0("//div/a[text() = '",str,"']")) |> 
    html_attr("href")
  return(paste0(link_fia, hrefs))
}

word_remove <- function(str, words){
  x <- unlist(strsplit(str, "/"))
  x <- x[!x %in% words]
  return(paste(x, collapse = "/")  )
}

tibble_concat <- function(links, year, cols, column_names, name, 
                          write_to_csv = F, include_date = F){
  
  tibble_race <- tibble_builder(links[[1]], year, cols, column_names, include_date)
  #print(dim(tibble_race))
  for (i in 2:length(links)){
    tibble_temp <- tibble_builder(links[[i]], year, cols, column_names, include_date)
    #print(dim(tibble_temp))
    tibble_race <- rbind(tibble_race, tibble_temp)
    #print(dim(tibble_race))
  }
  if (write_to_csv){
    directory <- paste0("data/",name)
    if (!dir.exists(directory)){
      dir.create(directory, recursive = TRUE)
    }
    write.csv(tibble_race, paste0("data/",name,"/",name,"_",year,".csv"))
  }
  return(tibble_race)
}

get_date <- function(link){
  date <- read_html(link) |> 
    html_nodes(xpath = "//div[@class='_3G-GS']") |> 
    html_text()
  return(date[1])
}

get_circuit <- function(link){
  circuit <- read_html(link) |> 
    html_nodes(xpath = "//div[@class='_3G-GS']/a") |> 
    html_text()
  return(circuit)
}


# Download the races datasets for different years
years <- 1950:1960
seasons <- sapply(years, function(x)paste0(link_seasons, as.character(x)))
write_to_csv <- TRUE
column_names <- c("Pos", "No", "Driver", "Nat", "Team", "Laps", 
                  "Time", "Gap", "Interval", "Kph", "Best", "Lap")

gps <- get_gp_links(seasons[1])
races_df <- tibble_concat(gps, 1950, 12, column_names, "race", include_date = T,
                          write_to_csv = write_to_csv)
for (j in 2:length(years)){
  gps <- get_gp_links(seasons[j])
  temp_df <- tibble_concat(gps, years[j], 12, column_names, "race", include_date = T,
                           write_to_csv = write_to_csv)
  races_df <- rbind(races_df, temp_df)
  print(paste0("Downloaded races from: ", years[j]))
}

# Entries for different years and grand prix
write_to_csv <- TRUE
columns <- c("#", "Driver", "Nat", "Team", "Car", "Engine")
for (j in 1:length(years)){
  gps <- get_gp_links(seasons[j])
  entry_links <- lapply(gps, function(x)word_remove(x, c("classification")))
  
  tibble_temp <- tibble_concat(entry_links, year[j], 12, columns, "entries", 
                               write_to_csv = T)
  print(paste0("Downloaded entries from: ", years[j]))
}