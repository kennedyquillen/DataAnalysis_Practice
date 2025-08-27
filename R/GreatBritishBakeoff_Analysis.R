# Great British Bake-Off Analysis (By: Kennedy Quillen, August 2025) ----

## Organize the project into folders and sub-folders ----

# Download the required package
install.packages('fs')

# Create the project folder structure
fs::dir_create(c("R", "data", "outputs/figures", 
                 "outputs/tables", "outputs/data", 
                 "outputs/models", "resources"))

## Import the data ----
bakers     <- read.csv("data/Bakers.csv")
challenges <- read.csv("data/ChallengeBakes.csv")
episodes   <- read.csv("data/Episodes.csv")
outcomes   <- read.csv("data/Outcomes.csv")
seasons    <- read.csv("data/Seasons.csv")

# Pull library to enable data frame manipulation
library(dplyr)

# Combine individual data sets into a single file "dat"
dat        <- bind_rows(bakers, challenges, episodes, outcomes, seasons)

# Inspect the data set
View(dat)

## Clean the data ---- 

# Remove columns containing no data 
dat_clean  <- dat %>%
    select(where(~ !all(is.na(.))))

# Remove rows that do not contain a 'Baker' 
dat_clean  <- dat_clean %>%
    filter(!is.na(Baker) & Baker != "")

# Fill in missing 'Season' and 'Episode' data using 'SeasonEpisode' column
library(stringr)

# Fill missing Season from SeasonEpisode
dat_clean <- dat_clean %>%
    mutate(
        Season = ifelse(
            is.na(Season) | Season == "",
            str_extract(SeasonEpisode, "(?<=s)\\d+"),
            Season
        )
    )

# Fill missing Episode from SeasonEpisode
dat_clean <- dat_clean %>%
    mutate(
        Episode = ifelse(
            is.na(Episode) | Episode == "",
            str_extract(SeasonEpisode, "(?<=e)\\d+"),
            Episode
        )
    )

# Now we can delete the column "SeasonEpisode' because it is redundant
dat_clean <- dat_clean %>%
    select(-SeasonEpisode)

# Carry info about baker age and gender throughout the data set
dat_clean  <- dat_clean %>%
    group_by(Baker, Season) %>%
    mutate(
        Age    = first(na.omit(Age)),
        Gender = first(na.omit(Gender))
    ) %>%
    ungroup()

# Now that the info about each Baker has been carried throughout, we can delete 
# "Bakers.csv" from the data set (rows 1-168)
dat_clean  <- dat_clean[-c(1:168), ]

# Combine data so that each unique combination of Season+Episode+Baker has only 
# one row of data
dat_clean <- dat_clean %>%
    group_by(Season, Baker, Episode) %>%
    summarise(
        across(everything(), ~ paste(unique(.), collapse = "; ")),  # combine all other columns
        .groups = "drop"
    )

# Sort the data set by Season + Baker
dat_clean  <- dat_clean %>%
    arrange(as.numeric(Season), Baker)

# View the data set 
View(dat_clean)

# Save dat as an Excel file
install.packages('writexl')
library(writexl)
write_xlsx(dat, "data/dat.xlsx")
