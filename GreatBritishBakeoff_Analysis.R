# Great British Bake-Off Analysis (By: Kennedy Quillen, August 2025) ----

## Organize the project into folders and sub-folders ----

# Download the required package
install.packages('fs')

# Create the project folder structure
fs::dir_create(c("R", "data", "outputs/figures", 
                 "outputs/tables", "outputs/data", 
                 "outputs/models", "resources"))

## Import the data ----
bakers <- read.csv("data/Bakers.csv")
challenges <- read.csv("data/ChallengeBakes.csv")
episodes <- read.csv("data/Episodes.csv")
outcomes <- read.csv("data/Outcomes.csv")
seasons <- read.csv("data/Seasons.csv")

