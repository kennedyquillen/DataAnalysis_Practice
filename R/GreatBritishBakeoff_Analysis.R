# Great British Bake-Off Analysis (By: Kennedy Quillen, August 2025) ----

## Organize the project into folders ----

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
write_xlsx(dat_clean, "data/dat_clean.xlsx")

## Questions for analysis ----
### 1. Bake performance ----
# 
#     Which types of Signature bakes most often lead to a Showstopper performance?
#     
#     Does Technical ranking predict whether a baker becomes Star Baker in the 
#     same episode?
#     
#     Which bake categories or techniques consistently achieve the highest average 
#     MyRating..out.of.10?
#     
### 2. Baker demographics and outcomes ----
# 
#     Does Age or Gender correlate with performance in Technical, Signature, or 
#     Showstopper challenges?
#     
#     Do certain demographics (e.g., gender or age groups) achieve Star Baker more 
#     frequently?
#     
#     Which factors are most predictive of a baker winning the season?
#     
### 3. Episode, season, and theme trends ----
# 
#     Which Seasons had the highest MyViewership?
#     
#     Which Themes attract the most viewers?
#     
#     Are there Themes or challenges that consistently produce high MyRating or 
#     high viewership?
#     
### 4. Safety and long-term performance ----
# 
#     How often does being Safe in an episode correlate with long-term success 
#     (e.g., surviving to later episodes or winning the season)?
#     
### 5. Platform and distribution insights ----
# 
#     How does Network or streaming platform (Netflix.Collection, Roku.Season, 
#     PBS.Season) correlate with viewership?

## Analyses ----

### Question 1a - Which types of Signature bakes most often lead to a Showstopper performance? ----

# Load required libraries
library(dplyr)
library(ggplot2)

# Step 1: Filter and clean data
dat_showstopper <- dat_clean %>%
    filter(!is.na(Signature.Bake) & Signature.Bake != "") %>%  # only keep rows with Signature.Bake
    mutate(
        # Convert Showstopper column to binary: 1 = Showstopper present, 0 = otherwise
        Showstopper_binary = ifelse(!is.na(Showstopper) & Showstopper != "", 1, 0)
    )

# With such a large and diverse list, we can simplify all the signature bakes 
# into 10 broad categories that capture the main types without being overly specific.

# Categorize bakes into new column 'Category'
dat_showstopper <- dat_showstopper %>%
    mutate(Category = case_when(
        grepl("cake|sponge|roulade|Swiss roll|Battenberg|drizzle", Signature.Bake, ignore.case = TRUE) ~ "Cakes",
        grepl("pie|tart|galette|quiche|frangipane", Signature.Bake, ignore.case = TRUE) ~ "Pies, Tarts & Galettes",
        grepl("biscuit|cookie|shortbread|bar|Florentine|biscotti", Signature.Bake, ignore.case = TRUE) ~ "Biscuits, Cookies & Bars",
        grepl("trifle|mousse|custard|pudding|domed tart", Signature.Bake, ignore.case = TRUE) ~ "Trifles, Mousses & Custards",
        grepl("bread|focaccia|grissini|soda", Signature.Bake, ignore.case = TRUE) ~ "Breads & Loaves",
        grepl("croissant|choux|profiterole|vol-au-vent|Danish", Signature.Bake, ignore.case = TRUE) ~ "Pastries & Choux",
        grepl("steamed|spotted dick|clootie|figgy pudding|school pudding", Signature.Bake, ignore.case = TRUE) ~ "Puddings & Steamed Desserts",
        grepl("eclair|mini roll|chouxnut", Signature.Bake, ignore.case = TRUE) ~ "Eclairs, Mini Rolls & Choux Variants",
        grepl("meat pie|pastie|pizza|clanger", Signature.Bake, ignore.case = TRUE) ~ "Specialty Regional & Savory Bakes",
        TRUE ~ "Seasonal & Themed Creations"
    ))

#Ensure the 'Category' column was created
View(dat_showstopper)

# Step 2: Summarize by Category
signature_summary <- dat_showstopper %>%
    group_by(Category) %>%
    summarise(
        total_showstoppers = sum(Showstopper_binary, na.rm = TRUE),
        total_attempts = n(),
        showstopper_rate = total_showstoppers / total_attempts,
        .groups = "drop"
    ) %>%
    arrange(desc(showstopper_rate))  # sort by highest rate

# Step 3: View summary table
print(signature_summary)

# Plot total showstoppers by Category using viridis colorblind friendly colors
library(viridis)

showstoppers_by_category <- ggplot(signature_summary, 
                                   aes(x = reorder(Category, -total_showstoppers), 
                                       y = total_showstoppers, fill = total_showstoppers)) +
    geom_col() +
    scale_fill_viridis_c(option = "viridis") +
    labs(
        title = "Total Showstoppers by Signature Bake",
        x = "Signature Bake Category",
        y = "Total Showstoppers",
        fill = "Total Showstoppers"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank()
    )

# Display the plot
showstoppers_by_category

# Save the plot
ggsave(
    filename = "outputs/figures/showstoppers_by_category.png",
    plot = last_plot(),
    width = 10,
    height = 6,
    dpi = 300
)
