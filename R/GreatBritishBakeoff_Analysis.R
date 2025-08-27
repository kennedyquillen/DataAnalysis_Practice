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

# Carry information about viewership and ratings from 'episodes' to larger data set
# Ensure Season and Episode are characters for joining
library(dplyr)

# Ensure Season and Episode are character
dat_clean <- dat_clean %>%
    mutate(
        Season = as.character(Season),
        Episode = as.character(Episode)
    )

# Create lookup table from episodes
episodes_lookup <- episodes %>%
    select(Season, Episode, MyRating..out.of.10., MyViewership) %>%
    distinct() %>%
    mutate(
        Season = as.character(Season),
        Episode = as.character(Episode)
    )

# Fill missing values in dat_clean based on Season + Episode
dat_clean <- dat_clean %>%
    rowwise() %>%
    mutate(
        MyRating..out.of.10. = ifelse(
            is.na(MyRating..out.of.10.),
            episodes_lookup$MyRating..out.of.10.[
                episodes_lookup$Season == Season & episodes_lookup$Episode == Episode
            ],
            MyRating..out.of.10.
        ),
        MyViewership = ifelse(
            is.na(MyViewership),
            episodes_lookup$MyViewership[
                episodes_lookup$Season == Season & episodes_lookup$Episode == Episode
            ],
            MyViewership
        )
    ) %>%
    ungroup()

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
        axis.text.x = element_text(angle = 90, hjust = 1),
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
    plot = showstoppers_by_category,
    width = 10,
    height = 6,
    dpi = 300
)

# Run the statistical analysis for: Which types of Signature bakes most often 
# lead to a Showstopper performance?

# Install and load required packages
install.packages("logistf")
library(logistf)
library(broom)

# Step 1: Prepare data
dat_showstopper_clean <- dat_showstopper %>%
    filter(!is.na(Category) & !is.na(Showstopper_binary)) %>%
    mutate(Category = factor(Category))  # make sure Category is a factor

# Step 2: Fit Firth logistic regression
# Showstopper_binary (1 = yes, 0 = no) predicted by bake Category
firth_model <- logistf(Showstopper_binary ~ Category, data = dat_showstopper_clean)

# Step 3: View model summary
firth_summary <- summary(firth_model)

# Step 4: Extract a clean summary table
summary_table <- data.frame(
    Term = names(firth_model$coefficients),
    Estimate = firth_model$coefficients,
    Std_Error = sqrt(diag(firth_summary$var)),
    Z_value = firth_model$coefficients / sqrt(diag(firth_summary$var)),
    P_value = 2 * pnorm(-abs(firth_model$coefficients / sqrt(diag(firth_summary$var)))),
    Odds_Ratio = exp(firth_model$coefficients),
    CI_Lower = exp(firth_model$ci.lower),
    CI_Upper = exp(firth_model$ci.upper)
)

# Step 5: View the table
print(summary_table)

# Term
# (Intercept)                                                                   (Intercept)
# CategoryBreads & Loaves                                           CategoryBreads & Loaves
# CategoryCakes                                                               CategoryCakes
# CategoryEclairs, Mini Rolls & Choux Variants CategoryEclairs, Mini Rolls & Choux Variants
# CategoryPastries & Choux                                         CategoryPastries & Choux
# CategoryPies, Tarts & Galettes                             CategoryPies, Tarts & Galettes
# CategoryPuddings & Steamed Desserts                   CategoryPuddings & Steamed Desserts
# CategorySeasonal & Themed Creations                   CategorySeasonal & Themed Creations
# CategorySpecialty Regional & Savory Bakes       CategorySpecialty Regional & Savory Bakes
# CategoryTrifles, Mousses & Custards                   CategoryTrifles, Mousses & Custards
# Estimate Std_Error
# (Intercept)                                   5.27299956  1.417835
# CategoryBreads & Loaves                      -0.60017072  2.007224
# CategoryCakes                                 0.56473089  2.004018
# CategoryEclairs, Mini Rolls & Choux Variants -1.90570373  2.019708
# CategoryPastries & Choux                     -1.71765150  2.016779
# CategoryPies, Tarts & Galettes                0.05971923  2.004973
# CategoryPuddings & Steamed Desserts          -3.07577498  2.057299
# CategorySeasonal & Themed Creations           1.17430630  2.003354
# CategorySpecialty Regional & Savory Bakes    -1.38117926  2.012728
# CategoryTrifles, Mousses & Custards          -0.87855040  2.008718
# Z_value      P_value
# (Intercept)                                   3.71904991 0.0001999735
# CategoryBreads & Loaves                      -0.29900536 0.7649359546
# CategoryCakes                                 0.28179934 0.7780973781
# CategoryEclairs, Mini Rolls & Choux Variants -0.94355391 0.3453976562
# CategoryPastries & Choux                     -0.85168039 0.3943915059
# CategoryPies, Tarts & Galettes                0.02978555 0.9762380831
# CategoryPuddings & Steamed Desserts          -1.49505501 0.1349000859
# CategorySeasonal & Themed Creations           0.58617024 0.5577611173
# CategorySpecialty Regional & Savory Bakes    -0.68622262 0.4925727375
# CategoryTrifles, Mousses & Custards          -0.43736872 0.6618439621
# Odds_Ratio     CI_Lower
# (Intercept)                                  195.00000000 2.846055e+01
# CategoryBreads & Loaves                        0.54871795 2.954949e-03
# CategoryCakes                                  1.75897436 9.502701e-03
# CategoryEclairs, Mini Rolls & Choux Variants   0.14871795 7.908352e-04
# CategoryPastries & Choux                       0.17948718 9.573099e-04
# CategoryPies, Tarts & Galettes                 1.06153846 5.729422e-03
# CategoryPuddings & Steamed Desserts            0.04615385 2.358514e-04
# CategorySeasonal & Themed Creations            3.23589744 1.749317e-02
# CategorySpecialty Regional & Savory Bakes      0.25128205 1.345745e-03
# CategoryTrifles, Mousses & Custards            0.41538462 2.233583e-03
# CI_Upper
# (Intercept)                                  24564.046562
# CategoryBreads & Loaves                        101.891923
# CategoryCakes                                  325.592562
# CategoryEclairs, Mini Rolls & Choux Variants    27.956069
# CategoryPastries & Choux                        33.643595
# CategoryPies, Tarts & Galettes                 196.680400
# CategoryPuddings & Steamed Desserts              8.995559
# CategorySeasonal & Themed Creations            598.582913
# CategorySpecialty Regional & Savory Bakes       46.914275
# CategoryTrifles, Mousses & Custards             77.246847

# INTERPRETATION: 
# For Question 1a, we examined which types of Signature Bakes are most likely to 
# lead to a Showstopper performance. The Firth logistic regression results indicate 
# that some categories, such as Cakes and Seasonal & Themed Creations, tend to 
# have higher odds of producing Showstoppers, while categories like Puddings & 
# Steamed Desserts or Eclairs, Mini Rolls & Choux Variants tend to have lower 
# odds. For example, the odds ratio for Cakes is approximately 1.76, suggesting 
# they may be more likely than the baseline category to result in a Showstopper, 
# whereas the odds ratio for Puddings & Steamed Desserts is very low at around 
# 0.046, suggesting a lower likelihood. However, the confidence intervals for 
# most categories are extremely wide, reflecting high variability and limited data 
# within some groups. Additionally, the majority of p-values are large, indicating 
# that none of the categories are statistically significant predictors of a 
# Showstopper. Overall, while some trends are observable, no single category 
# clearly predicts a Showstopper performance.

### Question 1b - Does Technical ranking predict whether a baker becomes Star Baker in the same episode? ----

# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)

# Step 1: Clean data: remove NAs and empty strings
dat_star <- dat_star %>%
    rowwise() %>%
    mutate(
        # Clean Star.Baker column
        Star_Baker_binary = {
            vals <- unlist(strsplit(Star.Baker, ";"))
            vals <- trimws(vals)
            nums <- as.numeric(vals)
            nums <- nums[nums %in% c(0,1)]
            if(length(nums) > 0) nums[1] else NA_real_
        },
        # Clean Rank.in.Technical column
        Rank.in.Technical = as.numeric(Rank.in.Technical)
    ) %>%
    ungroup() %>%
    filter(!is.na(Star_Baker_binary) & !is.na(Rank.in.Technical)) %>%  # remove NAs
    mutate(Star_Baker = factor(Star_Baker_binary, levels = c(0,1), labels = c("No","Yes")))

# Step 2: Plot
starbaker_by_technicalrank <- ggplot(dat_star, aes(x = Star_Baker, y = Rank.in.Technical, fill = Star_Baker)) +
    geom_boxplot() +
    scale_fill_viridis_d(option = "viridis") +
    scale_y_reverse() +  # rank 1 = best
    labs(
        x = "Star Baker",
        y = "Technical Ranking (1 = Best)",
        title = "Technical Ranking vs. Becoming Star Baker",
        fill = "Star Baker"
    ) +
    theme_minimal() +
    theme(
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank()
    )

# Save the plot
ggsave(
    filename = "outputs/figures/starbaker_by_technicalrank.png",
    plot = starbaker_by_technicalrank,
    width = 10,
    height = 6,
    dpi = 300
)

# Fit logistic regression
logit_model <- glm(Star_Baker ~ Rank.in.Technical, 
                   data = dat_star, 
                   family = binomial)

# Extract coefficients and compute odds ratios
summary_table <- summary(logit_model)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Term") %>%
    dplyr::rename(
        Estimate = Estimate,
        Std_Error = `Std. Error`,
        Z_value = `z value`,
        P_value = `Pr(>|z|)`
    ) %>%
    dplyr::mutate(
        Odds_Ratio = exp(Estimate),
        CI_Lower = exp(Estimate - 1.96 * Std_Error),
        CI_Upper = exp(Estimate + 1.96 * Std_Error)
    )

# View table
summary_table

#                Term   Estimate  Std_Error  Z_value      P_value
# 1       (Intercept) -0.3537897 0.20431140 -1.73162 8.334126e-02
# 2 Rank.in.Technical -0.4133567 0.05680692 -7.27652 3.425418e-13
# Odds_Ratio  CI_Lower  CI_Upper
# 1  0.7020226 0.4703679 1.0477664
# 2  0.6614263 0.5917339 0.7393269

# Estimate = log odds change per 1-unit increase in Rank.in.Technical.
# 
# Odds_Ratio = multiplicative change in odds per unit increase.
# 
# CI_Lower / CI_Upper = 95% confidence interval for the odds ratio.
# 
# A negative estimate for Rank.in.Technical indicates better technical rank 
# increases the chance of Star Baker.

# INTERPRETATION:
# Bakers who do better in the technical challenge (lower rank numbers) are much 
# more likely to be named Star Baker in that episode. For each step worse in their 
# technical ranking, their chances of becoming Star Baker drop by about 34%. This 
# shows that performing well in the technical challenge is a strong predictor of 
# winning Star Baker. The confidence interval (0.59–0.74) confirms that this 
# effect is reliable.

### Question 1c - Which bake categories or techniques consistently achieve the highest average MyRating..out.of.10? ----

# Load libraries
library(dplyr)

# Convert to numeric
dat_showstopper_clean <- dat_showstopper_clean %>%
    mutate(MyRating..out.of.10. = as.numeric(as.character(MyRating..out.of.10.)))

# Group by bake category or technique and calculate average rating
avg_ratings <- dat_showstopper_clean %>%
    group_by(Category) %>%  
    summarise(
        avg_rating = mean(MyRating..out.of.10., na.rm = TRUE),
        n = n()
    ) %>%
    arrange(desc(avg_rating))

# View the results
print(avg_ratings)

# Visualize with barplot
library(ggplot2)
library(viridis)

ratings_by_category <- ggplot(avg_ratings, aes(x = reorder(Category, avg_rating), y = avg_rating, fill = avg_rating)) +
    geom_col() +
    scale_fill_viridis_c(option = "viridis") +
    labs(
        title = "Average Rating by Bake Category",
        x = "Bake Category",
        y = "Average Rating (out of 10)",
        fill = "Average Rating"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.title = element_text(face = "bold"),
        panel.grid = element_blank()
    )

print(ratings_by_category)

#Save the plot
ggsave(
    filename = "outputs/figures/ratings_by_category.png",
    plot = ratings_by_category,
    width = 10,
    height = 6,
    dpi = 300
)
