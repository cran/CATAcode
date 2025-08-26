## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  message  = FALSE,
  warning  = FALSE
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(CATAcode)
library(dplyr)

## ----install, eval=F----------------------------------------------------------
#  install.packages("CATAcode")

## ----dev-install, eval=F------------------------------------------------------
#  devtools::install_github("knickodem/CATAcode")

## ----load, eval=F-------------------------------------------------------------
#  library(CATAcode)

## ----longitudinal-data--------------------------------------------------------
data("sources_race")
head(sources_race)

## ----example-data, echo = TRUE------------------------------------------------

# Creating a cross-sectional dataset (N = 1000)
set.seed(123)  

n_cross = 1000

cross = data.frame(
  ID               = 1:n_cross,
  Funding          = sample(c("No", "Yes"), n_cross, replace = TRUE, prob = c(.15, .85)),
  Mentorship       = sample(c("No", "Yes"), n_cross, replace = TRUE, prob = c(.10, .90)),
  Infrastructure   = sample(c("No", "Yes"), n_cross, replace = TRUE, prob = c(.45, .55)),
  Time_Capacity    = sample(c("No", "Yes"), n_cross, replace = TRUE, prob = c(.25, .75)),
  Other_Barrier    = sample(c("No", "Yes"), n_cross, replace = TRUE, prob = c(.80, .20))
  )

# Display the first few rows of the dataset
head(cross)


## ----include = FALSE, eval=FALSE----------------------------------------------
#  ## cata_prep() does not currently do these but we could add these features
#  3. **Validates** that each id–Category combination is unique per time‑point, missing IDs or categories are flagged early.
#  4. **Adds** those attributes (ID column, time column, endorsement code) as metadata that all other helpers read automatically, keeping the pipeline self‑documenting.

## ----cata_prep, echo = TRUE---------------------------------------------------
# Prepare cross-sectional 
datacross_prep <- cata_prep(data = cross, id = ID, cols = Funding:Other_Barrier, names_to = "Barriers", values_to = "YN")

# Prepare longitudinal 
datalong_prep <- cata_prep(data = sources_race, id = ID, cols = c(Asian, Black:White), time = Wave)

# Display the first few rows of the prepared data
head(datacross_prep)
head(datalong_prep)


## ----all_cross, echo = TRUE---------------------------------------------------
# Explore all combinations in cross-sectional data
cross_all <- cata_code(data = datacross_prep,
                      id = ID,
                      categ = Barriers,
                      resp = YN,
                      approach = "all",
                      endorse = "Yes",
                      new.name = "Combinations",
                      sep = "-")

# Display the result
head(cross_all)
# 
# Count the frequency of each combination
table(cross_all$Combinations)


## ----count_long, echo = TRUE--------------------------------------------------
# Explore all combinations in cross-sectional data
# Get counts across waves
long_counts <- cata_code(data = datalong_prep,
                         id = ID,
                         categ = Category,
                         resp = Response,
                         approach = "counts",
                         endorse = 1)

# Display the result
head(long_counts)

## ----multiple, echo = TRUE----------------------------------------------------
# Apply the "multiple" approach
cross_multiple <- cata_code(data = datacross_prep,
                            id = ID,
                            categ = Barriers,
                            resp = YN,
                            approach = "multiple",
                            endorse = "Yes",
                            new.name = "Barrier",
                            multi.name = "Multiple")

# Display the results
table(cross_multiple$Barrier)

## ----priority, echo = TRUE----------------------------------------------------
# Apply the "priority" approach
cross_priority <- cata_code(data = datacross_prep,
                            id = ID,
                            categ = Barriers,
                            resp = YN,
                            approach = "priority",
                            endorse = "Yes",
                            new.name = "Barrier",
                            multi.name = "Multiple",
                            priority = c("Mentorship", "Infrastructure"))

# Display the results
table(cross_priority$Barrier)

## ----mode, echo = TRUE--------------------------------------------------------
# Apply the "mode" approach
long_mode <- cata_code(data = datalong_prep,
                       id = ID,
                       categ = Category,
                       resp = Response,
                       approach = "mode",
                       endorse = 1,
                       time = Wave,
                       new.name = "Race_Ethnicity",
                       multi.name = "Multiracial")

# Display the results
table(long_mode$Race_Ethnicity)

## ----mode_priority, echo = TRUE-----------------------------------------------
# Combining "mode" with "priority"
long_mode_priority <- cata_code(data = datalong_prep,
                                id = ID,
                                categ = Category,
                                resp = Response,
                                approach = "mode",
                                endorse = 1,
                                time = Wave,
                                new.name = "Race_Ethnicity",
                                multi.name = "Multiracial",
                                priority = c("Black", "Native_American"))

# Display the results
table(long_mode_priority$Race_Ethnicity)

## ----Visualize, echo = TRUE, message = FALSE, warning = FALSE, results='asis', fig.height=3, fig.width=6----
library(ggplot2)

# Get counts from the coded data frames created earlier
counts_multiple = cross_multiple |>
  count(Barrier, name = "Count") |>
  mutate(Approach = "Multiple")

counts_priority = cross_priority |>
  count(Barrier, name = "Count") |>
  mutate(Approach = "Priority")

# Display in a figure
cross_plot = bind_rows(counts_multiple, counts_priority) |>
  ggplot(aes(x = reorder(Barrier, -Count), y = Count,
             fill = Approach)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Multiple = "#1F78B4",
                               Priority  = "#FB9A99")) +
  labs(x = "Barrier", y = "Count",
       title = "Comparing Coding Approaches") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
cross_plot


## ----visualize_long, echo = TRUE, message = FALSE, warning = FALSE, results='asis', fig.height=3, fig.width=6----
library(ggplot2)

# Get counts from the coded data frames created earlier
counts_mode = long_mode|>
  count(Race_Ethnicity, name = "Count") |>
  mutate(Approach = "Mode")

counts_mwp = long_mode_priority |>
  count(Race_Ethnicity, name = "Count") |>
  mutate(Approach = "Mode with Priority")

# Display in a figure
long_plot = bind_rows(counts_mode, counts_mwp) |>
  ggplot(aes(x = reorder(Race_Ethnicity, -Count), y = Count,
             fill = Approach)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(Mode = "#1F78B4",
                               `Mode with Priority`  = "#FB9A99")) +
  labs(x = "Race/Ethnicity", y = "Count",
       title = "Comparing Coding Approaches") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
long_plot


