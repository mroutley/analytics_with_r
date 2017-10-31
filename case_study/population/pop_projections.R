# Checks if machine has required packages to run script ------------------

packages <- c("tidyverse", "readxl")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
library(tidyverse)

# Download and import -----------------------------------------------------

url <- "http://www.fin.gov.on.ca/en/economy/demographics/projections/table6.xlsx"
filename <- basename(url)
download.file(url, destfile = filename, mode = "wb")
# Read in the Excel file, starting on row 5, into a dataframe 
# and convert from wide with years as columns 
# to long with age, year, and count columns
projections <- readxl::read_xlsx(path = filename, skip = 4) %>% 
  tidyr::gather(Year, Count, -Age)
projections

# Reformat data -----------------------------------------------------------

projections <- projections %>%
  filter(!is.na(Age), !is.na(Count)) %>% # Drop blank and notes rows
  # Convert 90+ to just 90
  mutate(Age = stringr::str_replace(Age, "90\\+", "90")) %>%
  mutate(Age = as.integer(Age), Year = as.integer(Year)) %>%
  group_by(Year, Age)
projections

# Create demographic series -----------------------------------------------

child_threshold <- 18
senior_threshold <- 65

demographics <- projections %>%
  mutate(School_Age = ifelse(Age < child_threshold, 1, 0) * Count,
         Total_Age = ifelse(Age, 1, 0) * Count,
         Senior_Age = ifelse(Age > senior_threshold, 1, 0) * Count) %>%
  group_by(Year) %>% 
  summarise_each(funs(sum), contains("_Age")) %>%
  mutate(Child_Dependency = School_Age/Total_Age,
         Senior_Dependency = Senior_Age/Total_Age) %>% 
  select(Year, Child_Dependency, Senior_Dependency) %>% 
  tidyr::gather(Type, Ratio, -Year)
demographics

# Plot dependency ratios --------------------------------------------------

library(ggplot2)
p <- ggplot(demographics, aes(x = Year, y = Ratio, group = Type, colour = Type))
p + geom_line() + labs(title = "Projected dependency ratios")

# Statistical model -------------------------------------------------------

model <- lm(Ratio ~ Type + Year, data = demographics)
summary(model)
model_interaction <- update(model, . ~ Type*Year)
summary(model_interaction)
