library(readr)
library(tidyverse)
library(ggplot2)

lab_sodra_data <- read_csv("../data/lab_sodra.csv")

filtered_data <- lab_sodra_data %>%
  filter(ecoActCode == '862300')

# Task 1
p1 = filtered_data %>%
  ggplot(aes(x = avgWage)) + 
  geom_histogram(binwidth = 50) + 
  labs(title = "The average wage of employees")
ggsave(filename = "../img/plot1.png", plot = p1, width = 12, height = 6)

# Task 2
top_5_companies = filtered_data %>%
  group_by(name) %>%
  summarize(avg_wage = mean(avgWage)) %>%
  arrange(desc(avg_wage)) %>%
  head(5)

top_5_companies_data = filtered_data %>%
  filter(name %in% top_5_companies$name)

p2 <- top_5_companies_data %>%
  ggplot(aes(x = month, y = avgWage, color = name)) +
  geom_line(size = 0.75) +
  labs(title = "Average salary of the top five companies")
ggsave(filename = "../img/plot2.png", plot = p2, width = 12, height = 6)


# Task 3
top_5_companies_employees = top_5_companies_data %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE)

p3 = ggplot(top_5_companies_employees, aes(x = reorder(name, -numInsured), y = numInsured), color = name) + 
  geom_col(aes(fill = name)) +
  labs(title = "Number of insured employees", x = "company", y = "amount") +
  scale_x_discrete(guide = guide_axis(n.dodge=2))
ggsave(filename = "../img/plot3.png", plot = p3, width = 12, height = 6)

