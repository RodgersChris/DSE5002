library(dplyr)
library(tidyr)
library(ggplot2)

#read data
salaries_df <- read.csv("C:/Users/crodg/Documents/DSE5002/DSE5002/Week 5/r project data.csv")

#filter FT employees
salaries_df_ft <- salaries_df %>%
  filter(employment_type == "FT")

employees_ft <- salaries_df[salaries_df$employment_type == 'FT',]

# US employees
us_data <- salaries_df[salaries_df$employee_residence == 'US',]

#US companies
salaries_us <- salaries_df %>%
  filter(company_location == 'US')
salaries_us_ft <- salaries_df_ft %>%
  filter(company_location == 'US')

summary(salaries_us_ft)


#US based salaries by company size
company_sz_salary <- tapply(us_data$salary_in_usd, us_data$company_size, mean)

#plot mean salary by company size US
ggplot(us_data,
       aes(x = company_size, y= salary_in_usd, fill= company_size)) +
  geom_bar(stat = 'summary', fun= 'mean')+
  labs(x = "Company Size",
       y = 'Salary',
       title = 'Mean Salary by Company Size US')

#FT Data Scientist in US
data_scientist_us <- salaries_df %>%
  filter(employee_residence == "US",
         employment_type == "FT",
         job_title == "Data Scientist")
#FT mean salary data scientist in us across company size
mean_salary_ds_comp_size <- data_scientist_us %>%
  group_by(company_size) %>%
  summarise(mean_salary_usd = mean(salary_in_usd, na.rm = TRUE))
print(mean_salary_ds_comp_size)
#plot mean salary of DS across US company size
ggplot(mean_salary_ds_comp_size,
       aes(x = company_size, y = mean_salary_usd)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Salary of FT Data Scientist Across US Company Size",
       x = "Company Size",
       y = "Mean Salary (USD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust =1))
#means salary for FT data scientist by experience level
mean_salary_by_exp_lvl <- data_scientist_us %>%
  group_by(experience_level) %>%
  summarise(mean_salary_usd = mean(salary_in_usd, na.rm = TRUE))
#plot mean salary FT DS by exp lvl in US company
ggplot(mean_salary_by_exp_lvl,
       aes(x = experience_level, y = mean_salary_usd)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Mean Salary of FT Data Scientist in US by Experience Level",
       x = "Experience Level",
       y = "Mean Salary (USD)")

#overall mean salary across those who work in US by experience level and company size
us_ft_data <- salaries_df %>%
  filter(employee_residence == "US", employment_type == "FT")
mean_sal_by_exp_size <- us_ft_data %>%
  group_by(experience_level, company_size)
mean_salary_size_exp <- mean_sal_by_exp_size %>%
  summarise(mean_usd_salary = mean(salary_in_usd), .groups = "drop")
print(mean_salary_size_exp)
#plot mean salary by FT data scientist in US by EXP and Company Size
ggplot(mean_salary_size_exp,
       aes(x = experience_level, y = mean_usd_salary, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Experience Level", y = "Mean Salary (USD)", fill = "Company Size") +
  ggtitle("Mean Salary of Full-Time Data Scientist in US by Experience Level and Company Size") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
#adjusted salary based on 10% increase due to rapidly expanding, competitive market due to recession
filtered_data <- salaries_df %>%
  filter(employee_residence == "US", 
         employment_type == "FT", 
         company_size == "S")
summary(filtered_data$salary_in_usd)
adjusted_salaries <- filtered_data$salary_in_usd * 1.10
#plot
ggplot(data = NULL, aes(x = adjusted_salaries)) +
  geom_histogram(binwidth = 10000, fill = "skyblue", color = "black") +
  labs(title = "Adjusted Salary Distribution for US-based Data Scientists in Small Companies",
       x = "Adjusted Salary (USD)",
       y = "Frequency")