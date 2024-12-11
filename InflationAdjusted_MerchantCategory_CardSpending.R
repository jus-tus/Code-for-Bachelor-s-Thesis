# ---
# title: "Inflation Adjustment for Payments by Merchant Category"
# author: "Justus Felkel"
# date: "12.12.24"
# ---

# load packages 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(lubridate)
library(gridExtra)

#setting the working directory
#setwd("yourworkingdirectoy")

######################################################################################################################
#MAPPING CATEGORIES TO THEIR MCC CODES and LATER TO COICOP
################################################################################################################################

#loading relevant datasets
ourcat_to_mcc <- read_csv("mcc_to_noga.csv")
mcc_to_coicop <- read_csv("mcc_to_coicop.csv")

#merge the dataframes
merged_merchant_data <- left_join(ourcat_to_mcc, mcc_to_coicop, by = "MCC")

#group by custom category and summarize
coicop_summary <- merged_merchant_data %>%
  group_by(our_category) %>%
  summarize(COICOP = paste(unique(na.omit(Categorization)), collapse = ", "), .groups = 'drop')


#remove cash from our_category as this doesn't exist in our dataset & remove all non-number objects from COICOP, as COICOP categories are only numeral
coicop_summary1 <- coicop_summary %>%
     filter(our_category != 'Cash')

coicop_summary1$COICOP <- gsub("[^0-9,]", "", coicop_summary1$COICOP) #filter for only numeric values
coicop_summary1$COICOP <- gsub(",+", ",", coicop_summary1$COICOP)  #remove multiple commas with a single one
coicop_summary1$COICOP <- trimws(coicop_summary1$COICOP, which = "both")  #remove any whitespaces
coicop_summary1$COICOP <- gsub("(^,)|(,$)", "", coicop_summary1$COICOP) #remove any commas in the front and back

#remove "our_category" categories Motor Vehicles & Financial Services as they aren't included in the consumption data
coicop_summary2 <- coicop_summary1 %>%
  filter(!our_category %in% c("Financial Services", "Motor Vehicles"))

#rename some categories in "merchant_category" so that they're the same as in the consumption dataset
coicop_summary3 <- coicop_summary2 %>%
  mutate(our_category = recode(our_category, "Accomodation" = "Accommodation", "Entertainment & Sports" = "Entertainment and sports", "Food & Beverage Services" = "Food and beverage services", "Human Health Services" = "Human health services", "Personal Services" = "Personal services", "Professional Services" = "Professional services", "Transport Services" = "Transport services"))

######################################################################################################################
#Averaging the inflation for each respective consumption category (from BFS data) from before to receive inflation levels for each consumption our_category
################################################################################################################################

#load inflation data from the BFS
cpi_merchant1 <- read_delim("CPI Merchant Categories.csv", delim = ";")
#view(cpi_merchant1)

#reshape COICOP codes into long format for better merging
coicop_long <- coicop_summary3 %>%
  mutate(COICOP = strsplit(as.character(COICOP), ",")) %>%
  unnest(COICOP) %>%
  mutate(COICOP = trimws(COICOP))

#preparing cpi_merchant1 for averaging
cpi_long <- cpi_merchant1 %>%
  pivot_longer(cols = -date, names_to = "COICOP", values_to = "CPI") %>%
  mutate(COICOP = trimws(COICOP),
         CPI = as.numeric(CPI))

#merging cpimerchant with coicop
merged_data <- cpi_long %>%
  inner_join(coicop_long, by = "COICOP", multiple = "all", relationship = "many-to-many")

#calculating average inflation for each our_category for the respective date
average_inflation1 <- merged_data %>%
  group_by(our_category, date) %>%
  summarize(average_inflation1 = mean(CPI, na.rm = TRUE), .groups = 'drop')

#the dates are are rewritten in date format
average_inflation1$date <- as.Date(paste0("01-", average_inflation1$date), format = "%d-%b %y") #convert date to actual date format

#sort the dataset by date
average_inflation2 <- average_inflation1 %>%
  arrange(date)

#rename column "our_category" to "merchant_category"
average_inflation3 <- rename_with(average_inflation2, recode, "our_category" = "merchant_category")

#create a column for year and month each
average_inflation4 <- average_inflation3 %>%
  mutate(year = year(date),
         month = month(date))

#remove 2018 
average_inflation5 <- average_inflation4 %>%
  filter(!year %in% "2018")

#create a new dataframe that expands the monthly inflation data for each day in the month
expanded_inflation1 <- average_inflation5 %>% 
  group_by(merchant_category, year, month) %>%
  reframe(date = seq.Date(from = date, to = ceiling_date(date, "month") - 1, by = "day"),
            average_inflation1 = average_inflation1)

######################################################################################################################
#Correcting each consumption category values for inflation
################################################################################################################################

# loading the data 
data1 <- read_csv("ACQ_NOGA_CardholderOrigin.csv")

#exclude scaled number of transactions 
data2 <- select(data1, c('Date', 'Cardholder origin', 'Merchant category', 'Scaled Value'))

#rename column names
data2 <- rename_with(data2, recode, "Date" = "date", "Cardholder origin" = "cardholder_origin", "Merchant category" = "merchant_category", "Scaled Value" = "scaled_value")

#filter for domestic cardholders 
data3 <- filter(data2, cardholder_origin == 'Domestic')

#remove column cardholder_origin as all objects are domestic now
data4 <- data3[,-2]

#aggregate "other" category as there are two or more entries for the same date
data5 <- data4 %>%
  group_by(date, merchant_category) %>%
  summarize(total_value = sum(scaled_value))

#create a column for year and month each
data6 <- data5 %>%
  mutate(year = year(date),
         month = month(date))

#now merging them
merged_mercat1 <- data6 %>%
  left_join(expanded_inflation1, by = c("date", "merchant_category"))

#remove columns which are duplicates
merged_mercat2 <- merged_mercat1 %>%
  select(-c(year.y, month.y,year.x,month.x))

#divide CPI by 100 to get numbers which one can later divide by to get real consumption
merged_mercat2$average_inflation1 <- merged_mercat2$average_inflation1 / 100

#correct for inflation now (deflate): by dividing nominal consumption by deflator (1 + inflation rate) to get the real consumption
merged_mercat2$real_value <- merged_mercat2$total_value / merged_mercat2$average_inflation1

#remove average inflation and total_value columns
merged_mercat3 <- merged_mercat2 %>%
  select(-c(total_value, average_inflation1))

##make the dataset wide again so its easier to distinguish between consumption categories
merged_mercat4 <- merged_mercat3 %>%
  pivot_wider(names_from = merchant_category, values_from = real_value) %>%
  arrange(date)


######################################################################################################################
#Weekly: Aggregate the values by week
################################################################################################################################

#define the first Thursday of 2019 as starting date
start_date <- as.Date("2019-01-03")

#creating week_start identifier based on starting date above
merged_mercat5 <- merged_mercat3 %>%
  mutate(
    week_start = start_date + 7 * (as.numeric(difftime(date, start_date, units = "days")) %/% 7)
  )

#remove 1st and 2nd of January 2019 (belonging to last week of 2018)
merged_mercat6 <- merged_mercat5[-(1:22), , drop =FALSE]

#aggregate to weekly using the new week_start identifier
weekly_mercat1 <- merged_mercat6 %>%
  group_by(week_start, merchant_category) %>%
  summarise(
    weekly_real_value = sum(real_value, na.rm = TRUE),
    .groups = 'drop'
  )

#create a week column that resets to 1 for each new year
weekly_mercat2 <- weekly_mercat1 %>%
  mutate(week_number = (as.numeric(difftime(week_start, as.Date("2019-01-03"), units = "weeks")) %/% 1) %% 52 + 1)

#arrange the data in the correct order assuredly
weekly_mercat4 <- weekly_mercat2 %>%
  arrange(week_start)

#create year column
weekly_mercat5 <- weekly_mercat4 %>%
  mutate(
    year = year(week_start),
    year = ifelse(week_number == 1 & month(week_start) == 12, year + 1, year)  #adjust for week 1 of new year
  )

######################################################################################################################
#Weekly: Calculate year-on-year growth
################################################################################################################################

#create a new column with previous year values
weekly_mercat_yoy1 <- weekly_mercat5 %>%
  left_join(weekly_mercat5 %>%
              mutate(year = year + 1),  #increase the year by 1 to get the next year
            by = c("week_number", "year", "merchant_category"),  #join by week number, new yearand merchant category
            suffix = c("", "_prev"))

#create yoy growth column
weekly_mercat_yoy2 <- weekly_mercat_yoy1 %>%
  mutate(
    yoy_change = (weekly_real_value - weekly_real_value_prev) / weekly_real_value_prev * 100)

#remove 2019 as there are no YoY growth rates yet
weekly_mercat_yoy3 <- weekly_mercat_yoy2  %>%
  filter(year != '2019')

#remove columns (to be able to pivot wider too)
weekly_mercat_yoy4 <- weekly_mercat_yoy3 %>%
  select(-c(weekly_real_value,weekly_real_value_prev,week_number,year,week_start_prev))

#rename week_start column to date
weekly_mercat_yoy5 <- rename_with(weekly_mercat_yoy4, recode, "week_start" = "date")


#pivot wide again for better overview
weekly_mercat_yoy6 <- weekly_mercat_yoy5 %>%
  pivot_wider(names_from = merchant_category, values_from = yoy_change) %>%
  arrange(date)

#delete the last row (OCTOBER + NOV)
weekly_mercat_yoy7 <- head(weekly_mercat_yoy6, -5)

#filter for dates starting with June 16, 2022
weekly_mercat_yoy8 <- weekly_mercat_yoy7 %>%
  filter(date >= as.Date("2022-06-02"))

######################################################################################################################
#Weekly: Plotting
################################################################################################################################

#define the breaks shown in graph
breaks_dates <- seq(from = as.Date("2022-06-16"), to = as.Date("2024-09-01"), by = "6 months")

##this one starting with first mp announcement
weekly_mercat_yoy9 <- weekly_mercat_yoy8[-(1:2), ]
weekly_mercat_yoy10 <- head(weekly_mercat_yoy9, -2)

#PLOT: Retail: Food, beverage, tobacco
plot_retfoo <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Retail: Food, beverage, tobacco`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Food, Beverage & Tob.", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Retail: Fuel stations
plot_retfue <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Retail: Fuel stations`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Fuel Stations", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Retail: Other goods
plot_retoth <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Retail: Other goods`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Other Goods", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Accommodation
plot_acc <- ggplot(weekly_mercat_yoy10, aes(x = date, y = Accommodation)) +
  geom_line(color = "black") +
  labs(title = "Accommodation", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Entertainment and sports
plot_ent <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Entertainment and sports`)) +
  geom_line(color = "black") +
  labs(title = "Entertainment and Sports", x = "Date", y = "YOY Growth Rate [%]")  +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angle x-axis text for better readability
    panel.grid.minor.x = element_blank()  # Remove minor x-axis grid lines
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Food and beverage services
plot_foo <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Food and beverage services`)) +
  geom_line(color = "black") +
  labs(title = "Food and Beverage Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Human health services
plot_hum <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Human health services`)) +
  geom_line(color = "black") +
  labs(title = "Human Health Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Personal services
plot_per <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Personal services`)) +
  geom_line(color = "black") +
  labs(title = "Personal Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Professional services
plot_pro <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Professional services`)) +
  geom_line(color = "black") +
  labs(title = "Professional Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Transport services
plot_tra <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Transport services`)) +
  geom_line(color = "black") +
  labs(title = "Transport Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")
  )

#PLOT: Other
plot_oth <- ggplot(weekly_mercat_yoy10, aes(x = date, y = `Other`)) +
  geom_line(color = "black") +
  labs(title = "Other", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")  
  )

combined_plot <- grid.arrange(
  plot_retfoo,
  plot_retfue, 
  plot_retoth,
  plot_acc,   
  plot_foo,    
  plot_hum,    
  plot_ent,   
  plot_per,   
  plot_pro,    
  plot_tra,    
  plot_oth,
  ncol = 4   
)

#ggsave("combinedplot.png", plot = combined_plot, width = 12, height = 8, dpi = 300)

######################################################################################################################
#Monthly: Aggregate the values by month
################################################################################################################################

#create month and year column
monthly_merchant1 <- merged_mercat3 %>%
  mutate(month = month(date),
         year = year(date))

#aggregate monthly
monthly_merchant2 <- monthly_merchant1 %>%
  arrange(date) %>%
  group_by(month,year,merchant_category) %>%                         
  summarise(monthly_real_totalvalue = sum(real_value, na.rm = TRUE), 
            date = first(date),
            .groups = 'drop')

######################################################################################################################
#Monthly: Calculate year-on-year growth
################################################################################################################################

#create a lagged column for the same month last year
monthly_merchant3 <- monthly_merchant2 %>%
  arrange(date) %>% 
  group_by(merchant_category) %>%   
  mutate(last_year_value = dplyr::lag(monthly_real_totalvalue, 12))

#calculate year-on-year growth
monthly_merchant4 <- monthly_merchant3 %>%
  mutate(yoy_growth = (monthly_real_totalvalue - last_year_value) / last_year_value * 100)

#remove 2019
monthly_merchant5 <- monthly_merchant4 %>%
  filter(year != 2019)

##NOW EXCLUDING FIRST 319 ROWS
#AS ANALYSIS WOULD NOW BE STARTING WITH JUNE 2022 (COVID-19 BEFORE & AFTER EFFECTS; MP DECISION JUNE 2022 FIRST CONSIDERABLE SHOCK)
monthly_merchant6 <- monthly_merchant5[-(1:319), ]

#remove not necessary columns to be able to pivot wider
monthly_merchant7 <- monthly_merchant6 %>%
  select(date,merchant_category,yoy_growth)

#make wide format again
monthly_merchant8 <- monthly_merchant7 %>%
  pivot_wider(names_from = merchant_category, values_from = yoy_growth) %>%
  arrange(date)

#remove last two rows (october and november 2024)
monthly_merchant8 <- head(monthly_merchant8, -2)

######################################################################################################################
#Weekly: Plotting
################################################################################################################################

#PLOT: Retail: Food, beverage, tobacco
plot_retfoo_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Retail: Food, beverage, tobacco`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Food, Beverage & Tob.", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,                
    labels = function(x) format(x, "%m.%Y")
  )

#PLOT: Retail: Fuel stations
plot_retfue_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Retail: Fuel stations`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Fuel Stations", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,                
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Retail: Other goods
plot_retoth_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Retail: Other goods`)) +
  geom_line(color = "black") +
  labs(title = "Retail: Other Goods", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank()
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,                
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Accommodation
plot_acc_mo <- ggplot(monthly_merchant8, aes(x = date, y = Accommodation)) +
  geom_line(color = "black") +
  labs(title = "Accommodation", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,                
    labels = function(x) format(x, "%m.%Y") 
  )

#PLOT: Entertainment and sports
plot_ent_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Entertainment and sports`)) +
  geom_line(color = "black") +
  labs(title = "Entertainment and Sports", x = "Date", y = "YOY Growth Rate [%]")  +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Food and beverage services
plot_foo_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Food and beverage services`)) +
  geom_line(color = "black") +
  labs(title = "Food and Beverage Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Human health services
plot_hum_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Human health services`)) +
  geom_line(color = "black") +
  labs(title = "Human Health Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Personal services
plot_per_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Personal services`)) +
  geom_line(color = "black") +
  labs(title = "Personal Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,                
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Professional services
plot_pro_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Professional services`)) +
  geom_line(color = "black") +
  labs(title = "Professional Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Transport services 
plot_tra_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Transport services`)) +
  geom_line(color = "black") +
  labs(title = "Transport Services", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.minor.x = element_blank() 
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,              
    labels = function(x) format(x, "%m.%Y")  
  )

#PLOT: Other
plot_oth_mo <- ggplot(monthly_merchant8, aes(x = date, y = `Other`)) +
  geom_line(color = "black") +
  labs(title = "Other", x = "Date", y = "YOY Growth Rate [%]") +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.minor.x = element_blank()  
  ) +
  theme_bw() +
  scale_x_date(
    breaks = breaks_dates,               
    labels = function(x) format(x, "%m.%Y")  
  )

combined_plot_month <- grid.arrange(
  plot_retfoo_mo,
  plot_retfue_mo, 
  plot_retoth_mo,
  plot_acc_mo,   
  plot_foo_mo,    
  plot_hum_mo,    
  plot_ent_mo,   
  plot_per_mo,   
  plot_pro_mo,    
  plot_tra_mo,    
  plot_oth_mo,
  ncol = 4   
)

#ggsave("combined_plot_month.png", plot = combined_plot_month, width = 12, height = 8, dpi = 300)
