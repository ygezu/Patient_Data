library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(stringr)


setwd("~/Desktop/R (DATA 332)/Patient_Data/Data")

#reading files 
patient <- read_excel('Patient.xlsx', .name_repair = 'universal')
billing <- read_excel('Billing.xlsx', .name_repair = 'universal')
visit <- read_excel('Visit.xlsx', .name_repair = 'universal')

#Left joining the Data sets
patient_visit <- left_join(patient, visit, by = "PatientID")
df <- left_join(patient_visit, billing, by = "VisitID")

#Cleaning the data 
df <- df%>%
  dplyr::mutate(Reason = ifelse((Reason == 'UTI follow-up'), 'UTI',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Rhinitis follow-up'), 'Rhinitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Migraine follow-up'), 'Migraine',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypothyroidism follow-up'), 'Hypothyroidism',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Dermatitis follow-up'), 'Dermatitis',Reason))%>%
  dplyr::mutate(Reason = ifelse(grepl("Cyst", Reason), "Cyst removal", Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Bronchitis follow-up'), 'Bronchitis',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Allergy reaction follow-up'), 'Allergy reaction',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Annual wellness visit - dermatitis'), 'Annual wellness visit',Reason))%>%
  dplyr::mutate(Reason = ifelse(grepl("Laceration", Reason), "Laceration", Reason))%>%
  dplyr::mutate(Reason = ifelse(grepl("Fracture", Reason), "Fracture", Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypertension monitoring'), 'Hypertension',Reason))%>%
  dplyr::mutate(Reason = ifelse((Reason == 'Hypotension monitoring'), 'Hypotension',Reason))%>%
  drop_na()

#1)Reason for visit segmented (stacked bar chart)by month of the year. 

# Extract month from 'Visit_date'
df <- df %>%
  mutate(visit_month = month(VisitDate, label = TRUE))  

#Pivot chart 
reason_bymonth <- df %>%
  group_by(visit_month, Reason) %>%
  summarize(Count = n())

reason_bymonth<-reason_bymonth%>%
  distinct(visit_month,Count,Reason, .keep_all = TRUE) #distincting the months in order


# Creating a stacked bar chart 
ggplot(reason_bymonth, aes(x = Count, y = Reason, fill = visit_month)) +
    geom_bar(stat = "identity") +
    labs(title = "Count of Reasons by Visit Month",
         x = "Reason",
         y = "Count",
         fill = "Visit Month") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2Reason for visit based on walk in or not. 
ggplot(df, aes(x = Reason, fill = WalkIn)) +
  geom_bar(position = "dodge") +
  labs(title = "Reason of Visit Based on walkIn",
       x = "Reason",
       y = "Count",
       fill = "Walk-In") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3Reason for visit based on City/State or zip code
ggplot(df, aes(x = Reason, fill = Zip)) +
  geom_bar(position = "dodge") +
  labs(title = "Reason of Visit Based on Zip Code",
       x = "Reason",
       y = "Count",
       fill = "Zip Code") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4Total invoice amount based on reason for visit. Segmented (stacked bar chart) with it was paid. 

ggplot(df, aes(x = Reason, y = InvoiceAmt, fill = InvoicePaid)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Invoice Amount Based on Reason for Visit (Segmented by Payment Status)",
       x = "Reason for Visit",
       y = "Total Invoice Amount",
       fill = "Payment Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bonus question 
#Showing any trends or patterns in the payment status over the months
ggplot(df, aes(x = visit_month, fill = InvoicePaid)) +
  geom_bar() +
  labs(title = "Trend of Invoice Payment Status Over Months",
       x = "Month",
       y = "Number of Invoices",
       fill = "Payment Status") +
  theme_minimal()


#Showing any correlation between patient age and invoice amount
# Calculate age from BirthDate
df$Age <- as.numeric(difftime(Sys.Date(), df$BirthDate, units = "days") / 365.25)

# Create the scatter plot with trend line
ggplot(df, aes(x = Age, y = InvoiceAmt)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation between Patient Age and Invoice Amount",
       x = "Patient Age",
       y = "Invoice Amount") +
  theme_minimal()




