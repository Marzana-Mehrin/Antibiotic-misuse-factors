#loading data
library(readxl)
data <- read_excel("C:/Users/mehri/Downloads/AMR_KAP_Data.xlsx")
#viewing first few rows of dataset
head(data)
#checking data structure
str(data)
#converting data structure
data[] <- lapply(data, function(x) if(is.character(x)) as.factor(x) else x)
#verifying the changes
str(data)
#summary statistics for each variale
summary(data)
#counting missing values
sum(is.na(data))
#remove rows with missing values
data <- na.omit(data)
sum(is.na(data))
#identifying duplications
duplicated_rows <- duplicated(data) 
duplicated_rows
#view column names
colnames(data)
# Create demographic data
data <- data.frame(
  Category = c("Male", "Female", "Age < 25", "Age 25-35", "Age 36-45", "Age > 45", 
               "Higher Secondary", "Postgraduate", "Undergraduate", "Primary Education", 
               "Unemployed", "Employed", "Nuclear Family", "Single-Parent Family", "Extended Family",
               "Middle Income", "Female Children", "Male Children", "One Child", "Two Children", 
               "More than Two Children", "Mother as Primary Caregiver"),  # 22 categories
  Count = c(153, 551, 13, 379, 267, 47, 380, 176, 113, 35, 503, 201, 373, 184, 147, 
            409, 380, 324, 176, 423, 105, 627),  
  Total = rep(704, 22)  
)
data

# Calculate the proportions
data$Proportion <- data$Count / data$Total * 100
data$Proportion
# View the demographic data
print(data)
# Generate demographic table
install.packages("knitr")
library(knitr)
kable(data, caption = "Demographic Characteristics of Study Participants")
# Pie chart for gender distribution
gender_data <- data.frame(
  Gender = c("Male", "Female"),
  Count = c(153, 551)
)
gender_data$Proportion <- gender_data$Count / sum(gender_data$Count) * 100
install.packages("ggplot2")
library(ggplot2)
ggplot(gender_data, aes(x = "", y = Proportion, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") +  
  labs(title = "Gender Distribution", x = NULL, y = NULL) +  
  theme_void() 
# Bar plot for age distribution
age_data <- data.frame(
  AgeGroup = c("Age < 25", "Age 25-35", "Age 36-45", "Age > 45"),
  Count = c(13, 379, 267, 47)
)

ggplot(age_data, aes(x = AgeGroup, y = Count, fill = AgeGroup)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Age Distribution of Parents")
# Load necessary libraries
library(ggplot2)
library(dplyr)
data <- data.frame(
  Amoxicillin_Knowledge = sample(c(0, 1), 704, replace = TRUE, prob = c(0.63, 0.37)),
  Azithromycin_Knowledge = sample(c(0, 1), 704, replace = TRUE, prob = c(0.56, 0.44)),
  Paracetamol_Knowledge = sample(c(0, 1), 704, replace = TRUE, prob = c(0.79, 0.21)),
  AR_Knowledge = sample(c(0, 1), 704, replace = TRUE, prob = c(0.25, 0.75)),
  ABR_Treatment_Knowledge = sample(c(0, 1), 704, replace = TRUE, prob = c(0.53, 0.47))
)
# Calculate the percentage of correct knowledge for each question
knowledge_summary <- data %>%
  summarise(
    Amoxicillin_Knowledge = mean(Amoxicillin_Knowledge) * 100,
    Azithromycin_Knowledge = mean(Azithromycin_Knowledge) * 100,
    Paracetamol_Knowledge = mean(Paracetamol_Knowledge) * 100,
    AR_Knowledge = mean(AR_Knowledge) * 100,
    ABR_Treatment_Knowledge = mean(ABR_Treatment_Knowledge) * 100
  )
# Reshape the data for plotting
knowledge_df <- as.data.frame(t(knowledge_summary))
colnames(knowledge_df) <- "Percentage"
knowledge_df$Question <- rownames(knowledge_df)
# Plot the knowledge distribution
ggplot(knowledge_df, aes(x = Question, y = Percentage, fill = Question)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Knowledge Regarding Antibiotics and Antibiotic Resistance",
    y = "Percentage of Participants (%)",
    x = "Knowledge Questions"
  ) +
  theme(legend.position = "none")
# Simulated responses for the attitude questions
set.seed(123) 
data <- data.frame(
  Non_Antibiotic_Prescription = sample(c(0, 1), 704, replace = TRUE, prob = c(0.2, 0.8)),
  Satisfied_Doctor_Prescription = sample(c(0, 1), 704, replace = TRUE, prob = c(0.1, 0.9)),
  Disagree_Antibiotics_Without_Indication = sample(c(0, 1), 704, replace = TRUE, prob = c(0.75, 0.25)),
  Antibiotics_for_Fever_Cold = sample(c(0, 1), 704, replace = TRUE, prob = c(0.37, 0.63)),
  Stop_Antibiotics_on_Improvement = sample(c(0, 1), 704, replace = TRUE, prob = c(0.74, 0.26)),
  Reuse_Antibiotics_Similar_Symptoms = sample(c(0, 1), 704, replace = TRUE, prob = c(0.73, 0.27))
)
colnames(data) <- c("NonAntibioticPrescription", "SatisfiedDoctorPrescription", 
                    "DisagreeAntibioticsWithoutIndication", "AntibioticsForFeverCold",
                    "StopAntibioticsOnImprovement", "ReuseAntibioticsSimilarSymptoms")

attitude_summary <- data %>%
  summarise(
    Positive_Non_Antibiotic_Prescription = mean(NonAntibioticPrescription) * 100,
    Positive_Satisfied_Doctor_Prescription = mean(SatisfiedDoctorPrescription) * 100,
    Negative_Provision_Without_Indication = mean(DisagreeAntibioticsWithoutIndication) * 100,
    Belief_Antibiotics_For_Fever_Cold = mean(AntibioticsForFeverCold) * 100,
    Will_Stop_Antibiotics_On_Improvement = mean(StopAntibioticsOnImprovement) * 100,
    Reuse_Antibiotics_Similar_Symptoms = mean(ReuseAntibioticsSimilarSymptoms) * 100
  )
# Simulated responses for the attitude questions
set.seed(123) 
# For reproducibility
data <- data.frame(
  Non_Antibiotic_Prescription = sample(c(0, 1), 704, replace = TRUE, prob = c(0.2, 0.8)),
  Satisfied_Doctor_Prescription = sample(c(0, 1), 704, replace = TRUE, prob = c(0.1, 0.9)),
  Disagree_Antibiotics_Without_Indication = sample(c(0, 1), 704, replace = TRUE, prob = c(0.75, 0.25)),
  Antibiotics_for_Fever_Cold = sample(c(0, 1), 704, replace = TRUE, prob = c(0.37, 0.63)),
  Stop_Antibiotics_on_Improvement = sample(c(0, 1), 704, replace = TRUE, prob = c(0.74, 0.26)),
  Reuse_Antibiotics_Similar_Symptoms = sample(c(0, 1), 704, replace = TRUE, prob = c(0.73, 0.27))
)
# Calculate the percentage of correct responses for each attitude question
colnames(data)
summary(data)
library(dplyr)

attitude_summary <- data %>%
  summarise(
    Positive_Non_Antibiotic_Prescription = mean(Non_Antibiotic_Prescription) * 100,
    Positive_Satisfied_Doctor_Prescription = mean(Satisfied_Doctor_Prescription) * 100,
    Negative_Provision_Without_Indication = mean(Disagree_Antibiotics_Without_Indication) * 100,
    Belief_Antibiotics_For_Fever_Cold = mean(Antibiotics_for_Fever_Cold) * 100,
    Will_Stop_Antibiotics_On_Improvement = mean(Stop_Antibiotics_on_Improvement) * 100,
    Reuse_Antibiotics_Similar_Symptoms = mean(Reuse_Antibiotics_Similar_Symptoms) * 100
  )

print(attitude_summary)
# Reshape the data for plotting
attitude_df <- as.data.frame(t(attitude_summary))
colnames(attitude_df) <- "Percentage"
attitude_df$Attitude_Question <- rownames(attitude_df)
# Plot the attitude distribution
ggplot(attitude_df, aes(x = Attitude_Question, y = Percentage, fill = Attitude_Question)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Distribution of Parents’ Attitudes Towards Misuse of Antibiotics",
    y = "Percentage of Participants (%)",
    x = "Attitude Questions"
  ) +
  theme(legend.position = "none")
# Calculate the percentages for each practice
practice_summary <- data %>%
  summarise(
    Percentage_Give_Antibiotics_Without_Doctor_Consultation = mean(Give_Antibiotics_Without_Doctor_Consultation) * 100,
    Percentage_Antibiotics_From_Pharmacy = mean(Antibiotics_From_Pharmacy) * 100,
    Percentage_Give_Antibiotics_For_Cough = mean(Give_Antibiotics_For_Cough) * 100,
    Percentage_Check_Expiry_Date = mean(Check_Expiry_Date) * 100
  )
# Print the result
print(practice_summary)
# Reshape data into a long format for plotting
practice_df <- as.data.frame(t(practice_summary))
colnames(practice_df) <- "Percentage"
practice_df$Practice <- rownames(practice_df)

# Print the reshaped data for checking
print(practice_df)
# Plotting the practices regarding the use of antibiotics
library(ggplot2)

ggplot(practice_df, aes(x = Practice, y = Percentage, fill = Practice)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Practices Regarding the Use of Antibiotics",
    x = "Practice",
    y = "Percentage (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the percentage of parents who obtained information from each source
library(dplyr)
colnames(data)
# Summarize practices regarding antibiotic use
practice_summary <- data %>%
  summarise(
    Percentage_Give_Antibiotics_Without_Doctor_Consultation = mean(Give_Antibiotics_Without_Doctor_Consultation, na.rm = TRUE) * 100,
    Percentage_Antibiotics_From_Pharmacy = mean(Antibiotics_From_Pharmacy, na.rm = TRUE) * 100,
    Percentage_Give_Antibiotics_For_Cough = mean(Give_Antibiotics_For_Cough, na.rm = TRUE) * 100,
    Percentage_Check_Expiry_Date = mean(Check_Expiry_Date, na.rm = TRUE) * 100
  )

# Print the summary
print(practice_summary)



set.seed(123)

# Simulate KAP data for 704 respondents
data <- data.frame(
  Knowledge_Score = sample(0:12, 704, replace = TRUE),
  Attitude_Score = sample(0:10, 704, replace = TRUE),
  Practice_Score = sample(0:6, 704, replace = TRUE)
)

# Define thresholds for categorizing KAP
data$Knowledge_Level <- ifelse(data$Knowledge_Score <= 5, "Poor",
                               ifelse(data$Knowledge_Score <= 9, "Moderate", "Good"))
data$Attitude_Level <- ifelse(data$Attitude_Score <= 4, "Negative",
                              ifelse(data$Attitude_Score <= 7, "Uncertain", "Positive"))
data$Practice_Level <- ifelse(data$Practice_Score <= 4, "Inappropriate", "Appropriate")

# Print the first few rows of the data to check
head(data)
# Load necessary libraries
library(dplyr)

# Summarize KAP data
kap_summary <- data %>%
  summarise(
    Knowledge_Good = mean(Knowledge_Level == "Good") * 100,
    Knowledge_Moderate = mean(Knowledge_Level == "Moderate") * 100,
    Knowledge_Poor = mean(Knowledge_Level == "Poor") * 100,
    
    Attitude_Positive = mean(Attitude_Level == "Positive") * 100,
    Attitude_Uncertain = mean(Attitude_Level == "Uncertain") * 100,
    Attitude_Negative = mean(Attitude_Level == "Negative") * 100,
    
    Practice_Appropriate = mean(Practice_Level == "Appropriate") * 100,
    Practice_Inappropriate = mean(Practice_Level == "Inappropriate") * 100
  )

# Print the summary
print(kap_summary)
# Load ggplot2 for plotting
library(ggplot2)

# Reshape the summary data for plotting
kap_summary_long <- data.frame(
  Category = c("Knowledge", "Knowledge", "Knowledge", "Attitude", "Attitude", "Attitude", "Practice", "Practice"),
  Level = c("Good", "Moderate", "Poor", "Positive", "Uncertain", "Negative", "Appropriate", "Inappropriate"),
  Percentage = c(kap_summary$Knowledge_Good, kap_summary$Knowledge_Moderate, kap_summary$Knowledge_Poor,
                 kap_summary$Attitude_Positive, kap_summary$Attitude_Uncertain, kap_summary$Attitude_Negative,
                 kap_summary$Practice_Appropriate, kap_summary$Practice_Inappropriate)
)

# Plot the KAP data as bar charts
ggplot(kap_summary_long, aes(x = Category, y = Percentage, fill = Level)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Knowledge, Attitudes, and Practices Regarding Antibiotic Use",
       x = "Category", y = "Percentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
# using gtsummary for reproducible tables
install.packages("gtsummary")

library(gtsummary)

# Create a summary table
summary_table <- data %>%
  tbl_summary(by = Knowledge_Level) %>%
  add_p()

# Print the summary table
summary_table

