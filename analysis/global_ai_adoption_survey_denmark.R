
# Load libraries -------------------------------------------------------


library(readxl)
library(table1)
library(skimr)
library(dplyr)
library(gtsummary)
library(ggplot2)
library(stringr)
library(tidyr)
library(psych)
library(openxlsx)
library(tibble)
library(forcats)
library(patchwork)
library(ggh4x)

# ===============================================================
# 0. Data preparation - SECTION 1: SOCIODEMOGRAPHIC CHARACTERISTICS
# ===============================================================

##Load the Denmark_Dataset Data and check variables

Denmark_Dataset <- read_excel("Denmark_Dataset.xlsx")

# View data, names and missing values
View(Denmark_Dataset)

head(Denmark_Dataset)

names(Denmark_Dataset)

skimr::skim(Denmark_Dataset)


## Change the labels from Qx to survey items

Denmark_Dataset <- Denmark_Dataset %>%
  rename(
    "Country of organization" = Q1,
    "Organization" = Q2,
    "Level of government" = Q3,
    "Level of government other" = Q3_4_text,
    "Domain" = Q4,
    "Domain other" = Q4_11_text,
    "Role" = Q5,
    "Role other" = Q5_3_text,
    "Work area" = Q6,
    "Work area other" = Q6_3_text,
    "Number of employees within the organization" = Q7,
    "Sex" = Q8,
    "Age" = Q9,
    "Years of work experience in the public sector" = Q10,
    "Years of total work experience" = Q11,
    "Education" = Q12,
    "Field of education" = Q13,
  )

## Some of the organizations have been spelled differently. This means we need
## to standardize 

unique(Denmark_Dataset$Organization)

# Standardize the organization names

Denmark_Dataset <- Denmark_Dataset %>%
  mutate(
    Organization_clean = case_when(
      
      # Aarhus Universitetshospital
      Organization %in% c(
        "AUH",
        "Aarhus Universiteteshospital",
        "Aarhus universitetshospital",
        "århus universitetshospital, Skejby Sygehus",
        "Hudafdeling Aarhus Universitetshospital"
      ) ~ "Aarhus Universitetshospital",
      
      # Odense Universitetshospital
      Organization %in% c(
        "Odense universitetssygehus",
        "Odense universitetshospital",
        "Odense Universitetshospital"
      ) ~ "Odense Universitetshospital",
      
      # Danmarks Meteorologiske Institut
      Organization == "DMI" ~ "Danmarks Meteorologiske Institut",
      
      # Region Midtjylland
      Organization == "Region Midt" ~ "Region Midtjylland",
      
      # Ministry spelling
      Organization == "Energi ministeriet" ~ "Klima-, Energi- og Forsyningsministeriet",
      
      # Standalone spelling corrections
      Organization == "Universitets hospital" ~ "Universitetshospital",
      
      # Keep everything else
      TRUE ~ Organization
    )
  )


# Check to see if the "organization_clean" column has been added
names(Denmark_Dataset)



## Recode categorical variables according to the codebook and convert them to factors

str(Denmark_Dataset)

Denmark_Dataset$"Country of organization" <- factor(Denmark_Dataset$"Country of organization")

Denmark_Dataset$Organization_clean <- factor(Denmark_Dataset$Organization_clean)


Denmark_Dataset$"Level of government" <- factor(Denmark_Dataset$"Level of government",
           levels = c(1:4),
           labels = c("Local", "Regional", "National","Other")
)

Denmark_Dataset$"Level of government other" <- factor(Denmark_Dataset$"Level of government other")



Denmark_Dataset$"Domain" <- factor(Denmark_Dataset$"Domain",
          levels = c(1:11),
          labels = c("Defense", "Economic affairs", "Education",
          "Environmental protection","General public services","Health","Housing and community amenities",
          "Public order and safety","Recreation, culture and religion","Social protection","Other")
)

Denmark_Dataset$"Domain other" <- factor(Denmark_Dataset$"Domain other")



Denmark_Dataset$"Role" <- factor(Denmark_Dataset$"Role",
          levels = c(1:3),
          labels = c("Leadership position", "Non-leadership position", "Other")
)

Denmark_Dataset$"Role other" <- factor(Denmark_Dataset$"Role other")



Denmark_Dataset$"Work area" <- factor(Denmark_Dataset$"Work area",
          levels = c(1:3),
          labels = c("Professional staff", "Support staff", "Other")
)

Denmark_Dataset$"Work area other" <- factor(Denmark_Dataset$"Work area other")



Denmark_Dataset$"Number of employees within the organization" <- factor(Denmark_Dataset$"Number of employees within the organization",
          levels = c(1:7),
          labels = c("1–10", "11–50", "51–200","201–500","501–1,000","More than 1,000","I don't know / I'm not sure")
)

Denmark_Dataset$"Sex" <- factor(Denmark_Dataset$"Sex",
          levels = c(1:4),
          labels = c("Male", "Female", "Other","Prefer not to say")
)

Denmark_Dataset$"Age" <- factor(Denmark_Dataset$"Age",
          levels = c(1:83),
          labels = c(18:100)
)

Denmark_Dataset$"Years of work experience in the public sector" <- factor(Denmark_Dataset$"Years of work experience in the public sector",
          levels = c(1:100),
          labels = c(1:100)
)

Denmark_Dataset$"Years of total work experience" <- factor(Denmark_Dataset$"Years of total work experience",
          levels = c(1:100),
          labels = c(1:100)
)

Denmark_Dataset$"Education" <- factor(Denmark_Dataset$"Education",
          levels = c(1:7),
          labels = c("Secondary vocational or equivalent, or lower",
                     "Secondary technical, professional, or general education",
                     "Higher vocational program or equivalent","Bachelor’s degree or equivalent",
                     "Master’s degree or equivalent","Specialized postgraduate degree or master of science",
                     "Doctorate / PhD")
)

Denmark_Dataset$"Field of education" <- factor(Denmark_Dataset$"Field of education",
          levels = c(1:4),
          labels = c("Arts and Humanities",
                    "Social Sciences",
                    "Applied Sciences",
                    "Natural and Life Sciences")
)


## Convert age and work experience variables to numeric format for statistical analysis

Denmark_Dataset$"Age" <- as.numeric(as.character(Denmark_Dataset$"Age"))

Denmark_Dataset$"Years of work experience in the public sector" <-
  as.numeric(as.character(
    Denmark_Dataset$"Years of work experience in the public sector"
  ))

Denmark_Dataset$"Years of total work experience" <-
  as.numeric(as.character(
    Denmark_Dataset$"Years of total work experience"
  ))


# Aggregate all variables into one frame

varsToFactor <- c(
  "Country of organization",
  "Organization_clean",
  "Level of government",
  "Level of government other",
  "Domain",
  "Domain other",
  "Role",
  "Role other",
  "Work area",
  "Work area other",
  "Number of employees within the organization",
  "Sex",
  "Education",
  "Field of education"
)

# Convert to factor and add it to the main data frame "Denmark_Dataset". 
# It ready to use when creating table 1

Denmark_Dataset[varsToFactor] <- lapply(Denmark_Dataset[varsToFactor], factor)



# ===============================================================
# 1. Create table 1
# ===============================================================

## First create a new table without the 1 respondent from "i don't know" in terms of usage

Denmark_Comparison <- subset(
  Denmark_Dataset,
  USAGE != "I don't know"
)

View(Denmark_Comparison)



## Create a variable list that will be shown in your tableone
## This has all variables including "others"

vars <- c(
  "Country of organization",
  "Organization_clean",
  "Level of government",
  "Level of government other",
  "Domain",
  "Domain other",
  "Role",
  "Role other",
  "Work area",
  "Work area other",
  "Number of employees within the organization",
  "Sex",
  "Age",
  "Years of work experience in the public sector",
  "Years of total work experience",
  "Education",
  "Field of education"
)



###### This is for ALL varibles ########
# Prepare data
Denmark_Comparison <- Denmark_Comparison %>%
  mutate(
    across(all_of(varsToFactor), as.factor),
    
    # Required column order
    USAGE = factor(
      USAGE,
      levels = c("Use", "I don't use")
    )
  )

# USAGE creates the columns and should not also be summarized as a row
vars_table1 <- setdiff(vars, "USAGE")

# Overall denominator
total_n <- nrow(Denmark_Comparison)



# Categorical-variable renderer. Percentages use the overall N, not each column N
render_categorical_overall_n <- function(x, ...) {
  counts <- table(x, useNA = "no")
  values <- sprintf(
    "%d (%.1f%%)",
    as.integer(counts),
    100 * as.integer(counts) / total_n
  )
  c(
    "",
    setNames(values, names(counts))
  )
}


# Missing-value renderer. Missing percentages also use the overall N
render_missing_overall_n <- function(x, ...) {
  n_missing <- sum(is.na(x))
  c(
    "Missing" = sprintf(
      "%d (%.1f%%)",
      n_missing,
      100 * n_missing / total_n
    )
  )
}



# Build formula from all variables in vars
table1_formula <- as.formula(
  paste0(
    "~ ",
    paste(sprintf("`%s`", vars_table1), collapse = " + "),
    " | USAGE"
  )
)



# Create Table 1
tableOne <- table1(
  table1_formula,
  data = Denmark_Comparison,
  
  # Overall first
  overall = c(left = "Overall"),
  
  # Use overall N as denominator for categorical percentages
  render.categorical = render_categorical_overall_n,
  
  # Use overall N for missing percentages
  render.missing = render_missing_overall_n
)

# View table
tableOne


# Export --> "save as web page"





##### WITHOUT ORGANIZATION AND "OTHERS" (GDPR) ######

# Use same data as before "Denmark_Comparison"


# Create a copy for Table 1

Denmark_Comparison_table1 <- Denmark_Comparison %>%
  mutate(
    Role = case_when(
      Role == "Other" ~ NA_character_,
      TRUE ~ as.character(Role)
    ),
    
    `Work area` = case_when(
      `Work area` == "Other" ~ NA_character_,
      TRUE ~ as.character(`Work area`)
    ),
    
    Sex = case_when(
      Sex == "Prefer not to say" ~ NA_character_,
      TRUE ~ as.character(Sex)
    ),
    
    `Field of education` = case_when(
      `Field of education` %in% c(
        "Arts and Humanities",
        "Social Sciences"
      ) ~ "Arts, humanities and social sciences",
      
      TRUE ~ as.character(`Field of education`)
    )
  )


# Variables shown in Table 1

vars_modified <- c(
  "Country of organization",
  "Level of government",
  "Domain",
  "Role",
  "Work area",
  "Number of employees within the organization",
  "Sex",
  "Age",
  "Years of work experience in the public sector",
  "Years of total work experience",
  "Education",
  "Field of education"
)



# Overall denominator
total_n_modified <- nrow(Denmark_Comparison_table1)


# Categorical variables:
# - n (%) when n >= 5
# - <5 without percentage when n < 5

render_categorical_suppressed <- function(x, ...) {
  counts <- table(x, useNA = "no")
  n <- as.integer(counts)
  
  values <- ifelse(
    n < 5,
    "<5",
    sprintf(
      "%d (%.1f%%)",
      n,
      100 * n / total_n_modified
    )
  )
  
  c("", setNames(values, names(counts)))
}



# Genuine missing values:
# - omit row when there are no missing values
# - <5 when fewer than 5 are missing
# - n (%) when 5 or more are missing

render_missing_suppressed <- function(x, ...) {
  n_missing <- sum(is.na(x))
  
  if (n_missing == 0) {
    return(NULL)
  }
  
  value <- if (n_missing < 5) {
    "<5"
  } else {
    sprintf(
      "%d (%.1f%%)",
      n_missing,
      100 * n_missing / total_n_modified
    )
  }
  
  c("Missing" = value)
}




# Build the table formula
table1_formula_modified <- as.formula(
  paste0(
    "~ ",
    paste(
      sprintf("`%s`", vars_modified),
      collapse = " + "
    )
  )
)

# Create Table 1 (without missing values)
tableOne_modified <- table1(
  table1_formula_modified,
  data = Denmark_Comparison_table1,
  overall = c(left = "Overall"),
  render.categorical = render_categorical_suppressed,
  render.missing = NULL
)
tableOne_modified




# =======================================================================================
# 0. Data preparation - SECTION 2-11 + NON-USERS’ PERSPECTIVES + GENERAL REFLECTIONS
# =======================================================================================


## Create a new dataset from the Denmark_Comparison comparing only users vs non-users

Data_all_sections <- Denmark_Comparison

View(Data_all_sections)

## Change the labels from Qx to survey items

Data_all_sections <- Data_all_sections %>%
  rename(
    "Chatbots and AI assistants" = Q14a,
    "Digital assistants" = Q14b,
    "Automation of repetitive tasks or processes" = Q14c,
    "Systems for searching, organizing, and sharing knowledge" = Q14d,
    "Speech understanding and analysis" = Q14e,
    "Cybersecurity and threat detection" = Q14f,
    "Prediction of future events or patterns" = Q14g,
    "Recommendation systems" = Q14h,
    "Recognition of people and identities" = Q14i,
    "Smart robots and autonomous systems" = Q14j,
    "Other" = Q14k,
    "Other_text" = Q14k_text,
    "I don't use any AI solution" = Q14l,
    "I don't know" = Q14m,
    "How often do you use AI?" = Q15,
    "What is your experience with AI tools?" = Q16,
    "To what extent is the use of AI tools expected in your work?" = Q17,
    "I intend to keep using AI tools in my work in the future" = Q18a,
    "I will try to use AI tools in my work whenever I have a chance" = Q18b,
    "I plan to use AI tools often in my daily work" = Q18c,
    "I support the use of AI tools in my own work" = Q18d,
    "I think adding AI tools to my work is a good idea" = Q18e,
    "AI tools are helpful for my work" = Q19a,
    "AI tools help me reach important goals at work" = Q19b,
    "AI tools help me finish my work tasks faster" = Q19c,
    "AI tools make the quality of my work better" = Q19d,
    "AI tools help me come up with more creative ideas in my work" = Q19e,
    "It is easy for me to learn how to use AI tools" = Q20a,
    "AI tools are easy for me to use at work" = Q20b,
    "I understand how to use AI tools" = Q20c,
    "I feel sure that I can become good at using AI tools" = Q20d,
    "Using AI tools at work doesn't take much mental effort" = Q20e,
    "I have attended enough training to use AI tools at work" = Q20f,
    "In our organization, we are provided with opportunities to learn new technological skills due to changes related to AI" = Q20g,
    "In our organization, we are provided with opportunities to learn new soft skills due to changes related to AI" = Q20h,
    "In our organization, we are provided with opportunities to learn about the risks of using AI tools as part of developing a critical approach to their use" = Q20i,
    "I am worried that some employees could be left out if they do not get training on AI" = Q20j,
    "I am familiar with the risks associated with using AI tools at work" = Q21a,
    "I believe that AI tools provide reliable support for our organization’s operations" = Q21b,
    "I believe that AI tools do not violate intellectual property rights" = Q21c,
    "I believe that AI tools operate transparently" = Q21d,
    "I believe that AI tools protect the data I provide to them" = Q21e,
    "I feel comfortable providing data to AI tools" = Q21f,
    "I am aware of the risks of data misuse when using AI tools" = Q21g,
    "I believe that AI tools in our organization do not unjustifiably infringe on employees’ privacy" = Q21h,
    "This is a test question. To confirm you are paying attention, please select “Somewhat agree” for this statement" = Q21i,
    "I am aware of how AI tools collect and use data" = Q21j,
    "I believe that AI tools treat users fairly and without discrimination" = Q21k,
    "I believe that AI tools handle users’ data responsibly" = Q21l,
    "I believe that AI tools adhere to ethical standards" = Q21m,
    "I believe that the results produced by AI tools are accurate" = Q21n,
    "I am concerned that public employees may become overly reliant on AI tools" = Q22a,
    "The use of AI tools could hinder the development of key competencies among public employees" = Q22b,
    "I am concerned that AI tools could widen the digital gap among public employees" = Q22c,
    "I am concerned that citizens may become overly reliant on AI tools" = Q22d,
    "I am concerned that AI tools could widen the digital gap among citizens" = Q22e,
    "AI tools could reduce opportunities for direct interaction between public employees and citizens" = Q22f,
    "AI tools can produce false information, known as hallucinations" = Q22g,
    "I am concerned that relying on AI tool outputs could lead to incorrect decisions at work" = Q22h,
    "AI tools can exhibit linguistic or cultural biases in their outputs" = Q22i,
    "I am concerned that AI tools could lead to discrimination" = Q22j,
    "I am concerned that AI tools diminish the value of my professional work" = Q22k,
    "I am concerned that AI tools reduce my autonomy at work" = Q22l,
    "I am concerned that my work is being monitored through the AI tools I use" = Q22m,
    "People who are important to me believe that I should use AI tools at work" = Q23a,
    "People who influence my work decisions believe that I should use AI tools" = Q23b,
    "People whose opinions I value support my use of AI tools at work" = Q23c,
    "My coworkers generally support the use of AI tools" = Q23d,
    "In my workplace, using AI tools is seen as something positive" = Q23e,
    "I have the resources I need to use AI tools in my work" = Q23f,
    "I have the knowledge I need to use AI tools well" = Q23g,
    "AI tools fit well with how I normally do my work" = Q23h,
    "If I needed help using AI tools, a colleague would be available to assist me" = Q23i,
    "In our organization, we have sufficient support for using AI tools at work" = Q23j,
    "In my wider community, the use of AI tools is generally accepted" = Q23k,
    "In our organization, we have sufficient financial resources to purchase and maintain AI tools" = Q24a,
    "In our organization, we have opportunities to learn the latest ways to work with AI tools" = Q24b,
    "In our organization, the ICT infrastructure is regularly updated to better leverage AI tools" = Q24c,
    "In our organization, new ideas are supported" = Q24d,
    "In our organization, the organizational structure is adapted to keep up with AI-based innovations" = Q24e,
    "In our organization, employees are involved in the preparation and implementation of AI solutions" = Q24f,
    "In our organization, we have the opportunity to learn about implemented AI solutions and their design" = Q24g,
    "I look for ways to use AI tools to improve my work" = Q25a,
    "I try out AI tools to complete tasks more efficiently" = Q25b,
    "I proactively explore how AI tools can help me with my work" = Q25c,
    "I identify possible problems with AI tools and try to solve them early" = Q25d,
    "I encourage my coworkers to try AI tools" = Q25e,
    "I am excited by exploring new possibilities for using AI" = Q25f,
    "I can quickly get used to new AI tools at work" = Q26a,
    "I am open to changing how I work to use AI tools better" = Q26b,
    "I actively search for training/information to improve my AI skills" = Q26c,
    "AI allows me to be more flexible in my work tasks" = Q26d,
    "I keep myself informed about how AI tools are used in my field" = Q26e,
    "I stay calm when AI tools change how I work" = Q27a,
    "I manage stress well when learning or adjusting to AI tools" = Q27b,
    "I feel confident dealing with challenges related to AI at work" = Q27c,
    "I focus on finding solutions when AI tools do not work as expected" = Q27d,
    "I can keep doing my job well even when AI tools bring uncertainty" = Q27e,
    "I am worried that I could lose my job because AI is taking over tasks currently performed by humans" = Q28a,
    "AI tools have clearly changed the kind of work I do" = Q28b,
    "I believe that AI will create new jobs" = Q28c,
    "Our organization has established rules to protect employees from potential harm caused by AI" = Q28d,
    "Trade unions are involved in decision-making regarding the use of AI in my organization" = Q28e,
    "I believe that trade unions should have a greater role in shaping rules for the use of AI in the public sector" = Q28f,
    "I feel that my opinion matters when new technologies like AI are introduced at work" = Q28g,
    "I feel stressed by how fast AI tools are being introduced in my workplace" = Q28h,
    "I avoid using AI tools because I am afraid of making a mistake that no one will notice" = Q28i,
    "I would think about leaving my job if AI took over important parts of my work" = Q28j,
    "My managers support using AI tools at work" = Q28k,
    "I feel relaxed when using AI tools" = Q28l,
    "AI tools help save time on routine tasks" = Q29a,
    "AI tools lower the amount of manual work" = Q29b,
    "AI tools let us focus on more important tasks" = Q29c,
    "It takes a lot of time to check the results from AI tools" = Q29d,
    "AI tools help us finish tasks faster overall" = Q29e,
    "AI tools help reduce errors in our work" = Q29f,
    "AI tools make our work more accurate" = Q29g,
    "AI tools help us spot problems early" = Q29h,
    "AI tools reduce the quality of the final results" = Q29i,
    "AI tools make it easier for citizens to give feedback/make requests" = Q30a,
    "AI tools reduce direct contact between citizens and public employees" = Q30b,
    "AI tools help involve stakeholders in decision-making" = Q30c,
    "The use of AI tools is clearly documented" = Q30d,
    "It is difficult to explain the final result when AI tools are used" = Q30e,
    "It is clear who is responsible for decisions made with the help of AI tools" = Q30f,
    "Our organization has a system in place for reporting and resolving issues with AI tools" = Q30g,
    "Our organization has a system in place to verify the accuracy of results produced by AI tools" = Q30h,
    "Stakeholders can access the data that our AI tools use" = Q31a,
    "Citizens are notified in writing when AI tools are used in administrative decision-making" = Q31b,
    "The public does not always know if AI tools were used to influence a decision" = Q31c,
    "AI-generated content is clearly labelled" = Q31d,
    "We keep records of what prompts and instructions were given to AI tools" = Q31e,
    "We follow legal rules when using AI tools" = Q31f,
    "When using AI tools, we protect the rights of citizens" = Q31g,
    "The use of AI tools in our organization complies with regulations protecting citizens’ rights" = Q31h,
    "Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law" = Q31i,
    "I don’t understand how to use AI tools" = Q32a,
    "I don’t feel adequately prepared to use AI tools" = Q32b,
    "I find it difficult to find time to learn how to use AI tools" = Q32c,
    "I don’t trust the results of AI tools" = Q32d,
    "I am concerned about how AI tools handle personal data" = Q32e,
    "I don’t trust the intentions of those who develop AI tools" = Q32f,
    "My organization does not encourage the use of AI tools" = Q32g,
    "My organization does not allow the use of AI tools" = Q32h,
    "I want to wait for official guidelines before using AI tools" = Q32i,
    "I sense reluctance from colleagues regarding the use of AI tools" = Q32j,
    "I don’t see the point in using AI tools for my work" = Q32k,
    "I believe that using AI tools threatens professional expertise" = Q32l,
    "Can you share some general views/words about how you see AI?" = Q33,
  )


View(Data_all_sections)


##  Recode categorical variables according to the codebook and convert them to factors

str(Data_all_sections)

Data_all_sections$Other_text <- factor(Data_all_sections$Other_text)

Data_all_sections$"Can you share some general views/words about how you see AI?" <- factor(Data_all_sections$"Can you share some general views/words about how you see AI?")


Data_all_sections$"How often do you use AI?" <- factor(Data_all_sections$"How often do you use AI?",
      levels = c(1:5),
      labels = c("Rarely","Occasionally","Moderately","Quite often","Very often")
)

Data_all_sections$"What is your experience with AI tools?" <- factor(Data_all_sections$"What is your experience with AI tools?",
      levels = c(1:5),
      labels = c("Very bad","Bad","Neutral","Good","Very good")
)


Data_all_sections$"To what extent is the use of AI tools expected in your work?" <- factor(Data_all_sections$"To what extent is the use of AI tools expected in your work?",
      levels = c(1:5),
      labels = c("Completely voluntary","Slightly encouraged","Moderately encouraged","Strongly encouraged","Required")
)



# Part 1: Group all categorical variables that has the same level and variable name in the same group. 
# Leave open text and the once  since they have different names

same_vars <- c(
  "Chatbots and AI assistants",
  "Digital assistants",
  "Automation of repetitive tasks or processes",
  "Systems for searching, organizing, and sharing knowledge",
  "Speech understanding and analysis",
  "Cybersecurity and threat detection",
  "Prediction of future events or patterns",
  "Recommendation systems",
  "Recognition of people and identities",
  "Smart robots and autonomous systems",
  "Other",
  "I don't use any AI solution",
  "I don't know"
)

same_labels <- c(
  "Not selected",
  "Selected"
)

Data_all_sections <- Data_all_sections %>%
  mutate(
    across(
      all_of(same_vars),
      ~ factor(.x,
               levels = 0:1,
               labels = same_labels)
    )
  )



# Part 2: Do the same for these variables since they have identical level and label name

same_vars_2 <- c(
  "I intend to keep using AI tools in my work in the future",
  "I will try to use AI tools in my work whenever I have a chance",
  "I plan to use AI tools often in my daily work",
  "I support the use of AI tools in my own work",
  "I think adding AI tools to my work is a good idea",
  "AI tools are helpful for my work",
  "AI tools help me reach important goals at work",
  "AI tools help me finish my work tasks faster",
  "AI tools make the quality of my work better",
  "AI tools help me come up with more creative ideas in my work",
  "It is easy for me to learn how to use AI tools",
  "AI tools are easy for me to use at work",
  "I understand how to use AI tools",
  "I feel sure that I can become good at using AI tools",
  "Using AI tools at work doesn't take much mental effort",
  "I have attended enough training to use AI tools at work",
  "In our organization, we are provided with opportunities to learn new technological skills due to changes related to AI",
  "In our organization, we are provided with opportunities to learn new soft skills due to changes related to AI",
  "In our organization, we are provided with opportunities to learn about the risks of using AI tools as part of developing a critical approach to their use",
  "I am worried that some employees could be left out if they do not get training on AI",
  "I am familiar with the risks associated with using AI tools at work",
  "I believe that AI tools provide reliable support for our organization’s operations",
  "I believe that AI tools do not violate intellectual property rights",
  "I believe that AI tools operate transparently",
  "I believe that AI tools protect the data I provide to them",
  "I feel comfortable providing data to AI tools",
  "I am aware of the risks of data misuse when using AI tools",
  "I believe that AI tools in our organization do not unjustifiably infringe on employees’ privacy",
  "This is a test question. To confirm you are paying attention, please select “Somewhat agree” for this statement",
  "I am aware of how AI tools collect and use data",
  "I believe that AI tools treat users fairly and without discrimination",
  "I believe that AI tools handle users’ data responsibly",
  "I believe that AI tools adhere to ethical standards",
  "I believe that the results produced by AI tools are accurate",
  "I am concerned that public employees may become overly reliant on AI tools",
  "The use of AI tools could hinder the development of key competencies among public employees",
  "I am concerned that AI tools could widen the digital gap among public employees",
  "I am concerned that citizens may become overly reliant on AI tools",
  "I am concerned that AI tools could widen the digital gap among citizens",
  "AI tools could reduce opportunities for direct interaction between public employees and citizens",
  "AI tools can produce false information, known as hallucinations",
  "I am concerned that relying on AI tool outputs could lead to incorrect decisions at work",
  "AI tools can exhibit linguistic or cultural biases in their outputs",
  "I am concerned that AI tools could lead to discrimination",
  "I am concerned that AI tools diminish the value of my professional work",
  "I am concerned that AI tools reduce my autonomy at work",
  "I am concerned that my work is being monitored through the AI tools I use",
  "People who are important to me believe that I should use AI tools at work",
  "People who influence my work decisions believe that I should use AI tools",
  "People whose opinions I value support my use of AI tools at work",
  "My coworkers generally support the use of AI tools",
  "In my workplace, using AI tools is seen as something positive",
  "I have the resources I need to use AI tools in my work",
  "I have the knowledge I need to use AI tools well",
  "AI tools fit well with how I normally do my work",
  "If I needed help using AI tools, a colleague would be available to assist me",
  "In our organization, we have sufficient support for using AI tools at work",
  "In my wider community, the use of AI tools is generally accepted",
  "In our organization, we have sufficient financial resources to purchase and maintain AI tools",
  "In our organization, we have opportunities to learn the latest ways to work with AI tools",
  "In our organization, the ICT infrastructure is regularly updated to better leverage AI tools",
  "In our organization, new ideas are supported",
  "In our organization, the organizational structure is adapted to keep up with AI-based innovations",
  "In our organization, employees are involved in the preparation and implementation of AI solutions",
  "In our organization, we have the opportunity to learn about implemented AI solutions and their design",
  "I look for ways to use AI tools to improve my work",
  "I try out AI tools to complete tasks more efficiently",
  "I proactively explore how AI tools can help me with my work",
  "I identify possible problems with AI tools and try to solve them early",
  "I encourage my coworkers to try AI tools",
  "I am excited by exploring new possibilities for using AI",
  "I can quickly get used to new AI tools at work",
  "I am open to changing how I work to use AI tools better",
  "I actively search for training/information to improve my AI skills",
  "AI allows me to be more flexible in my work tasks",
  "I keep myself informed about how AI tools are used in my field",
  "I stay calm when AI tools change how I work",
  "I manage stress well when learning or adjusting to AI tools",
  "I feel confident dealing with challenges related to AI at work",
  "I focus on finding solutions when AI tools do not work as expected",
  "I can keep doing my job well even when AI tools bring uncertainty",
  "I am worried that I could lose my job because AI is taking over tasks currently performed by humans",
  "AI tools have clearly changed the kind of work I do",
  "I believe that AI will create new jobs",
  "Our organization has established rules to protect employees from potential harm caused by AI",
  "Trade unions are involved in decision-making regarding the use of AI in my organization",
  "I believe that trade unions should have a greater role in shaping rules for the use of AI in the public sector",
  "I feel that my opinion matters when new technologies like AI are introduced at work",
  "I feel stressed by how fast AI tools are being introduced in my workplace",
  "I avoid using AI tools because I am afraid of making a mistake that no one will notice",
  "I would think about leaving my job if AI took over important parts of my work",
  "My managers support using AI tools at work",
  "I feel relaxed when using AI tools",
  "AI tools help save time on routine tasks",
  "AI tools lower the amount of manual work",
  "AI tools let us focus on more important tasks",
  "It takes a lot of time to check the results from AI tools",
  "AI tools help us finish tasks faster overall",
  "AI tools help reduce errors in our work",
  "AI tools make our work more accurate",
  "AI tools help us spot problems early",
  "AI tools reduce the quality of the final results",
  "AI tools make it easier for citizens to give feedback/make requests",
  "AI tools reduce direct contact between citizens and public employees",
  "AI tools help involve stakeholders in decision-making",
  "The use of AI tools is clearly documented",
  "It is difficult to explain the final result when AI tools are used",
  "It is clear who is responsible for decisions made with the help of AI tools",
  "Our organization has a system in place for reporting and resolving issues with AI tools",
  "Our organization has a system in place to verify the accuracy of results produced by AI tools",
  "Stakeholders can access the data that our AI tools use",
  "Citizens are notified in writing when AI tools are used in administrative decision-making",
  "The public does not always know if AI tools were used to influence a decision",
  "AI-generated content is clearly labelled",
  "We keep records of what prompts and instructions were given to AI tools",
  "We follow legal rules when using AI tools",
  "When using AI tools, we protect the rights of citizens",
  "The use of AI tools in our organization complies with regulations protecting citizens’ rights",
  "Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law",
  "I don’t understand how to use AI tools",
  "I don’t feel adequately prepared to use AI tools",
  "I find it difficult to find time to learn how to use AI tools",
  "I don’t trust the results of AI tools",
  "I am concerned about how AI tools handle personal data",
  "I don’t trust the intentions of those who develop AI tools",
  "My organization does not encourage the use of AI tools",
  "My organization does not allow the use of AI tools",
  "I want to wait for official guidelines before using AI tools",
  "I sense reluctance from colleagues regarding the use of AI tools",
  "I don’t see the point in using AI tools for my work",
  "I believe that using AI tools threatens professional expertise"
)

same_labels_2 <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Undecided",
  "Somewhat agree",
  "Strongly agree"
)

Data_all_sections <- Data_all_sections %>%
  mutate(
    across(
      all_of(same_vars_2),
      ~ factor(.x,
               levels = 1:5,
               labels = same_labels_2)
    )
  )



# Save the final data set "Data_all_sections" which can then be loaded anytime

save(Data_all_sections, file = "Data_all_sections.RData")



# =======================================================================================
# 1. Representativeness (Domain)
# =======================================================================================

# ------------------------------------------------------------
# 1. OBESK2 expected counts: 2025 Q4
# ------------------------------------------------------------

expected_government <- c(
  "Local"    = 441399,  # Municipal government
  "Regional" = 134898,  # Regional government
  "National" = 200852   # Central government
)



# ------------------------------------------------------------
# 2. Count respondents by level of government
# ------------------------------------------------------------

government_counts <- Data_all_sections %>%
  filter(
    !is.na(`Level of government`),
    `Level of government` != "",
    `Level of government` != "Other"
  ) %>%
  count(
    `Level of government`,
    name = "Respondents"
  ) %>%
  rename(
    Government_level = `Level of government`
  ) %>%
  complete(
    Government_level = names(expected_government),
    fill = list(Respondents = 0)
  )

government_counts




# ------------------------------------------------------------
# 3. Calculate respondent and OBESK2 shares
# ------------------------------------------------------------

total_sample_government <- sum(government_counts$Respondents)

total_public_employment <- sum(expected_government)

government_comparison_wide <- government_counts %>%
  mutate(
    Public_employees =
      unname(expected_government[Government_level]),
    
    # Blue bars: share of study respondents
    Sample_share =
      100 * Respondents / total_sample_government,
    
    # Orange bars: share of comparable public employees
    Public_sector_share =
      100 * Public_employees / total_public_employment
  ) %>%
  mutate(
    Government_level = factor(
      Government_level,
      levels = c("Local", "Regional", "National")
    )
  )

government_comparison_wide


# ------------------------------------------------------------
# 4. Convert to long format for plotting
# ------------------------------------------------------------

government_comparison_df <- government_comparison_wide %>%
  select(
    Government_level,
    Sample_share,
    Public_sector_share
  ) %>%
  pivot_longer(
    cols = c(Sample_share, Public_sector_share),
    names_to = "Group",
    values_to = "Percent"
  ) %>%
  mutate(
    Group = recode(
      Group,
      "Sample_share" = "Study sample",
      "Public_sector_share" = "DK public sector"
    ),
    
    Group = factor(
      Group,
      levels = c(
        "Study sample",
        "DK public sector"
      )
    )
  )


# ------------------------------------------------------------
# 6. Plot Panel A
# ------------------------------------------------------------

p_government_compare <- ggplot(
  government_comparison_df,
  aes(
    x = Government_level,
    y = Percent,
    fill = Group
  )
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68
  ) +
  geom_text(
    aes(label = sprintf("%.1f", Percent)),
    position = position_dodge(width = 0.78),
    vjust = -0.5,
    size = 3.1,
    family = "sans"
  ) +
  scale_fill_manual(
    values = c(
      "Study sample" = "#4C72B0",
      "DK public sector" = "#DD8452"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(government_comparison_df$Percent) * 1.15
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Level of government",
    subtitle = "Respondent share compared with public-sector employment",
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 13
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10.5,
      colour = "grey35",
      margin = margin(b = 10)
    ),
    axis.text.x = element_text(
      colour = "black",
      size = 9.5,
      margin = margin(t = 7)
    ),
    axis.text.y = element_text(
      colour = "black",
      size = 9.5
    ),
    axis.title.y = element_text(
      size = 10
    ),
    legend.position = "top",
    legend.text = element_text(
      size = 9.5
    ),
    legend.title = element_blank()
  )

p_government_compare



# ------------------------------------------------------------
# 1. Overall Danish public-sector benchmark
# ------------------------------------------------------------
# Source:
# Approximately 70% female and 30% male
# in the Danish public sector overall.

sex_benchmark <- c(
  "Female" = 70,
  "Male" = 30
)

# ------------------------------------------------------------
# 2. Check existing labels
# ------------------------------------------------------------

unique(Data_all_sections$Sex)

# ------------------------------------------------------------
# 3. Count respondents by sex
# ------------------------------------------------------------
# Only directly comparable categories are retained.
# Any missing values or other response categories are excluded.

sex_counts <- Data_all_sections %>%
  filter(
    !is.na(Sex),
    Sex %in% c("Female", "Male")
  ) %>%
  count(
    Sex,
    name = "Respondents"
  ) %>%
  complete(
    Sex = names(sex_benchmark),
    fill = list(
      Respondents = 0
    )
  )

sex_counts

# ------------------------------------------------------------
# 4. Calculate study-sample sex shares
# ------------------------------------------------------------

total_sex_respondents <- sum(sex_counts$Respondents)

sex_comparison_wide <- sex_counts %>%
  mutate(
    Sample_share = 100 * Respondents / total_sex_respondents,
    Public_sector_share = unname(sex_benchmark[as.character(Sex)]),
    Sex = factor(
      Sex,
      levels = c("Female", "Male")
    )
  )

sex_comparison_wide

# ------------------------------------------------------------
# 5. Convert to long format for plotting
# ------------------------------------------------------------

sex_comparison_df <- sex_comparison_wide %>%
  select(
    Sex,
    Respondents,
    Sample_share,
    Public_sector_share
  ) %>%
  pivot_longer(
    cols = c(
      Sample_share,
      Public_sector_share
    ),
    names_to = "Group",
    values_to = "Percent"
  ) %>%
  mutate(
    Group = recode(
      Group,
      "Sample_share" = "Study sample",
      "Public_sector_share" = "DK public sector"
    ),
    Group = factor(
      Group,
      levels = c(
        "Study sample",
        "DK public sector"
      )
    )
  )

sex_comparison_df

# ------------------------------------------------------------
# 6. Plot Panel B
# ------------------------------------------------------------

p_sex_compare <- ggplot(
  sex_comparison_df,
  aes(
    x = Sex,
    y = Percent,
    fill = Group
  )
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68
  ) +
  geom_text(
    aes(
      label = sprintf("%.1f", Percent)
    ),
    position = position_dodge(width = 0.78),
    vjust = -0.5,
    size = 3.1,
    family = "sans",
    na.rm = TRUE
  ) +
  scale_fill_manual(
    values = c(
      "Study sample" = "#4C72B0",
      "DK public sector" = "#DD8452"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(
      mult = c(0, 0.06)
    )
  ) +
  labs(
    title = "Sex",
    subtitle = "Respondent share compared with public-sector employment",
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      hjust = 0.5,
      size = 13
    ),
    plot.subtitle = element_text(
      family = "sans",
      face = "plain",
      hjust = 0.5,
      size = 10.5,
      colour = "grey35",
      margin = margin(b = 10)
    ),
    axis.text.x = element_text(
      family = "sans",
      face = "plain",
      colour = "black",
      size = 9.5,
      margin = margin(t = 7)
    ),
    axis.text.y = element_text(
      family = "sans",
      face = "plain",
      colour = "black",
      size = 9.5
    ),
    axis.title.y = element_text(
      family = "sans",
      face = "plain",
      size = 10
    ),
    legend.position = "top",
    legend.text = element_text(
      family = "sans",
      face = "plain",
      size = 9.5
    ),
    legend.title = element_blank()
  )

p_sex_compare



# ------------------------------------------------------------
# 1. OFF29-derived expected counts
# ------------------------------------------------------------

expected <- c(
  "Health" = 158,
  "General public services" = 110,
  "Defense" = 41,
  "Other" = 102,
  "Social protection" = 370,
  "Education" = 105,
  "Public order and safety" = 18
)

# ------------------------------------------------------------
# 2. Standardize Domain labels
# ------------------------------------------------------------

Data_all_sections <- Data_all_sections %>%
  mutate(
    Domain = str_squish(as.character(Domain)),
    Domain = recode(
      Domain,
      "Defence" = "Defense"
    )
  )

# Check the labels
unique(Data_all_sections$Domain)

# ------------------------------------------------------------
# 3. Count respondents in each Domain
# ------------------------------------------------------------

domain_counts <- Data_all_sections %>%
  filter(!is.na(Domain), Domain != "") %>%
  count(Domain, name = "Respondents") %>%
  complete(
    Domain = names(expected),
    fill = list(Respondents = 0)
  )

domain_counts

# ------------------------------------------------------------
# 4. Calculate respondent share and COFOG share
# ------------------------------------------------------------

total_respondents <- sum(domain_counts$Respondents)

comparison_wide <- domain_counts %>%
  mutate(
    Expected = unname(expected[Domain]),
    
    # Blue bars: share of respondents
    Sample_share = 100 * Respondents / total_respondents,
    
    # Orange bars: OFF29 spending share
    COFOG_share = 100 * Expected / 904
  ) %>%
  arrange(desc(Sample_share)) %>%        # sort by study sample
  mutate(
    Domain = factor(Domain, levels = Domain)
  )


# Order domains by study sample (highest to lowest)
domain_order <- comparison_wide %>%
  arrange(desc(Sample_share)) %>%
  pull(Domain)


# ------------------------------------------------------------
# 5. Convert to long format for plotting
# ------------------------------------------------------------

comparison_df <- comparison_wide %>%
  select(
    Domain,
    Sample_share,
    COFOG_share
  ) %>%
  pivot_longer(
    cols = c(Sample_share, COFOG_share),
    names_to = "Group",
    values_to = "Percent"
  ) %>%
  mutate(
    Group = recode(
      Group,
      "Sample_share" = "Study sample",
      "COFOG_share" = "COFOG spending share"
    )
  )

# ------------------------------------------------------------
# 6. Set the domain order
# ------------------------------------------------------------

comparison_df <- comparison_df %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "Study sample",
        "COFOG spending share"
      )
    )
  )

# ------------------------------------------------------------
# 7. Plot
# ------------------------------------------------------------

p_domain_compare <- ggplot(
  comparison_df,
  aes(
    x = Domain,
    y = Percent,
    fill = Group
  )
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68
  ) +
  geom_text(
    aes(label = sprintf("%.1f", Percent)),
    position = position_dodge(width = 0.78),
    vjust = -0.5,
    size = 3.1,
    family = "sans"
  ) +
  scale_fill_manual(
    values = c(
      "Study sample" = "#4C72B0",
      "COFOG spending share" = "#DD8452"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      max(comparison_df$Percent) * 1.15
    ),
    expand = expansion(mult = c(0, 0))
  ) + 
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 15)
  ) +
  labs(
    title = "Domain distribution",
    subtitle = "Respondent share compared with COFOG spending share",
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      hjust = 0.5,
      size = 13
    ),
    plot.subtitle = element_text(
      family = "sans",
      face = "plain",
      hjust = 0.5,
      size = 10.5,
      colour = "grey35",
      margin = margin(b = 10)
    ),
    axis.text.x = element_text(
      family = "sans",
      face = "plain",
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      lineheight = 0.9,
      colour = "black",
      size = 9,
      margin = margin(t = 7)
    ),
    axis.text.y = element_text(
      family = "sans",
      face = "plain",
      colour = "black",
      size = 9.5
    ),
    axis.title.y = element_text(
      family = "sans",
      face = "plain",
      size = 10
    ),
    legend.position = "top",
    legend.text = element_text(
      family = "sans",
      face = "plain",
      size = 9.5
    ),
    legend.title = element_blank()
  )

p_domain_compare





# ============================================================
# 2. Shared axis theme
# ============================================================

clean_axis_theme <- theme(
  axis.line.x = element_line(
    colour = "black",
    linewidth = 0.35
  ),
  axis.line.y = element_line(
    colour = "black",
    linewidth = 0.35
  ),
  axis.ticks.x = element_line(
    colour = "black",
    linewidth = 0.35
  ),
  axis.ticks.y = element_line(
    colour = "black",
    linewidth = 0.35
  ),
  axis.ticks.length = unit(
    0.10,
    "cm"
  ),
  axis.text.y = element_text(
    family = "sans",
    colour = "black",
    size = 9
  ),
  axis.title.y = element_text(
    family = "sans",
    colour = "black",
    size = 10
  )
)


# ------------------------------------------------------------
# Panel A: Level of government
# Keep y-axis numbers and % title
# ------------------------------------------------------------

p_government_panel_updated <- p_government_no_values +
  labs(
    title = "Level of government",
    subtitle = NULL,
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(
      0,
      60,
      by = 20
    ),
    labels = waiver(),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  clean_axis_theme +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      hjust = 0.5,
      size = 12
    ),
    
    axis.text.x = element_text(
      family = "sans",
      colour = "black",
      size = 11,
      margin = margin(t = 7)
    ),
    
    legend.position = c(0.62, 0.98),
    legend.justification = c(0, 1),
    legend.direction = "vertical",
    legend.background = element_blank(),
    legend.key.size = unit(0.35, "cm"),
    legend.text = element_text(
      family = "sans",
      size = 11
    ),
    legend.title = element_blank(),
    
    plot.margin = margin(
      t = 5,
      r = 14,
      b = 5,
      l = 5
    )
  )


# ------------------------------------------------------------
# Panel B: Sex
# Keep y-axis numbers and % title
# Remove duplicate legend
# ------------------------------------------------------------

p_sex_panel_updated <- p_sex_no_values +
  labs(
    title = "Sex",
    subtitle = NULL,
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  guides(
    fill = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(
      0,
      100,
      by = 20
    ),
    labels = waiver(),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  clean_axis_theme +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      hjust = 0.5,
      size = 12
    ),
    
    axis.text.x = element_text(
      family = "sans",
      colour = "black",
      size = 11,
      margin = margin(t = 7)
    ),
    
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 5,
      l = 8
    )
  )


# ------------------------------------------------------------
# Panel C: Domain
# Keep y-axis numbers and % title
# Remove duplicate legend
# ------------------------------------------------------------

p_domain_panel_updated <- p_domain_no_values +
  labs(
    title = "Domain (vs. COFOG spending share)",
    subtitle = NULL,
    x = NULL,
    y = "%",
    fill = NULL
  ) +
  guides(
    fill = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    breaks = seq(
      0,
      60,
      by = 20
    ),
    labels = waiver(),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  scale_x_discrete(
    labels = function(x) {
      stringr::str_wrap(
        x,
        width = 15
      )
    }
  ) +
  clean_axis_theme +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      hjust = 0.5,
      size = 12
    ),
    
    axis.text.x = element_text(
      family = "sans",
      colour = "black",
      size = 11,
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      lineheight = 0.9,
      margin = margin(t = 7)
    ),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 8
    )
  )


# ============================================================
# 3. Combine the three panels
# ============================================================

p_government_panel_updated <- p_government_panel_updated +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(0.1, 1),
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )


p_sex_panel_updated <- p_sex_panel_updated +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(0.1, 1),
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )



p_domain_panel_updated <- p_domain_panel_updated +
  labs(tag = "C") +
  theme(
    plot.tag.position = c(0.065, 1),
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )





combined_figure_rep <- (
  p_government_panel_updated |
    p_sex_panel_updated |
    p_domain_panel_updated
) +
  plot_layout(
    widths = c(0.82, 0.82, 1.28)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )


combined_figure_rep




## ----------------------------------------------------------------------------

# ============================================================
#  Coverage --> respondents 
# ============================================================


organization_lookup <- tribble(
  ~Organization_clean,                                      ~Domain_from_org,
  
  "Aarhus Universitet",                                    "Education",
  "Aarhus Universitetshospital",                           "Health",
  "Danmarks Meteorologiske Institut",                      "Other",
  "Department of Clinical Medicine",                       "Education",
  "Folketingets Ombudsmand",                               "General public services",
  "Forsvaret",                                             "Defense",
  "Handicapformidlingen",                                  "Social protection",
  "Hospital",                                              "Health",
  "Hud og kønssygdomme",                                   "Health",
  "Justitsministeriet",                                    "Public order and safety",
  "Jydske Dragonregiment, 2. Kampvognseskadron",           "Defense",
  "Klima-, Energi- og Forsyningsministeriet",              "Other",
  "Københavns Kommune",                                    "General public services",
  "Kofoeds Skole",                                         "Social protection",
  "Miljøstyrelsen",                                        "Other",
  "Odense Universitetshospital",                           "Health",
  "Region Midtjylland",                                    "Health",
  "Regionen Sundhedsministeriet",                          "Health",
  "Sundhedssektoren",                                      "Health",
  "Sundhedsvæsenet",                                       "Health",
  "Sygehusvæsenet",                                        "Health",
  "Udviklings- og forenklingsstyrelsen",                   "General public services",
  "Unge, Job og Uddannelse, Jobcenter Aarhus",             "Social protection",
  "Universitetshospital",                                  "Health"
)


#   Check if all 24 is included

nrow(organization_lookup)
[1] 24


# Add the classification onto the dataset:

Data_all_sections <- Data_all_sections %>%
  left_join(
    organization_lookup,
    by = "Organization_clean"
  )


# Check result/unmatch organizations

Data_all_sections %>%
  distinct(Organization_clean, Domain_from_org) %>%
  arrange(Domain_from_org, Organization_clean)

Data_all_sections %>%
  filter(is.na(Domain_from_org)) %>%
  distinct(Organization_clean)




# Expected institutions from OFF29 (2025)

expected <- c(
  "Health" = 158,
  "General public services" = 110,
  "Defense" = 41,
  "Other" = 102,
  "Social protection" = 370,
  "Education" = 105,
  "Public order and safety" = 18
)


# Count each unique organization only once

coverage <- Data_all_sections %>%
  distinct(Organization_clean, Domain_from_org) %>%
  filter(
    !is.na(Organization_clean),
    !is.na(Domain_from_org)
  ) %>%
  count(Domain_from_org, name = "Organizations") %>%
  rename(Domain = Domain_from_org) %>%
  complete(
    Domain = names(expected),
    fill = list(Organizations = 0)
  ) %>%
  mutate(
    Expected = unname(expected[Domain]),
    Coverage = 100 * Organizations / Expected,
    Label = sprintf(
      "%.2f%% (%d/%d)",
      Coverage,
      Organizations,
      Expected
    ),
    Domain = fct_reorder(Domain, Coverage)
  )

coverage


# Check that there are exactly 24 unique organizations
Data_all_sections %>%
  distinct(Organization_clean, Domain_from_org) %>%
  summarise(total_unique_organizations = n())


# Overall coverage based on 24 unique organizations
total_organizations <- n_distinct(
  Data_all_sections$Organization_clean,
  na.rm = TRUE
)

overall <- 100 * total_organizations / 904


# Create plot
p_coverage <- ggplot(
  coverage,
  aes(x = Coverage, y = Domain)
) +
  geom_col(
    width = 0.58,
    fill = "#4C72B0"
  ) +
  geom_text(
    aes(label = Label),
    hjust = -0.08,
    size = 3.4
  ) +
  geom_vline(
    xintercept = overall,
    linetype = "22",
    linewidth = 0.22,
    colour = "red",
    alpha = 0.7
  ) +
  annotate(
    "text",
    x = overall,
    y = Inf,
    label = sprintf(
      "Overall coverage: %.1f%%",
      overall
    ),
    colour = "red",
    size = 3.5,
    vjust = -0.9
  ) +
  scale_x_continuous(
    limits = c(
      0,
      max(coverage$Coverage) * 1.18
    ),
    breaks = seq(
      0,
      ceiling(max(coverage$Coverage) * 1.18),
      by = 2
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Response Coverage by Domain",
    subtitle = paste0(
      "Based on ",
      total_organizations,
      " unique responding organizations"
    ),
    x = "Coverage (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15,
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(
      size = 10.5,
      colour = "grey35",
      margin = margin(b = 28)
    ),
    axis.text.y = element_text(
      colour = "black",
      size = 10
    ),
    axis.text.x = element_text(
      colour = "grey35",
      size = 9
    ),
    axis.title.x = element_text(
      size = 10.5,
      margin = margin(t = 8)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey88",
      linewidth = 0.3
    ),
    plot.margin = margin(
      t = 24,
      r = 35,
      b = 12,
      l = 12
    )
  ) +
  coord_cartesian(
    clip = "off"
  )

p_coverage






# =======================================================================================
# 1.1 Data analysis - SECTION 2-11 + NON-USERS’ PERSPECTIVES + GENERAL REFLECTIONS
# =======================================================================================


## Step 1 (AI tools used): Create Horizontal bar chart Ordered by frequency


tool_counts <- c(
  "Chatbots and AI assistants" = sum(Data_all_sections$"Chatbots and AI assistants"== "Selected", na.rm = TRUE),
  "Digital assistants" = sum(Data_all_sections$"Digital assistants"== "Selected", na.rm = TRUE),
  "Automation of repetitive tasks or processes" = sum(Data_all_sections$"Automation of repetitive tasks or processes"== "Selected", na.rm = TRUE),
  "Systems for searching, organizing, and sharing knowledge" = sum(Data_all_sections$"Systems for searching, organizing, and sharing knowledge"== "Selected", na.rm = TRUE),
  "Speech understanding and analysis" = sum(Data_all_sections$"Speech understanding and analysis"== "Selected", na.rm = TRUE),
  "Cybersecurity and threat detection" = sum(Data_all_sections$"Cybersecurity and threat detection"== "Selected", na.rm = TRUE),
  "Prediction of future events or patterns" = sum(Data_all_sections$"Prediction of future events or patterns"== "Selected", na.rm = TRUE),
  "Recommendation systems" = sum(Data_all_sections$"Recommendation systems"== "Selected", na.rm = TRUE),
  "Recognition of people and identities" = sum(Data_all_sections$"Recognition of people and identities"== "Selected", na.rm = TRUE),
  "Smart robots and autonomous systems" = sum(Data_all_sections$"Smart robots and autonomous systems"== "Selected", na.rm = TRUE),
  "Other" = sum(Data_all_sections$"Other"== "Selected", na.rm = TRUE)
)

tool_counts


# Convert to a data frame
tool_df <- data.frame(
  tool = names(tool_counts),
  count = as.numeric(tool_counts)
)


# Total number of respondents
n_total <- nrow(Data_all_sections)

# Calculate percentages
tool_df$percentage <- round(tool_df$count / n_total * 100, 1)

# Create labels with count and percentage
tool_df$label <- paste0(
  tool_df$count,
  " (",
  tool_df$percentage,
  "%)"
)


# Reorder categories by count
tool_df$tool <- fct_reorder(tool_df$tool, tool_df$count)


# Wrap long text
tool_df$tool <- dplyr::recode(
  tool_df$tool,
  "Systems for searching, organizing, and sharing knowledge" =
    "Systems for searching, organizing,\nand sharing knowledge"
)


## Plot (horizontal)
p <- ggplot(
  tool_df,
  aes(
    x = count,
    y = tool
  )
) +
  geom_col(
    width = 0.70,
    fill = "#4C72B0",
    show.legend = FALSE
  ) +
  
  geom_text(
    aes(label = label),
    nudge_x = 0.35,
    hjust = 0,
    size = 3.0,
    family = "sans",
    colour = "black"
  ) +
  
  scale_x_continuous(
    limits = c(0, 62),
    breaks = seq(0, 50, 10),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "AI tools type used",
    x = "Number of respondents",
    y = NULL
  ) +
  
  theme_classic(
    base_size = 9,
    base_family = "sans"
  ) +
  
  theme(
    
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      colour = "black",
      margin = margin(r = 2)
    ),
    
    axis.text.x = element_text(
      size = 8.5,
      colour = "black"
    ),
    
    axis.title.x = element_text(
      size = 9,
      colour = "black",
      margin = margin(t = 8)
    ),
    
    axis.title.y = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.50
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.50
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(
      t = 8,
      r = 20,
      b = 8,
      l = 2
    )
  )

p



## ----------------------------------------------------------------------------


## Step 2 (Frequency of AI use): Create ordered bar chart/diverging stacked bar when comparing groups


# Filter so if respondents said i don't use = non-user or else user

tool_cols <- c(
  "Chatbots and AI assistants",
  "Digital assistants",
  "Automation of repetitive tasks or processes",
  "Systems for searching, organizing, and sharing knowledge",
  "Speech understanding and analysis",
  "Cybersecurity and threat detection",
  "Prediction of future events or patterns",
  "Recommendation systems",
  "Recognition of people and identities",
  "Smart robots and autonomous systems",
  "Other"
)

# Convert to numeric values

Data_all_sections[tool_cols] <- lapply(
  Data_all_sections[tool_cols],
  function(x) ifelse(x == "Selected", 1, 0)
)

Data_all_sections$AI_user <- ifelse(
  rowSums(Data_all_sections[, tool_cols], na.rm = TRUE) > 0,
  "User",
  "Non-user"
)

# Check how many users vs non-users
table(Data_all_sections$AI_user)


# Create frequency data frame for AI users only
freq_df <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  count(frequency = `How often do you use AI?`) %>%
  mutate(
    frequency = factor(
      frequency,
      levels = c(
        "Rarely",
        "Occasionally",
        "Moderately",
        "Quite often",
        "Very often"
       )
    ),
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  )


# Plot
p2 <- ggplot(
  freq_df,
  aes(
    x = frequency,
    y = n,
    fill = n
  )
) +
  
  geom_col(
    width = 0.60,
    show.legend = FALSE
  ) +
  
  geom_text(
    aes(label = label),
    vjust = -1,
    size = 3,
    family = "sans",
    colour = "black"
  ) +
  
  geom_col(
    width = 0.70,
    fill = "#4C72B0",
    show.legend = FALSE
  ) +
  
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 15),
    expand = expansion(add = c(0.5, 0))
  ) +
  
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, 2),
    expand = expansion(mult = c(0, 0.01))
  ) +
  
  labs(
    title = "Frequency of AI use",
    x = NULL,
    y = "Respondents"
  ) +
  
  theme_classic(
    base_size = 9,
    base_family = "sans"
  ) +
  
  theme(
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    
    axis.text.x = element_text(
      size = 8.5,
      colour = "black",
      angle = 0,
      hjust = 0.5,
      margin = margin(t = 4)
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      colour = "black"
    ),
    
    axis.title.y = element_text(
      size = 9,
      margin = margin(r = 6)
    ),
    
    axis.title.x = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.35
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(
      t = 5,
      r = 7,
      b = 5,
      l = 8
    )
  )

p2




## ----------------------------------------------------------------------------

## Step 3 (self-rated experience): Create ordered bar chart/diverging stacked bar when comparing groups

# We can use the same data from Data_all_sections since its converted to factor if reported one of the tools = users


# Create self-rated experience data frame for AI users only
self_df <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  count(experience = `What is your experience with AI tools?`) %>%
  mutate(
    experience = factor(
      experience,
      levels = c(
        "Bad",
        "Neutral",
        "Good",
        "Very good",
        "Very bad"
      )
    ),
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  filter(!is.na(experience))


# Plot
p3 <- ggplot(
  self_df,
  aes(
    x = experience,
    y = n
  )
) +
  
  geom_col(
    width = 0.70,
    fill = "#4C72B0",
    show.legend = FALSE
  ) +
  
  geom_text(
    aes(label = label),
    vjust = -1,
    size = 3,
    family = "sans",
    colour = "black"
  ) +
  
  scale_x_discrete(
    drop = TRUE,
    labels = function(x) stringr::str_wrap(x, width = 15),
    expand = expansion(mult = c(0.5, 0))
  ) +
  
  scale_y_continuous(
    limits = c(0, 31),
    breaks = seq(0, 30, 5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Self-rated experience",
    x = NULL,
    y = "Respondents"
  ) +
  
  theme_classic(
    base_size = 9,
    base_family = "sans"
  ) +
  
  theme(
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 6)
    ),
    
    axis.text.x = element_text(
      size = 8.5,
      colour = "black",
      angle = 0,
      hjust = 0.5,
      margin = margin(t = 4)
    ),
    
    axis.text.y = element_text(
      size = 8.5,
      colour = "black"
    ),
    
    axis.title.y = element_text(
      size = 9,
      margin = margin(r = 6)
    ),
    
    axis.title.x = element_blank(),
    
    axis.line = element_line(
      colour = "black",
      linewidth = 0.5
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.4
    ),
    
    panel.grid = element_blank(),
    
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

p3




#### STORE INTO 1 PANEL OF A, B AND C ####


p2 <- p2 +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 15),
    expand = expansion(add = c(0.45, 0.20))
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )



p3 <- p3 +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 15),
    expand = expansion(add = c(0.45, 0.20))
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )



p <- p +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(0.37, 1),
    plot.tag = element_text(
      size = 12,
      face = "bold"
    )
  )

p2 <- p2 +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(0.095, 1),
    plot.tag = element_text(
      size = 12,
      face = "bold"
    )
  )

p3 <- p3 +
  labs(tag = "C") +
  theme(
    plot.tag.position = c(0.1, 1),
    plot.tag = element_text(
      size = 12,
      face = "bold"
    )
  )


combined_plot <- (
  p |
    plot_spacer() |
    p2 |
    plot_spacer() |
    p3
) +
  plot_layout(
    widths = c(
      1.2,  # Panel A
      -0.15,  # small gap
      1.2,  # Panel B: wider
      0,  # small gap
      1.00   # Panel C
    )
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(
      size = 12,
      face = "bold"
    )
  )

combined_plot





## ----------------------------------------------------------------------------


## Step 4 (SECTION 3: USAGE PATTERNS): Create a stacked bar chart


# Create an ordered factor

future_use <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `I intend to keep using AI tools in my work in the future`,
    `I will try to use AI tools in my work whenever I have a chance`,
    `I plan to use AI tools often in my daily work`,
    `I support the use of AI tools in my own work`,
    `I think adding AI tools to my work is a good idea`
  ) %>%
  mutate(across(everything(), ~ str_trim(as.character(.))))

# Define response order (GREEN -> RED)
valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)

# Convert to ordered factors
future_use[] <- lapply(
  future_use,
  function(x)
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
)

future_use <- as.data.frame(future_use)

# Create long data frame
future_all <- future_use %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()

# Calculate number of respondents per question
question_n <- future_use %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(Question, name = "Question_n")

# Add n to each question
future_all <- future_all %>%
  left_join(question_n, by = "Question") %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )

# Number of respondents
n_future <- sum(
  rowSums(!is.na(future_use)) > 0
)



# The following plot can we reused for all bars simply by reusing "make_plot"

make_plot <- function(data, plot_title) {
  
  ggplot(
    data,
    aes(
      x = paste0(
        stringr::str_wrap(as.character(Question), 45),
        " (n = ",
        Question_n,
        ")"
      ),
      y = Percentage,
      fill = Response
    )
  ) +
    
    geom_col(
      width = 0.55,
      colour = "white",
      linewidth = 0.3,
      position = position_stack(reverse = TRUE),
      na.rm = TRUE
    ) +
    
    geom_text(
      aes(
        label = ifelse(
          !is.na(Percentage) & Percentage >= 5,
          Label,
          ""
        )
      ),
      position = position_stack(
        vjust = 0.5,
        reverse = TRUE
      ),
      size = 2.8,
      colour = "black",
      family = "sans",
      na.rm = TRUE
    ) +
    
    coord_flip(
      ylim = c(0, 100),
      clip = "off"
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    
    scale_fill_manual(
      values = c(
        "Strongly agree" = "#2F6C99",
        "Somewhat agree" = "#8BB8D9",
        "Undecided" = "#D9D9D9",
        "Somewhat disagree" = "#E89C62",
        "Strongly disagree" = "#C63D2F"
      ),
      breaks = c(
        "Strongly agree",
        "Somewhat agree",
        "Undecided",
        "Somewhat disagree",
        "Strongly disagree"
      ),
      drop = TRUE
    ) +
    
    labs(
      title = plot_title,
      x = NULL,
      y = "Percentage of respondents",
      fill = NULL
    ) +
    
    theme_classic(
      base_size = 10,
      base_family = "sans"
    ) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        size = 11,
        colour = "black",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      
      axis.text.y = element_text(
        size = 10,
        colour = "black",
        lineheight = 0.95,
        margin = margin(r = 8)
      ),
      
      axis.text.x = element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.title.x = element_text(
        size = 10,
        colour = "black",
        margin = margin(t = 8)
      ),
      
      axis.title.y = element_blank(),
      
      axis.line.x = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.line.y = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 9,
        colour = "black"
      ),
      
      legend.key.size = unit(0.45, "cm"),
      legend.spacing.x = unit(0.15, "cm"),
      legend.margin = margin(t = 10),
      
      plot.margin = margin(
        t = 10,
        r = 15,
        b = 10,
        l = 10
      )
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
}


# Create plot
future_plot <- make_plot(
  future_all,
  "Expected future use of AI tools among public sector employees"
)
future_plot





## ----------------------------------------------------------------------------


## Step 4 (SECTION 4: WORK PERFORMANCE): Create a stacked bar chart


# Create an ordered factor

# Create the data set
perceived_performance <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `AI tools are helpful for my work`,
    `AI tools help me reach important goals at work`,
    `AI tools help me finish my work tasks faster`,
    `AI tools make the quality of my work better`,
    `AI tools help me come up with more creative ideas in my work`
  ) %>%
  mutate(across(everything(), ~ str_trim(as.character(.))))

# Define response order (GREEN -> RED)
valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)

# Convert to ordered factors
perceived_performance[] <- lapply(
  perceived_performance,
  function(x)
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
)

perceived_performance <- as.data.frame(perceived_performance)

# Create long data frame
performance_all <- perceived_performance %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()

# Calculate number of respondents per question
question_n <- perceived_performance %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(Question, name = "Question_n")

# Add n to each question
performance_all <- performance_all %>%
  left_join(question_n, by = "Question") %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )

# Number of respondents
n_performance <- sum(
  rowSums(!is.na(perceived_performance)) > 0
)



# create plot

performance_plot <- make_plot(
  performance_all,
  "Perceived performance impact of AI in public sector work"
)
performance_plot




## ----------------------------------------------------------------------------


## Step 5 (SECTION 5: LEARNING AND INTERACTION): Create a stacked bar chart


# 1. Select ethical concern questions -------------------------------------

learning <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `It is easy for me to learn how to use AI tools`,
    `AI tools are easy for me to use at work`,
    `I understand how to use AI tools`,
    `I feel sure that I can become good at using AI tools`,
    `Using AI tools at work doesn't take much mental effort`,
    `I have attended enough training to use AI tools at work`,
    `In our organization, we are provided with opportunities to learn new technological skills due to changes related to AI`,
    `In our organization, we are provided with opportunities to learn new soft skills due to changes related to AI`,
    `In our organization, we are provided with opportunities to learn about the risks of using AI tools as part of developing a critical approach to their use`,
    `I am worried that some employees could be left out if they do not get training on AI`
  ) %>%
  mutate(across(everything(),~ na_if(str_trim(as.character(.)), "")))


# Define response order
valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)

# Convert responses to ordered factors
learning <- learning %>%
  mutate(
    across(
      everything(),
      ~ factor(
        .,
        levels = valid_levels,
        ordered = TRUE
      )
    )
  )

# Create long data frame
learning_long <- learning %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  )

# Number of valid respondents for each question
question_n <- learning_long %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )

# Calculate counts and percentages
learning_all <- learning_long %>%
  mutate(
    Response = factor(
      as.character(Response),
      levels = valid_levels,
      ordered = TRUE
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = if_else(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup() %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )

# Total number of respondents who answered at least one item
n_learning <- sum(
  rowSums(!is.na(learning)) > 0
)

# Define conceptual groups
confidence_items <- c(
  "Using AI tools at work doesn't take much mental effort",
  "It is easy for me to learn how to use AI tools",
  "I understand how to use AI tools",
  "I feel sure that I can become good at using AI tools",
  "AI tools are easy for me to use at work"
)

learning_items <- c(
  "I have attended enough training to use AI tools at work",
  "I am worried that some employees could be left out if they do not get training on AI",
  "In our organization, we are provided with opportunities to learn new technological skills due to changes related to AI",
  "In our organization, we are provided with opportunities to learn new soft skills due to changes related to AI",
  "In our organization, we are provided with opportunities to learn about the risks of using AI tools as part of developing a critical approach to their use"
)

# Create factor levels that include the n labels
confidence_label_levels <- learning_all %>%
  filter(Question %in% confidence_items) %>%
  distinct(Question, Question_label) %>%
  mutate(
    Question = factor(
      Question,
      levels = confidence_items
    )
  ) %>%
  arrange(Question) %>%
  pull(Question_label)

learning_label_levels <- learning_all %>%
  filter(Question %in% learning_items) %>%
  distinct(Question, Question_label) %>%
  mutate(
    Question = factor(
      Question,
      levels = learning_items
    )
  ) %>%
  arrange(Question) %>%
  pull(Question_label)

# Split data into the two groups
confidence_df <- learning_all %>%
  filter(Question %in% confidence_items) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(confidence_label_levels)
    )
  )

learning_support_df <- learning_all %>%
  filter(Question %in% learning_items) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(learning_label_levels)
    )
  )



# create plots
make_plot_long <- function(data, plot_title) {
  
  ggplot(
    data,
    aes(
      x = paste0(
        stringr::str_wrap(as.character(Question), 55),
        " (n = ",
        Question_n,
        ")"
      ),
      y = Percentage,
      fill = Response
    )
  ) +
    
    geom_col(
      width = 0.55,
      colour = "white",
      linewidth = 0.3,
      position = position_stack(reverse = TRUE),
      na.rm = TRUE
    ) +
    
    geom_text(
      aes(
        label = ifelse(
          !is.na(Percentage) & Percentage >= 5,
          Label,
          ""
        )
      ),
      position = position_stack(
        vjust = 0.5,
        reverse = TRUE
      ),
      size = 2.8,
      colour = "black",
      family = "sans",
      na.rm = TRUE
    ) +
    
    coord_flip(
      ylim = c(0, 100),
      clip = "off"
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    
    scale_fill_manual(
      values = c(
        "Strongly agree" = "#2F6C99",
        "Somewhat agree" = "#8BB8D9",
        "Undecided" = "#D9D9D9",
        "Somewhat disagree" = "#E89C62",
        "Strongly disagree" = "#C63D2F"
      ),
      breaks = c(
        "Strongly agree",
        "Somewhat agree",
        "Undecided",
        "Somewhat disagree",
        "Strongly disagree"
      ),
      drop = TRUE
    ) +
    
    labs(
      title = plot_title,
      x = NULL,
      y = "Percentage of respondents",
      fill = NULL
    ) +
    
    theme_classic(
      base_size = 10,
      base_family = "sans"
    ) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        size = 11,
        colour = "black",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      
      axis.text.y = element_text(
        size = 10,
        colour = "black",
        lineheight = 0.95,
        margin = margin(r = 8)
      ),
      
      axis.text.x = element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.title.x = element_text(
        size = 10,
        colour = "black",
        margin = margin(t = 8)
      ),
      
      axis.title.y = element_blank(),
      
      axis.line.x = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.line.y = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 9,
        colour = "black"
      ),
      
      legend.key.size = unit(0.45, "cm"),
      legend.spacing.x = unit(0.15, "cm"),
      legend.margin = margin(t = 10),
      
      plot.margin = margin(
        t = 10,
        r = 15,
        b = 10,
        l = 10
      )
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
}


confidence_plot <- make_plot(
  confidence_df,
  "AI knowledge and confidence"
)
confidence_plot


learning_support_plot <- make_plot_long(
  training_support_df,
  "Training and organizational support"
)
learning_support_plot





## ----------------------------------------------------------------------------


## Step 6 ( ETHICAL CONSIDERATIONS AND CONCERNS): Create a stacked bar chart


# 1. Select ethical concern questions -------------------------------------

ethical_consideration <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `I am familiar with the risks associated with using AI tools at work`,
    `I believe that AI tools provide reliable support for our organization’s operations`,
    `I believe that AI tools do not violate intellectual property rights`,
    `I believe that AI tools operate transparently`,
    `I believe that AI tools protect the data I provide to them`,
    `I feel comfortable providing data to AI tools`,
    `I am aware of the risks of data misuse when using AI tools`,
    `I believe that AI tools in our organization do not unjustifiably infringe on employees’ privacy`,
    `I am aware of how AI tools collect and use data`,
    `I believe that AI tools treat users fairly and without discrimination`,
    `I believe that AI tools handle users’ data responsibly`,
    `I believe that AI tools adhere to ethical standards`,
    `I believe that the results produced by AI tools are accurate`
  ) %>%
  mutate(across(everything(), ~ str_trim(as.character(.))))


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

ethical_consideration[] <- lapply(
  ethical_consideration,
  function(x)
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
)

ethical_consideration <- as.data.frame(ethical_consideration)


# 4. Number of respondents with at least one valid response --------------

n_consideration <- sum(
  rowSums(!is.na(ethical_consideration)) > 0
)

# 5. Create long-format plotting data -------------------------------------

consideration_all <- ethical_consideration %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate number of respondents per question -------------------------

question_n <- ethical_consideration %>%
  pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(Question, name = "Question_n")


# 7. Add sample size to each question label -------------------------------

consideration_all <- consideration_all %>%
  left_join(question_n, by = "Question") %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

trust_accuracy_items <- c(
  "I believe that the results produced by AI tools are accurate",
  "I believe that AI tools provide reliable support for our organization’s operations",
  "I believe that AI tools operate transparently",
  "I am familiar with the risks associated with using AI tools at work",
  "I am aware of how AI tools collect and use data",
  "I am aware of the risks of data misuse when using AI tools"
)

privacy_data_items <- c(
  "I feel comfortable providing data to AI tools",
  "I believe that AI tools protect the data I provide to them",
  "I believe that AI tools handle users’ data responsibly",
  "I believe that AI tools in our organization do not unjustifiably infringe on employees’ privacy",
  "I believe that AI tools do not violate intellectual property rights",
  "I believe that AI tools treat users fairly and without discrimination",
  "I believe that AI tools adhere to ethical standards"
)



# 9. Create question ordering variable ------------------------------------

consideration_all <- consideration_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        trust_accuracy_items,
        privacy_data_items
      )
    )
  )


# 10. Create ethical-concern plotting dataset -----------------------------

# Group 1: Trust, transparency, and accuracy

trust_accuracy <- consideration_all %>%
  filter(
    Question %in% trust_accuracy_items
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# Group 2: Privacy and data responsibility

privacy_data <- consideration_all %>%
  filter(
    Question %in% privacy_data_items
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )



# 11. Create plots ---------------------------------------------------------
trust_accuracy_plot <- make_plot(
  trust_accuracy,
  "Trust and risk awareness"
)
trust_accuracy_plot


privacy_data_plot <- make_plot_long(
  privacy_data,
  "Privacy and responsible AI use"
)
privacy_data_plot




# 1. Select ethical concern questions -------------------------------------

ethical_concern <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `I am concerned that public employees may become overly reliant on AI tools`,
    `The use of AI tools could hinder the development of key competencies among public employees`,
    `I am concerned that AI tools could widen the digital gap among public employees`,
    `I am concerned that citizens may become overly reliant on AI tools`,
    `I am concerned that AI tools could widen the digital gap among citizens`,
    `AI tools could reduce opportunities for direct interaction between public employees and citizens`,
    `AI tools can produce false information, known as hallucinations`,
    `I am concerned that relying on AI tool outputs could lead to incorrect decisions at work`,
    `AI tools can exhibit linguistic or cultural biases in their outputs`,
    `I am concerned that AI tools could lead to discrimination`,
    `I am concerned that AI tools diminish the value of my professional work`,
    `I am concerned that AI tools reduce my autonomy at work`,
    `I am concerned that my work is being monitored through the AI tools I use`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

ethical_concern[] <- lapply(
  ethical_concern,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

ethical_concern <- as.data.frame(ethical_concern)


# 4. Number of respondents with at least one valid response --------------

n_concern <- sum(
  rowSums(!is.na(ethical_concern)) > 0
)


# 5. Create long-format plotting data -------------------------------------

concern_all <- ethical_concern %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate number of respondents per question -------------------------

question_n <- ethical_concern %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add sample size to each question label -------------------------------

concern_all <- concern_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

societal_questions <- c(
  "I am concerned that public employees may become overly reliant on AI tools",
  "The use of AI tools could hinder the development of key competencies among public employees",
  "I am concerned that AI tools could widen the digital gap among public employees",
  "I am concerned that citizens may become overly reliant on AI tools",
  "I am concerned that AI tools could widen the digital gap among citizens",
  "AI tools could reduce opportunities for direct interaction between public employees and citizens"
)

workplace_questions <- c(
  "AI tools can produce false information, known as hallucinations",
  "I am concerned that relying on AI tool outputs could lead to incorrect decisions at work",
  "AI tools can exhibit linguistic or cultural biases in their outputs",
  "I am concerned that AI tools could lead to discrimination",
  "I am concerned that AI tools diminish the value of my professional work",
  "I am concerned that AI tools reduce my autonomy at work",
  "I am concerned that my work is being monitored through the AI tools I use"
)


# 9. Create question ordering variable ------------------------------------

concern_all <- concern_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        societal_questions,
        workplace_questions
      )
    )
  )


# 10. Create societal-concern plotting dataset -----------------------------

concern_societal <- concern_all %>%
  filter(
    Question %in% societal_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create workplace-concern plotting dataset ---------------------------

concern_workplace <- concern_all %>%
  filter(
    Question %in% workplace_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_societal <- make_plot_long(
  concern_societal,
  "Concerns about AI use in society and public services"
)
plot_societal


plot_workplace <- make_plot_long(
  concern_workplace,
  "Concerns about AI use in the workplace"
)
plot_workplace



## ----------------------------------------------------------------------------


## Step 7 (SOCIAL AND CONTEXTUAL SETTINGS): Create a stacked bar chart

# 1. Create the data set ---------------------------------------------------

social_setting <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `People who are important to me believe that I should use AI tools at work`,
    `People who influence my work decisions believe that I should use AI tools`,
    `People whose opinions I value support my use of AI tools at work`,
    `My coworkers generally support the use of AI tools`,
    `In my workplace, using AI tools is seen as something positive`,
    `I have the resources I need to use AI tools in my work`,
    `I have the knowledge I need to use AI tools well`,
    `AI tools fit well with how I normally do my work`,
    `If I needed help using AI tools, a colleague would be available to assist me`,
    `In our organization, we have sufficient support for using AI tools at work`,
    `In my wider community, the use of AI tools is generally accepted`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

social_setting[] <- lapply(
  social_setting,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

social_setting <- as.data.frame(social_setting)


# 4. Number of respondents with at least one valid response --------------

n_social <- sum(
  rowSums(!is.na(social_setting)) > 0
)


# 5. Create long-format plotting data -------------------------------------

social_all <- social_setting %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- social_setting %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

social_all <- social_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

social_influence_questions <- c(
  "People who are important to me believe that I should use AI tools at work",
  "People who influence my work decisions believe that I should use AI tools",
  "People whose opinions I value support my use of AI tools at work",
  "My coworkers generally support the use of AI tools",
  "In my workplace, using AI tools is seen as something positive",
  "In my wider community, the use of AI tools is generally accepted"
)

support_conditions_questions <- c(
  "I have the resources I need to use AI tools in my work",
  "I have the knowledge I need to use AI tools well",
  "AI tools fit well with how I normally do my work",
  "If I needed help using AI tools, a colleague would be available to assist me",
  "In our organization, we have sufficient support for using AI tools at work"
)


# 9. Create question-order variable ---------------------------------------

social_all <- social_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        social_influence_questions,
        support_conditions_questions
      )
    )
  )


# 10. Create social-influence plotting dataset -----------------------------

social_influence <- social_all %>%
  filter(
    Question %in% social_influence_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create support-conditions plotting dataset --------------------------

social_support_conditions <- social_all %>%
  filter(
    Question %in% support_conditions_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )



# 12. Create plots ---------------------------------------------------------

plot_social_influence <- make_plot_long(
  social_influence,
  "Social influence and acceptance of AI use at work"
)
plot_social_influence


plot_support_conditions <- make_plot(
  social_support_conditions,
  "Resources, knowledge, compatibility, and support for AI use"
)
plot_support_conditions



## ----------------------------------------------------------------------------


## Step 8 (ORGANIZATIONAL AND INDIVIDUAL READINESS): Create a stacked bar chart


# 1. Create the data set ---------------------------------------------------

AI_readiness <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `In our organization, we have sufficient financial resources to purchase and maintain AI tools`,
    `In our organization, we have opportunities to learn the latest ways to work with AI tools`,
    `In our organization, the ICT infrastructure is regularly updated to better leverage AI tools`,
    `In our organization, new ideas are supported`,
    `In our organization, the organizational structure is adapted to keep up with AI-based innovations`,
    `In our organization, employees are involved in the preparation and implementation of AI solutions`,
    `In our organization, we have the opportunity to learn about implemented AI solutions and their design`,
    `I look for ways to use AI tools to improve my work`,
    `I try out AI tools to complete tasks more efficiently`,
    `I proactively explore how AI tools can help me with my work`,
    `I identify possible problems with AI tools and try to solve them early`,
    `I encourage my coworkers to try AI tools`,
    `I am excited by exploring new possibilities for using AI`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

AI_readiness[] <- lapply(
  AI_readiness,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

AI_readiness <- as.data.frame(AI_readiness)


# 4. Number of respondents with at least one valid response --------------

n_readiness <- sum(
  rowSums(!is.na(AI_readiness)) > 0
)


# 5. Create long-format plotting data -------------------------------------

readiness_all <- AI_readiness %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(as.character(Response)),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- AI_readiness %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

readiness_all <- readiness_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

organizational_readiness_questions <- c(
  "In our organization, we have sufficient financial resources to purchase and maintain AI tools",
  "In our organization, we have opportunities to learn the latest ways to work with AI tools",
  "In our organization, the ICT infrastructure is regularly updated to better leverage AI tools",
  "In our organization, new ideas are supported",
  "In our organization, the organizational structure is adapted to keep up with AI-based innovations",
  "In our organization, employees are involved in the preparation and implementation of AI solutions",
  "In our organization, we have the opportunity to learn about implemented AI solutions and their design"
)

individual_readiness_questions <- c(
  "I look for ways to use AI tools to improve my work",
  "I try out AI tools to complete tasks more efficiently",
  "I proactively explore how AI tools can help me with my work",
  "I identify possible problems with AI tools and try to solve them early",
  "I encourage my coworkers to try AI tools",
  "I am excited by exploring new possibilities for using AI"
)


# 9. Create question-order variable ---------------------------------------

readiness_all <- readiness_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        organizational_readiness_questions,
        individual_readiness_questions
      )
    )
  )


# 10. Create organizational-readiness plotting dataset ---------------------

organizational_readiness <- readiness_all %>%
  filter(
    Question %in% organizational_readiness_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create individual-readiness plotting dataset ------------------------

individual_readiness <- readiness_all %>%
  filter(
    Question %in% individual_readiness_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_organizational_readiness <- make_plot_long(
  organizational_readiness,
  "Organizational readiness for adopting and implementing AI"
)
plot_organizational_readiness


plot_individual_readiness <- make_plot(
  individual_readiness,
  "Proactive individual use and exploration of AI tools"
)
plot_individual_readiness



## ----------------------------------------------------------------------------


## Step 9 (ADAPTABILITY AND RESILIENCE): Create a stacked bar chart

# 1. Create the data set ---------------------------------------------------

adaptability_resilience <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `I can quickly get used to new AI tools at work`,
    `I am open to changing how I work to use AI tools better`,
    `I actively search for training/information to improve my AI skills`,
    `AI allows me to be more flexible in my work tasks`,
    `I keep myself informed about how AI tools are used in my field`,
    `I stay calm when AI tools change how I work`,
    `I manage stress well when learning or adjusting to AI tools`,
    `I feel confident dealing with challenges related to AI at work`,
    `I focus on finding solutions when AI tools do not work as expected`,
    `I can keep doing my job well even when AI tools bring uncertainty`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

adaptability_resilience[] <- lapply(
  adaptability_resilience,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

adaptability_resilience <- as.data.frame(
  adaptability_resilience
)


# 4. Number of respondents with at least one valid response --------------

n_adaptability_resilience <- sum(
  rowSums(!is.na(adaptability_resilience)) > 0
)


# 5. Create long-format plotting data -------------------------------------

adaptability_all <- adaptability_resilience %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- adaptability_resilience %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

adaptability_all <- adaptability_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

adaptability_questions <- c(
  "I can quickly get used to new AI tools at work",
  "I am open to changing how I work to use AI tools better",
  "I actively search for training/information to improve my AI skills",
  "AI allows me to be more flexible in my work tasks",
  "I keep myself informed about how AI tools are used in my field"
)

resilience_questions <- c(
  "I stay calm when AI tools change how I work",
  "I manage stress well when learning or adjusting to AI tools",
  "I feel confident dealing with challenges related to AI at work",
  "I focus on finding solutions when AI tools do not work as expected",
  "I can keep doing my job well even when AI tools bring uncertainty"
)


# 9. Create question-order variable ---------------------------------------

adaptability_all <- adaptability_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        adaptability_questions,
        resilience_questions
      )
    )
  )


# 10. Create adaptability plotting dataset ---------------------------------

adaptability <- adaptability_all %>%
  filter(
    Question %in% adaptability_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create resilience plotting dataset ----------------------------------

resilience <- adaptability_all %>%
  filter(
    Question %in% resilience_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )



# 12. Create plots ---------------------------------------------------------

plot_adaptability <- make_plot(
  adaptability,
  "Adaptability to AI tools and changing work practices"
)
plot_adaptability


plot_resilience <- make_plot(
  resilience,
  "Resilience when facing AI-related change and uncertainty"
)
plot_resilience


## ----------------------------------------------------------------------------


## Step 10 (AI AND YOUR WORK EXPERIENCE): Create a stacked bar chart


# 1. Create the data set ---------------------------------------------------

work_experience <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `I am worried that I could lose my job because AI is taking over tasks currently performed by humans`,
    `AI tools have clearly changed the kind of work I do`,
    `I believe that AI will create new jobs`,
    `Our organization has established rules to protect employees from potential harm caused by AI`,
    `Trade unions are involved in decision-making regarding the use of AI in my organization`,
    `I believe that trade unions should have a greater role in shaping rules for the use of AI in the public sector`,
    `I feel that my opinion matters when new technologies like AI are introduced at work`,
    `I feel stressed by how fast AI tools are being introduced in my workplace`,
    `I avoid using AI tools because I am afraid of making a mistake that no one will notice`,
    `I would think about leaving my job if AI took over important parts of my work`,
    `My managers support using AI tools at work`,
    `I feel relaxed when using AI tools`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

work_experience[] <- lapply(
  work_experience,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

work_experience <- as.data.frame(
  work_experience
)


# 4. Number of respondents with at least one valid response --------------

n_work <- sum(
  rowSums(!is.na(work_experience)) > 0
)


# 5. Create long-format plotting data -------------------------------------

work_all <- work_experience %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- work_experience %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

work_all <- work_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

employment_governance_questions <- c(
  "I am worried that I could lose my job because AI is taking over tasks currently performed by humans",
  "AI tools have clearly changed the kind of work I do",
  "I believe that AI will create new jobs",
  "Our organization has established rules to protect employees from potential harm caused by AI",
  "Trade unions are involved in decision-making regarding the use of AI in my organization",
  "I believe that trade unions should have a greater role in shaping rules for the use of AI in the public sector"
)

employee_experience_questions <- c(
  "I feel that my opinion matters when new technologies like AI are introduced at work",
  "I feel stressed by how fast AI tools are being introduced in my workplace",
  "I avoid using AI tools because I am afraid of making a mistake that no one will notice",
  "I would think about leaving my job if AI took over important parts of my work",
  "My managers support using AI tools at work",
  "I feel relaxed when using AI tools"
)


# 9. Create question-order variable ---------------------------------------

work_all <- work_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        employment_governance_questions,
        employee_experience_questions
      )
    )
  )


# 10. Create employment and governance plotting dataset --------------------

employment_governance <- work_all %>%
  filter(
    Question %in% employment_governance_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create employee-experience plotting dataset -------------------------

employee_experience <- work_all %>%
  filter(
    Question %in% employee_experience_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_employment_governance <- make_plot_long(
  employment_governance,
  "Employment impacts, governance, and protection from AI-related harm"
)
plot_employment_governance


plot_employee_experience <- make_plot_long(
  employee_experience,
  "Employee voice, managerial support, and experiences of using AI"
)
plot_employee_experience



## ----------------------------------------------------------------------------


## Step 11 (OUTCOMES OF USING AI TOOLS): Create a stacked bar chart


# 1. Create the data set ---------------------------------------------------

efficiency_quality <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `AI tools help save time on routine tasks`,
    `AI tools lower the amount of manual work`,
    `AI tools let us focus on more important tasks`,
    `It takes a lot of time to check the results from AI tools`,
    `AI tools help us finish tasks faster overall`,
    `AI tools help reduce errors in our work`,
    `AI tools make our work more accurate`,
    `AI tools help us spot problems early`,
    `AI tools reduce the quality of the final results`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

efficiency_quality[] <- lapply(
  efficiency_quality,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

efficiency_quality <- as.data.frame(
  efficiency_quality
)


# 4. Number of respondents with at least one valid response --------------

n_efficiency <- sum(
  rowSums(!is.na(efficiency_quality)) > 0
)


# 5. Create long-format plotting data -------------------------------------

efficiency_all <- efficiency_quality %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- efficiency_quality %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

efficiency_all <- efficiency_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

efficiency_workload_questions <- c(
  "AI tools help save time on routine tasks",
  "AI tools lower the amount of manual work",
  "AI tools let us focus on more important tasks",
  "It takes a lot of time to check the results from AI tools",
  "AI tools help us finish tasks faster overall"
)

quality_accuracy_questions <- c(
  "AI tools help reduce errors in our work",
  "AI tools make our work more accurate",
  "AI tools help us spot problems early",
  "AI tools reduce the quality of the final results"
)


# 9. Create question-order variable ---------------------------------------

efficiency_all <- efficiency_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        efficiency_workload_questions,
        quality_accuracy_questions
      )
    )
  )


# 10. Create efficiency and workload plotting dataset ----------------------

efficiency_workload <- efficiency_all %>%
  filter(
    Question %in% efficiency_workload_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create quality and accuracy plotting dataset ------------------------

quality_accuracy <- efficiency_all %>%
  filter(
    Question %in% quality_accuracy_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_efficiency_workload <- make_plot(
  efficiency_workload,
  "Efficiency, workload reduction, and task completion"
)
plot_efficiency_workload


plot_quality_accuracy <- make_plot_long(
  quality_accuracy,
  "Quality, accuracy, and early problem detection"
)
plot_quality_accuracy




# 1. Create the data set ---------------------------------------------------

involvement_accountability <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `AI tools make it easier for citizens to give feedback/make requests`,
    `AI tools reduce direct contact between citizens and public employees`,
    `AI tools help involve stakeholders in decision-making`,
    `The use of AI tools is clearly documented`,
    `It is difficult to explain the final result when AI tools are used`,
    `It is clear who is responsible for decisions made with the help of AI tools`,
    `Our organization has a system in place for reporting and resolving issues with AI tools`,
    `Our organization has a system in place to verify the accuracy of results produced by AI tools`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

involvement_accountability[] <- lapply(
  involvement_accountability,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

involvement_accountability <- as.data.frame(
  involvement_accountability
)


# 4. Number of respondents with at least one valid response --------------

n_involvement <- sum(
  rowSums(!is.na(involvement_accountability)) > 0
)


# 5. Create long-format plotting data -------------------------------------

involvement_all <- involvement_accountability %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- involvement_accountability %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

involvement_all <- involvement_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

citizen_stakeholder_involvement_questions <- c(
  "AI tools make it easier for citizens to give feedback/make requests",
  "AI tools reduce direct contact between citizens and public employees",
  "AI tools help involve stakeholders in decision-making"
)

transparency_accountability_questions <- c(
  "The use of AI tools is clearly documented",
  "It is difficult to explain the final result when AI tools are used",
  "It is clear who is responsible for decisions made with the help of AI tools",
  "Our organization has a system in place for reporting and resolving issues with AI tools",
  "Our organization has a system in place to verify the accuracy of results produced by AI tools"
)


# 9. Create question-order variable ---------------------------------------

involvement_all <- involvement_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        citizen_stakeholder_involvement_questions,
        transparency_accountability_questions
      )
    )
  )


# 10. Create citizen and stakeholder involvement dataset -------------------

citizen_stakeholder_involvement <- involvement_all %>%
  filter(
    Question %in% citizen_stakeholder_involvement_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create transparency and accountability dataset ----------------------

transparency_accountability <- involvement_all %>%
  filter(
    Question %in% transparency_accountability_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_citizen_stakeholder_involvement <- make_plot_long(
  citizen_stakeholder_involvement,
  "Citizen and stakeholder engagement"
)
plot_citizen_stakeholder_involvement


plot_transparency_accountability <- make_plot_long(
  transparency_accountability,
  "AI governance"
)
plot_transparency_accountability



# 1. Create the data set ---------------------------------------------------

transparent_compliant <- Data_all_sections %>%
  filter(AI_user == "User") %>%
  select(
    `Stakeholders can access the data that our AI tools use`,
    `Citizens are notified in writing when AI tools are used in administrative decision-making`,
    `The public does not always know if AI tools were used to influence a decision`,
    `AI-generated content is clearly labelled`,
    `We keep records of what prompts and instructions were given to AI tools`,
    `We follow legal rules when using AI tools`,
    `When using AI tools, we protect the rights of citizens`,
    `The use of AI tools in our organization complies with regulations protecting citizens’ rights`,
    `Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

transparent_compliant[] <- lapply(
  transparent_compliant,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

transparent_compliant <- as.data.frame(
  transparent_compliant
)


# 4. Number of respondents with at least one valid response --------------

n_transparent <- sum(
  rowSums(!is.na(transparent_compliant)) > 0
)


# 5. Create long-format plotting data -------------------------------------

transparent_all <- transparent_compliant %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- transparent_compliant %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

transparent_all <- transparent_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

transparency_disclosure_questions <- c(
  "Stakeholders can access the data that our AI tools use",
  "Citizens are notified in writing when AI tools are used in administrative decision-making",
  "The public does not always know if AI tools were used to influence a decision",
  "AI-generated content is clearly labelled",
  "We keep records of what prompts and instructions were given to AI tools"
)

legal_compliance_questions <- c(
  "We follow legal rules when using AI tools",
  "When using AI tools, we protect the rights of citizens",
  "The use of AI tools in our organization complies with regulations protecting citizens’ rights",
  "Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law"
)


# 9. Create question-order variable ---------------------------------------

transparent_all <- transparent_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        transparency_disclosure_questions,
        legal_compliance_questions
      )
    )
  )


# 10. Create transparency and disclosure plotting dataset ------------------

transparency_disclosure <- transparent_all %>%
  filter(
    Question %in% transparency_disclosure_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create legal compliance plotting dataset ----------------------------

legal_compliance <- transparent_all %>%
  filter(
    Question %in% legal_compliance_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 12. Create plots ---------------------------------------------------------

plot_transparency_disclosure <- make_plot_long(
  transparency_disclosure,
  "Transparency and disclosure"
)
plot_transparency_disclosure


plot_legal_compliance <- make_plot_long(
  legal_compliance,
  "Legal compliance"
)
plot_legal_compliance




## ----------------------------------------------------------------------------

## Step 12 (NON-USERS’ PERSPECTIVES): Create a stacked bar chart


# 1. Create the data set ---------------------------------------------------

AI_nonuser <- Data_all_sections %>%
  filter(AI_user == "Non-user") %>%
  select(
    `I don’t understand how to use AI tools`,
    `I don’t feel adequately prepared to use AI tools`,
    `I find it difficult to find time to learn how to use AI tools`,
    `I don’t trust the results of AI tools`,
    `I am concerned about how AI tools handle personal data`,
    `I don’t trust the intentions of those who develop AI tools`,
    `My organization does not encourage the use of AI tools`,
    `My organization does not allow the use of AI tools`,
    `I want to wait for official guidelines before using AI tools`,
    `I sense reluctance from colleagues regarding the use of AI tools`,
    `I don’t see the point in using AI tools for my work`,
    `I believe that using AI tools threatens professional expertise`
  ) %>%
  mutate(
    across(
      everything(),
      ~ stringr::str_trim(as.character(.))
    )
  )


# 2. Define response order -------------------------------------------------

valid_levels <- c(
  "Strongly agree",
  "Somewhat agree",
  "Undecided",
  "Somewhat disagree",
  "Strongly disagree"
)


# 3. Convert responses to ordered factors ---------------------------------

AI_nonuser[] <- lapply(
  AI_nonuser,
  function(x) {
    factor(
      x,
      levels = valid_levels,
      ordered = TRUE
    )
  }
)

AI_nonuser <- as.data.frame(
  AI_nonuser
)


# 4. Number of respondents with at least one valid response --------------

n_nonuser <- sum(
  rowSums(!is.na(AI_nonuser)) > 0
)


# 5. Create long-format plotting data -------------------------------------

nonuser_all <- AI_nonuser %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  mutate(
    Response = stringr::str_trim(
      as.character(Response)
    ),
    Response = factor(
      Response,
      levels = valid_levels
    )
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    Response,
    .drop = FALSE,
    name = "Count"
  ) %>%
  group_by(Question) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = ifelse(
      Percentage >= 5,
      paste0(round(Percentage), "%"),
      ""
    )
  ) %>%
  ungroup()


# 6. Calculate respondents per question -----------------------------------

question_n <- AI_nonuser %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "Question",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response)) %>%
  count(
    Question,
    name = "Question_n"
  )


# 7. Add n to each question label -----------------------------------------

nonuser_all <- nonuser_all %>%
  left_join(
    question_n,
    by = "Question"
  ) %>%
  mutate(
    Question_label = paste0(
      Question,
      " (n = ",
      Question_n,
      ")"
    )
  )


# 8. Define thematic question groups --------------------------------------

skills_trust_barriers_questions <- c(
  "I don’t understand how to use AI tools",
  "I don’t feel adequately prepared to use AI tools",
  "I find it difficult to find time to learn how to use AI tools",
  "I don’t trust the results of AI tools",
  "I am concerned about how AI tools handle personal data",
  "I don’t trust the intentions of those who develop AI tools"
)

organizational_professional_barriers_questions <- c(
  "My organization does not encourage the use of AI tools",
  "My organization does not allow the use of AI tools",
  "I want to wait for official guidelines before using AI tools",
  "I sense reluctance from colleagues regarding the use of AI tools",
  "I don’t see the point in using AI tools for my work",
  "I believe that using AI tools threatens professional expertise"
)


# 9. Create question-order variable ---------------------------------------

nonuser_all <- nonuser_all %>%
  mutate(
    Question_order = match(
      Question,
      c(
        skills_trust_barriers_questions,
        organizational_professional_barriers_questions
      )
    )
  )


# 10. Create skills and trust barriers plotting dataset --------------------

skills_trust_barriers <- nonuser_all %>%
  filter(
    Question %in% skills_trust_barriers_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )


# 11. Create organizational and professional barriers dataset ------------

organizational_professional_barriers <- nonuser_all %>%
  filter(
    Question %in% organizational_professional_barriers_questions
  ) %>%
  arrange(Question_order) %>%
  mutate(
    Question_label = factor(
      Question_label,
      levels = rev(unique(Question_label))
    )
  )



# 12. Create plots ---------------------------------------------------------

make_plot_barriers <- function(data, plot_title) {
  
  ggplot(
    data,
    aes(
      x = paste0(
        stringr::str_wrap(as.character(Question), 45),
        " (n = ",
        Question_n,
        ")"
      ),
      y = Percentage,
      fill = Response
    )
  ) +
    
    geom_col(
      width = 0.55,
      colour = "white",
      linewidth = 0.3,
      position = position_stack(reverse = TRUE),
      na.rm = TRUE
    ) +
    
    geom_text(
      aes(
        label = ifelse(
          !is.na(Percentage) & Percentage >= 5,
          Label,
          ""
        )
      ),
      position = position_stack(
        vjust = 0.5,
        reverse = TRUE
      ),
      size = 2.8,
      colour = "black",
      family = "sans",
      na.rm = TRUE
    ) +
    
    coord_flip(
      ylim = c(0, 100),
      clip = "off"
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0.01))
    ) +
    
    scale_fill_manual(
      values = c(
        "Strongly agree" = "#C63D2F",
        "Somewhat agree" = "#E89C62",
        "Undecided" = "#D9D9D9",
        "Somewhat disagree" = "#8BB8D9",
        "Strongly disagree" = "#2F6C99"
      ),
      breaks = c(
        "Strongly agree",
        "Somewhat agree",
        "Undecided",
        "Somewhat disagree",
        "Strongly disagree"
      ),
      drop = TRUE
    ) +
    
    labs(
      title = plot_title,
      x = NULL,
      y = "Percentage of respondents",
      fill = NULL
    ) +
    
    theme_classic(
      base_size = 10,
      base_family = "sans"
    ) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        size = 11,
        colour = "black",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      
      axis.text.y = element_text(
        size = 10,
        colour = "black",
        lineheight = 0.95,
        margin = margin(r = 8)
      ),
      
      axis.text.x = element_text(
        size = 10,
        colour = "black"
      ),
      
      axis.title.x = element_text(
        size = 10,
        colour = "black",
        margin = margin(t = 8)
      ),
      
      axis.title.y = element_blank(),
      
      axis.line.x = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.line.y = element_line(
        colour = "black",
        linewidth = 0.5
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.4
      ),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      
      legend.text = element_text(
        size = 9,
        colour = "black"
      ),
      
      legend.key.size = unit(0.45, "cm"),
      legend.spacing.x = unit(0.15, "cm"),
      legend.margin = margin(t = 10),
      
      plot.margin = margin(
        t = 10,
        r = 25,
        b = 10,
        l = 10
      )
    ) +
    
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
}

plot_skills_trust_barriers <- make_plot_barriers(
  skills_trust_barriers,
  "Skills, preparedness, and trust barriers to AI use"
)
plot_skills_trust_barriers


plot_organizational_professional_barriers <- make_plot_barriers(
  organizational_professional_barriers,
  "Organizational, social, and professional barriers to AI use"
)
plot_organizational_professional_barriers



## ----------------------------------------------------------------------------

## Step 13 (GENERAL REFLECTIONS): Create a pie chart


# First check how many responded (counts)
open_responses <- Data_all_sections %>%
  select(
    response = `Can you share some general views/words about how you see AI?`
  ) %>%
  mutate(
    response = str_trim(as.character(response))
  ) %>%
  filter(
    !is.na(response),
    response != "",
    response != "Nej",
    response != ".",
    response != "*",
    response != "x",
    response != "Ikke mere end jeg har svaret."
  )

nrow(open_responses)
[1] 27


# See each individual response in text
open_responses <- Data_all_sections %>%
  select(
    Response = `Can you share some general views/words about how you see AI?`
  ) %>%
  mutate(
    Response = str_trim(as.character(Response))
  ) %>%
  filter(
    !is.na(Response),
    Response != "",
    !Response %in% c(
      "Nej",
      ".",
      "*",
      "x",
      "Ikke mere end jeg har svaret."
    )
  )

open_responses

# To see all 27 responses
options(width = 1000)
print(open_responses, n = Inf)


# Based on the response, we can group them into themes and and counts + percentage
# and plot it

theme_pie <- data.frame(
  Theme = c(
    "AI as a support tool for improved efficiency and productivity",
    "AI has great potential and is here to stay",
    "AI requires critical evaluation and human oversight",
    "Concerns about trust, transparency, and regulation",
    "Data security and organizational barriers limit AI use",
    "AI is both promising and concerning"
  ),
  Count = c(11, 5, 4, 3, 2, 2)
)

n_open <- sum(theme_pie$Count)


theme_pie <- theme_pie |>
  mutate(
    Percentage = Count / sum(Count) * 100,
    Label = paste0(
      sprintf("%.1f%%", Percentage),
      "\n(n = ", Count, ")"
    )
  )


# Muted, color-blind-friendly palette
ai_colors <- c(
  "#4E79A7",  # blue
  "#E58E3D",  # orange
  "#6FA38A",  # green
  "#8B78A8",  # purple
  "#D4B34C",  # mustard
  "#7C8794"   # blue-grey
)


open_ended <- ggplot(theme_pie, aes(x = 0.75, y = Count, fill = Theme)) +
  geom_col(
    width = 1,
    color = "white",
    linewidth = 1.2
  ) +
  coord_polar(theta = "y", clip = "off") +
  geom_text(
    aes(
      x = 0.90,
      label = Label
    ),
    position = position_stack(vjust = 0.5),
    size = 4.2,
    lineheight = 0.9,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = ai_colors) +
  labs(
    title = "General perceptions of AI among public sector employees",
    subtitle = paste0(
      "Themes identified in open-ended responses (n = ",
      n_open, ")"
    ),
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(color = NA),
      keyheight = unit(0.65, "cm"),
      keywidth = unit(0.65, "cm")
    )
  ) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 17,
      margin = margin(t = 15, b = 6)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 12,
      color = "black",
      margin = margin(b = -15)
    ),
    legend.position = "right",
    legend.text = element_text(
      size = 12,
      lineheight = 0.9
    ),
    legend.key.height = unit(0.7, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    plot.margin = margin(
      t = 15,
      r = 25,
      b = 15,
      l = 25
    )
  )

open_ended


# ===========================================================================
# EXTRA --> FIGURES WIHTH IMPROVED DESIGN AND A 4-PANEL FIGURE
# ==========================================================================


# Manually improve line breaks for long questions
format_question <- function(x) {
  
  x <- as.character(x)
  
  dplyr::case_when(
    
    # Expected future use
    x == "I will try to use AI tools in my work whenever I have a chance" ~
      "I will try to use AI tools in my work\nwhenever I have a chance",
    
    x == "I think adding AI tools to my work is a good idea" ~
      "I think adding AI tools to my work\nis a good idea",
    
    x == "I support the use of AI tools in my own work" ~
      "I support the use of AI tools\nin my own work",
    
    x == "I plan to use AI tools often in my daily work" ~
      "I plan to use AI tools often\nin my daily work",
    
    x == "I intend to keep using AI tools in my work in the future" ~
      "I intend to keep using AI tools\nin my work in the future",
    
    # Perceived performance impact
    x == "AI tools make the quality of my work better" ~
      "AI tools make the quality of\nmy work better",
    
    x == "AI tools help me reach important goals at work" ~
      "AI tools help me reach important\ngoals at work",
    
    x == "AI tools help me finish my work tasks faster" ~
      "AI tools help me finish\nmy work tasks faster",
    
    x == "AI tools help me come up with more creative ideas in my work" ~
      "AI tools help me come up with more\ncreative ideas in my work",
    
    x == "AI tools are helpful for my work" ~
      "AI tools are helpful\nfor my work",
    
    # AI knowledge and confidence
    x == "Using AI tools at work doesn't take much mental effort" ~
      "Using AI tools at work doesn't take\nmuch mental effort",
    
    x == "It is easy for me to learn how to use AI tools" ~
      "It is easy for me to learn\nhow to use AI tools",
    
    x == "I understand how to use AI tools" ~
      "I understand how\nto use AI tools",
    
    x == "I feel sure that I can become good at using AI tools" ~
      "I feel sure that I can become good\nat using AI tools",
    
    x == "AI tools are easy for me to use at work" ~
      "AI tools are easy for me\nto use at work",
    
    # Training and organizational support
    x == "In our organization, we are provided with opportunities to learn new technological skills due to changes related to AI" ~
      "In our organization, we are provided with opportunities\nto learn new technological skills due to changes\nrelated to AI",
    
    x == "In our organization, we are provided with opportunities to learn new soft skills due to changes related to AI" ~
      "In our organization, we are provided with opportunities\nto learn new soft skills due to changes\nrelated to AI",
    
    x == "In our organization, we are provided with opportunities to learn about the risks of using AI tools as part of developing a critical approach to their use" ~
      "In our organization, we are provided with opportunities\nto learn about the risks of using AI tools as part of\ndeveloping a critical approach to their use",
    
    x == "I have attended enough training to use AI tools at work" ~
      "I have attended enough training\nto use AI tools at work",
    
    x == "I am worried that some employees could be left out if they do not get training on AI" ~
      "I am worried that some employees could be left out\nif they do not receive training on AI",
    
    # Trust, transparency, risk awareness and accuracy
    x == "I believe that the results produced by AI tools are accurate" ~
      "I believe that the results \nproduced by AI tools are accurate",
    
    x == "I believe that AI tools provide reliable support for our organization’s operations" ~
      "I believe that AI tools provide reliable\nsupport for our organization’s operations",
    
    x == "I believe that AI tools provide reliable support for our organization's operations" ~
      "I believe that AI tools provide reliable\nsupport for our organization's operations",
    
    x == "I believe that AI tools operate transparently" ~
      "I believe that AI tools\noperate transparently",
    
    x == "I am familiar with the risks associated with using AI tools at work" ~
      "I am familiar with the risks \nassociated with using AI tools at work",
    
    # Privacy, data protection and responsible data use
    x == "I feel comfortable providing data to AI tools" ~
      "I feel comfortable providing\ndata to AI tools",
    
    x == "I believe that AI tools protect the data I provide to them" ~
      "I believe that AI tools protect\nthe data I provide to them",
    
    x == "I believe that AI tools in our organization do not unjustifiably infringe on employees' privacy" ~
      "I believe that AI tools in our organization do not\nunjustifiably infringe on employees' privacy",
    
    x == "I believe that AI tools in our organization do not unjustifiably infringe on employees’ privacy" ~
      "I believe that AI tools in our organization do not\nunjustifiably infringe on employees’ privacy",
    
    x == "I believe that AI tools handle users’ data responsibly" ~
      "I believe that AI tools handle\nusers’ data responsibly",
    
    x == "I believe that AI tools handle users' data responsibly" ~
      "I believe that AI tools handle\nusers' data responsibly",
    
    x == "I am aware of the risks of data misuse when using AI tools" ~
      "I am aware of the risks of data misuse\nwhen using AI tools",
    
    x == "I am aware of how AI tools collect and use data" ~
      "I am aware of how AI tools\ncollect and use data",
    
    # Fairness, intellectual property and ethical standards
    x == "I believe that AI tools treat users fairly and without discrimination" ~
      "I believe that AI tools treat users fairly\nand without discrimination",
    
    x == "I believe that AI tools do not violate intellectual property rights" ~
      "I believe that AI tools do not violate\nintellectual property rights",
    
    x == "I believe that AI tools adhere to ethical standards" ~
      "I believe that AI tools adhere\nto ethical standards",
    
    # Concerns about AI use in society and public services
    x == "The use of AI tools could hinder the development of key competencies among public employees" ~
      "The use of AI tools could hinder the development of\nkey competencies among public employees",
    
    x == "I am concerned that public employees may become overly reliant on AI tools" ~
      "I am concerned that public employees may become\noverly reliant on AI tools",
    
    x == "I am concerned that citizens may become overly reliant on AI tools" ~
      "I am concerned that citizens may become\noverly reliant on AI tools",
    
    x == "I am concerned that AI tools could widen the digital gap among public employees" ~
      "I am concerned that AI tools could widen the\ndigital gap among public employees",
    
    x == "I am concerned that AI tools could widen the digital gap among citizens" ~
      "I am concerned that AI tools could widen the\ndigital gap among citizens",
    
    x == "AI tools could reduce opportunities for direct interaction between public employees and citizens" ~
      "AI tools could reduce opportunities for direct interaction\nbetween public employees and citizens",
    
    # Concerns about AI use in the workplace
    x == "I am concerned that relying on AI tool outputs could lead to incorrect decisions at work" ~
      "I am concerned that relying on AI tool outputs could\nlead to incorrect decisions at work",
    
    x == "I am concerned that my work is being monitored through the AI tools I use" ~
      "I am concerned that my work is being monitored through\nthe AI tools I use",
    
    x == "I am concerned that AI tools reduce my autonomy at work" ~
      "I am concerned that AI tools\nreduce my autonomy at work",
    
    x == "I am concerned that AI tools diminish the value of my professional work" ~
      "I am concerned that AI tools diminish the value of\nmy professional work",
    
    x == "I am concerned that AI tools could lead to discrimination" ~
      "I am concerned that AI tools\ncould lead to discrimination",
    
    x == "AI tools can produce false information, known as hallucinations" ~
      "AI tools can produce false information,\nknown as hallucinations",
    
    x == "AI tools can exhibit linguistic or cultural biases in their outputs" ~
      "AI tools can exhibit linguistic or cultural biases\nin their outputs",
    
    # Social influence and acceptance
    x == "People whose opinions I value support my use of AI tools at work" ~
      "People whose opinions I value support \nmy use of AI tools at work",
    
    x == "People who influence my work decisions believe that I should use AI tools" ~
      "People who influence my work decisions \nbelieve that I should use AI tools",
    
    x == "People who are important to me believe that I should use AI tools at work" ~
      "People who are important to me believe \nthat I should use AI tools at work",
    
    x == "My coworkers generally support the use of AI tools" ~
      "My coworkers generally \nsupport the use of AI tools",
    
    x == "In my workplace, using AI tools is seen as something positive" ~
      "In my workplace, using AI tools \nis seen as something positive",
    
    x == "In my wider community, the use of AI tools is generally accepted" ~
      "In my wider community, the use of \nAI tools is generally accepted",
    
    # Resources, knowledge, compatibility and support
    x == "In our organization, we have sufficient support for using AI tools at work" ~
      "In our organization, we have sufficient \nsupport for using AI tools at work",
    
    x == "If I needed help using AI tools, a colleague would be available to assist me" ~
      "If I needed help using AI tools, a colleague\nwould be available to assist me",
    
    x == "I have the resources I need to use AI tools in my work" ~
      "I have the resources I need \nto use AI tools in my work",
    
    x == "I have the knowledge I need to use AI tools well" ~
      "I have the knowledge I need\nto use AI tools well",
    
    x == "AI tools fit well with how I normally do my work" ~
      "AI tools fit well with \nhow I normally do my work",
    
    # Organizational readiness
    x == "In our organization, we have the opportunity to learn about implemented AI solutions and their design" ~
      "In our organization, we have the opportunity to learn\nabout implemented AI solutions and their design",
    
    x == "In our organization, we have sufficient financial resources to purchase and maintain AI tools" ~
      "In our organization, we have sufficient financial\nresources to purchase and maintain AI tools",
    
    x == "In our organization, we have opportunities to learn the latest ways to work with AI tools" ~
      "In our organization, we have opportunities to learn\nthe latest ways to work with AI tools",
    
    x == "In our organization, the organizational structure is adapted to keep up with AI-based innovations" ~
      "In our organization, the organizational structure is\nadapted to keep up with AI-based innovations",
    
    x == "In our organization, the ICT infrastructure is regularly updated to better leverage AI tools" ~
      "In our organization, the ICT infrastructure is\nregularly updated to better leverage AI tools",
    
    x == "In our organization, new ideas are supported" ~
      "In our organization,\nnew ideas are supported",
    
    x == "In our organization, employees are involved in the preparation and implementation of AI solutions" ~
      "In our organization, employees are involved in the\npreparation and implementation of AI solutions",
    
    # Proactive individual use and exploration
    x == "I try out AI tools to complete tasks more efficiently" ~
      "I try out AI tools to complete tasks\nmore efficiently",
    
    x == "I proactively explore how AI tools can help me with my work" ~
      "I proactively explore how AI tools can help\nme with my work",
    
    x == "I look for ways to use AI tools to improve my work" ~
      "I look for ways to use AI tools\nto improve my work",
    
    x == "I identify possible problems with AI tools and try to solve them early" ~
      "I identify possible problems with AI tools\nand try to solve them early",
    
    x == "I encourage my coworkers to try AI tools" ~
      "I encourage my coworkers\nto try AI tools",
    
    x == "I am excited by exploring new possibilities for using AI" ~
      "I am excited by exploring new possibilities\nfor using AI",
    
    # Adaptability
    x == "I keep myself informed about how AI tools are used in my field" ~
      "I keep myself informed about how AI tools are\nused in my field",
    
    x == "I can quickly get used to new AI tools at work" ~
      "I can quickly get used to new AI tools\nat work",
    
    x == "I am open to changing how I work to use AI tools better" ~
      "I am open to changing how I work\nto use AI tools better",
    
    x == "I actively search for training/information to improve my AI skills" ~
      "I actively search for training or information\nto improve my AI skills",
    
    x == "AI allows me to be more flexible in my work tasks" ~
      "AI allows me to be more flexible\nin my work tasks",
    
    # Resilience
    x == "I stay calm when AI tools change how I work" ~
      "I stay calm when AI tools\nchange how I work",
    
    x == "I manage stress well when learning or adjusting to AI tools" ~
      "I manage stress well when learning\nor adjusting to AI tools",
    
    x == "I focus on finding solutions when AI tools do not work as expected" ~
      "I focus on finding solutions when AI tools do\nnot work as expected",
    
    x == "I feel confident dealing with challenges related to AI at work" ~
      "I feel confident dealing with challenges\nrelated to AI at work",
    
    x == "I can keep doing my job well even when AI tools bring uncertainty" ~
      "I can keep doing my job well even when AI\ntools bring uncertainty",
    
    # Employment impacts and governance
    x == "Trade unions are involved in decision-making regarding the use of AI in my organization" ~
      "Trade unions are involved in decision-making regarding\nthe use of AI in my organization",
    
    x == "Our organization has established rules to protect employees from potential harm caused by AI" ~
      "Our organization has established rules to protect\nemployees from potential harm caused by AI",
    
    x == "I believe that trade unions should have a greater role in shaping rules for the use of AI in the public sector" ~
      "I believe that trade unions should have a greater role\nin shaping rules for AI use in the public sector",
    
    x == "I believe that AI will create new jobs" ~
      "I believe that AI\nwill create new jobs",
    
    x == "I am worried that I could lose my job because AI is taking over tasks currently performed by humans" ~
      "I am worried that I could lose my job because AI is\ntaking over tasks currently performed by humans",
    
    x == "AI tools have clearly changed the kind of work I do" ~
      "AI tools have clearly changed\nthe kind of work I do",
    
    # Employee voice and workplace experiences
    x == "My managers support using AI tools at work" ~
      "My managers support using\nAI tools at work",
    
    x == "I would think about leaving my job if AI took over important parts of my work" ~
      "I would think about leaving my job if AI took over\nimportant parts of my work",
    
    x == "I feel that my opinion matters when new technologies like AI are introduced at work" ~
      "I feel that my opinion matters when new technologies\nlike AI are introduced at work",
    
    x == "I feel stressed by how fast AI tools are being introduced in my workplace" ~
      "I feel stressed by how fast AI tools are being\nintroduced in my workplace",
    
    x == "I feel relaxed when using AI tools" ~
      "I feel relaxed\nwhen using AI tools",
    
    x == "I avoid using AI tools because I am afraid of making a mistake that no one will notice" ~
      "I avoid using AI tools because I am afraid of making a\nmistake that no one will notice",
    
    # Efficiency, workload reduction and task completion
    x == "It takes a lot of time to check the results from AI tools" ~
      "It takes a lot of time to check the results\nfrom AI tools",
    
    x == "AI tools lower the amount of manual work" ~
      "AI tools lower the amount\nof manual work",
    
    x == "AI tools let us focus on more important tasks" ~
      "AI tools let us focus\non more important tasks",
    
    x == "AI tools help us finish tasks faster overall" ~
      "AI tools help us finish tasks\nfaster overall",
    
    x == "AI tools help save time on routine tasks" ~
      "AI tools help save time\non routine tasks",
    
    # Quality, accuracy and problem detection
    x == "AI tools reduce the quality of the final results" ~
      "AI tools reduce the quality\nof the final results",
    
    x == "AI tools make our work more accurate" ~
      "AI tools make our work\nmore accurate",
    
    x == "AI tools help us spot problems early" ~
      "AI tools help us spot\nproblems early",
    
    x == "AI tools help reduce errors in our work" ~
      "AI tools help reduce errors\nin our work",
    
    # Citizen feedback and stakeholder involvement
    x == "AI tools reduce direct contact between citizens and public employees" ~
      "AI tools reduce direct contact between citizens and\npublic employees",
    
    x == "AI tools make it easier for citizens to give feedback/make requests" ~
      "AI tools make it easier for citizens to give\nfeedback or make requests",
    
    x == "AI tools help involve stakeholders in decision-making" ~
      "AI tools help involve stakeholders\nin decision-making",
    
    # Transparency and organizational safeguards
    x == "The use of AI tools is clearly documented" ~
      "The use of AI tools\nis clearly documented",
    
    x == "Our organization has a system in place to verify the accuracy of results produced by AI tools" ~
      "Our organization has a system in place to verify\nthe accuracy of results produced by AI tools",
    
    x == "Our organization has a system in place for reporting and resolving issues with AI tools" ~
      "Our organization has a system in place for reporting\nand resolving issues with AI tools",
    
    x == "It is difficult to explain the final result when AI tools are used" ~
      "It is difficult to explain the final result\nwhen AI tools are used",
    
    x == "It is clear who is responsible for decisions made with the help of AI tools" ~
      "It is clear who is responsible for decisions made\nwith the help of AI tools",
    
    # Public disclosure and documentation
    x == "We keep records of what prompts and instructions were given to AI tools" ~
      "We keep records of what prompts and\ninstructions were given to AI tools",
    
    x == "The public does not always know if AI tools were used to influence a decision" ~
      "The public does not always know if AI tools were used\nto influence a decision",
    
    x == "Stakeholders can access the data that our AI tools use" ~
      "Stakeholders can access the data\nthat our AI tools use",
    
    x == "Citizens are notified in writing when AI tools are used in administrative decision-making" ~
      "Citizens are notified in writing when AI tools are used\nin administrative decision-making",
    
    x == "AI-generated content is clearly labelled" ~
      "AI-generated content\nis clearly labelled",
    
    # Legal compliance and citizens' rights
    x == "When using AI tools, we protect the rights of citizens" ~
      "When using AI tools, we protect\nthe rights of citizens",
    
    x == "We follow legal rules when using AI tools" ~
      "We follow legal rules\nwhen using AI tools",
    
    x == "The use of AI tools in our organization complies with regulations protecting citizens' rights" ~
      "The use of AI tools in our organization complies with\nregulations protecting citizens' rights",
    
    x == "The use of AI tools in our organization complies with regulations protecting citizens’ rights" ~
      "The use of AI tools in our organization complies with\nregulations protecting citizens’ rights",
    
    x == "Because AI works in hidden ways (\"black box\"), it is hard to make sure it follows the law" ~
      "Because AI works in hidden ways (\"black box\"), it is\nhard to make sure it follows the law",
    
    x == "Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law" ~
      "Because AI works in hidden ways (“black box”), it is\nhard to make sure it follows the law",
    
    # Leave all unmatched questions unchanged
    TRUE ~ x
  )
}

# Add the sample size (n) to the formatted question

format_question_n <- function(question, n) {
  
  formatted_question <- format_question(as.character(question))
  
  paste0(
    formatted_question,
    " (n = ",
    n,
    ")"
  )
}


# Create a diverging Likert plot

make_plot_with <- function(
    data,
    plot_title,
    question_spacing = 1.35
) {
  
  response_levels <- c(
    "Strongly disagree",
    "Somewhat disagree",
    "Undecided",
    "Somewhat agree",
    "Strongly agree"
  )
  
  response_colours <- c(
    "Strongly disagree" = "#C63D2F",
    "Somewhat disagree" = "#E89C62",
    "Undecided"         = "#D9D9D9",
    "Somewhat agree"    = "#8BB8D9",
    "Strongly agree"    = "#2F6C99"
  )
  
  # Preserve the order of questions in the input data
  question_order <- data |>
    dplyr::distinct(Question) |>
    dplyr::pull(Question)
  
  # Prepare one row per question and response category
  plot_data <- data |>
    dplyr::mutate(
      Response = as.character(Response),
      
      Question_label = mapply(
        format_question_n,
        question = as.character(Question),
        n = Question_n,
        USE.NAMES = FALSE
      )
    ) |>
    dplyr::select(
      Question,
      Question_label,
      Question_n,
      Response,
      Percentage
    ) |>
    tidyr::complete(
      Question,
      Response = response_levels,
      fill = list(Percentage = 0)
    ) |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Question_n = dplyr::first(
        stats::na.omit(Question_n)
      ),
      
      Question_label = format_question_n(
        dplyr::first(as.character(Question)),
        dplyr::first(Question_n)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Response = factor(
        Response,
        levels = response_levels,
        ordered = FALSE
      )
    ) |>
    tidyr::pivot_wider(
      names_from = Response,
      values_from = Percentage,
      values_fill = 0
    )
  
  # Calculate the position of each diverging bar segment
  segments <- plot_data |>
    dplyr::rowwise() |>
    dplyr::do({
      x <- .
      
      neutral_half <- x[["Undecided"]] / 2
      
      tibble::tibble(
        Question = x$Question,
        Question_label = x$Question_label,
        
        Response = factor(
          response_levels,
          levels = response_levels,
          ordered = FALSE
        ),
        
        Percentage = c(
          x[["Strongly disagree"]],
          x[["Somewhat disagree"]],
          x[["Undecided"]],
          x[["Somewhat agree"]],
          x[["Strongly agree"]]
        ),
        
        xmin = c(
          -(
            x[["Strongly disagree"]] +
              x[["Somewhat disagree"]] +
              neutral_half
          ),
          -(x[["Somewhat disagree"]] + neutral_half),
          -neutral_half,
          neutral_half,
          neutral_half + x[["Somewhat agree"]]
        ),
        
        xmax = c(
          -(x[["Somewhat disagree"]] + neutral_half),
          -neutral_half,
          neutral_half,
          neutral_half + x[["Somewhat agree"]],
          neutral_half +
            x[["Somewhat agree"]] +
            x[["Strongly agree"]]
        )
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Keep the first question at the top
      Question_label = factor(
        Question_label,
        levels = rev(
          unique(
            plot_data$Question_label[
              match(question_order, plot_data$Question)
            ]
          )
        )
      ),
      
      # Same question spacing as the no-label version
      y = as.numeric(Question_label) * question_spacing,
      
      # Percentage labels shown only when the segment is large enough
      label = dplyr::if_else(
        Percentage >= 5,
        paste0(round(Percentage), "%"),
        ""
      ),
      
      # Place labels in the centre of each segment
      label_x = dplyr::if_else(
        Response == "Undecided",
        0,
        (xmin + xmax) / 2
      )
    )
  
  # Y-axis positions must exactly match the bar positions
  y_breaks <- seq(
    from = question_spacing,
    by = question_spacing,
    length.out = length(
      levels(segments$Question_label)
    )
  )
  
  ggplot2::ggplot(segments) +
    
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = y - 0.35,
        ymax = y + 0.35,
        fill = Response
      ),
      colour = "white",
      linewidth = 0.3
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        x = label_x,
        y = y,
        label = label
      ),
      size = 2.8,
      colour = "black",
      family = "sans"
    ) +
    
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey45",
      linewidth = 0.3
    ) +
    
    ggplot2::scale_x_continuous(
      limits = c(-100, 100),
      breaks = seq(-100, 100, 50),
      labels = function(x) {
        paste0(abs(x), "%")
      },
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = levels(
        segments$Question_label
      ),
      expand = ggplot2::expansion(add = 0.5)
    ) +
    
    ggplot2::scale_fill_manual(
      values = response_colours,
      breaks = response_levels,
      drop = FALSE
    ) +
    
    ggplot2::labs(
      title = plot_title,
      x = "Percentage of respondents",
      y = NULL,
      fill = NULL
    ) +
    
    ggplot2::theme_classic(
      base_size = 12,
      base_family = "sans"
    ) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13,
        colour = "black",
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      
      axis.text.y = ggplot2::element_text(
        size = 12,
        colour = "black",
        lineheight = 1,
        margin = ggplot2::margin(r = 2)
      ),
      
      axis.text.x = ggplot2::element_text(
        size = 12,
        colour = "black"
      ),
      
      axis.title.x = ggplot2::element_text(
        size = 12,
        colour = "black",
        margin = ggplot2::margin(t = 8)
      ),
      
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      
      legend.text = ggplot2::element_text(
        size = 12,
        colour = "black"
      ),
      
      legend.key.size = grid::unit(0.45, "cm"),
      legend.spacing.x = grid::unit(0.15, "cm"),
      legend.margin = ggplot2::margin(t = 10),
      
      plot.margin = ggplot2::margin(
        t = 8,
        r = 25,
        b = 8,
        l = 25
      )
    ) +
    
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
}


### WITH LABELS ###



confidence_plot_with <- make_plot_with(
  confidence_df,
  "AI knowledge and confidence"
)
confidence_plot_with



plot_social_influence_with <- make_plot_with(
  social_influence,
  "Social influence"
)
plot_social_influence_with


learning_support_plot_with <- make_plot_with(
  training_support_df,
  "Training and organizational support"
)
learning_support_plot_with


privacy_data_plot_with <- make_plot_with(
  privacy_data,
  "Privacy, fairness, and responsible AI use"
)
privacy_data_plot_with





plot_societal_with <- make_plot_with(
  concern_societal,
  "Concerns about AI use in society and public services"
)
plot_societal_with


plot_workplace_with <- make_plot_with(
  concern_workplace,
  "Concerns about AI use in the workplace"
)
plot_workplace_with


plot_organizational_readiness_with <- make_plot_with(
  organizational_readiness,
  "Organizational readiness for adopting and implementing AI"
)
plot_organizational_readiness_with


plot_individual_readiness_with <- make_plot_with(
  individual_readiness,
  "Proactive individual use and exploration of AI tools"
)
plot_individual_readiness_with




plot_adaptability_with <- make_plot_with(
  adaptability,
  "Adaptability to AI tools and changing work practices"
)
plot_adaptability_with


plot_resilience_with <- make_plot_with(
  resilience,
  "Resilience when facing AI-related change and uncertainty"
)
plot_resilience_with


plot_employment_governance_with <- make_plot_with(
  employment_governance,
  "Employment impacts, governance, and protection from AI-related harm"
)
plot_employment_governance_with


plot_employee_experience_with <- make_plot_with(
  employee_experience,
  "Employee voice, managerial support, and experiences of using AI"
)
plot_employee_experience_with




plot_efficiency_workload_with <- make_plot_with(
  efficiency_workload,
  "Efficiency, workload reduction, and task completion"
)
plot_efficiency_workload_with


plot_quality_accuracy_with <- make_plot_with(
  quality_accuracy,
  "Quality, accuracy, and early problem detection"
)
plot_quality_accuracy_with


plot_citizen_stakeholder_involvement_with <- make_plot_with(
  citizen_stakeholder_involvement,
  "Citizen feedback, contact, and stakeholder involvement"
)
plot_citizen_stakeholder_involvement_with


plot_transparency_accountability_with <- make_plot_with(
  transparency_accountability,
  "Transparency, accountability, and organizational safeguards"
)
plot_transparency_accountability_with




plot_transparency_disclosure_with <- make_plot_with(
  transparency_disclosure,
  "Transparency, public disclosure, and documentation of AI use"
)
plot_transparency_disclosure_with


plot_legal_compliance_with <- make_plot_with(
  legal_compliance,
  "Legal compliance and protection of citizens’ rights"
)
plot_legal_compliance_test




# ------------------------------------------------------------------------

##### Without sample sizes, for use in a four-panel figure #######

make_plot_test_no_labels <- function(
    data,
    plot_title,
    question_spacing = 1.35
) {
  
  response_levels <- c(
    "Strongly disagree",
    "Somewhat disagree",
    "Undecided",
    "Somewhat agree",
    "Strongly agree"
  )
  
  response_colours <- c(
    "Strongly disagree" = "#C63D2F",
    "Somewhat disagree" = "#E89C62",
    "Undecided"         = "#D9D9D9",
    "Somewhat agree"    = "#8BB8D9",
    "Strongly agree"    = "#2F6C99"
  )
  
  # Preserve the order of questions in the input data
  question_order <- data |>
    dplyr::distinct(Question) |>
    dplyr::pull(Question)
  
  # Prepare one row per question and response category
  plot_data <- data |>
    dplyr::mutate(
      Response = as.character(Response),
      
      Question_label = format_question(
        as.character(Question)
      )
    ) |>
    dplyr::select(
      Question,
      Question_label,
      Response,
      Percentage
    ) |>
    tidyr::complete(
      Question,
      Response = response_levels,
      fill = list(Percentage = 0)
    ) |>
    dplyr::group_by(Question) |>
    dplyr::mutate(
      Question_label = format_question(
        dplyr::first(as.character(Question))
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Response = factor(
        Response,
        levels = response_levels,
        ordered = FALSE
      )
    ) |>
    tidyr::pivot_wider(
      names_from = Response,
      values_from = Percentage,
      values_fill = 0
    )
  
  # Calculate the position of each diverging bar segment
  segments <- plot_data |>
    dplyr::rowwise() |>
    dplyr::do({
      x <- .
      
      neutral_half <- x[["Undecided"]] / 2
      
      tibble::tibble(
        Question = x$Question,
        Question_label = x$Question_label,
        
        Response = factor(
          response_levels,
          levels = response_levels,
          ordered = FALSE
        ),
        
        Percentage = c(
          x[["Strongly disagree"]],
          x[["Somewhat disagree"]],
          x[["Undecided"]],
          x[["Somewhat agree"]],
          x[["Strongly agree"]]
        ),
        
        xmin = c(
          -(
            x[["Strongly disagree"]] +
              x[["Somewhat disagree"]] +
              neutral_half
          ),
          -(x[["Somewhat disagree"]] + neutral_half),
          -neutral_half,
          neutral_half,
          neutral_half + x[["Somewhat agree"]]
        ),
        
        xmax = c(
          -(x[["Somewhat disagree"]] + neutral_half),
          -neutral_half,
          neutral_half,
          neutral_half + x[["Somewhat agree"]],
          neutral_half +
            x[["Somewhat agree"]] +
            x[["Strongly agree"]]
        )
      )
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # Keep the first question at the top
      Question_label = factor(
        Question_label,
        levels = rev(
          unique(
            plot_data$Question_label[
              match(question_order, plot_data$Question)
            ]
          )
        )
      ),
      
      # Increase distance between question centres
      # without changing bar thickness
      y = as.numeric(Question_label) * 1.4
    )
  
  # Y-axis positions must match the spaced question centres
  y_breaks <- seq(
    from = question_spacing,
    by = question_spacing,
    length.out = length(levels(segments$Question_label))
  )
  
  ggplot2::ggplot(segments) +
    
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        
        # Original bar thickness retained
        ymin = y - 0.35,
        ymax = y + 0.35,
        
        fill = Response
      ),
      colour = "white",
      linewidth = 0.3
    ) +
    
    ggplot2::geom_vline(
      xintercept = 0,
      colour = "grey45",
      linewidth = 0.3
    ) +
    
    ggplot2::scale_x_continuous(
      limits = c(-100, 100),
      breaks = seq(-100, 100, 50),
      labels = function(x) paste0(abs(x), "%"),
      expand = ggplot2::expansion(
        mult = c(0.01, 0.01)
      )
    ) +
    
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = levels(segments$Question_label),
      expand = ggplot2::expansion(add = 0.5)
    ) +
    
    ggplot2::scale_fill_manual(
      values = response_colours,
      breaks = response_levels,
      drop = FALSE
    ) +
    
    ggplot2::labs(
      title = plot_title,
      x = "Percentage of respondents",
      y = NULL,
      fill = NULL
    ) +
    
    ggplot2::theme_classic(
      base_size = 12,
      base_family = "sans"
    ) +
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 13,
        colour = "black",
        hjust = 0.5,
        margin = ggplot2::margin(b = 12)
      ),
      
      axis.text.y = ggplot2::element_text(
        size = 12,
        colour = "black",
        lineheight = 1,
        margin = ggplot2::margin(r = 2)
      ),
      
      axis.text.x = ggplot2::element_text(
        size = 12,
        colour = "black"
      ),
      
      axis.title.x = ggplot2::element_text(
        size = 12,
        colour = "black",
        margin = ggplot2::margin(t = 8)
      ),
      
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      
      legend.position = "bottom",
      legend.direction = "horizontal",
      
      legend.text = ggplot2::element_text(
        size = 12,
        colour = "black"
      ),
      
      legend.key.size = grid::unit(0.45, "cm"),
      legend.spacing.x = grid::unit(0.15, "cm"),
      legend.margin = ggplot2::margin(t = 10),
      
      plot.margin = ggplot2::margin(
        t = 8,
        r = 25,
        b = 8,
        l = 25
      )
    ) +
    
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE
      )
    )
}




# Plot with no sample size

future_plot_test <- make_plot_test_no_labels(
  future_all,
  "Expected future use"
)
future_plot_test


performance_plot_test <- make_plot_test_no_labels(
  performance_all,
  "Perceived performance impact"
)
performance_plot_test


confidence_plot_test <- make_plot_test_no_labels(
  confidence_df,
  "AI knowledge and confidence"
)
confidence_plot_test


plot_social_influence_test <- make_plot_test_no_labels(
  social_influence,
  "Social influence"
)
plot_social_influence_test


plot_support_conditions_test <- make_plot_test_no_labels(
  social_support_conditions,
  "Resources and capability"
)
plot_support_conditions_test


trust_accuracy_plot_test <- make_plot_test_no_labels(
  trust_accuracy,
  "Trust in AI"
)
trust_accuracy_plot_test




### COMBINE 1-4) #####

combined_plot_1_4 <-
  (
    future_plot_test | performance_plot_test
  )  /
  (
    plot_support_conditions_test | trust_accuracy_plot_test
  ) +
  plot_layout(
    widths = c(1.3, 1.3),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    
    plot.margin = margin(
      t = 12,
      r = 20,
      b = 12,
      l = 20
    ),
    
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 13
    ),
    plot.tag.position = c(0.02, 0.98)
  )

combined_plot_1_4






# =======================================================================================
# 2. Statistical analysis - SECTION 2-11 + NON-USERS’ PERSPECTIVES
# =======================================================================================

# (AI users): Create descriptive table


## Step 1: Create numeric versions of each section/construct


likert_num <- c(
  "Strongly disagree" = 1,
  "Somewhat disagree" = 2,
  "Undecided" = 3,
  "Somewhat agree" = 4,
  "Strongly agree" = 5
)


future_num <- future_use %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

performance_num <- perceived_performance %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

learning_num <- learning %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

ethical_consideration_num <- ethical_consideration %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

ethical_concern_num <- ethical_concern %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

social_setting_num <- social_setting %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

AI_readiness_num <- AI_readiness %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

adaptability_resilience_num <- adaptability_resilience %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

work_experience_num <- work_experience %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

efficiency_quality_num <- efficiency_quality %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

involvement_accountability_num <- involvement_accountability %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))

transparent_compliant_num <- transparent_compliant %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))





## Step 2: Calculate alpha, means and SDs for each construct

# If R gives a warning use this code with correct variable: work_experience_alpha$item.stats 
# and look at r.drop to see values affecting alpha.if r.drop is negative or if 
# r.drop < .10 then investigate and evaluate if its important for the analysis otherwise remove


# -------------------------------------------------------------------------
# Function: Summarize a multi-item construct
# -------------------------------------------------------------------------
# Calculates:
#   - Composite score (row mean across items)
#   - Number of respondents (N)
#   - Mean and standard deviation (SD)
#   - 95% confidence interval for the mean
#   - Cronbach's alpha
#   - Bootstrap 95% confidence interval for Cronbach's alpha
# -------------------------------------------------------------------------

construct_summary <- function(
    data,
    construct_name,
    n_boot = 1000
) {
  
  # psych::alpha() works most reliably with a standard data frame
  data <- as.data.frame(data)
  
  # Calculate respondent-level composite scores
  scores <- rowMeans(
    data,
    na.rm = TRUE
  )
  
  # rowMeans(..., na.rm = TRUE) returns NaN for rows with all values missing
  scores[
    rowSums(!is.na(data)) == 0
  ] <- NA_real_
  
  # Estimate internal consistency and bootstrap CI
  alpha_result <- psych::alpha(
    data,
    n.iter = n_boot,
    check.keys = FALSE,
    warnings = FALSE
  )
  
  # Descriptive statistics
  n <- sum(!is.na(scores))
  mean_score <- mean(scores, na.rm = TRUE)
  sd_score <- sd(scores, na.rm = TRUE)
  
  # 95% confidence interval for the composite mean
  se <- sd_score / sqrt(n)
  t_crit <- qt(
    0.975,
    df = n - 1
  )
  
  # Bootstrap confidence interval for raw Cronbach's alpha
  alpha_ci <- alpha_result$boot.ci
  
  data.frame(
    Construct = construct_name,
    Items = ncol(data),
    N = n,
    Mean = round(mean_score, 2),
    SD = round(sd_score, 2),
    Lower95 = round(mean_score - t_crit * se, 2),
    Upper95 = round(mean_score + t_crit * se, 2),
    Alpha = round(alpha_result$total$raw_alpha, 2),
    Alpha_lower = round(alpha_ci[1], 2),
    Alpha_upper = round(alpha_ci[3], 2)
  )
}


# Future 

future_summary <-
  construct_summary(
    future_num,
    "Expected future use"
  )
future_summary



# performance

performance_summary <-
  construct_summary(
    performance_num,
    "Perceived performance impact"
  )
performance_summary



# learning

learning_summary <-
  construct_summary(
    training_num,
    "Perceived learning"
  )
learning_summary



# ethical consideration (2 questions were removed to improve reliability)

ethical_consideration_reduced <-
  ethical_consideration_num %>%
  select(
    -`I am familiar with the risks associated with using AI tools at work`,
    -`I am aware of the risks of data misuse when using AI tools`
  )

ethical_consideration_summary <-
  construct_summary(
    ethical_consideration_reduced,
    "Ethical considerations related to AI use"
  )
ethical_consideration_summary



# ethical concerns 

ethical_concern_summary <-
  construct_summary(
    ethical_concern_num,
    "Ethical concerns related to AI use"
  )
ethical_concern_summary



# social and contextual setting 

social_setting_summary <-
  construct_summary(
    social_setting_num,
    "Perceived organizational and social support for AI use"
  )
social_setting_summary



# AI readiness 

AI_readiness_summary <-
  construct_summary(
    AI_readiness_num,
    "Perceived organizational and individual readiness in AI use"
  )
AI_readiness_summary



# adaptability and resilience 

adaptability_resilience_summary <-
  construct_summary(
    adaptability_resilience_num,
    "Perceived adaptability and resilience in AI use"
  )
adaptability_resilience_summary



# efficiency quality (1 question was removed to improve reliability)

efficiency_quality_reduced <-
  efficiency_quality_num %>%
  select(
    -`It takes a lot of time to check the results from AI tools`
  )

efficiency_quality_summary <-
  construct_summary(
    efficiency_quality_reduced,
    "Perceived efficiency and quality related to AI use"
  )
efficiency_quality_summary



# involvement accountability (2 questions were removed to improve reliability)

involvement_accountability_reduced <-
  involvement_accountability_num %>%
  select(
    -`It is difficult to explain the final result when AI tools are used`,
    -`AI tools reduce direct contact between citizens and public employees`
  )

involvement_accountability_summary <-
  construct_summary(
    involvement_accountability_reduced,
    "Perceived citizen involvement and accountability in the use of AI"
  )
involvement_accountability_summary



# transparent and compliant (4 questions were removed to improve reliability) 

transparent_compliant_reduced <-
  transparent_compliant_num %>%
  select(
    -`Stakeholders can access the data that our AI tools use`,
    -`The public does not always know if AI tools were used to influence a decision`,
    -`We keep records of what prompts and instructions were given to AI tools`,
    -`Because AI works in hidden ways (“black box”), it is hard to make sure it follows the law`
  )

transparent_compliant_summary <-
  construct_summary(
    transparent_compliant_reduced,
    "Perceived transparency and compliance in AI use"
  )
transparent_compliant_summary



#### Descriptive ######

# work and experience (without aplha) since its not possible 

work_experience_descriptives <- data.frame(
  Item = names(work_experience_num),
  N = sapply(
    work_experience_num,
    function(x) sum(!is.na(x))
  ),
  Mean = sapply(
    work_experience_num,
    function(x) round(mean(x, na.rm = TRUE), 2)
  ),
  SD = sapply(
    work_experience_num,
    function(x) round(sd(x, na.rm = TRUE), 2)
  )
)

work_experience_descriptives



## 3. Combine everything into one table ---------------------------------

summary_table <- bind_rows(
  future_summary,
  performance_summary,
  learning_summary,
  ethical_consideration_summary,
  ethical_concern_summary,
  social_setting_summary,
  AI_readiness_summary,
  adaptability_resilience_summary,
  efficiency_quality_summary,
  involvement_accountability_summary,
  transparent_compliant_summary
)

summary_table


## ----------------------------------------------------------------------------


## (AI non-users): Create descriptive table - without aplha

# You can use the same likert_num value as above

AI_nonuser_num <- AI_nonuser %>%
  mutate(across(everything(),
                ~ unname(likert_num[as.character(.)])))


# AI non-users

AI_nonuser_descriptives <- data.frame(
  Item = names(AI_nonuser_num),
  N = sapply(
    AI_nonuser_num,
    function(x) sum(!is.na(x))
  ),
  Mean = sapply(
    AI_nonuser_num,
    function(x) round(mean(x, na.rm = TRUE), 2)
  ),
  SD = sapply(
    AI_nonuser_num,
    function(x) round(sd(x, na.rm = TRUE), 2)
  )
)

AI_nonuser_descriptives




# =============================================================
# BEFORE CREATING THE PLOTS
# =============================================================


# Function for constructs without Cronbach's alpha -------------------------

make_no_alpha_summary <- function(data, construct_name) {
  
  data <- as.data.frame(data)
  
  # One composite score per respondent
  composite_score <- rowMeans(
    data,
    na.rm = TRUE
  )
  
  # Keep completely unanswered rows as NA
  composite_score[
    rowSums(!is.na(data)) == 0
  ] <- NA_real_
  
  n_valid <- sum(!is.na(composite_score))
  mean_value <- mean(composite_score, na.rm = TRUE)
  sd_value <- sd(composite_score, na.rm = TRUE)
  
  ci_margin <- qt(
    0.975,
    df = n_valid - 1
  ) * sd_value / sqrt(n_valid)
  
  data.frame(
    Construct = construct_name,
    Items = ncol(data),
    N = n_valid,
    Mean = mean_value,
    SD = sd_value,
    Lower95 = mean_value - ci_margin,
    Upper95 = mean_value + ci_margin,
    Alpha = NA_real_
  )
}

work_experience_summary <- make_no_alpha_summary(
  work_experience_num,
  "AI and work"
)

AI_nonuser_summary <- make_no_alpha_summary(
  AI_nonuser_num,
  "Barriers"
)


# 4. Prepare summary table for plotting ------------------------------------

plot_df <- bind_rows(
  summary_table,
  work_experience_summary,
  AI_nonuser_summary
) %>%
  mutate(
    Construct_label = case_when(
      Construct == "Expected future use" ~
        "Future use",
      
      Construct == "Perceived performance impact" ~
        "Performance impact",
      
      Construct == "Perceived learning" ~
        "Learning",
      
      Construct == "Ethical considerations related to AI use" ~
        "Ethical considerations",
      
      Construct == "Ethical concerns related to AI use" ~
        "Ethical concerns",
      
      Construct == "Perceived organizational and social support for AI use" ~
        "Support",
      
      Construct == "Perceived organizational and individual readiness in AI use" ~
        "Readiness",
      
      Construct == "Perceived adaptability and resilience in AI use" ~
        "Adaptability and resilience",
      
      Construct == "Perceived efficiency and quality related to AI use" ~
        "Efficiency and quality",
      
      Construct == "Perceived citizen involvement and accountability in the use of AI" ~
        " Accountability",
      
      Construct == "Perceived transparency and compliance in AI use" ~
        "Transparency",
      
      Construct == "Work experience with AI" ~
        "AI and Work",
      
      Construct == "Reasons for not using AI" ~
        "Barriers",
      
      TRUE ~ Construct
    ),
    
    Construct_label = factor(
      Construct_label,
      levels = rev(unique(Construct_label))
    )
  )


# 5. Construct score plot --------------------------------------------------

# Reorder constructs so the highest mean appears first

plot_df <- plot_df %>%
  mutate(
    Construct_label = reorder(
      Construct_label,
      Mean
    ),
    
    Construct_type = case_when(
      as.character(Construct_label) == "Ethical concerns" ~ "Negative",
      as.character(Construct_label) == "Barriers" ~ "Negative",
      as.character(Construct_label) == "AI and work" ~ "Mixed",
      TRUE ~ "Positive"
    )
  )


# Create construct score plot

plot_constructs <- ggplot(
  plot_df,
  aes(
    x = Mean,
    y = Construct_label
  )
) +
  geom_vline(
    xintercept = 3,
    linetype = "dashed",
    colour = "grey70"
  ) +
  geom_errorbar(
    aes(
      xmin = Lower95,
      xmax = Upper95,
      colour = Construct_type
    ),
    orientation = "y",
    width = 0.18,
    linewidth = 0.7
  ) +
  geom_point(
    aes(
      colour = Construct_type
    ),
    size = 3
  ) +
  scale_colour_manual(
    values = c(
      Positive = "#2878B8",   # blue
      Negative = "#C44E52",   # red
      Mixed = "#7A7A7A"        # grey
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(1, 5),
    breaks = seq(
      1,
      5,
      by = 0.5
    )
  ) +
  labs(
    title = "Construct scores",
    x = "Composite score (1–5), mean ± 95% CI",
    y = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.text.x = element_text(
      size = 11,
      family = "sans"
    ),
    axis.text.y = element_text(
      size = 11,
      family = "sans"
    ),
    axis.title.x = element_text(
      size = 11,
      family = "sans"
    )
  )

plot_constructs




# 6. Reliability plot ------------------------------------------------------

reliability_df <- plot_df %>%
  filter(!is.na(Alpha))


plot_reliability <- ggplot(
  reliability_df,
  aes(
    x = Alpha,
    y = Construct_label
  )
) +
  geom_vline(
    xintercept = 0.70,
    linetype = "dashed",
    colour = "black"
  ) +
  geom_errorbar(
    aes(
      xmin = Alpha_lower,
      xmax = Alpha_upper
    ),
    orientation = "y",
    width = 0.18,
    colour = "#9AC7AE"
  ) +
  geom_point(
    size = 3,
    colour = "#377F5B"
  ) +
  scale_x_continuous(
    limits = c(0, 1.08),
    breaks = seq(0, 1, by = 0.2)
  ) +
  labs(
    title = "Scale reliability",
    x = "Cronbach's α",
    y = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.text.x = element_text(
      size = 11,
      family = "sans"
    ),
    axis.text.y = element_text(
      size = 11,
      family = "sans"
    ),
    axis.title.x = element_text(
      size = 11,
      family = "sans"
    )
  )

plot_reliability




# 7. Combine plots ---------------------------------------------------------


plot_constructs <- plot_constructs +
  labs(tag = "A") +
  theme(
    plot.tag.position = c(0.25, 1),
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )


plot_reliability <- plot_reliability +
  labs(tag = "B") +
  theme(
    plot.tag.position = c(0.25, 1),
    plot.tag = element_text(
      family = "sans",
      face = "bold",
      size = 12
    )
  )


combined_plot <-
  plot_constructs +
  plot_reliability +
  plot_layout(
    widths = c(1, 1)
  ) +
  plot_annotation(
    caption = paste(
      "Red = negatively valenced constructs",
      "(higher scores indicate greater concerns or barriers).",
      "Grey = mixed construct containing both positive",
      "and negative workplace perceptions."
    ),
    theme = theme(
      plot.caption = element_text(
        family = "sans",
        size = 9,
        hjust = 0.5,
        colour = "grey40",
        margin = margin(t = 10)
      )
    )
  )

combined_plot
    




# =============================================================
# SPEARMAN CORRELATION FIGURE
# =============================================================

# -------------------------------------------------------------
# 1. Function to calculate one composite score per respondent
# -------------------------------------------------------------

make_composite <- function(data) {
  
  data <- as.data.frame(data)
  
  score <- rowMeans(
    data,
    na.rm = TRUE
  )
  
  # Respondents with all items missing should remain missing
  score[
    rowSums(!is.na(data)) == 0
  ] <- NA_real_
  
  score
}


# -------------------------------------------------------------
# 2. Create respondent-level construct scores
# -------------------------------------------------------------
# IMPORTANT:
# These data frames must contain the same respondents,
# in the same row order.

correlation_data <- tibble(
  `Future use` =
    make_composite(future_num),
  
  `Performance impact` =
    make_composite(performance_num),
  
  `Learning` =
    make_composite(learning_num),
  
  `Ethical considerations` =
    make_composite(ethical_consideration_reduced),
  
  `Ethical concerns` =
    make_composite(ethical_concern_num),
  
  `Support` =
    make_composite(social_setting_num),
  
  `Readiness` =
    make_composite(AI_readiness_num),
  
  `Adaptability and resilience` =
    make_composite(adaptability_resilience_num),
  
  `Efficiency and quality` =
    make_composite(efficiency_quality_reduced),
  
  `Accountability` =
    make_composite(involvement_accountability_reduced),
  
  `Transparency` =
    make_composite(transparent_compliant_reduced),
  
  `AI and work` =
    make_composite(work_experience_num)
)


# Inspect the composite-score data
summary(correlation_data)


# -------------------------------------------------------------
# 3. Calculate Spearman correlations
# -------------------------------------------------------------

spearman_matrix <- cor(
  correlation_data,
  method = "spearman",
  use = "pairwise.complete.obs"
)

round(spearman_matrix, 2)



# -------------------------------------------------------------
# 4. Convert matrix to long format
# -------------------------------------------------------------

correlation_long <- as.data.frame(spearman_matrix) %>%
  rownames_to_column("Construct_y") %>%
  pivot_longer(
    cols = -Construct_y,
    names_to = "Construct_x",
    values_to = "rho"
  )



# Use the original column order.
# Do not use levels(correlation_long$Construct_x).
construct_order <- colnames(correlation_data)

correlation_triangle <- correlation_long %>%
  mutate(
    x_position = match(Construct_x, construct_order),
    y_position = match(Construct_y, construct_order)
  ) %>%
  filter(
    x_position >= y_position
  ) %>%
  mutate(
    Construct_x = factor(
      Construct_x,
      levels = construct_order
    ),
    
    # Do not reverse this:
    # the first construct should appear at the bottom.
    Construct_y = factor(
      Construct_y,
      levels = construct_order
    )
  )


# Optional check: this must be greater than zero
nrow(correlation_triangle)



# -------------------------------------------------------------
# 5. Panel A: correlation heatmap
# -------------------------------------------------------------
plot_heatmap <- ggplot(
  correlation_long,
  aes(
    x = Construct_x,
    y = Construct_y,
    fill = rho
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f", rho),
      colour = abs(rho) >= 0.55
    ),
    size = 2.35
  ) +
  scale_colour_manual(
    values = c(
      `TRUE` = "white",
      `FALSE` = "grey25"
    ),
    guide = "none"
  ) + 
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.25),
    labels = scales::label_number(
      accuracy = 0.01
    ),
    name = NULL
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = grid::unit(0.38, "cm"),
      barheight = grid::unit(6.8, "cm"),
      ticks = TRUE,
      frame.colour = "black",
      frame.linewidth = 0.35
    )
  ) +
  coord_fixed(
    expand = FALSE
  ) +
  labs(
    title = "Spearman correlations between constructs",
    x = NULL,
    y = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 11,
      margin = margin(b = 5)
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 11,
      colour = "black",
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      size = 11,
      colour = "black",
      margin = margin(r = 8)
    ),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.35
    ),
    legend.position = "right",
    legend.box.spacing = grid::unit(-0.50, "cm"),
    legend.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 2
    ),
    plot.margin = margin(
      t = 3,
      r = 7,
      b = 2,
      l = 2
    )
  )

plot_heatmap




# -------------------------------------------------------------
# 6. Prepare correlations with Expected future use
# -------------------------------------------------------------

outcome_name <- "Future use"

future_correlations <- tibble(
  Construct = rownames(spearman_matrix),
  rho = spearman_matrix[, outcome_name]
) %>%
  filter(
    Construct != outcome_name
  ) %>%
  arrange(rho) %>%
  mutate(
    Construct = factor(
      Construct,
      levels = Construct
    ),
    Direction = if_else(
      rho >= 0,
      "Positive",
      "Negative"
    )
  )

future_correlations


future_correlations <- future_correlations %>%
  mutate(
    Bar_colour = case_when(
      as.character(Construct) == "AI and work" ~ "Mixed",
      rho >= 0 ~ "Positive",
      TRUE ~ "Negative"
    )
  )


# -------------------------------------------------------------
# 7. Panel B: correlation with future use
# -------------------------------------------------------------

plot_future_correlations <- ggplot(
  future_correlations,
  aes(
    x = rho,
    y = Construct,
    fill = Bar_colour
  )
) +
  geom_vline(
    xintercept = 0,
    colour = "grey40",
    linewidth = 0.55
  ) +
  geom_col(
    width = 0.78
  ) +
  scale_fill_manual(
    values = c(
      Positive = "#3478B9",
      Negative = "#C94F51",
      Mixed = "#7A7A7A"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-0.3, 0.80),
    breaks = seq(-0.6, 0.8, by = 0.2),
    labels = function(x) {
      x[abs(x) < 1e-10] <- 0
      sprintf("%.1f", x)
    },
    expand = expansion(
      mult = c(0.01, 0.02)
    )
  ) +
  labs(
    title = "Correlation with expected future use",
    x = expression("Spearman " * rho),
    y = NULL
  ) +
  theme_classic(
    base_size = 11,
    base_family = "sans"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 11,
      margin = margin(b = 5)
    ),
    axis.text.y = element_text(
      size = 11,
      colour = "black",
      lineheight = 0.9
    ),
    axis.text.x = element_text(
      size = 11,
      colour = "black"
    ),
    axis.title.x = element_text(
      size = 11,
      margin = margin(t = 8)
    ),
    plot.margin = margin(
      t = 3,
      r = 3,
      b = 2,
      l = 7
    )
  )

plot_future_correlations


# -------------------------------------------------------------
# 8. Combine panels
# -------------------------------------------------------------

correlation_figure <-
  plot_heatmap +
  free(
    plot_future_correlations,
    type = "space",
    side = "b"
  ) +
  plot_layout(
    widths = c(1.3, 1)
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(
      face = "bold",
      size = 13
    ),
    plot.tag.position = c(0.05, 0.98)
  )

correlation_figure


















