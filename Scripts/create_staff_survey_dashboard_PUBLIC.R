# Instructions: Create student survey dashboard must be ran first!
#Update currentyear parameter,set base directory, then run remainder of script. If running just a single year of data (without joining prioryears, skip 'Adding in Prior Years' & reactivate section above it)
require(readxl)
require(dplyr)
require(readr)
require(here)

#Update this year to match the parent folders of data/1_raw and data/2_output
currentyear <- 2024

# Set base directory to match the folder you want to read and write to
base_dir <- "path/to/your/folder"  # Modify this to match the user's setup

# Define file paths dynamically
output_dir <- file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), "Staff")
dashboard_dir <- file.path(base_dir, "data", "2_output", "dashboard_files_finalized", "Staff")

# Function to write files
write_dashboard_file <- function(data, filename, folder) {
  file_path <- file.path(folder, filename)
  write.table(data, file = file_path, append = FALSE, quote = FALSE, sep = "|",
              eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
}
# Read in Data
df_staff <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), 
                                 "Staff", "h2p_staff_longfile.csv"), 
                       delim = "|", guess_max = 5000)
df_dim_question <- read_xlsx(file.path(base_dir,"data/1_raw/SurveyCodebook_H2P.xlsx"),sheet = "dim_question_staff",skip = 0) %>% filter(Year == paste(currentyear)) %>% select(-Year)
df_school_ref <- read_xlsx(file.path(base_dir,"data/1_raw/H2P School Reference.xlsx"),sheet = "School Reference",skip = 0)

# Replicate Tableau Processing
# Join with dim_question_staff
# Clean up Invalid Race entries
# Group Genders for reporting

# Clean buggy external reference values
df_staff$ext_reference_cleaned <- substr(df_staff$ext_reference,1,5)

# Remove student survey columns that are not being used
df_staff <- df_staff %>%
  select('ext_reference_cleaned',
         'grade_09',
         'grade_10',
         'grade_11',
         'grade_12',
         'race_a',
         'race_b',
         'race_i',
         'race_w',
         'race_h',
         'race_m',
         'race_p',
         'all_race',
         'gender',
         'question',
         'value',
         'text_value'
  )

# Join all data files together
df_detail <- left_join(df_staff,df_school_ref, by = c("ext_reference_cleaned"="ExternalReference")) %>%
  left_join(df_dim_question, by = c("question"="normalized_question_id"))


# Create Race/Ethnicity Reporting Groups
# Replace nulls with 0
df_detail[is.na(df_detail$race_a),]$race_a <- 0
df_detail[is.na(df_detail$race_b),]$race_b <- 0
df_detail[is.na(df_detail$race_h),]$race_h <- 0
df_detail[is.na(df_detail$race_i),]$race_i <- 0
df_detail[is.na(df_detail$race_m),]$race_m <- 0
df_detail[is.na(df_detail$race_p),]$race_p <- 0
df_detail[is.na(df_detail$race_w),]$race_w <- 0

df_detail$race_count <- df_detail$race_a + df_detail$race_b + df_detail$race_h + df_detail$race_i + df_detail$race_m + df_detail$race_p + df_detail$race_w

df_detail$race_ethnicity_rptgroup <- 
  case_when(df_detail$all_race == 1 ~ 'Unknown',
            df_detail$race_count > 1 ~ 'Multiracial', # if we want to match fed 7, we have to move Hispanic/Latine/x condition above this
            df_detail$race_a == 1 ~ "Asian/Asian American",
            df_detail$race_b == 1 ~ "Black/African American",
            df_detail$race_h == 1 ~ "Hispanic/Latine/x",
            df_detail$race_i == 1 ~ "American Indian/Alaska Native",
            df_detail$race_m == 1 ~ "Middle Eastern/North African",
            df_detail$race_p == 1 ~ "Native Hawaiian/Pacific Islander",
            df_detail$race_w == 1 ~ "White",
            df_detail$race_count == 0 ~ "Unknown",
            TRUE ~ "REVIEW"
  )


# Create Gender Reporting Groups
# group all non-Male/Female genders into a single group due to smaller counts.

df_detail$gender_rptgroup <-
  case_when(df_detail$gender == "Male" ~ "Male",
            df_detail$gender == "Female" ~ "Female",
            df_detail$gender == "Non-Binary" ~ "Other/Unknown/Prefer Not to Say",
            df_detail$gender == "Gender not exclusively male or female" ~ "Other/Unknown/Prefer Not to Say",
            df_detail$gender == "Other" ~ "Other/Unknown/Prefer Not to Say",
            df_detail$gender == "Prefer Not to Say" ~ "Other/Unknown/Prefer Not to Say",
            df_detail$gender == "Not Sure/Questioning" ~ "Other/Unknown/Prefer Not to Say",
            df_detail$gender == "Incomplete" ~ "Other/Unknown/Prefer Not to Say",
            TRUE ~ "REVIEW"
  )

# Code Pct Positive Values

df_detail$met_flag <-
  case_when(df_detail$answer_type != "Multiselect" & is.na(df_detail$value) ~ -999, # Using -999 to represent null values
            df_detail$answer_type == "Yes/No" & df_detail$value == 1 ~ 1,
            df_detail$answer_type == "Multiselect" & df_detail$value == 1  ~ 1,
            df_detail$answer_type == "Multiselect" & is.na(df_detail$value) ~ 0, 
            df_detail$answer_type == "Knowledgeable" & df_detail$value >= 3  ~ 1,
            df_detail$answer_type == "Continuous" & df_detail$question == "q03a" & df_detail$value >= 86  ~ 1, # set value based on the regional student value for this question for the year
            df_detail$answer_type == "Continuous" & df_detail$question == "q21a" & df_detail$value >= 84  ~ 1, # set value based on the regional student value for this question for the year
            df_detail$answer_type == "Continuous" & df_detail$question == "q08a" & df_detail$value >= 70  ~ 1, # set value based on the regional student value for this question for the year
            df_detail$answer_type == "Dual Credit" & df_detail$value == 1 ~ 1, # "positive" value is considered if they are currently teaching a dual credit course
            df_detail$answer_type == "Not Applicable/Yes/No/Not Offered" & df_detail$value == 2 ~ 1, # "positive" value is considered if they answered yes
            TRUE ~ 0
  )

# Code Denominator Flag --> basically setting all -999s from previous section to 0, and all other values to 1.
df_detail$denominator_flag <-
  case_when(df_detail$answer_type != "Multiselect" & is.na(df_detail$value) ~ 0,
            df_detail$answer_type == "Not Applicable/Yes/No/Not Offered" & df_detail$value == 1 ~ 0, # Not Applicable for Not Applicable/Yes/No/Not Offered questions
            df_detail$answer_type == "Not Applicable/Yes/No/Not Offered" & df_detail$value == 4 ~ 0, # Not Offered for Not Applicable/Yes/No/Not Offered questions
            TRUE ~ 1
  )


#######################
####
#### Add Preaggregation Steps for Dashboard files
####
######################

# Create the Stacked Bar Chart processed version
# Create the Pct Positive Version
# 

# Stacked Bar Chart Processed Version (no suppression version)
# Group By: Region, School, Grade Level, Race/Ethnicity, Gender, First Gen, Question Details, Value, Text,Value, Met_Flags
# Summarize: Count of Values

df_dashboard_stacked <- df_detail %>%
  group_by(ext_reference_cleaned
           ,School
           ,Region
           ,grade_09,grade_10,grade_11,grade_12
           ,race_ethnicity_rptgroup
           ,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_staffs = n()) %>%
  ungroup()


#######################
####
#### Percent Met Processed Version (no suppression)
####
######################

# Need to aggregate all groupings separately (All, Grade Level, Race/Ethnicity, Gender, First Gen) and then union them all together
# Summarize: Count Met, Count Denominator, Pct Met


# All Staff
# School
df_dashboard_pctmet_all <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all$report_group <- "All"
df_dashboard_pctmet_all$staff_group <- "All"


# Region
df_dashboard_pctmet_all_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all_region$report_group <- "All"
df_dashboard_pctmet_all_region$staff_group <- "All"
df_dashboard_pctmet_all_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_all_region$School <- "All Schools"


# State
df_dashboard_pctmet_all_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all_state$report_group <- "All"
df_dashboard_pctmet_all_state$staff_group <- "All"
df_dashboard_pctmet_all_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_all_state$School <- "All Schools"
df_dashboard_pctmet_all_state$Region <- "All Regions"


## Grade Level is skipped due to challenge of reporting teachers teaching multiple grade levels

# Race/Ethnicity
# School
df_dashboard_pctmet_race <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           ,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race$staff_group <- df_dashboard_pctmet_race$race_ethnicity_rptgroup

# Region
df_dashboard_pctmet_race_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    ,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_region$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_region$staff_group <- df_dashboard_pctmet_race_region$race_ethnicity_rptgroup
df_dashboard_pctmet_race_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_region$School <- "All Schools"

# State
df_dashboard_pctmet_race_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_state$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_state$staff_group <- df_dashboard_pctmet_race_state$race_ethnicity_rptgroup
df_dashboard_pctmet_race_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_state$School <- "All Schools"
df_dashboard_pctmet_race_state$Region <- "All Regions"


# Race/Ethnicity (All Staff of Color)
# School
df_dashboard_pctmet_race_allbipoc <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc$staff_group <- "Staff of Color"

# Region
df_dashboard_pctmet_race_allbipoc_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc_region$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc_region$staff_group <- "Staff of Color"
df_dashboard_pctmet_race_allbipoc_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_allbipoc_region$School <- "All Schools"

# State
df_dashboard_pctmet_race_allbipoc_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc_state$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc_state$staff_group <- "Staff of Color"
df_dashboard_pctmet_race_allbipoc_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_allbipoc_state$School <- "All Schools"
df_dashboard_pctmet_race_allbipoc_state$Region <- "All Regions"


# Gender
# School
df_dashboard_pctmet_gender <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           ,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender$report_group <- "Gender"
df_dashboard_pctmet_gender$staff_group <- df_dashboard_pctmet_gender$gender_rptgroup

# Region
df_dashboard_pctmet_gender_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    ,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender_region$report_group <- "Gender"
df_dashboard_pctmet_gender_region$staff_group <- df_dashboard_pctmet_gender_region$gender_rptgroup
df_dashboard_pctmet_gender_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_gender_region$School <- "All Schools"

# State
df_dashboard_pctmet_gender_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_staffs = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender_state$report_group <- "Gender"
df_dashboard_pctmet_gender_state$staff_group <- df_dashboard_pctmet_gender_state$gender_rptgroup
df_dashboard_pctmet_gender_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_gender_state$School <- "All Schools"
df_dashboard_pctmet_gender_state$Region <- "All Regions"


# Normalize All Groups (drop the row used to aggregate)
#df_dashboard_pctmet_grade <- df_dashboard_pctmet_grade %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
#df_dashboard_pctmet_grade_region <- df_dashboard_pctmet_grade_region %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
#df_dashboard_pctmet_grade_state <- df_dashboard_pctmet_grade_state %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
df_dashboard_pctmet_race <- df_dashboard_pctmet_race %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_race_region <- df_dashboard_pctmet_race_region %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_race_state <- df_dashboard_pctmet_race_state %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_gender <- df_dashboard_pctmet_gender %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_pctmet_gender_region <- df_dashboard_pctmet_gender_region %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_pctmet_gender_state <- df_dashboard_pctmet_gender_state %>% ungroup() %>% select(-gender_rptgroup)

# Union All Groups Together
df_dashboard_pctmet <- df_dashboard_pctmet_all %>%
  union_all(df_dashboard_pctmet_all_region) %>%
  union_all(df_dashboard_pctmet_all_state) %>%
  #union_all(df_dashboard_pctmet_grade) %>%
  #union_all(df_dashboard_pctmet_grade_region) %>%
  #union_all(df_dashboard_pctmet_grade_state) %>%
  union_all(df_dashboard_pctmet_race) %>%
  union_all(df_dashboard_pctmet_race_region) %>%
  union_all(df_dashboard_pctmet_race_state) %>%
  union_all(df_dashboard_pctmet_race_allbipoc) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_region) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_state) %>%
  union_all(df_dashboard_pctmet_gender) %>%
  union_all(df_dashboard_pctmet_gender_region) %>%
  union_all(df_dashboard_pctmet_gender_state) 

# Union All State Groups Together
df_dashboard_pctmet_state <- df_dashboard_pctmet_all_state %>%
  union_all(df_dashboard_pctmet_race_state) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_state) %>%
  union_all(df_dashboard_pctmet_gender_state)

# Union All Region Groups Together
df_dashboard_pctmet_region <- df_dashboard_pctmet_all_region %>%
  #union_all(df_dashboard_pctmet_grade_region) %>%
  union_all(df_dashboard_pctmet_race_region) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_region) %>%
  union_all(df_dashboard_pctmet_gender_region) 


#######################
####
#### Stacked, but using defined aggregation groups
####
######################


# All Staff
# School
df_dashboard_stacked_all <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_all$report_group <- "All"
df_dashboard_stacked_all$staff_group <- "All"

# Region
df_dashboard_stacked_all_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_all_region$report_group <- "All"
df_dashboard_stacked_all_region$staff_group <- "All"
df_dashboard_stacked_all_region$ext_reference_cleaned <- NA
df_dashboard_stacked_all_region$School <- "All Schools"

# State
df_dashboard_stacked_all_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_all_state$report_group <- "All"
df_dashboard_stacked_all_state$staff_group <- "All"
df_dashboard_stacked_all_state$ext_reference_cleaned <- NA
df_dashboard_stacked_all_state$School <- "All Schools"
df_dashboard_stacked_all_state$Region <- "All Regions"

# Grade Level is currently inactive due to challenge with teachers who teach multiple grade levels

# Race/Ethnicity
# School
df_dashboard_stacked_race <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           ,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race$staff_group <- df_dashboard_stacked_race$race_ethnicity_rptgroup

# Region
df_dashboard_stacked_race_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    ,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race_region$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_region$staff_group <- df_dashboard_stacked_race_region$race_ethnicity_rptgroup
df_dashboard_stacked_race_region$ext_reference_cleaned <- NA
df_dashboard_stacked_race_region$School <- "All Schools"

# State
df_dashboard_stacked_race_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race_state$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_state$staff_group <- df_dashboard_stacked_race_state$race_ethnicity_rptgroup
df_dashboard_stacked_race_state$ext_reference_cleaned <- NA
df_dashboard_stacked_race_state$School <- "All Schools"
df_dashboard_stacked_race_state$Region <- "All Regions"


# Race/Ethnicity (Staff of Color)
# School
df_dashboard_stacked_race_allbipoc <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race_allbipoc$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc$staff_group <- "Staff of Color"

# Region
df_dashboard_stacked_race_allbipoc_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race_allbipoc_region$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc_region$staff_group <- "Staff of Color"
df_dashboard_stacked_race_allbipoc_region$ext_reference_cleaned <- NA
df_dashboard_stacked_race_allbipoc_region$School <- "All Schools"


# State
df_dashboard_stacked_race_allbipoc_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_race_allbipoc_state$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc_state$staff_group <- "Staff of Color"
df_dashboard_stacked_race_allbipoc_state$ext_reference_cleaned <- NA
df_dashboard_stacked_race_allbipoc_state$School <- "All Schools"
df_dashboard_stacked_race_allbipoc_state$Region <- "All Regions"


# Gender
# School
df_dashboard_stacked_gender <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           ,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_gender$report_group <- "Gender"
df_dashboard_stacked_gender$staff_group <- df_dashboard_stacked_gender$gender_rptgroup

# Region
df_dashboard_stacked_gender_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    ,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_gender_region$report_group <- "Gender"
df_dashboard_stacked_gender_region$staff_group <- df_dashboard_stacked_gender_region$gender_rptgroup
df_dashboard_stacked_gender_region$ext_reference_cleaned <- NA
df_dashboard_stacked_gender_region$School <- "All Schools"

# State
df_dashboard_stacked_gender_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_staffs = n())

df_dashboard_stacked_gender_state$report_group <- "Gender"
df_dashboard_stacked_gender_state$staff_group <- df_dashboard_stacked_gender_state$gender_rptgroup
df_dashboard_stacked_gender_state$ext_reference_cleaned <- NA
df_dashboard_stacked_gender_state$School <- "All Schools"
df_dashboard_stacked_gender_state$Region <- "All Regions"


# Normalize All Groups (drop the row used to aggregate)
#df_dashboard_stacked_grade <- df_dashboard_stacked_grade %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
#df_dashboard_stacked_grade_region <- df_dashboard_stacked_grade_region %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
#df_dashboard_stacked_grade_state <- df_dashboard_stacked_grade_state %>% ungroup() %>% select(-grade_09,grade_10,grade_11,grade_12)
df_dashboard_stacked_race <- df_dashboard_stacked_race %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_race_region <- df_dashboard_stacked_race_region %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_race_state <- df_dashboard_stacked_race_state %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_gender <- df_dashboard_stacked_gender %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_stacked_gender_region <- df_dashboard_stacked_gender_region %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_stacked_gender_state <- df_dashboard_stacked_gender_state %>% ungroup() %>% select(-gender_rptgroup)


# Union All Groups Together
df_dashboard_stacked_rpt <- df_dashboard_stacked_all %>%
  union_all(df_dashboard_stacked_all_region) %>%
  union_all(df_dashboard_stacked_all_state) %>%
  #union_all(df_dashboard_stacked_grade) %>%
  #union_all(df_dashboard_stacked_grade_region) %>%
  #union_all(df_dashboard_stacked_grade_state) %>%
  union_all(df_dashboard_stacked_race) %>%
  union_all(df_dashboard_stacked_race_region) %>%
  union_all(df_dashboard_stacked_race_state) %>%
  union_all(df_dashboard_stacked_race_allbipoc) %>%
  union_all(df_dashboard_stacked_race_allbipoc_region) %>%
  union_all(df_dashboard_stacked_race_allbipoc_state) %>%
  union_all(df_dashboard_stacked_gender) %>%
  union_all(df_dashboard_stacked_gender_region) %>%
  union_all(df_dashboard_stacked_gender_state)


# Union All State Groups Together
df_dashboard_stacked_rpt_state <- df_dashboard_stacked_all_state %>%
  #union_all(df_dashboard_stacked_grade_state) %>%
  union_all(df_dashboard_stacked_race_state) %>%
  union_all(df_dashboard_stacked_race_allbipoc_state) %>%
  union_all(df_dashboard_stacked_gender_state)

# Union All Region Groups Together
df_dashboard_stacked_rpt_region <- df_dashboard_stacked_all_region %>%
  #union_all(df_dashboard_stacked_grade_region) %>%
  union_all(df_dashboard_stacked_race_region) %>%
  union_all(df_dashboard_stacked_race_allbipoc_region) %>%
  union_all(df_dashboard_stacked_gender_region)



#Add fields for Tableau
df_dashboard_stacked <- df_dashboard_stacked %>% mutate(Construct_rpt = case_when(
  construct == 'DC' ~ 'Dual Credit',
  construct == 'DC/DEM' ~ 'Dual Credit',
  construct == 'FA' ~ 'Financial Aid',
  construct == 'HSBP' ~ 'High School & Beyond Plan',
  construct == 'PS' ~ 'Postsecondary',
  construct == 'SS' ~ 'School Supports',
  TRUE ~ 'REVIEW'
))



#Add fields for Tableau
df_dashboard_pctmet$Placeholder <- 1

df_dashboard_pctmet <- df_dashboard_pctmet %>% mutate(Construct_rpt = case_when(
  construct == 'DC' ~ 'Dual Credit',
  construct == 'DC/DEM' ~ 'Dual Credit',
  construct == 'FA' ~ 'Financial Aid',
  construct == 'HSBP' ~ 'High School & Beyond Plan',
  construct == 'PS' ~ 'Postsecondary',
  construct == 'SS' ~ 'School Supports',
  TRUE ~ 'REVIEW'
))

df_dashboard_pctmet <- df_dashboard_pctmet %>% mutate(Survey_Question_Filter = paste(Construct_rpt, ' - ', question_header, sep = ''))



#add fields for Tableau
df_dashboard_stacked_rpt$Placeholder <- 1

df_dashboard_stacked_rpt <- df_dashboard_stacked_rpt %>% mutate(Construct_rpt = case_when(
  construct == 'DC' ~ 'Dual Credit',
  construct == 'DC/DEM' ~ 'Dual Credit',
  construct == 'FA' ~ 'Financial Aid',
  construct == 'HSBP' ~ 'High School & Beyond Plan',
  construct == 'PS' ~ 'Postsecondary',
  construct == 'SS' ~ 'School Supports',
  TRUE ~ 'REVIEW'
))

df_dashboard_stacked_rpt <- df_dashboard_stacked_rpt %>% mutate(Survey_Question_Filter = paste(Construct_rpt, ' - ', question_header, sep = ''))


#df_dashboard_stacked doesn't need suppression bc it's only shared internally. 
# Suppress < 10
# 1. Less than N suppression at each row 
df_dashboard_pctmet$lt10supp <- 0
df_dashboard_pctmet[df_dashboard_pctmet$count_staffs <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_dashboard_pctmet <- df_dashboard_pctmet %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_staffs) %>%  
  mutate(rank_order = row_number())

df_dashboard_pctmet <- df_dashboard_pctmet %>%
  arrange(Region, School, question, report_group, rank_order)

# 3. Count how many instances of N suppression are within the reporting group (Race/Ethnicity, Gender)
df_dashboard_pctmet_agg <- df_dashboard_pctmet %>%
  group_by(Region, School, question, report_group) %>%
  summarise(count_lt10supp = sum(lt10supp))

# 4. if there is exactly 1 suppression for a reporting group, then suppress the row with rank 2
df_dashboard_pctmet <-  df_dashboard_pctmet %>%
  inner_join(df_dashboard_pctmet_agg, by = c("Region","School","question","report_group"))

df_dashboard_pctmet$compsupp <- 0
df_dashboard_pctmet[df_dashboard_pctmet$count_lt10supp==1 & df_dashboard_pctmet$rank_order == 2,]$compsupp <- 1

df_dashboard_pctmet$flag_supp <- 0
df_dashboard_pctmet$count_met_og <- df_dashboard_pctmet$count_met
df_dashboard_pctmet$count_staffs_og <- df_dashboard_pctmet$count_staffs
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$count_staffs <- NA
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$count_staffs <- NA
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet$suppression_notes <- "na"
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$suppression_notes <- "staff count less than 10, suppressed"

# Suppress < 10
# 1. Group by region, school, group, question and determine group level count
df_dashboard_stacked_rpt_grouped <- df_dashboard_stacked_rpt %>% group_by(Region,School,staff_group,question) %>% summarise(group_level_count = sum(count_staffs))

# 2. Determine if that group level count is greater than or equal to 10
df_dashboard_stacked_rpt_grouped$grouplv_lt10 <- 0  
df_dashboard_stacked_rpt_grouped[df_dashboard_stacked_rpt_grouped$group_level_count <10,]$grouplv_lt10 <- 1

# 3. Join back to original stacked bar chart dataframe on region, school, group, question
df_dashboard_stacked_rpt <- inner_join(df_dashboard_stacked_rpt_grouped, df_dashboard_stacked_rpt, by=c("Region","School","staff_group","question"))

# 4. Determine if count should be suppressed for all text Values within the group
df_dashboard_stacked_rpt$count_staffs_og <- df_dashboard_stacked_rpt$count_staffs
df_dashboard_stacked_rpt[df_dashboard_stacked_rpt$grouplv_lt10 == 1,]$count_staffs <- NA

# 5. Set suppression notes
df_dashboard_stacked_rpt$suppression_notes <- "na"
df_dashboard_stacked_rpt[df_dashboard_stacked_rpt$grouplv_lt10 == 1,]$suppression_notes <- "group level less than 10, suppressed"

# Suppress < 10
# 1. Less than N suppression at each row 
df_dashboard_pctmet_state$lt10supp <- 0
df_dashboard_pctmet_state[df_dashboard_pctmet_state$count_staffs <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_dashboard_pctmet_state <- df_dashboard_pctmet_state %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_staffs) %>%  
  mutate(rank_order = row_number())

df_dashboard_pctmet_state <- df_dashboard_pctmet_state %>%
  arrange(Region, School, question, report_group, rank_order)

# 3. Count how many instances of N suppression are within the reporting group (Race/Ethnicity, Gender)
df_dashboard_pctmet_state_agg <- df_dashboard_pctmet_state %>%
  group_by(Region, School, question, report_group) %>%
  summarise(count_lt10supp = sum(lt10supp))

# 4. if there is exactly 1 suppression for a reporting group, then suppress the row with rank 2
df_dashboard_pctmet_state <-  df_dashboard_pctmet_state %>%
  inner_join(df_dashboard_pctmet_state_agg, by = c("Region","School","question","report_group"))

df_dashboard_pctmet_state$compsupp <- 0
df_dashboard_pctmet_state[df_dashboard_pctmet_state$count_lt10supp==1 & df_dashboard_pctmet_state$rank_order == 2,]$compsupp <- 1

df_dashboard_pctmet_state$flag_supp <- 0
df_dashboard_pctmet_state$count_met_og <- df_dashboard_pctmet_state$count_met
df_dashboard_pctmet_state$count_staffs_og <- df_dashboard_pctmet_state$count_staffs
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$count_staffs <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$count_staffs <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet_state$suppression_notes <- "na"
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$suppression_notes <- "staff count less than 10, suppressed"

## suppression not necessary for dashboard since state is only used as reference line- kept for future use if relevant
#1. Less than N suppression at each row 
df_dashboard_pctmet_region$lt10supp <- 0
df_dashboard_pctmet_region[df_dashboard_pctmet_region$count_staffs <10,]$lt10supp <- 1

#2. Rank order of the rows within the group
df_dashboard_pctmet_region <- df_dashboard_pctmet_region %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_staffs) %>%  
  mutate(rank_order = row_number())

df_dashboard_pctmet_region <- df_dashboard_pctmet_region %>%
  arrange(Region, School, question, report_group, rank_order)

#3. Count how many instances of N suppression are within the reporting group (Race/Ethnicity, Gender)
df_dashboard_pctmet_region_agg <- df_dashboard_pctmet_region %>%
  group_by(Region, School, question, report_group) %>%
  summarise(count_lt10supp = sum(lt10supp))

# 4. if there is exactly 1 suppression for a reporting group, then suppress the row with rank 2
df_dashboard_pctmet_region <-  df_dashboard_pctmet_region %>%
  inner_join(df_dashboard_pctmet_region_agg, by = c("Region","School","question","report_group"))

df_dashboard_pctmet_region$compsupp <- 0
df_dashboard_pctmet_region[df_dashboard_pctmet_region$count_lt10supp==1 & df_dashboard_pctmet_region$rank_order == 2,]$compsupp <- 1

df_dashboard_pctmet_region$flag_supp <- 0
df_dashboard_pctmet_region$count_met_og <- df_dashboard_pctmet_region$count_met
df_dashboard_pctmet_region$count_staffs_og <- df_dashboard_pctmet_region$count_staffs
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$count_staffs <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$count_staffs <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet_region$suppression_notes <- "na"
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$suppression_notes <- "staff count less than 10, suppressed"

# no suppression for df_dashboard_stacked_rpt_state, needed un-suppressed for references

# Add Current Year & Filter
df_dashboard_stacked$year <- currentyear
df_dashboard_stacked_currentyear <- df_dashboard_stacked %>% filter(denominator_flag==1)

df_dashboard_pctmet$year <- currentyear
df_dashboard_pctmet_currentyear <- df_dashboard_pctmet %>% filter(!is.na(Region))

df_dashboard_stacked_rpt$year <- currentyear
df_dashboard_stacked_rpt$value[is.na(df_dashboard_stacked_rpt$value)] <- '-999'
df_dashboard_stacked_rpt_currentyear <- df_dashboard_stacked_rpt %>% filter(!is.na(Region))

df_dashboard_stacked_rpt_region$year <- currentyear
df_dashboard_stacked_rpt_region_currentyear <- df_dashboard_stacked_rpt_region %>% filter(!is.na(Region))

df_dashboard_pctmet_state$year <- currentyear
df_dashboard_pctmet_state_currentyear <- df_dashboard_pctmet_state %>% filter(!is.na(Region))

df_dashboard_pctmet_region$year <- currentyear
df_dashboard_pctmet_region_currentyear <- df_dashboard_pctmet_region %>% filter(!is.na(Region))

df_dashboard_stacked_rpt_state$year <- currentyear
df_dashboard_stacked_rpt_state$value[is.na(df_dashboard_stacked_rpt_state$value)] <- '-999'
df_dashboard_stacked_rpt_state_currentyear <- df_dashboard_stacked_rpt_state %>% filter(!is.na(Region))

# If not joining prior year data, use this shortcut, skip 'Adding In Prior Years', and resume at 'Write Outputs'
df_dashboard_stacked <- df_dashboard_stacked_currentyear
df_dashboard_pctmet <- df_dashboard_pctmet_currentyear
df_dashboard_stacked_rpt <- df_dashboard_stacked_rpt_currentyear
df_dashboard_pctmet_state <- df_dashboard_pctmet_state_currentyear 
df_dashboard_pctmet_region <- df_dashboard_pctmet_region_currentyear
df_dashboard_stacked_rpt_state <- df_dashboard_stacked_rpt_state_currentyear
df_dashboard_stacked_rpt_region <- df_dashboard_stacked_rpt_region_currentyear

###################################################
### Adding in Prior Years - Activate if using multi-year data
###################################################
# Read in Prior Years
#df_dashboard_stacked_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_stacked.csv"),delim = "|", guess_max = 5000)

#df_dashboard_pctmet_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_pctmet.csv"),delim = "|", guess_max = 5000)

#df_dashboard_stacked_rpt_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_stacked_rpt.csv"),delim = "|", guess_max = 5000)

#df_dashboard_stacked_rpt_region_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_stacked_rpt_region.csv"),delim = "|", guess_max = 5000)

#df_dashboard_pctmet_state_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_pctmet_state.csv"),delim = "|", guess_max = 5000)

#df_dashboard_pctmet_region_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_pctmet_region.csv"),delim = "|", guess_max = 5000)

#df_dashboard_stacked_rpt_state_previousyears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_stacked_rpt_state.csv"),delim = "|", guess_max = 5000)

# Bind Current Year to Prior Years
#df_dashboard_stacked <- bind_rows(df_dashboard_stacked_previousyears,df_dashboard_stacked_currentyear) 

#df_dashboard_pctmet <- bind_rows(df_dashboard_pctmet_previousyears,df_dashboard_pctmet_currentyear) 

#change type
#df_dashboard_stacked_rpt_previousyears$value <- as.character(df_dashboard_stacked_rpt_previousyears$value)
#df_dashboard_stacked_rpt <- bind_rows(df_dashboard_stacked_rpt_previousyears,df_dashboard_stacked_rpt_currentyear) 

#df_dashboard_pctmet_state <- bind_rows(df_dashboard_pctmet_state_previousyears,df_dashboard_pctmet_state_currentyear) 

#df_dashboard_pctmet_region <- bind_rows(df_dashboard_pctmet_region_previousyears,df_dashboard_pctmet_region_currentyear) 

#change type
#df_dashboard_stacked_rpt_state_previousyears$value <- as.character(df_dashboard_stacked_rpt_state_previousyears$value)
#df_dashboard_stacked_rpt_state <- bind_rows(df_dashboard_stacked_rpt_state_previousyears,df_dashboard_stacked_rpt_state_currentyear)

#df_dashboard_stacked_rpt_region <- bind_rows(df_dashboard_stacked_rpt_region_previousyears,df_dashboard_stacked_rpt_region_currentyear) 

### Write Outputs
write_dashboard_file(df_dashboard_stacked,"h2p_staff_dashboard_stacked.csv",dashboard_dir)

write_dashboard_file(df_dashboard_pctmet,"h2p_staff_dashboard_pctmet.csv",dashboard_dir)

write_dashboard_file(df_dashboard_stacked_rpt,"h2p_staff_dashboard_stacked_rpt.csv",dashboard_dir)

# Write Reference Aggregations to CSV
write_dashboard_file(df_dashboard_pctmet_state,"h2p_staff_dashboard_pctmet_state.csv",dashboard_dir)

write_dashboard_file(df_dashboard_pctmet_region,"h2p_staff_dashboard_pctmet_region.csv",dashboard_dir)


write_dashboard_file(df_dashboard_stacked_rpt_state,"h2p_staff_dashboard_stacked_rpt_state.csv",dashboard_dir)

write_dashboard_file(df_dashboard_stacked_rpt_region,"h2p_staff_dashboard_stacked_rpt_region.csv",dashboard_dir)

### Write outputs to yearly folder
write_dashboard_file(df_dashboard_stacked,"h2p_staff_dashboard_stacked.csv",output_dir)

write_dashboard_file(df_dashboard_pctmet,"h2p_staff_dashboard_pctmet.csv",output_dir)

write_dashboard_file(df_dashboard_stacked_rpt,"h2p_staff_dashboard_stacked_rpt.csv",output_dir)

# Write Reference Aggregations to CSV
write_dashboard_file(df_dashboard_pctmet_state,"h2p_staff_dashboard_pctmet_state.csv",output_dir)

write_dashboard_file(df_dashboard_pctmet_region,"h2p_staff_dashboard_pctmet_region.csv",output_dir)

write_dashboard_file(df_dashboard_stacked_rpt_state,"h2p_staff_dashboard_stacked_rpt_state.csv",output_dir)
                     
write_dashboard_file(df_dashboard_stacked_rpt_region,"h2p_staff_dashboard_stacked_rpt_region.csv",output_dir)

# Sandbox - Ring Chart
# q03a, q08a, q21a

# group by state, region, school
# Group by student reporting group (all, race, bipoc, gender)
# avg count staff = sum(count staff * value) / sum(count staff)
# for each group, find the 1 - pct value for ring making purposes

# for each group

# Create base to start
df_rings <- df_detail %>%
  filter(question %in% c('q03a','q08a','q21a')) %>%
  filter(denominator_flag == 1)

# All
# School
df_rings_pct_all <- df_rings %>%
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_all$report_group <- "All"
df_rings_pct_all$staff_group <- "All"

# Region
df_rings_pct_all_region <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_all_region$report_group <- "All"
df_rings_pct_all_region$staff_group <- "All"
df_rings_pct_all_region$ext_reference_cleaned <- NA
df_rings_pct_all_region$School <- "All Schools"

# State
df_rings_pct_all_state <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_all_state$report_group <- "All"
df_rings_pct_all_state$staff_group <- "All"
df_rings_pct_all_state$ext_reference_cleaned <- NA
df_rings_pct_all_state$School <- "All Schools"
df_rings_pct_all_state$Region <- "All Regions"

# Race
# School
df_rings_pct_race <- df_rings %>%
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           ,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race$report_group <- "Race/Ethnicity"
df_rings_pct_race$staff_group <- df_rings_pct_race$race_ethnicity_rptgroup

# Region
df_rings_pct_race_region <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    ,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race_region$report_group <- "Race/Ethnicity"
df_rings_pct_race_region$staff_group <- df_rings_pct_race_region$race_ethnicity_rptgroup
df_rings_pct_race_region$ext_reference_cleaned <- NA
df_rings_pct_race_region$School <- "All Schools"

# State
df_rings_pct_race_state <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race_state$report_group <- "Race/Ethnicity"
df_rings_pct_race_state$staff_group <- df_rings_pct_race_state$race_ethnicity_rptgroup
df_rings_pct_race_state$ext_reference_cleaned <- NA
df_rings_pct_race_state$School <- "All Schools"
df_rings_pct_race_state$Region <- "All Regions"

# Race/Ethnicity (Staff of Color)
# School
df_rings_pct_race_allbipoc <- df_rings %>%
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race_allbipoc$report_group <- "Race/Ethnicity"
df_rings_pct_race_allbipoc$staff_group <- "Staff of Color"

# Region
df_rings_pct_race_allbipoc_region <- df_rings %>%
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race_allbipoc_region$report_group <- "Race/Ethnicity"
df_rings_pct_race_allbipoc_region$staff_group <- "Staff of Color"
df_rings_pct_race_allbipoc_region$ext_reference_cleaned <- NA
df_rings_pct_race_allbipoc_region$School <- "All Schools"


# State
df_rings_pct_race_allbipoc_state <- df_rings %>%
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White staffs & group
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_race_allbipoc_state$report_group <- "Race/Ethnicity"
df_rings_pct_race_allbipoc_state$staff_group <- "Staff of Color"
df_rings_pct_race_allbipoc_state$ext_reference_cleaned <- NA
df_rings_pct_race_allbipoc_state$School <- "All Schools"
df_rings_pct_race_allbipoc_state$Region <- "All Regions"

# Gender
# School
df_rings_pct_gender <- df_rings %>%
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_09,grade_10,grade_11,grade_12
           #,race_ethnicity_rptgroup
           ,gender_rptgroup
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_gender$report_group <- "Gender"
df_rings_pct_gender$staff_group <- df_rings_pct_gender$gender_rptgroup

# Region
df_rings_pct_gender_region <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    ,gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_gender_region$report_group <- "Gender"
df_rings_pct_gender_region$staff_group <- df_rings_pct_gender_region$gender_rptgroup
df_rings_pct_gender_region$ext_reference_cleaned <- NA
df_rings_pct_gender_region$School <- "All Schools"

# State
df_rings_pct_gender_state <- df_rings %>%
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_09,grade_10,grade_11,grade_12
    #,race_ethnicity_rptgroup
    gender_rptgroup
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type)  %>%
  summarise(avg_staff_response = sum(value/100)/sum(denominator_flag),
            count_staff=n())

df_rings_pct_gender_state$report_group <- "Gender"
df_rings_pct_gender_state$staff_group <- df_rings_pct_gender_state$gender_rptgroup
df_rings_pct_gender_state$ext_reference_cleaned <- NA
df_rings_pct_gender_state$School <- "All Schools"
df_rings_pct_gender_state$Region <- "All Regions"

# Normalize All Groups (drop the row used to aggregate)
df_rings_pct_race <- df_rings_pct_race %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_rings_pct_race_region <- df_rings_pct_race_region %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_rings_pct_race_state <- df_rings_pct_race_state %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_rings_pct_gender <- df_rings_pct_gender %>% ungroup() %>% select(-gender_rptgroup)
df_rings_pct_gender_region <- df_rings_pct_gender_region %>% ungroup() %>% select(-gender_rptgroup)
df_rings_pct_gender_state <- df_rings_pct_gender_state %>% ungroup() %>% select(-gender_rptgroup)

# Union All Groups Together
df_rings_pct_rpt <- df_rings_pct_all %>%
  union_all(df_rings_pct_all_region) %>%
  union_all(df_rings_pct_all_state) %>%
  #union_all(df_dashboard_stacked_grade) %>%
  #union_all(df_dashboard_stacked_grade_region) %>%
  #union_all(df_dashboard_stacked_grade_state) %>%
  union_all(df_rings_pct_race) %>%
  union_all(df_rings_pct_race_region) %>%
  union_all(df_rings_pct_race_state) %>%
  union_all(df_rings_pct_race_allbipoc) %>%
  union_all(df_rings_pct_race_allbipoc_region) %>%
  union_all(df_rings_pct_race_allbipoc_state) %>%
  union_all(df_rings_pct_gender) %>%
  union_all(df_rings_pct_gender_region) %>%
  union_all(df_rings_pct_gender_state)

# create the other half of the ring by calculating 1-pct for all rows, and assigning it to the same group
# tag original as "Ring"
df_rings_pct_rpt$ring_group <- "Ring"

# duplicate the original
df_rings_pct_rpt_complement <- df_rings_pct_rpt

# tag as "Complement"
df_rings_pct_rpt_complement$ring_group <- "Complement"

# change avg_staff_response to 1 - avg_staff_response"
df_rings_pct_rpt_complement$avg_staff_response <- 1 - df_rings_pct_rpt_complement$avg_staff_response

# union the two data frames together
df_rings_pct_rpt <- union_all(df_rings_pct_rpt,df_rings_pct_rpt_complement)

# Add fields for Tableau
df_rings_pct_rpt$Placeholder <- 1

df_rings_pct_rpt <- df_rings_pct_rpt %>% mutate(Construct_rpt = case_when(
  construct == 'DC' ~ 'Dual Credit',
  construct == 'DC/DEM' ~ 'Dual Credit',
  construct == 'FA' ~ 'Financial Aid',
  construct == 'HSBP' ~ 'High School & Beyond Plan',
  construct == 'PS' ~ 'Postsecondary',
  construct == 'SS' ~ 'School Supports',
  TRUE ~ 'REVIEW'
))

df_rings_pct_rpt <- df_rings_pct_rpt %>% mutate(Survey_Question_Filter = paste(Construct_rpt, ' - ', question_header, sep = ''))



# Suppress < 10
# 1. Less than N suppression at each row 
df_rings_pct_rpt$lt10supp <- 0
df_rings_pct_rpt[df_rings_pct_rpt$count_staff <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_rings_pct_rpt <- df_rings_pct_rpt %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_staff) %>%  
  mutate(rank_order = row_number())

df_rings_pct_rpt <- df_rings_pct_rpt %>%
  arrange(Region, School, question, report_group, rank_order)

# 3. Count how many instances of N suppression are within the reporting group (Race/Ethnicity, Gender)
df_rings_pct_rpt_agg <- df_rings_pct_rpt %>%
  group_by(Region, School, question, report_group) %>%
  summarise(count_lt10supp = sum(lt10supp))

# If needed 
# 4. If there is exactly 1 suppression for a reporting group, then suppress the row with rank 2
# df_rings_pct_rpt <-  df_rings_pct_rpt %>%
# inner_join(df_rings_pct_rpt_agg, by = c("Region","School","question","report_group"))
# df_rings_pct_rpt$compsupp <- 0
# df_rings_pct_rpt[df_rings_pct_rpt$count_lt10supp==1 & df_rings_pct_rpt$rank_order == 2,]$compsupp <- 1

df_rings_pct_rpt$flag_supp <- 0
df_rings_pct_rpt$count_staff_og <- df_rings_pct_rpt$count_staff
df_rings_pct_rpt[df_rings_pct_rpt$lt10supp == 1,]$flag_supp <- 1
df_rings_pct_rpt[df_rings_pct_rpt$lt10supp == 1,]$count_staff <- NA
# If needed
# df_rings_pct_rpt[df_rings_pct_rpt$compsupp == 1,]$flag_supp <- 1
# df_rings_pct_rpt[df_rings_pct_rpt$compsupp == 1,]$count_staff <- NA

# 5. Set suppression notes
df_rings_pct_rpt$suppression_notes <- "na"
df_rings_pct_rpt[df_rings_pct_rpt$lt10supp == 1,]$suppression_notes <- "staff count less than 10, suppressed"

# Add Current Year & Filter
df_rings_pct_rpt$year <- currentyear
df_rings_pct_rpt_currentyear <- df_rings_pct_rpt %>% filter(!is.na(Region))

# If not joining prior year data, utilize this shortcut & skip 'Adding in Prior Years'. Run again at 'Write Outputs':
df_rings_pct_rpt <- df_rings_pct_rpt_currentyear

###################################################
### Adding in Prior Years
###################################################

# Read in Prior Years
#df_rings_pct_rpt_prioryears <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear-1,"_finalized"),"Staff","h2p_staff_dashboard_rings_pct_rpt.csv"),delim = "|", guess_max = 5000)
#df_rings_pct_rpt_prioryears$year <- as.numeric(df_rings_pct_rpt_prioryears$year)


# Bind Current Year to Prior Years
#df_rings_pct_rpt <- bind_rows(df_rings_pct_rpt_prioryears, df_rings_pct_rpt_currentyear)

#write to finalized folder
write_dashboard_file(df_rings_pct_rpt,"h2p_staff_dashboard_rings_pct_rpt.csv",dashboard_dir)

#write to yearly folder
write_dashboard_file(df_rings_pct_rpt,"h2p_staff_dashboard_rings_pct_rpt.csv",output_dir)
# Create Student rings

#read in student data (created in create_student_survey_dashboard script)
df_student <- read_delim(file.path(base_dir,"data","2_output",paste0(currentyear,"_finalized"),"Student","h2p_student_dashboard_pctmet.csv"),delim = "|", guess_max = 5000)

# filter questions q08a, q14a, q22a
df_rings_student <- df_student %>%
  filter(question %in% c('q08a','q14a','q22a')) %>%
  filter(report_group %in% c('All','Race/Ethnicity','Gender'))

# map to appropriate staff survey question
df_rings_student$question_staff <-
  case_when(df_rings_student$question == 'q08a'  ~ 'q03a',
            df_rings_student$question == 'q14a'  ~ 'q08a',
            df_rings_student$question == 'q22a'  ~ 'q21a',
            TRUE ~ "REVIEW"
  )

# create complementary ring value
# tag original as "Ring"
df_rings_student$ring_group <- "Ring"

# duplicate the original
df_rings_student_complement <- df_rings_student

# tag as "Complement"
df_rings_student_complement$ring_group <- "Complement"

# change pct_met to 1 - pct_met and count_met to count_student - count_met
df_rings_student_complement$pct_met <- 1 - df_rings_student_complement$pct_met
df_rings_student_complement$count_met <- df_rings_student_complement$count_students - df_rings_student_complement$count_met

# union the two data frames together
df_rings_student <- union_all(df_rings_student,df_rings_student_complement)
df_rings_student$year <- as.character(df_rings_student$year)

# write output to finalized folder
write_dashboard_file(df_rings_student,"h2p_staff_dashboard_student_rings_pct_rpt.csv",dashboard_dir)
# write output to yearly folder
write_dashboard_file(df_rings_student,"h2p_staff_dashboard_student_rings_pct_rpt.csv",output_dir)