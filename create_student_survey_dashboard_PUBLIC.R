# Instructions: Update currentyear parameter,set base directory, then run remainder of script. If running just a single year of data (without joining prioryears, skip 'Adding in Prior Years' section & reactivate section above it)
require(readxl)
require(dplyr)
require(readr)
require(here)

#Update this year to match the parent folders of data/1_raw and data/2_output
currentyear <- 2024

# Set base directory to match the folder you want to read and write to
base_dir <- "path/to/your/folder"  # Modify this to match the user's setup

# Define file paths dynamically
output_dir <- file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), "Student")
dashboard_dir <- file.path(base_dir, "data", "2_output", "dashboard_files_finalized", "Student")

# Function to write files
write_dashboard_file <- function(data, filename, folder) {
  file_path <- file.path(folder, filename)
  write.table(data, file = file_path, append = FALSE, quote = FALSE, sep = "|",
              eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
}

# Read in Data
df_student <- read_delim(file.path(base_dir,"data/2_output/dashboard_files_finalized/Student/h2p_student_longfile.csv"),delim = "|", guess_max = 5000)

df_dim_question <- read_xlsx(file.path(base_dir,"data/1_raw/SurveyCodebook_H2P.xlsx"),sheet = "dim_question_student") %>% filter(Year == paste(currentyear)) %>% select(-Year)

df_school_ref <- read_xlsx(file.path(base_dir,"data/1_raw/H2P School Reference.xlsx"),sheet = "School Reference",skip = 0) %>% select(-"Staff Count", -"Prev. Rate Completion Rate", -"Est. Completion Students", -"Est. Completion Staff (Last YR total)")

# Replicate tableau processing
# Join with dim_student_question
# Clean up invalid race entries (students who selected none or every option)
# Group genders for reporting

# Clean buggy external reference values
df_student$ext_reference_cleaned <- substr(df_student$ext_reference,1,5)

# Remove student survey columns that are not being used
df_student <- df_student %>%
  select('ext_reference_cleaned',
         'grade_level',
         'race_a',
         'race_b',
         'race_i',
         'race_w',
         'race_h',
         'race_m',
         'race_p',
         'all_race',
         'gender',
         'first_gen',
         'question',
         'value',
         'text_value'
  )

# Join all data files together
df_detail <- left_join(df_student,df_school_ref, by = c("ext_reference_cleaned"="ExternalReference")) %>%
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
df_detail$gender_rptgroup <-
  case_when(df_detail$gender == "Male" ~ "Male",
            df_detail$gender == "Female" ~ "Female",
            df_detail$gender == "Non-Binary" ~ "Non-Binary/Not exclusively male or female",
            df_detail$gender == "Other" ~ "Other/Prefer Not to Say",
            df_detail$gender == "Prefer Not to Say" ~ "Other/Prefer Not to Say",
            df_detail$gender == "Not Sure/Questioning" ~ "Other/Prefer Not to Say",
            df_detail$gender == "Incomplete" ~ "Unknown",
            TRUE ~ "REVIEW"
  )

# Code Pct Positive Values
df_detail$met_flag <-
  case_when(df_detail$answer_type != "Multiselect" & is.na(df_detail$value) ~ -999, # Using -999 to represent null values
            df_detail$answer_type == "Yes/No" & df_detail$value == 1 ~ 1,
            df_detail$answer_type == "Multiselect" & df_detail$value == 1  ~ 1,
            df_detail$answer_type == "Multiselect" & is.na(df_detail$value) ~ 0, # This condition likely doesn't hit, address at first multiselect condition
            df_detail$answer_type == "Knowledgeable" & df_detail$value >= 3  ~ 1,
            df_detail$answer_type == "Rank" & df_detail$value <= 3  ~ 1,
            df_detail$answer_type == "Likeliness" & df_detail$value >= 3  ~ 1,
            df_detail$answer_type == "Likeliness w/ NA" & df_detail$value >= 3 & df_detail$value <= 4 ~ 1, # Coded this way because 5 value is a N/A equivalent
            df_detail$answer_type == "Likeliness w/ NA" & df_detail$value == 5  ~ -999, # Coded "I do not have an advisory/homeroom class" as a null value
            df_detail$answer_type == "Agree/Disagree" & df_detail$value >= 3  ~ 1,
            df_detail$answer_type == "Education Level" & df_detail$value >= 2 & df_detail$value <= 6 ~ 1, # Coded all but high school and Other as meeting
            TRUE ~ 0
  )

# Code Denominator Flag --> basically setting all -999s from previous section to 0, and all other values to 1.
df_detail$denominator_flag <-
  case_when(df_detail$answer_type != "Multiselect" & is.na(df_detail$value) ~ 0,
            df_detail$answer_type == "Likeliness w/ NA" & df_detail$value == 5  ~ 0,
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
           ,grade_level
           ,race_ethnicity_rptgroup
           ,gender_rptgroup
           ,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n()) %>%
  ungroup()

#######################
####
#### Percent Met Processed Version (no suppression)
####
######################

# Need to aggregate all groupings separately (All, Grade Level, Race/Ethnicity, Gender, First Gen) and then union them all together
# Summarize: Count Met, Count Denominator, Pct Met

# All Students
# School
df_dashboard_pctmet_all <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all$report_group <- "All"
df_dashboard_pctmet_all$student_group <- "All"

# Region
df_dashboard_pctmet_all_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all_region$report_group <- "All"
df_dashboard_pctmet_all_region$student_group <- "All"
df_dashboard_pctmet_all_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_all_region$School <- "All Schools"

# State
df_dashboard_pctmet_all_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_all_state$report_group <- "All"
df_dashboard_pctmet_all_state$student_group <- "All"
df_dashboard_pctmet_all_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_all_state$School <- "All Schools"
df_dashboard_pctmet_all_state$Region <- "All Regions"

# Grade Level
# School
df_dashboard_pctmet_grade <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           ,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_grade$report_group <- "Grade Level"
df_dashboard_pctmet_grade$student_group <- df_dashboard_pctmet_grade$grade_level

# Region
df_dashboard_pctmet_grade_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    ,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_grade_region$report_group <- "Grade Level"
df_dashboard_pctmet_grade_region$student_group <- df_dashboard_pctmet_grade_region$grade_level
df_dashboard_pctmet_grade_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_grade_region$School <- "All Schools"

# State
df_dashboard_pctmet_grade_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_grade_state$report_group <- "Grade Level"
df_dashboard_pctmet_grade_state$student_group <- df_dashboard_pctmet_grade_state$grade_level
df_dashboard_pctmet_grade_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_grade_state$School <- "All Schools"
df_dashboard_pctmet_grade_state$Region <- "All Regions"

# Race/Ethnicity
# School
df_dashboard_pctmet_race <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           ,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race$student_group <- df_dashboard_pctmet_race$race_ethnicity_rptgroup

# Region
df_dashboard_pctmet_race_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    ,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_region$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_region$student_group <- df_dashboard_pctmet_race_region$race_ethnicity_rptgroup
df_dashboard_pctmet_race_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_region$School <- "All Schools"

# State
df_dashboard_pctmet_race_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_state$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_state$student_group <- df_dashboard_pctmet_race_state$race_ethnicity_rptgroup
df_dashboard_pctmet_race_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_state$School <- "All Schools"
df_dashboard_pctmet_race_state$Region <- "All Regions"

# Race/Ethnicity (All Students of Color)
# School
df_dashboard_pctmet_race_allbipoc <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc$student_group <- "Students of Color"

# Region
df_dashboard_pctmet_race_allbipoc_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc_region$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc_region$student_group <- "Students of Color"
df_dashboard_pctmet_race_allbipoc_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_race_allbipoc_region$School <- "All Schools"

# State
df_dashboard_pctmet_race_allbipoc_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_race_allbipoc_state$report_group <- "Race/Ethnicity"
df_dashboard_pctmet_race_allbipoc_state$student_group <- "Students of Color"
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
           #,grade_level
           #,race_ethnicity_rptgroup
           ,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender$report_group <- "Gender"
df_dashboard_pctmet_gender$student_group <- df_dashboard_pctmet_gender$gender_rptgroup

# Region
df_dashboard_pctmet_gender_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    ,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender_region$report_group <- "Gender"
df_dashboard_pctmet_gender_region$student_group <- df_dashboard_pctmet_gender_region$gender_rptgroup
df_dashboard_pctmet_gender_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_gender_region$School <- "All Schools"

# State
df_dashboard_pctmet_gender_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_gender_state$report_group <- "Gender"
df_dashboard_pctmet_gender_state$student_group <- df_dashboard_pctmet_gender_state$gender_rptgroup
df_dashboard_pctmet_gender_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_gender_state$School <- "All Schools"
df_dashboard_pctmet_gender_state$Region <- "All Regions"

# First Generation
# School
df_dashboard_pctmet_firstgen <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_firstgen$report_group <- "First Generation Status"
df_dashboard_pctmet_firstgen$student_group <- df_dashboard_pctmet_firstgen$first_gen

# Region
df_dashboard_pctmet_firstgen_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_firstgen_region$report_group <- "First Generation Status"
df_dashboard_pctmet_firstgen_region$student_group <- df_dashboard_pctmet_firstgen_region$first_gen
df_dashboard_pctmet_firstgen_region$ext_reference_cleaned <- NA
df_dashboard_pctmet_firstgen_region$School <- "All Schools"

# State
df_dashboard_pctmet_firstgen_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type) %>%
  summarise(count_met = sum(met_flag),
            count_students = sum(denominator_flag),
            pct_met = sum(met_flag)/sum(denominator_flag))

df_dashboard_pctmet_firstgen_state$report_group <- "First Generation Status"
df_dashboard_pctmet_firstgen_state$student_group <- df_dashboard_pctmet_firstgen_state$first_gen
df_dashboard_pctmet_firstgen_state$ext_reference_cleaned <- NA
df_dashboard_pctmet_firstgen_state$School <- "All Schools"
df_dashboard_pctmet_firstgen_state$Region <- "All Regions"

# Normalize All Groups (drop the row used to aggregate)
df_dashboard_pctmet_grade <- df_dashboard_pctmet_grade %>% ungroup() %>% select(-grade_level)
df_dashboard_pctmet_grade_region <- df_dashboard_pctmet_grade_region %>% ungroup() %>% select(-grade_level)
df_dashboard_pctmet_grade_state <- df_dashboard_pctmet_grade_state %>% ungroup() %>% select(-grade_level)
df_dashboard_pctmet_race <- df_dashboard_pctmet_race %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_race_region <- df_dashboard_pctmet_race_region %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_race_state <- df_dashboard_pctmet_race_state %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_pctmet_gender <- df_dashboard_pctmet_gender %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_pctmet_gender_region <- df_dashboard_pctmet_gender_region %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_pctmet_gender_state <- df_dashboard_pctmet_gender_state %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_pctmet_firstgen <- df_dashboard_pctmet_firstgen %>% ungroup() %>% select(-first_gen)
df_dashboard_pctmet_firstgen_region <- df_dashboard_pctmet_firstgen_region %>% ungroup() %>% select(-first_gen)
df_dashboard_pctmet_firstgen_state <- df_dashboard_pctmet_firstgen_state %>% ungroup() %>% select(-first_gen)

# Union All Groups Together
df_dashboard_pctmet <- df_dashboard_pctmet_all %>%
  union_all(df_dashboard_pctmet_all_region) %>%
  union_all(df_dashboard_pctmet_all_state) %>%
  union_all(df_dashboard_pctmet_grade) %>%
  union_all(df_dashboard_pctmet_grade_region) %>%
  union_all(df_dashboard_pctmet_grade_state) %>%
  union_all(df_dashboard_pctmet_race) %>%
  union_all(df_dashboard_pctmet_race_region) %>%
  union_all(df_dashboard_pctmet_race_state) %>%
  union_all(df_dashboard_pctmet_race_allbipoc) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_region) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_state) %>%
  union_all(df_dashboard_pctmet_gender) %>%
  union_all(df_dashboard_pctmet_gender_region) %>%
  union_all(df_dashboard_pctmet_gender_state) %>%
  union_all(df_dashboard_pctmet_firstgen) %>%
  union_all(df_dashboard_pctmet_firstgen_region) %>%
  union_all(df_dashboard_pctmet_firstgen_state)

# Union All State Groups Together
df_dashboard_pctmet_state <- df_dashboard_pctmet_all_state %>%
  union_all(df_dashboard_pctmet_grade_state) %>%
  union_all(df_dashboard_pctmet_race_state) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_state) %>%
  union_all(df_dashboard_pctmet_gender_state) %>%
  union_all(df_dashboard_pctmet_firstgen_state)

# Union All Region Groups Together
df_dashboard_pctmet_region <- df_dashboard_pctmet_all_region %>%
  union_all(df_dashboard_pctmet_grade_region) %>%
  union_all(df_dashboard_pctmet_race_region) %>%
  union_all(df_dashboard_pctmet_race_allbipoc_region) %>%
  union_all(df_dashboard_pctmet_gender_region) %>%
  union_all(df_dashboard_pctmet_firstgen_region)

#######################
####
#### Stacked, but using defined aggregation groups
####
######################

# All Students
# School
df_dashboard_stacked_all <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_all$report_group <- "All"
df_dashboard_stacked_all$student_group <- "All"

# Region
df_dashboard_stacked_all_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_all_region$report_group <- "All"
df_dashboard_stacked_all_region$student_group <- "All"
df_dashboard_stacked_all_region$ext_reference_cleaned <- NA
df_dashboard_stacked_all_region$School <- "All Schools"

# State
df_dashboard_stacked_all_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_all_state$report_group <- "All"
df_dashboard_stacked_all_state$student_group <- "All"
df_dashboard_stacked_all_state$ext_reference_cleaned <- NA
df_dashboard_stacked_all_state$School <- "All Schools"
df_dashboard_stacked_all_state$Region <- "All Regions"

# Grade Level
# School
df_dashboard_stacked_grade <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           ,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_grade$report_group <- "Grade Level"
df_dashboard_stacked_grade$student_group <- df_dashboard_stacked_grade$grade_level

# Region
df_dashboard_stacked_grade_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    ,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_grade_region$report_group <- "Grade Level"
df_dashboard_stacked_grade_region$student_group <- df_dashboard_stacked_grade_region$grade_level
df_dashboard_stacked_grade_region$ext_reference_cleaned <- NA
df_dashboard_stacked_grade_region$School <- "All Schools"

# State
df_dashboard_stacked_grade_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_grade_state$report_group <- "Grade Level"
df_dashboard_stacked_grade_state$student_group <- df_dashboard_stacked_grade_state$grade_level
df_dashboard_stacked_grade_state$ext_reference_cleaned <- NA
df_dashboard_stacked_grade_state$School <- "All Schools"
df_dashboard_stacked_grade_state$Region <- "All Regions"


# Race/Ethnicity
# School
df_dashboard_stacked_race <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           ,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race$student_group <- df_dashboard_stacked_race$race_ethnicity_rptgroup

# Region
df_dashboard_stacked_race_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    ,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race_region$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_region$student_group <- df_dashboard_stacked_race_region$race_ethnicity_rptgroup
df_dashboard_stacked_race_region$ext_reference_cleaned <- NA
df_dashboard_stacked_race_region$School <- "All Schools"

# State
df_dashboard_stacked_race_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race_state$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_state$student_group <- df_dashboard_stacked_race_state$race_ethnicity_rptgroup
df_dashboard_stacked_race_state$ext_reference_cleaned <- NA
df_dashboard_stacked_race_state$School <- "All Schools"
df_dashboard_stacked_race_state$Region <- "All Regions"

# Race/Ethnicity (Students of Color)
# School
df_dashboard_stacked_race_allbipoc <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race_allbipoc$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc$student_group <- "Students of Color"

# Region
df_dashboard_stacked_race_allbipoc_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race_allbipoc_region$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc_region$student_group <- "Students of Color"
df_dashboard_stacked_race_allbipoc_region$ext_reference_cleaned <- NA
df_dashboard_stacked_race_allbipoc_region$School <- "All Schools"

# State
df_dashboard_stacked_race_allbipoc_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  filter(race_ethnicity_rptgroup != "White" & race_ethnicity_rptgroup != "Unknown" & race_ethnicity_rptgroup != "REVIEW") %>% # Filter out all non-White students & group
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    #,first_gen
    construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_race_allbipoc_state$report_group <- "Race/Ethnicity"
df_dashboard_stacked_race_allbipoc_state$student_group <- "Students of Color"
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
           #,grade_level
           #,race_ethnicity_rptgroup
           ,gender_rptgroup
           #,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_gender$report_group <- "Gender"
df_dashboard_stacked_gender$student_group <- df_dashboard_stacked_gender$gender_rptgroup

# Region
df_dashboard_stacked_gender_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    ,gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_gender_region$report_group <- "Gender"
df_dashboard_stacked_gender_region$student_group <- df_dashboard_stacked_gender_region$gender_rptgroup
df_dashboard_stacked_gender_region$ext_reference_cleaned <- NA
df_dashboard_stacked_gender_region$School <- "All Schools"

# State
df_dashboard_stacked_gender_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    gender_rptgroup
    #,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_gender_state$report_group <- "Gender"
df_dashboard_stacked_gender_state$student_group <- df_dashboard_stacked_gender_state$gender_rptgroup
df_dashboard_stacked_gender_state$ext_reference_cleaned <- NA
df_dashboard_stacked_gender_state$School <- "All Schools"
df_dashboard_stacked_gender_state$Region <- "All Regions"

# First Generation
# School
df_dashboard_stacked_firstgen <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(ext_reference_cleaned
           ,School
           ,Region
           #,grade_level
           #,race_ethnicity_rptgroup
           #,gender_rptgroup
           ,first_gen
           ,construct
           ,question
           ,question_header
           ,question_subheader
           ,answer_type
           ,value
           ,text_value
           ,met_flag
           ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_firstgen$report_group <- "First Generation Status"
df_dashboard_stacked_firstgen$student_group <- df_dashboard_stacked_firstgen$first_gen

# Region
df_dashboard_stacked_firstgen_region <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    ,first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_firstgen_region$report_group <- "First Generation Status"
df_dashboard_stacked_firstgen_region$student_group <- df_dashboard_stacked_firstgen_region$first_gen
df_dashboard_stacked_firstgen_region$ext_reference_cleaned <- NA
df_dashboard_stacked_firstgen_region$School <- "All Schools"

# State
df_dashboard_stacked_firstgen_state <- df_detail %>%
  filter(denominator_flag == 1) %>% # Filter out all values that selected a NA value or did not complete the question
  group_by(#ext_reference_cleaned
    #,School
    #Region
    #,grade_level
    #,race_ethnicity_rptgroup
    #,gender_rptgroup
    first_gen
    ,construct
    ,question
    ,question_header
    ,question_subheader
    ,answer_type
    ,value
    ,text_value
    ,met_flag
    ,denominator_flag) %>%
  summarise(count_students = n())

df_dashboard_stacked_firstgen_state$report_group <- "First Generation Status"
df_dashboard_stacked_firstgen_state$student_group <- df_dashboard_stacked_firstgen_state$first_gen
df_dashboard_stacked_firstgen_state$ext_reference_cleaned <- NA
df_dashboard_stacked_firstgen_state$School <- "All Schools"
df_dashboard_stacked_firstgen_state$Region <- "All Regions"

# Normalize All Groups (drop the row used to aggregate)
df_dashboard_stacked_grade <- df_dashboard_stacked_grade %>% ungroup() %>% select(-grade_level)
df_dashboard_stacked_grade_region <- df_dashboard_stacked_grade_region %>% ungroup() %>% select(-grade_level)
df_dashboard_stacked_grade_state <- df_dashboard_stacked_grade_state %>% ungroup() %>% select(-grade_level)
df_dashboard_stacked_race <- df_dashboard_stacked_race %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_race_region <- df_dashboard_stacked_race_region %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_race_state <- df_dashboard_stacked_race_state %>% ungroup() %>% select(-race_ethnicity_rptgroup)
df_dashboard_stacked_gender <- df_dashboard_stacked_gender %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_stacked_gender_region <- df_dashboard_stacked_gender_region %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_stacked_gender_state <- df_dashboard_stacked_gender_state %>% ungroup() %>% select(-gender_rptgroup)
df_dashboard_stacked_firstgen <- df_dashboard_stacked_firstgen %>% ungroup() %>% select(-first_gen)
df_dashboard_stacked_firstgen_region <- df_dashboard_stacked_firstgen_region %>% ungroup() %>% select(-first_gen) 
df_dashboard_stacked_firstgen_state <- df_dashboard_stacked_firstgen_state %>% ungroup() %>% select(-first_gen)

# Union All Groups Together
df_dashboard_stacked_rpt <- df_dashboard_stacked_all %>%
  union_all(df_dashboard_stacked_all_region) %>%
  union_all(df_dashboard_stacked_all_state) %>%
  union_all(df_dashboard_stacked_grade) %>%
  union_all(df_dashboard_stacked_grade_region) %>%
  union_all(df_dashboard_stacked_grade_state) %>%
  union_all(df_dashboard_stacked_race) %>%
  union_all(df_dashboard_stacked_race_region) %>%
  union_all(df_dashboard_stacked_race_state) %>%
  union_all(df_dashboard_stacked_race_allbipoc) %>%
  union_all(df_dashboard_stacked_race_allbipoc_region) %>%
  union_all(df_dashboard_stacked_race_allbipoc_state) %>%
  union_all(df_dashboard_stacked_gender) %>%
  union_all(df_dashboard_stacked_gender_region) %>%
  union_all(df_dashboard_stacked_gender_state) %>%
  union_all(df_dashboard_stacked_firstgen) %>%
  union_all(df_dashboard_stacked_firstgen_region) %>%
  union_all(df_dashboard_stacked_firstgen_state)

# Union All State Groups Together
df_dashboard_stacked_rpt_state <- df_dashboard_stacked_all_state %>%
  union_all(df_dashboard_stacked_grade_state) %>%
  union_all(df_dashboard_stacked_race_state) %>%
  union_all(df_dashboard_stacked_race_allbipoc_state) %>%
  union_all(df_dashboard_stacked_gender_state) %>%
  union_all(df_dashboard_stacked_firstgen_state)

# Union All Region Groups Together
df_dashboard_stacked_rpt_region <- df_dashboard_stacked_all_region %>%
  union_all(df_dashboard_stacked_grade_region) %>%
  union_all(df_dashboard_stacked_race_region) %>%
  union_all(df_dashboard_stacked_race_allbipoc_region) %>%
  union_all(df_dashboard_stacked_gender_region) %>%
  union_all(df_dashboard_stacked_firstgen_region)


#######################
####
####   TO DO
####
######################
# Add Final Pieces
# Reference Values based on School Level Aggregations
#df_dashboard_stacked doesn't need suppression bc it's only used internally.

# Suppress < 10

# 1. Less than N suppression at each row 
df_dashboard_pctmet$lt10supp <- 0
df_dashboard_pctmet[df_dashboard_pctmet$count_students <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_dashboard_pctmet <- df_dashboard_pctmet %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_students) %>%  
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
df_dashboard_pctmet$count_students_og <- df_dashboard_pctmet$count_students
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet[df_dashboard_pctmet$lt10supp == 1,]$count_students <- NA
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$count_students <- NA
df_dashboard_pctmet[df_dashboard_pctmet$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet$suppression_notes <- "na"
df_dashboard_pctmet[df_dashboard_pctmet$flag_supp == 1,]$suppression_notes <- "student count less than 10, suppressed"

# Suppress < 10
# 1. Group by region, school, group, question and determine group level count
df_dashboard_stacked_rpt_grouped <- df_dashboard_stacked_rpt %>% group_by(Region,School,student_group,question) %>% summarise(group_level_count = sum(count_students))

# 2. Determine if that group level count is greater than or equal to 10
df_dashboard_stacked_rpt_grouped$grouplv_lt10 <- 0  
df_dashboard_stacked_rpt_grouped[df_dashboard_stacked_rpt_grouped$group_level_count <10,]$grouplv_lt10 <- 1

# 3. Join back to original stacked bar chart data frame on region, school, group, question
df_dashboard_stacked_rpt <- inner_join(df_dashboard_stacked_rpt_grouped, df_dashboard_stacked_rpt, by=c("Region","School","student_group","question"))

# 4. Determine if count should be suppressed for all text values within the group
df_dashboard_stacked_rpt$count_students_og <- df_dashboard_stacked_rpt$count_students
df_dashboard_stacked_rpt[df_dashboard_stacked_rpt$grouplv_lt10 == 1,]$count_students <- NA

# 5. Set suppression notes
df_dashboard_stacked_rpt$suppression_notes <- "na"
df_dashboard_stacked_rpt[df_dashboard_stacked_rpt$grouplv_lt10 == 1,]$suppression_notes <- "group level less than 10, suppressed"

# Suppress < 10
# 1. Less than N suppression at each row 
df_dashboard_pctmet_state$lt10supp <- 0
df_dashboard_pctmet_state[df_dashboard_pctmet_state$count_students <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_dashboard_pctmet_state <- df_dashboard_pctmet_state %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_students) %>%  
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
df_dashboard_pctmet_state$count_students_og <- df_dashboard_pctmet_state$count_students
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$lt10supp == 1,]$count_students <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$count_students <- NA
df_dashboard_pctmet_state[df_dashboard_pctmet_state$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet_state$suppression_notes <- "na"
df_dashboard_pctmet_state[df_dashboard_pctmet_state$flag_supp == 1,]$suppression_notes <- "student count less than 10, suppressed"

# Suppress < 10
# 1. Less than N suppression at each row 
df_dashboard_pctmet_region$lt10supp <- 0
df_dashboard_pctmet_region[df_dashboard_pctmet_region$count_students <10,]$lt10supp <- 1

# 2. Rank order of the rows within the group
df_dashboard_pctmet_region <- df_dashboard_pctmet_region %>% 
  group_by(Region, School, question, report_group) %>%
  arrange(count_students) %>%  
  mutate(rank_order = row_number())

df_dashboard_pctmet_region <- df_dashboard_pctmet_region %>%
  arrange(Region, School, question, report_group, rank_order)

# 3. Count how many instances of N suppression are within the reporting group (Race/Ethnicity, Gender)
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
df_dashboard_pctmet_region$count_students_og <- df_dashboard_pctmet_region$count_students
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$flag_supp <- 1
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$count_met <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$lt10supp == 1,]$count_students <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$flag_supp <- 1
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$count_students <- NA
df_dashboard_pctmet_region[df_dashboard_pctmet_region$compsupp == 1,]$count_met <- NA

# 5. Set suppression notes
df_dashboard_pctmet_region$suppression_notes <- "na"
df_dashboard_pctmet_region[df_dashboard_pctmet_region$flag_supp == 1,]$suppression_notes <- "student count less than 10, suppressed"

# no suppression for df_dashboard_stacked_rpt_state, needed un-suppressed for references

# Add Current Year & Filter
df_dashboard_stacked$year <- currentyear 
# Add field for Tableau
df_dashboard_stacked$Placeholder <- 1
df_dashboard_stacked_currentyear <- df_dashboard_stacked %>% filter(!is.na(Region))

df_dashboard_pctmet$year <- currentyear
df_dashboard_pctmet_currentyear <- df_dashboard_pctmet %>% filter(!is.na(Region))

### Adjust NA to -999
df_dashboard_stacked_rpt$year <- currentyear
df_dashboard_stacked_rpt$value[is.na(df_dashboard_stacked_rpt$value)] <- '-999'
df_dashboard_stacked_rpt_currentyear <- df_dashboard_stacked_rpt %>% filter(!is.na(Region))

df_dashboard_pctmet_state$year <- currentyear
df_dashboard_pctmet_state_currentyear <- df_dashboard_pctmet_state %>% filter(!is.na(Region))

df_dashboard_pctmet_region$year <- currentyear
df_dashboard_pctmet_region_currentyear <- df_dashboard_pctmet_region %>% filter(!is.na(Region))

df_dashboard_stacked_rpt_state$year <- currentyear
df_dashboard_stacked_rpt_state_currentyear <- df_dashboard_stacked_rpt_state %>% filter(!is.na(Region))

df_dashboard_stacked_rpt_region$year <- currentyear
df_dashboard_stacked_rpt_region_currentyear <- df_dashboard_stacked_rpt_region %>% filter(!is.na(Region))

# If not joining prior year data, utilize this shortcut & skip 'Adding in Prior Years'. Run again at 'Write Outputs':
df_dashboard_stacked <- df_dashboard_stacked_currentyear
df_dashboard_pctmet <- df_dashboard_pctmet_currentyear
df_dashboard_stacked_rpt <- df_dashboard_stacked_rpt_currentyear
df_dashboard_pctmet_state <- df_dashboard_pctmet_state_currentyear
df_dashboard_pctmet_region <- df_dashboard_pctmet_region_currentyear
df_dashboard_stacked_rpt_state <- df_dashboard_stacked_rpt_state_currentyear
df_dashboard_stacked_rpt_region <- df_dashboard_stacked_rpt_region_currentyear

###################################################
### Adding in Prior Years - Activate if only using multi-year data
###################################################
# Read in Prior Years
#df_dashboard_stacked_prioryears <- read_delim(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_stacked.csv"),delim = "|", guess_max = 5000)

#df_dashboard_pctmet_prioryears <- read_delim(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_pctmet.csv"),delim = "|", guess_max = 5000)  

#df_dashboard_stacked_rpt_prioryears <- read_delim(paste(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_stacked_rpt.csv"),delim = "|", guess_max = 5000)

#df_dashboard_pctmet_state_prioryears <- read_delim(paste(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_pctmet_state.csv"),delim = "|", guess_max = 5000) 

#df_dashboard_pctmet_region_prioryears <- read_delim(paste(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_pctmet_region.csv"),delim = "|", guess_max = 5000)

#df_dashboard_stacked_rpt_state_prioryears <- read_delim(paste(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_stacked_rpt_state.csv"),delim = "|", guess_max = 5000)  

#df_dashboard_stacked_rpt_region_prioryears <- read_delim(paste(file.path(base_dir,"data", "2_output", paste0(currentyear-1,"_finalized"),"Student","h2p_student_dashboard_stacked_rpt_region.csv"),delim = "|", guess_max = 5000)  


# Align to character (as needed)
#df_dashboard_stacked_currentyear$year <- as.character(df_dashboard_stacked_currentyear$year) 
#df_dashboard_pctmet_currentyear$year <- as.character(df_dashboard_pctmet_currentyear$year)
#df_dashboard_stacked_rpt_currentyear$year <- as.character(df_dashboard_stacked_rpt_currentyear$year)
#df_dashboard_stacked_rpt_currentyear$value <- as.character(df_dashboard_stacked_rpt_currentyear$value)
#df_dashboard_pctmet_state_currentyear$year <- as.character(df_dashboard_pctmet_state_currentyear$year)
#df_dashboard_pctmet_region_currentyear$year <- as.character(df_dashboard_pctmet_region_currentyear$year)
#df_dashboard_stacked_rpt_state_currentyear$year <- as.character(df_dashboard_stacked_rpt_state_currentyear$year)
#df_dashboard_stacked_rpt_region_currentyear$year <- as.character(df_dashboard_stacked_rpt_region_currentyear$year)

#df_dashboard_stacked_prioryears$year <- as.character(df_dashboard_stacked_prioryears$year) 
#df_dashboard_pctmet_prioryears$year <- as.character(df_dashboard_pctmet_prioryears$year)
#df_dashboard_stacked_rpt_prioryears$year <- as.character(df_dashboard_stacked_rpt_prioryears$year)
#df_dashboard_stacked_rpt_prioryears$value <- as.character(df_dashboard_stacked_rpt_prioryears$value)
#df_dashboard_pctmet_state_prioryears$year <- as.character(df_dashboard_pctmet_state_prioryears$year)
#df_dashboard_pctmet_region_prioryears$year <- as.character(df_dashboard_pctmet_region_prioryears$year)
#df_dashboard_stacked_rpt_state_prioryears$year <- as.character(df_dashboard_stacked_rpt_state_prioryears$year)
#df_dashboard_stacked_rpt_region_prioryears$year <- as.character(df_dashboard_stacked_rpt_region_prioryears$year)


# Bind Current Year to Prior Years

#df_dashboard_stacked <- bind_rows(df_dashboard_stacked_currentyear,df_dashboard_stacked_prioryears)

#df_dashboard_pctmet <- bind_rows(df_dashboard_pctmet_currentyear,df_dashboard_pctmet_prioryears)

#df_dashboard_stacked_rpt <- bind_rows(df_dashboard_stacked_rpt_currentyear,df_dashboard_stacked_rpt_prioryears)

#df_dashboard_pctmet_state <- bind_rows(df_dashboard_pctmet_state_currentyear,df_dashboard_pctmet_state_prioryears)

#df_dashboard_pctmet_region <- bind_rows(df_dashboard_pctmet_region_currentyear,df_dashboard_pctmet_region_prioryears)

#df_dashboard_stacked_rpt_state <- bind_rows(df_dashboard_stacked_rpt_state_currentyear,df_dashboard_stacked_rpt_state_prioryears)

#df_dashboard_stacked_rpt_region <- bind_rows(df_dashboard_stacked_rpt_region_currentyear,df_dashboard_stacked_rpt_region_prioryears)


###Write Outputs - yearly folder

write_dashboard_file(df_dashboard_stacked,"h2p_student_dashboard_stacked.csv",output_dir)

write_dashboard_file(df_dashboard_pctmet,"h2p_student_dashboard_pctmet.csv",output_dir)

write_dashboard_file(df_dashboard_stacked_rpt,"h2p_student_dashboard_stacked_rpt.csv",output_dir)

write_dashboard_file(df_dashboard_pctmet_state,"h2p_student_dashboard_pctmet_state.csv",output_dir)

write_dashboard_file(df_dashboard_pctmet_region,"h2p_student_dashboard_pctmet_region.csv",output_dir)

write_dashboard_file(df_dashboard_stacked_rpt_state,"h2p_student_dashboard_stacked_rpt_state.csv",output_dir)

write_dashboard_file(df_dashboard_stacked_rpt_region,"h2p_student_dashboard_stacked_rpt_region.csv",output_dir)


### Write to folder used by Tableau
write_dashboard_file(df_dashboard_stacked,"h2p_student_dashboard_stacked.csv",dashboard_dir)

write_dashboard_file(df_dashboard_pctmet,"h2p_student_dashboard_pctmet.csv",dashboard_dir)

write_dashboard_file(df_dashboard_stacked_rpt,"h2p_student_dashboard_stacked_rpt.csv",dashboard_dir)

write_dashboard_file(df_dashboard_pctmet_state,"h2p_student_dashboard_pctmet_state.csv",dashboard_dir)

write_dashboard_file(df_dashboard_pctmet_region,"h2p_student_dashboard_pctmet_region.csv",dashboard_dir)

write_dashboard_file(df_dashboard_stacked_rpt_state,"h2p_student_dashboard_stacked_rpt_state.csv",dashboard_dir)

write_dashboard_file(df_dashboard_stacked_rpt_region,"h2p_student_dashboard_stacked_rpt_region.csv",dashboard_dir)
