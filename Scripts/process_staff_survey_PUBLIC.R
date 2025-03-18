#PROCESS STAFF SURVEY:
# Instructions: Update currentyear parameter,set base directory, & validate union accuracy
require(readxl)
require(dplyr)
require(tidyr)
require(here)

#Update this year to match the parent folders of data/1_raw and data/2_output
currentyear <- 2024

# Set base directory to match the folder you want to read and write to
base_dir <- "path/to/your/folder"  # Change this to your local directory
# Define raw data directory dynamically
raw_data_dir <- file.path(base_dir, "data", "1_raw", currentyear)

# Define file paths dynamically
output_dir <- file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), "Staff")
dashboard_dir <- file.path(base_dir, "data", "2_output", "dashboard_files_finalized", "Staff")

# Function to write files
write_dashboard_file <- function(data, filename, folder) {
  file_path <- file.path(folder, filename)
  write.table(data, file = file_path, append = FALSE, quote = FALSE, sep = "|",
              eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
}

# Ready in Raw Survey Data
df_staff <- read_xlsx(file.path(raw_data_dir,"QuestionPro - H2P Staff MASTER SURVEY.xlsx"), sheet = "Raw Data", skip = 0)
###################################################
### Format Staff Survey Raw File
###################################################

# Delete the first row with sub header
df_staff <- df_staff[-1,]

# Delete the columns
df_staff <- df_staff[,-c(18,19,20,21,26,27,33,40,41,47,60,61,73,78,79,81,82)]

# Replace column names with normalized headers
colnames_staff <- c(
  'staff_id',
  'response_status',
  'ip_address',
  'timestamp',
  'duplicate',
  'time_to_complete',
  'seq_number',
  'ext_reference',
  'school_01',
  'school_02',
  'school_03',
  'school_04',
  'school_05',
  'respondent_email',
  'email_list',
  'country_code',
  'region',
  'grade_09',
  'grade_10',
  'grade_11',
  'grade_12',
  'q01a',
  'q01b',
  'q01c',
  'q01d',
  'q01e',
  'q03a',
  'q21a',
  'q06a',
  'q06b',
  'q06c',
  'q06d',
  'q07a',
  'q07e',
  'q07b',
  'q07c',
  'q07f',
  'q07d',
  'q08a',
  'q11g',
  'q11h',
  'q11i',
  'q11j',
  'q11k',
  'q11l',
  'q11c',
  'q11d',
  'q11e',
  'q11f',
  'q12a',
  'q13a',
  'q13b',
  'q13c',
  'q13d',
  'q13e',
  'q14b',
  'q14c',
  'q14d',
  'q14e',
  'q14f',
  'q18b',
  'q18c',
  'q18d',
  'q18e',
  'q20a',
  'race_a',
  'race_b',
  'race_i',
  'race_w',
  'race_h',
  'race_m',
  'race_p',
  'tribe',
  'gen',
  'gen_other'
)

colnames(df_staff) <- colnames_staff

# Add a survey version field
df_staff$version <- 'v1'

# Code Gender Numerical Values to normalized Text Values
df_staff$gender <- case_when(df_staff$gen == 1 ~ "Male",
                             df_staff$gen == 2 ~ "Female",
                             df_staff$gen == 3 ~ "Non-Binary",
                             df_staff$gen == 4 ~ "Not Sure/Questioning",
                             df_staff$gen == 5 ~ "Prefer Not to Say",
                             df_staff$gen == 6 ~ "Other",
                             TRUE ~ "Incomplete")

# Filter out incomplete 
df_staff_completed <- df_staff %>%
  filter(response_status == 'Completed')

# Union Versions into a single normalized file
# for staff there is just a single version.  Leaving this step in so I can utilize the existing code.
df_completed_combined <- df_staff_completed

# Remove all line breaks from text_value field
df_completed_combined$q20a <- gsub("[\r\n]+", " ", df_completed_combined$q20a)

# Remove all line breaks from gen_other field
df_completed_combined$gen_other <- gsub("[\r\n]+", " ", df_completed_combined$gen_other)

# Check for questionable demographic data
#Race
df_completed_combined$all_race <-as.integer(df_completed_combined$race_a) *
  as.integer(df_completed_combined$race_b) *
  as.integer(df_completed_combined$race_i) *
  as.integer(df_completed_combined$race_w) *
  as.integer(df_completed_combined$race_h) *
  as.integer(df_completed_combined$race_m) *
  as.integer(df_completed_combined$race_p)

# Remove Unused Columns
df_rpt <- df_completed_combined %>%
  select(
    #'staff_id', # Remove Staff ID to protect privacy
    'timestamp',
    'time_to_complete',
    'ext_reference',
    'grade_09',
    'grade_10',
    'grade_11',
    'grade_12',
    'q01a',
    'q01b',
    'q01c',
    'q01d',
    'q01e',
    'q03a',
    'q21a',
    'q06a',
    'q06b',
    'q06c',
    'q06d',
    'q07a',
    'q07e',
    'q07b',
    'q07c',
    'q07f',
    'q07d',
    'q08a',
    'q11g',
    'q11h',
    'q11i',
    'q11j',
    'q11k',
    'q11l',
    'q11c',
    'q11d',
    'q11e',
    'q11f',
    'q12a',
    'q13a',
    'q13b',
    'q13c',
    'q13d',
    'q13e',
    'q14b',
    'q14c',
    'q14d',
    'q14e',
    'q14f',
    'q18b',
    'q18c',
    'q18d',
    'q18e',
    'q20a',
    'race_a',
    'race_b',
    'race_i',
    'race_w',
    'race_h',
    'race_m',
    'race_p',
    'all_race',
    'tribe',
    'gender',
    'gen_other',
    'version')

# Pivot Data Set
df_longfile <- gather(df_rpt,
                      key = "question",
                      value = "value",
                      c(q01a:q20a),
                      convert = FALSE,
                      factor_key=TRUE)

# Code Answer Text values & Positive Measures
# Denominator/Count
# Numberator/Measure

df_longfile$text_value <- df_longfile$value

df_longfile$value <- as.integer(df_longfile$value)

# Defined Functions to convert values
yesvaluecoding <- function(x) case_when(x == "1.0" ~ "Yes",
                                        x == "2.0" ~ "No",
                                        x == "1" ~ "Yes",
                                        x == "2" ~ "No")

knowledgeablevaluecoding <- function(x) case_when(x == "1.0" ~ "No Knowledge",
                                                  x == "2.0" ~ "Slightly Knowledgeable",
                                                  x == "3.0" ~ "Knowledgeable",
                                                  x == "4.0" ~ "Extremely Knowledgeable",
                                                  x == "1" ~ "No Knowledge",
                                                  x == "2" ~ "Slightly Knowledgeable",
                                                  x == "3" ~ "Knowledgeable",
                                                  x == "4" ~ "Extremely Knowledgeable")

helpfulvaluecoding <- function(x) case_when(x == "1.0" ~ "Not Applicable/Did Not Participate",
                                            x == "2.0" ~ "Not Helpful",
                                            x == "3.0" ~ "Slightly Helpful",
                                            x == "4.0" ~ "Helpful",
                                            x == "5.0" ~ "Extremely Helpful",
                                            x == "1" ~ "Not Applicable/Did Not Participate",
                                            x == "2" ~ "Not Helpful",
                                            x == "3" ~ "Slightly Helpful",
                                            x == "4" ~ "Helpful",
                                            x == "5" ~ "Extremely Helpful")

edlevelvaluecoding <- function(x) case_when(x == "1.0" ~ "High School Diploma",
                                            x == "2.0" ~ "1- Year Certificate/ Trade Programs",
                                            x == "3.0" ~ "Some College but Less Than a 2-Year or 4-Year College Degree",
                                            x == "4.0" ~ "2-Year College Degree (Associates)",
                                            x == "5.0" ~ "Apprenticeship (a paid job that provides both hands-on experience and classroom instruction in a specific field)",
                                            x == "6.0" ~ "4-Year College Degree (Bachelors) or Higher",
                                            x == "7.0" ~ "Other (please specify)",
                                            x == "1" ~ "High School Diploma",
                                            x == "2" ~ "1- Year Certificate/ Trade Programs",
                                            x == "3" ~ "Some College but Less Than a 2-Year or 4-Year College Degree",
                                            x == "4" ~ "2-Year College Degree (Associates)",
                                            x == "5" ~ "Apprenticeship (a paid job that provides both hands-on experience and classroom instruction in a specific field)",
                                            x == "6" ~ "4-Year College Degree (Bachelors) or Higher",
                                            x == "7" ~ "Other (please specify)")

likelyvaluecoding <- function(x) case_when(x == "1.0" ~ "No, not at all",
                                           x == "2.0" ~ "Most likely, no",
                                           x == "3.0" ~ "Most likely, yes",
                                           x == "4.0" ~ "Yes, definitely",
                                           x == "1" ~ "No, not at all",
                                           x == "2" ~ "Most likely, no",
                                           x == "3" ~ "Most likely, yes",
                                           x == "4" ~ "Yes, definitely")

likelyadvvaluecoding <- function(x) case_when(x == "1.0" ~ "No, not at all",
                                              x == "2.0" ~ "Most likely, no",
                                              x == "3.0" ~ "Most likely, yes",
                                              x == "4.0" ~ "Yes, definitely",
                                              x == "5.0" ~ "I do not have an advisory/homeroom class",
                                              x == "1" ~ "No, not at all",
                                              x == "2" ~ "Most likely, no",
                                              x == "3" ~ "Most likely, yes",
                                              x == "4" ~ "Yes, definitely",
                                              x == "5" ~ "I do not have an advisory/homeroom class")


agreevaluecoding <- function(x) case_when(x == "1.0" ~ "Strongly Disagree",
                                          x == "2.0" ~ "Disagree",
                                          x == "3.0" ~ "Agree",
                                          x == "4.0" ~ "Strongly Agree",
                                          x == "1" ~ "Strongly Disagree",
                                          x == "2" ~ "Disagree",
                                          x == "3" ~ "Agree",
                                          x == "4" ~ "Strongly Agree")

teachdccoding <- function(x) case_when(x == "1.0" ~ "I am currently teaching a dual credit course.",
                                       x == "2.0" ~ "I have taught a dual credit course at this school before, but I am not currently teaching it at the moment.",
                                       x == "3.0" ~ "I am not currently teaching a dual credit course nor have I taught it before in the past.",
                                       x == "1" ~ "I am currently teaching a dual credit course.",
                                       x == "2" ~ "I have taught a dual credit course at this school before, but I am not currently teaching it at the moment.",
                                       x == "3" ~ "I am not currently teaching a dual credit course nor have I taught it before in the past.")

yesnodccoding <- function(x) case_when(x == "1.0" ~ "Not Applicable",
                                       x == "2.0" ~ "Yes",
                                       x == "3.0" ~ "No",
                                       x == "4.0" ~ "Not Offered At My School",
                                       x == "1" ~ "Not Applicable",
                                       x == "2" ~ "Yes",
                                       x == "3" ~ "No",
                                       x == "4" ~ "Not Offered At My School")

# Yes/No
df_longfile_yes <- df_longfile %>%
  filter(question %in% c('q01a','q01b','q01c','q01d','q01e','q07a','q07e','q07b','q07c','q07f','q07d')) %>%
  mutate_at(.vars = vars(text_value), .funs = yesvaluecoding)

# Yes/No Dual Credit
df_longfile_yesdc <- df_longfile %>%
  filter(question %in% c('q14b','q14c','q14d','q14e','q14f')) %>%
  mutate_at(.vars = vars(text_value), .funs = yesnodccoding)

# Knowledgeable
df_longfile_knowledgeable <- df_longfile %>%
  filter(question %in% c('q06a','q06b','q06c','q06d','q11g','q11h','q11i','q11j','q11k','q11l','q11c','q11d','q11e','q11f','q18b','q18c','q18d','q18e')) %>%
  mutate_at(.vars = vars(text_value), .funs = knowledgeablevaluecoding)

# Teach DC
df_longfile_teachdc <- df_longfile %>%
  filter(question %in% c('q12a')) %>%
  mutate_at(.vars = vars(text_value), .funs = teachdccoding)

# Multiselect
df_longfile_multiselect <- df_longfile %>%
  filter(question %in% c('q13a','q13b','q13c','q13d','q13e')) %>%
  mutate_at(.vars = vars(text_value), .funs = yesvaluecoding)

# Open Ended 
df_longfile_open <- df_longfile %>%
  filter(question %in% c('q20a')) 

# Continuous (no changes needed)
df_longfile_continuous <- df_longfile %>%
  filter(question %in% c('q03a','q21a','q08a')) 

df_longfile_final <- union_all(df_longfile_yes, df_longfile_knowledgeable) %>%
  union_all(df_longfile_teachdc) %>%
  union_all(df_longfile_multiselect) %>%
  union_all(df_longfile_open) %>%
  union_all(df_longfile_continuous) %>%
  union_all(df_longfile_yesdc)

# validate union accuracy
nrow(df_longfile_final)
nrow(df_longfile)

# Write to yearly folder
write_dashboard_file(df_longfile_final, "h2p_staff_longfile.csv", output_dir)
write_dashboard_file(df_completed_combined, "h2p_staff_analysisfile.csv", output_dir)

# Write to dashboard folder
write_dashboard_file(df_longfile_final, "h2p_staff_longfile.csv", dashboard_dir)
write_dashboard_file(df_completed_combined, "h2p_staff_analysisfile.csv", dashboard_dir)
