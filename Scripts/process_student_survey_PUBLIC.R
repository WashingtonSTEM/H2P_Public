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
output_dir <- file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), "Student")
dashboard_dir <- file.path(base_dir, "data", "2_output", "dashboard_files_finalized", "Student")

# Function to write files
write_dashboard_file <- function(data, filename, folder) {
  file_path <- file.path(folder, filename)
  write.table(data, file = file_path, append = FALSE, quote = FALSE, sep = "|",
              eol = "\n", na = "", dec = ".", row.names = FALSE, col.names = TRUE)
}

# Read in Raw Survey Data
df_v1 <- read_xlsx(file.path(raw_data_dir,"QuestionPro - H2P Student MASTER SURVEY V1_1.xlsx"), sheet = "Raw Data", skip = 0)
df_vSpanish <- read_xlsx(file.path(raw_data_dir,"QuestionPro - H2P Student MASTER SURVEY Spanish.xlsx"), sheet = "Raw Data", skip = 0)

###################################################
### Format v1
###################################################

# Delete the first row with sub header from each part
df_v1 <- df_v1[-1,]

# Delete the columns
df_v1 <- df_v1[,-c(18,19,20,22,23,29,50,51,57,74,75,88,104,105,109,110,114,115)]

# Replace column names with normalized headers
colnames_v1 <- c(
  'stu_id',
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
  'grade',
  'q01a',
  'q01b',
  'q01c',
  'q01d',
  'q01e',
  'q02a',
  'q02b',
  'q02c',
  'q02d',
  'q04a',
  'q04b',
  'q04c',
  'q04d',
  'q04e',
  'q04f',
  'q04g',
  'q04h',
  'q04i',
  'q04j',
  'q08a',
  'q08b',
  'q22a',
  'q22b',
  'q23a',
  'q10a',
  'q11a',
  'q11b',
  'q11c',
  'q11e',
  'q11d',
  'q12a',
  'q12b',
  'q12c',
  'q12d',
  'q12f',
  'q13a',
  'q13b',
  'q13c',
  'q13d',
  'q13e',
  'q13f',
  'q13g',
  'q13h',
  'q13i',
  'q13j',
  'q14a',
  'q15a',
  'q15b',
  'q15c',
  'q15d',
  'q15e',
  'q15f',
  'q17a',
  'q17b',
  'q17c',
  'q17d',
  'q17e',
  'q17f',
  'q18a',
  'q18b',
  'q18c',
  'q18d',
  'q18e',
  'q19a',
  'q19b',
  'q19c',
  'q19d',
  'q19e',
  'q19f',
  'q19g',
  'q19h',
  'q19i',
  'q19j',
  'q24a',
  'q25a',
  'q21a',
  'q26a',
  'q27a',
  'q28a',
  'race_a',
  'race_b',
  'race_i',
  'race_w',
  'race_h',
  'race_m',
  'race_p',
  'lang_eng',
  'lang_spa',
  'lang_rus',
  'lang_tag',
  'lang_ger',
  'lang_chi',
  'lang_viet',
  'lang_other',
  'tribe',
  'tribe_bristol',
  'tribe_duwamish',
  'tribe_chinook',
  'tribe_coeurdalene',
  'tribe_chehalis',
  'tribe_colville',
  'tribe_grandronde',
  'tribe_salishkootenai',
  'tribe_umatilla',
  'tribe_warmsprings',
  'tribe_athabaskan',
  'tribe_cowlitz',
  'tribe_hoh',
  'tribe_jamestownsklallam',
  'tribe_klamath',
  'tribe_kikiallus',
  'tribe_kootenai',
  'tribe_lowerelwhasklallam',
  'tribe_lummi',
  'tribe_makah',
  'tribe_maniilaq',
  'tribe_mariettabandofnooksack',
  'tribe_muckleshoot',
  'tribe_nezpierce',
  'tribe_nisqually',
  'tribe_portgamblesklallam',
  'tribe_puyallup',
  'tribe_quileute',
  'tribe_quinault',
  'tribe_samish',
  'tribe_sauk-suiattle',
  'tribe_shoalwaterbay',
  'tribe_skokomish',
  'tribe_snohomish',
  'tribe_snoqualmoo',
  'tribe_spokane',
  'tribe_squaxinisland',
  'tribe_stillaguamish',
  'tribe_steilacoom',
  'tribe_swinomish',
  'tribe_tananaathabaskan',
  'tribe_tulalip',
  'tribe_upperskagit',
  'tribe_yakama',
  'tribe_yukonkuskokwimdelta',
  'gen',
  'gen_other'
)

colnames(df_v1) <- colnames_v1

# Add a survey version field
df_v1$version <- 'v1'

# Code Gender Numerical Values to normalized Text Values
df_v1$gender <- case_when(df_v1$gen == 1 ~ "Male",
                          df_v1$gen == 2 ~ "Female",
                          df_v1$gen == 3 ~ "Non-Binary",
                          df_v1$gen == 4 ~ "Not Sure/Questioning",
                          df_v1$gen == 5 ~ "Prefer Not to Say",
                          df_v1$gen == 6 ~ "Other",
                          TRUE ~ "Incomplete")

# Filter out incomplete 
df_v1_completed <- df_v1 %>%
  filter(response_status == 'Completed')

###################################################
### Format vSpanish
###################################################

# Delete the first row with sub header
df_vSpanish <- df_vSpanish[-1,]

# Delete the columns
df_vSpanish <- df_vSpanish[,-c(18,19,20,22,23,49,50,72,73,86,102,103,107,108,112,113)]

# Replace column names with normalized headers
colnames_vSpanish <- c(
  'stu_id', 
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
  'grade',
  'q01a',
  'q01b',
  'q01c',
  'q01d',
  'q01e',
  'q02a',
  'q02b',
  'q02c',
  'q02d',
  'q04a',
  'q04b',
  'q04c',
  'q04d',
  'q04e',
  'q04f',
  'q04g',
  'q04h',
  'q04i',
  'q04j',
  'q08a',
  'q08b',
  'q22a',
  'q22b',
  'q23a',
  'q10a',
  'q11a',
  'q11b',
  'q11c',
  'q11e',
  'q11d',
  'q12a',
  'q12b',
  'q12c',
  'q12d',
  'q12f',
  'q13a',
  'q13b',
  'q13c',
  'q13d',
  'q13e',
  'q13f',
  'q13g',
  'q13h',
  'q13i',
  'q13j',
  'q14a',
  'q15a',
  'q15b',
  'q15c',
  'q15d',
  'q15e',
  'q15f',
  'q17a',
  'q17b',
  'q17c',
  'q17d',
  'q17e',
  'q17f',
  'q18a',
  'q18b',
  'q18c',
  'q18d',
  'q18e',
  'q19a',
  'q19b',
  'q19c',
  'q19d',
  'q19e',
  'q19f',
  'q19g',
  'q19h',
  'q19i',
  'q19j',
  'q24a',
  'q25a',
  'q21a',
  'q26a',
  'q27a',
  'q28a',
  'race_a',
  'race_b',
  'race_i',
  'race_w',
  'race_h',
  'race_m',
  'race_p',
  'lang_eng',
  'lang_spa',
  'lang_rus',
  'lang_tag',
  'lang_ger',
  'lang_chi',
  'lang_viet',
  'lang_other',
  'tribe',
  'tribe_bristol',
  'tribe_duwamish',
  'tribe_chinook',
  'tribe_coeurdalene',
  'tribe_chehalis',
  'tribe_colville',
  'tribe_grandronde',
  'tribe_salishkootenai',
  'tribe_umatilla',
  'tribe_warmsprings',
  'tribe_athabaskan',
  'tribe_cowlitz',
  'tribe_hoh',
  'tribe_jamestownsklallam',
  'tribe_klamath',
  'tribe_kikiallus',
  'tribe_kootenai',
  'tribe_lowerelwhasklallam',
  'tribe_lummi',
  'tribe_makah',
  'tribe_maniilaq',
  'tribe_mariettabandofnooksack',
  'tribe_muckleshoot',
  'tribe_nezpierce',
  'tribe_nisqually',
  'tribe_portgamblesklallam',
  'tribe_puyallup',
  'tribe_quileute',
  'tribe_quinault',
  'tribe_samish',
  'tribe_sauk-suiattle',
  'tribe_shoalwaterbay',
  'tribe_skokomish',
  'tribe_snohomish',
  'tribe_snoqualmoo',
  'tribe_spokane',
  'tribe_squaxinisland',
  'tribe_stillaguamish',
  'tribe_steilacoom',
  'tribe_swinomish',
  'tribe_tananaathabaskan',
  'tribe_tulalip',
  'tribe_upperskagit',
  'tribe_yakama',
  'tribe_yukonkuskokwimdelta',
  'gen',
  'gen_other'
)

colnames(df_vSpanish) <- colnames_vSpanish

# Add a survey version field
df_vSpanish$version <- 'Spanish'

# Code Gender Numerical Values to normalized Text Values
df_vSpanish$gender <- case_when(df_vSpanish$gen == 1 ~ "Male",
                                df_vSpanish$gen == 2 ~ "Female",
                                df_vSpanish$gen == 3 ~ "Non-Binary",
                                df_vSpanish$gen == 4 ~ "Not Sure/Questioning",
                                df_vSpanish$gen == 5 ~ "Prefer Not to Say",
                                df_vSpanish$gen == 6 ~ "Other",
                                TRUE ~ "Incomplete")

# Filter out incomplete 
df_vSpanish_completed <- df_vSpanish %>%
  filter(response_status == 'Completed')

###################################################
### finalize formatting
###################################################

# Union Spanish & English Versions into a single normalized file
# Ensure all columns have the same data type
df_v1_completed <- df_v1_completed %>%
  mutate(across(everything(), as.character))

df_vSpanish_completed <- df_vSpanish_completed %>%
  mutate(across(everything(), as.character))

# Now combine the two datasets
df_completed_combined <- union_all(df_v1_completed,df_vSpanish_completed)

# Remove all line breaks from text_value field
df_completed_combined$q08b <- gsub("[\r\n]+", " ", df_completed_combined$q08b)
df_completed_combined$q21a <- gsub("[\r\n]+", " ", df_completed_combined$q21a)
df_completed_combined$q22b <- gsub("[\r\n]+", " ", df_completed_combined$q22b)

# Remove all line breaks from gen_other field
df_completed_combined$gen_other <- gsub("[\r\n]+", " ", df_completed_combined$gen_other)

# Remove all line breaks from lang_other field
df_completed_combined$lang_other <- gsub("[\r\n]+", " ", df_completed_combined$lang_other)

# Code Grade Level
df_completed_combined$grade_level <- case_when(df_completed_combined$grade == 1 ~ "9th Grade",
                                               df_completed_combined$grade == 2 ~ "10th Grade",
                                               df_completed_combined$grade == 3 ~ "11th Grade",
                                               df_completed_combined$grade == 4 ~ "12th Grade",
                                               TRUE ~ "Incomplete")

# Check for questionable demographic data
#Race
df_completed_combined$all_race <-as.integer(df_completed_combined$race_a) *
  as.integer(df_completed_combined$race_b) *
  as.integer(df_completed_combined$race_i) *
  as.integer(df_completed_combined$race_w) *
  as.integer(df_completed_combined$race_h) *
  as.integer(df_completed_combined$race_m) *
  as.integer(df_completed_combined$race_p)

# Language
df_completed_combined$all_lang <-as.integer(df_completed_combined$lang_eng) *
  as.integer(df_completed_combined$lang_spa) *
  as.integer(df_completed_combined$lang_rus) *
  as.integer(df_completed_combined$lang_tag) *
  as.integer(df_completed_combined$lang_ger) *
  as.integer(df_completed_combined$lang_chi) *
  as.integer(df_completed_combined$lang_viet)

# Tribe
df_completed_combined$all_tribe <-as.integer(df_completed_combined$tribe_bristol) *
  as.integer(df_completed_combined$tribe_duwamish) *
  as.integer(df_completed_combined$tribe_chinook) *
  as.integer(df_completed_combined$tribe_coeurdalene) *
  as.integer(df_completed_combined$tribe_chehalis) *
  as.integer(df_completed_combined$tribe_colville) *
  as.integer(df_completed_combined$tribe_grandronde) *
  as.integer(df_completed_combined$tribe_salishkootenai) *
  as.integer(df_completed_combined$tribe_umatilla) *
  as.integer(df_completed_combined$tribe_warmsprings) *
  as.integer(df_completed_combined$tribe_athabaskan) *
  as.integer(df_completed_combined$tribe_cowlitz) *
  as.integer(df_completed_combined$tribe_hoh) *
  as.integer(df_completed_combined$tribe_jamestownsklallam) *
  as.integer(df_completed_combined$tribe_klamath) *
  as.integer(df_completed_combined$tribe_kikiallus) *
  as.integer(df_completed_combined$tribe_kootenai) *
  as.integer(df_completed_combined$tribe_lowerelwhasklallam) *
  as.integer(df_completed_combined$tribe_lummi) *
  as.integer(df_completed_combined$tribe_makah) *
  as.integer(df_completed_combined$tribe_maniilaq) *
  as.integer(df_completed_combined$tribe_mariettabandofnooksack) *
  as.integer(df_completed_combined$tribe_muckleshoot) *
  as.integer(df_completed_combined$tribe_nezpierce) *
  as.integer(df_completed_combined$tribe_nisqually) *
  as.integer(df_completed_combined$tribe_portgamblesklallam) *
  as.integer(df_completed_combined$tribe_puyallup) *
  as.integer(df_completed_combined$tribe_quileute) *
  as.integer(df_completed_combined$tribe_quinault) *
  as.integer(df_completed_combined$tribe_samish) *
  as.integer(df_completed_combined$`tribe_sauk-suiattle`) *
  as.integer(df_completed_combined$tribe_shoalwaterbay) *
  as.integer(df_completed_combined$tribe_skokomish) *
  as.integer(df_completed_combined$tribe_snohomish) *
  as.integer(df_completed_combined$tribe_snoqualmoo) *
  as.integer(df_completed_combined$tribe_spokane) *
  as.integer(df_completed_combined$tribe_squaxinisland) *
  as.integer(df_completed_combined$tribe_stillaguamish) *
  as.integer(df_completed_combined$tribe_steilacoom) *
  as.integer(df_completed_combined$tribe_swinomish) *
  as.integer(df_completed_combined$tribe_tananaathabaskan) *
  as.integer(df_completed_combined$tribe_tulalip) *
  as.integer(df_completed_combined$tribe_upperskagit) *
  as.integer(df_completed_combined$tribe_yakama) *
  as.integer(df_completed_combined$tribe_yukonkuskokwimdelta)

# Remove Unused Columns
df_rpt <- df_completed_combined %>%
  select(
    #'stu_id', # Remove Student ID to protect student privacy
    'timestamp',
    'time_to_complete',
    'ext_reference',
    'grade_level',
    'q01a',
    'q01b',
    'q01c',
    'q01d',
    'q01e',
    'q02a',
    'q02b',
    'q02c',
    'q02d',
    'q04a',
    'q04b',
    'q04c',
    'q04d',
    'q04e',
    'q04f',
    'q04g',
    'q04h',
    'q04i',
    'q04j',
    'q08a',
    'q08b',
    'q22a',
    'q22b',
    'q23a',
    'q10a',
    'q11a',
    'q11b',
    'q11c',
    'q11e',
    'q11d',
    'q12a',
    'q12b',
    'q12c',
    'q12d',
    'q12f',
    'q13a',
    'q13b',
    'q13c',
    'q13d',
    'q13e',
    'q13f',
    'q13g',
    'q13h',
    'q13i',
    'q13j',
    'q14a',
    'q15a',
    'q15b',
    'q15c',
    'q15d',
    'q15e',
    'q15f',
    'q17a',
    'q17b',
    'q17c',
    'q17d',
    'q17e',
    'q17f',
    'q18a',
    'q18b',
    'q18c',
    'q18d',
    'q18e',
    'q19a',
    'q19b',
    'q19c',
    'q19d',
    'q19e',
    'q19f',
    'q19g',
    'q19h',
    'q19i',
    'q19j',
    'q24a',
    'q25a',
    'q21a',
    'q26a',
    'q27a',
    'q28a',
    'race_a',
    'race_b',
    'race_i',
    'race_w',
    'race_h',
    'race_m',
    'race_p',
    'all_race',
    'lang_eng',
    'lang_spa',
    'lang_rus',
    'lang_tag',
    'lang_ger',
    'lang_chi',
    'lang_viet',
    'lang_other',
    'all_lang',
    'tribe',
    'tribe_bristol',
    'tribe_duwamish',
    'tribe_chinook',
    'tribe_coeurdalene',
    'tribe_chehalis',
    'tribe_colville',
    'tribe_grandronde',
    'tribe_salishkootenai',
    'tribe_umatilla',
    'tribe_warmsprings',
    'tribe_athabaskan',
    'tribe_cowlitz',
    'tribe_hoh',
    'tribe_jamestownsklallam',
    'tribe_klamath',
    'tribe_kikiallus',
    'tribe_kootenai',
    'tribe_lowerelwhasklallam',
    'tribe_lummi',
    'tribe_makah',
    'tribe_maniilaq',
    'tribe_mariettabandofnooksack',
    'tribe_muckleshoot',
    'tribe_nezpierce',
    'tribe_nisqually',
    'tribe_portgamblesklallam',
    'tribe_puyallup',
    'tribe_quileute',
    'tribe_quinault',
    'tribe_samish',
    'tribe_sauk-suiattle',
    'tribe_shoalwaterbay',
    'tribe_skokomish',
    'tribe_snohomish',
    'tribe_snoqualmoo',
    'tribe_spokane',
    'tribe_squaxinisland',
    'tribe_stillaguamish',
    'tribe_steilacoom',
    'tribe_swinomish',
    'tribe_tananaathabaskan',
    'tribe_tulalip',
    'tribe_upperskagit',
    'tribe_yakama',
    'tribe_yukonkuskokwimdelta',
    'all_tribe',
    'gender',
    'gen_other',
    'version')

# Need to reverse the way that Yes & No get coded for the First Gen flag because the question asks if student's parent went to college
df_rpt$first_gen <- case_when(df_rpt$q10a == "1" ~ "Not First Generation",
                              df_rpt$q10a == "2" ~ "First Generation")

# Pivot Data Set
df_longfile <- gather(df_rpt,
                      key = "question",
                      value = "value",
                      c(q01a:q28a),
                      convert = FALSE,
                      factor_key=TRUE)

# Code Answer Text values & Positive Measures
# Denominator/Count
# Numerator/Measure

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


rankvaluecoding <- function(x) case_when(x == "1.0" ~ "1st",
                                         x == "2.0" ~ "2nd",
                                         x == "3.0" ~ "3rd",
                                         x == "4.0" ~ "4th",
                                         x == "5.0" ~ "5th",
                                         x == "6.0" ~ "6th",
                                         x == "7.0" ~ "7th",
                                         x == "8.0" ~ "8th",
                                         x == "9.0" ~ "9th",
                                         x == "10.0" ~ "10th",
                                         x == "1" ~ "1st",
                                         x == "2" ~ "2nd",
                                         x == "3" ~ "3rd",
                                         x == "4" ~ "4th",
                                         x == "5" ~ "5th",
                                         x == "6" ~ "6th",
                                         x == "7" ~ "7th",
                                         x == "8" ~ "8th",
                                         x == "9" ~ "9th",
                                         x == "10" ~ "10th")

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

# Yes/No
df_longfile_yes <- df_longfile %>%
  filter(question %in% c('q01a', 'q01b', 'q01c', 'q01d', 'q01e', 'q10a', 'q11a', 'q11b', 'q11c', 'q11e', 'q11d', 'q17a', 'q17b', 'q17c', 'q17d', 'q17e', 'q17f', 'q18a', 'q18b', 'q18c', 'q18d', 'q18e')) %>%
  mutate_at(.vars = vars(text_value), .funs = yesvaluecoding)

# Knowledgeable
df_longfile_knowledgeable <- df_longfile %>%
  filter(question %in% c('q02a', 'q02b', 'q02c', 'q02d', 'q12a', 'q12b', 'q12c', 'q12d', 'q12f')) %>%
  mutate_at(.vars = vars(text_value), .funs = knowledgeablevaluecoding)

# Rank - activated for 2024 survey
df_longfile_rank <- df_longfile %>%
  filter(question %in% c('q04a', 'q04b', 'q04c', 'q04d', 'q04e', 'q04f', 'q04g', 'q04h', 'q04i', 'q04j', 'q13a', 'q13b', 'q13c', 'q13d', 'q13e', 'q13f', 'q13g', 'q13h', 'q13i', 'q13j', 'q19a', 'q19b', 'q19c', 'q19d', 'q19e', 'q19f', 'q19g', 'q19h', 'q19i', 'q19j')) %>%
  mutate_at(.vars = vars(text_value), .funs = rankvaluecoding)

# Level of Education
df_longfile_edlevel <- df_longfile %>%
  filter(question %in% c('q08a', 'q22a')) %>%
  mutate_at(.vars = vars(text_value), .funs = edlevelvaluecoding)

# Multiselect
df_longfile_multiselect <- df_longfile %>%
  filter(question %in% c('q15a', 'q15b', 'q15c', 'q15d', 'q15e', 'q15f')) %>%
  mutate_at(.vars = vars(text_value), .funs = yesvaluecoding)

# Likely
df_longfile_likely <- df_longfile %>%
  filter(question %in% c('q23a', 'q14a', 'q24a')) %>%
  mutate_at(.vars = vars(text_value), .funs = likelyvaluecoding)

# Likely & Advisory/Homeroom
df_longfile_likelyadv <- df_longfile %>%
  filter(question %in% c('q25a')) %>%
  mutate_at(.vars = vars(text_value), .funs = likelyadvvaluecoding)

# Agree
df_longfile_agree <- df_longfile %>%
  filter(question %in% c('q26a', 'q27a', 'q28a')) %>%
  mutate_at(.vars = vars(text_value), .funs = agreevaluecoding)

# Open Ended 
df_longfile_open <- df_longfile %>%
  filter(question %in% c('q08b', 'q22b', 'q21a')) 

df_longfile_final <- union_all(df_longfile_yes, df_longfile_knowledgeable)
df_longfile_final <- union_all(df_longfile_final, df_longfile_rank)
df_longfile_final <- union_all(df_longfile_final, df_longfile_edlevel)
df_longfile_final <- union_all(df_longfile_final, df_longfile_multiselect)
df_longfile_final <- union_all(df_longfile_final, df_longfile_likely)
df_longfile_final <- union_all(df_longfile_final, df_longfile_likelyadv)
df_longfile_final <- union_all(df_longfile_final, df_longfile_agree)
df_longfile_final <- union_all(df_longfile_final, df_longfile_open)

# validate union accuracy
nrow(df_longfile_final)
nrow(df_longfile)

# Write to yearly folder
write_dashboard_file(df_longfile_final, "h2p_student_longfile.csv", output_dir)
write_dashboard_file(df_completed_combined, "h2p_student_analysisfile.csv", output_dir)

# Write to dashboard folder
write_dashboard_file(df_longfile_final, "h2p_student_longfile.csv", dashboard_dir)
write_dashboard_file(df_completed_combined, "h2p_student_analysisfile.csv", dashboard_dir)
