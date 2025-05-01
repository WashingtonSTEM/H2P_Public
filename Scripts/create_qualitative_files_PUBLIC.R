# Set Base Directory then run script all the way through

require(dplyr)
require(writexl)
require(tidyverse)
library(readxl)

#Update this year to match the parent folder of data/2_output
currentyear <- 2024

# Set base directory to match the folder you want to read and write to
base_dir <- "path/to/your/folder"  # Change this to your local directory

df_student <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), 
                                   "Student", "h2p_student_longfile.csv"), 
                         delim = "|", guess_max = 5000)

df_staff <- read_delim(file.path(base_dir, "data", "2_output", paste0(currentyear, "_finalized"), 
                                 "Staff", "h2p_staff_longfile.csv"), 
                       delim = "|", guess_max = 5000)

# Filter on q21a
#select columns (ext_reference,	question,	text_value)
df_q21a <- df_student %>%
  filter(question == "q21a") %>%
  select(ext_reference
         ,	question
         ,	text_value) %>%
  filter(!is.na(text_value))

df_q20a <- df_staff %>%
  filter(question == "q20a") %>%
  select(ext_reference
         ,	question
         ,	text_value) %>%
  filter(!is.na(text_value))

# clean ext_reference
df_q21a$ext_reference <- substr(df_q21a$ext_reference,1,5)
df_q20a$ext_reference <- substr(df_q20a$ext_reference,1,5)


# find unique list of ext_reference values
unique_schID_student <- unique(df_q21a$ext_reference)
unique_schID_staff <- unique(df_q20a$ext_reference)


# loop through all schools, and write a xlsx file for each school sch##.xlsx
for(s in unique_schID_student){
  schID <- s
  df_qual_student <- df_q21a %>%
    filter(ext_reference == schID)
  
  write_xlsx(df_qual_student,paste(base_dir,"data/2_output/",currentyear,"_finalized/qual_responses/Student/",schID,"_student.xlsx",sep=""))
}

for(s in unique_schID_staff){
  schID <- s
  df_qual_staff <- df_q20a %>%
    filter(ext_reference == schID)
  
  write_xlsx(df_qual_staff,paste(base_dir,"/data/2_output/2024_finalized/qual_responses/Staff/",schID,"_staff.xlsx",sep=""))
}
