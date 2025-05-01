# Instructions: Update currentyear parameter and base directory, then run script all the way through
### Notes: It is normal for script to take ~10 min to run. API calls can be viewed on Question Pro

require(readr)
require(httr)
require(jsonlite)
require(dplyr)
library(lubridate)

currentyear <- 2024
# Set base directory to match the folder you want to read and write to

base_dir <- "path/to/your/folder"  # Change this to your local directory to match the parent folder of data/2_output

# Set API Key
key = read_file("keys/QP.txt")

# Iterate survey list to pull the survey IDs for the API calls
df_surveys = read_csv("keys/survey_id_map_student.csv")

# set parameters for api calls
param_apiperpage <- 100

# build out list of surveys

#survey_name_vector <- append(survey_name_vector,df_surveys$survey_name[i])
#survey_id_vector <- append(survey_id_vector,df_surveys$survey_id[i])


# create blank dataframe to store all completion data

df_completion <- data.frame(
  survey_name = character(),
  survey_id = integer(),
  stuID = integer(),
  schoolID = character(),
  grade = character(),
  responseStatus = character(),
  completion_count = integer(),
  timestamp = character()
)

# loop through all surveys in the list
for (i in 1:nrow(df_surveys)) {
  survey_name <- df_surveys$survey_name[i]
  survey_id <- df_surveys$survey_id[i]
  
  # during first pull, identify how many pages there are, and set that as the end condition for the loop
  totalPages <- content(GET(paste("https://api.questionpro.com/a/api/v2/surveys/",survey_id,"/responses?page=1&perPage=",param_apiperpage,sep=""), add_headers(`api-key`=key)))[["pagination"]]$totalPages
  
  # loop through all pages
  for (p in 1:totalPages){
    # pull content from the API call
    page_api <- GET(paste("https://api.questionpro.com/a/api/v2/surveys/",survey_id,"/responses?page=",p,"&perPage=",param_apiperpage,sep=""), 
                    add_headers(`api-key`=key))
    page <- content(page_api)
    
    
    for (item in page[["response"]]){
      
      timestamp_value <- item$timestamp  # Adjust if the actual field is different
      timestamp <- as.character(timestamp_value)  # Ensure it's in character format for consistency
      
      # set grade to unknown if there wasn't a response
      grade_value <- if(length(item[["responseSet"]][[4]][["answerValues"]]) == 0){
        "Unknown"
      }else{
        item[["responseSet"]][[4]][["answerValues"]][[1]]$answerText
      }
      # pull all relevant information for the survey row
      new_row <- data.frame(
        survey_name = survey_name,
        survey_id = survey_id,
        stuID = item$responseID,
        schoolID = item$externalReference,
        grade = grade_value,
        responseStatus = item$responseStatus,
        completion_count = case_when(item$responseStatus == "Completed" ~ 1,
                                     item$responseStatus == "Started" ~ 0,
                                     TRUE ~ 0),
        timestamp = timestamp
      )
      
      # add new row to the completion data frame
      df_completion <- rbind(df_completion, new_row)
      
    } # end loop through all surveys within each page
  } # end loop through all pages from API call
}# end loop through all surveys

# filter to only completed responses
df_completed <- df_completion %>%
  filter(completion_count == 1)

# Remove the time zone part (e.g., " PDT") from the 'timestamp' column
df_completed$timestamp <- gsub(" PDT$", "", df_completed$timestamp)

# Parse the 'timestamp' column into Date-Time format
df_completed$timestamp <- parse_date_time(df_completed$timestamp, orders = "dmy HMS")

# Convert the 'timestamp' column to Date format for comparison (ignore time part)
df_completed$timestamp <- as.Date(df_completed$timestamp)

# Write dashboard file
write_csv(df_completed,
       file = paste(base_dir,"data/2_output/dashboard_files_finalized/school_grade_completed_response.csv",sep=""))

# If desired, write files to preserve history
#write_csv(df_completed,
 #         file = paste(base_dir,"data/2_output/",currentyear,"_finalized/school_grade_completed_response_",Sys.Date(),".csv",sep=""))
