## Standard section names for IAP documents

IAP_SECTION_NAMES <- c('incident' = "INCIDENT NAME",
                       'situation' = "SITUATION",
                       'mission' = "MISSION",
                       'execution' = "EXECUTION",
                       'administration' = "ADMINISTRATION",
                       'command' = "COMMAND AND COMMUNICATIONS",
                       'safety' = "SAFETY [Aa][Nn][Dd] HAZARDS",
                       'aviation' = "AVIATION")

usethis::use_data(IAP_SECTION_NAMES, overwrite = TRUE)

rm(IAP_SECTION_NAMES)
