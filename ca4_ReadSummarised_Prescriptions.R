# ca4_ni
# Reads previously aggregated summary data for practices
# a series of files, one per month, for the practice total items/

# Previously, practices were the core dataset
# Core dataset moves to time based data

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")


datapath <- "data/years"
# datapath <- "data/test"


file_out <- paste(datapath, "NI_prescribed_items_ts.csv", sep = "/")
file_out

coltypes <- list(
  col_integer(), col_integer(), col_integer(), col_integer()
)

#############################
# Read in the data
#############################

data_in <- list.files(path=datapath, pattern = "*summary.csv", full.names=TRUE, recursive = TRUE) %>% 
  lapply(read_csv, col_types = coltypes) %>%   bind_rows

colSums(is.na(data_in))

col_names <- colnames(data_in)
print(col_names)

# grab col names and replace blanks with underscore, some other 
# cleaning, name normalisation
col_names <- trimws(col_names)
col_names <- tolower(col_names) 

print(col_names)

colnames(data_in) <- col_names

# Only want Year, Month, Practice and Total_Items
# build keep list
keep_list <- sapply(col_names, function(x) FALSE)

keep_list[c("year", "month", "practice", "total_items")] <- TRUE
keep_list

data_in  <- data_in[, keep_list]
data_in

str(data_in)
data_in  <- data_in[complete.cases(data_in), ]
str(data_in)

# Align with "standard" names
colnames(data_in) <- c("Year", "Month", "Practice", "Total_Items")

#############################
# Exclusions
#############################

# The same list of practices as previously will be excluded
# This is to align with the previous piece of ca work which was based on two years of
# data whereas here its seven. The removal of those previously outlying practices 
# while may not be valid in light of the additonal years of data does on the other
# hand give better data for time series analysis. Therefore its assumed the same outliers 
# would exist though there could be others. In reality, the previous analysis would be redone
# over 7 years and the results incorporated, but thats not the requirement.

# Uni Health centre - remove
data_in <- subset(data_in, Practice != 157)

old_pr_main <- read.csv("data/temp_pr_main_data.csv")

str(old_pr_main)

data_in %>% group_by(Year) %>% tally()

chk <- data_in[!data_in$Practice %in% old_pr_main$Practice, ]

bad_pr <- unique(chk$Practice)
bad_pr

data_in <- data_in[data_in$Practice %in% old_pr_main$Practice, ]

str(data_in)

#############################


# ###########################################
# # Monthly Sum 
# ###########################################
# # Practices
# # Get sum for each year/month
# practice_items_sum_Monthly  <- aggregate(data_in$total_items,
#                                          by=list(
#                                            data_in$year,
#                                            data_in$month
#                                          ),
#                                          FUN=sum)
# colnames(practice_items_sum_Monthly) <- c("Year", "Month", "Total_Items") 
# 
# ##### end summing practices
# 
# write.csv(file=file_out, x=practice_items_sum_Monthly, quote=TRUE, row.names = FALSE)


###########################################
# Practice data
###########################################

docs_in <- read.csv("data/cleaned_practice_data.csv")
str(docs_in)

head(docs_in)

colSums(is.na(docs_in))

doc_data <- select(docs_in,  "PracNo", "Registered_Patients", "C_Group", "County" )


library(stringr)


doc_data$Registered_Patients <- as.character(doc_data$Registered_Patients)
colSums(is.na(doc_data))
doc_data$Registered_Patients <- str_replace_all(doc_data$Registered_Patients, "[[:punct:]]", "")
colSums(is.na(doc_data))
doc_data$Registered_Patients <- as.integer(doc_data$Registered_Patients)

# NA checking to be reinserted
chk <- doc_data[!complete.cases(doc_data), ]
chk

str(doc_data)

colnames(doc_data) <- c("Practice", 
                       "Registered_Patients", "C_Group", "County"
                       )


data_in <- left_join(data_in, doc_data, 
              by = c("Practice" = "Practice") )

str(data_in)

# data_in$Registered_Patients <- as.integer(data_in$Registered_Patients)
# data_in$Registered_Patients <- as.integer(data_in$Registered_Patients)

data_in$item_patient_ratio <- data_in$Total_Items / data_in$Registered_Patients

str(data_in)

col_names <- colnames(data_in)
col_names

# colnames(data_in) <- c("Year", "Month", "Practice", "Total_Items",
#                        "Registered_Patients", "C_Group", "County",  
#                        "item_patient_ratio")


chk

###########################################
# Monthly Sum
###########################################
# Practices
# Get sum for each year/month
practice_items_sum_Monthly  <- aggregate(data_in$Total_Items,
                                         by=list(
                                           data_in$Year,
                                           data_in$Month
                                         ),
                                         FUN=sum)
colnames(practice_items_sum_Monthly) <- c("Year", "Month", "Total_Items")

practice_patients_sum_Monthly  <- aggregate(data_in$Registered_Patients,
                                         by=list(
                                           data_in$Year,
                                           data_in$Month
                                         ),
                                         FUN=sum)
colnames(practice_patients_sum_Monthly) <- c("Year", "Month", "Total_Patients")

practice_items_sum_Monthly <- left_join(practice_items_sum_Monthly, practice_patients_sum_Monthly )
practice_items_sum_Monthly

practice_items_sum_Monthly$item_patient_ratio <- practice_items_sum_Monthly$Total_Items / practice_items_sum_Monthly$Total_Patients

##### end summing practices
 
file_out
write.csv(file=file_out, x=practice_items_sum_Monthly, quote=TRUE, row.names = FALSE)


# EOF
  