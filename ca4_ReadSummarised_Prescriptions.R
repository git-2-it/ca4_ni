# ca4_ni
# Reads previously aggregated summary data for practices
# a series of files, one per month, for teh practice total items/

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")


datapath <- "data/years"
datapath <- "data/test"


file_out <- paste(datapath, "NI_prescribed_items_ts.csv", sep = "/")
file_out

coltypes <- list(
  col_integer(), col_integer(), col_integer(), col_integer()
)

# Read in the data
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
  
  
  ###########################################
  # Monthly Sum 
  ###########################################
  # Practices
  # Get sum for each year/month
  practice_items_sum_Monthly  <- aggregate(data_in$total_items,
                                           by=list(
                                             data_in$year,
                                             data_in$month
                                           ),
                                           FUN=sum)
  colnames(practice_items_sum_Monthly) <- c("Year", "Month", "Total_Items") 
  
  ##### end summing practices
  
  write.csv(file=file_out, x=practice_items_sum_Monthly, quote=TRUE, row.names = FALSE)


# EOF
  