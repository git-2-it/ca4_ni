# ca4_ni

# Creates summarised data per practice, per month
# Total items
# For each file, crate equivalent summarised version
# Unzips, processe and cleans-up the extracts

# Init required libraries
library("readr")
library("tidyr")
library("dplyr")


datapath <- "data/years"
# datapath <- "data/test"


zip_list <- list.files(path = datapath, pattern = "*.zip", full.names = TRUE)
zip_list

outdir <- paste(datapath, "out", sep = "/")
outdir

unzip_and_summarise <- function(file_name, outdir) {
  # Accept file name, unzip, use data to create summary
  # Write summary to disk and cleanup

  str(file_name)
  
  file_out <- gsub("\\.zip", "", file_name)
  file_out <- gsub("\\(csv\\)", "", file_out)
  file_out <- gsub("\\.csv", "_summary\\.csv", file_out)
  na_out <- gsub("_summary\\.csv", "_na_count\\.csv", file_out)
  
  na_out
  
  # unzip the file
  file_csv <- unzip(file_name, exdir = outdir)  
  
  data_in <- read.csv(file_csv)
  
  colSums(is.na(data_in))
  
  col_names <- colnames(data_in)
  print(col_names)
  
  # grab col names and replace blanks with underscore, some other 
  # cleaning, name normalisation
  col_names <- gsub(" ", "_", col_names)
  col_names <- gsub("-", "_", col_names)
  col_names <- gsub("\\.", "_", col_names)
  col_names <- trimws(col_names)
  col_names <- tolower(col_names) 
  
  print(col_names)
  
  colnames(data_in) <- col_names
  str(data_in)
  
  # Only want Year, Month, Practice and Total_Items
  # build keep list
  keep_list <- sapply(col_names, function(x) FALSE)
  
  # store summary of the data
  na_count <- summary(data_in)
  write.csv(file=na_out, x=na_count, quote=TRUE, row.names = FALSE)
  
  keep_list[c("year", "month", "practice", "total_items")] <- TRUE
  keep_list
  
  data_in  <- data_in[, keep_list]
  data_in
  
  str(data_in)
  data_in  <- data_in[complete.cases(data_in), ]
  str(data_in)
  
  
  ###########################################
  # Monthly Sum for Practices
  ###########################################
  # Practices
  # Get sum for each, by year/month
  practice_items_sum_Monthly  <- aggregate(data_in$total_items,
                                           by=list(
                                             data_in$year,
                                             data_in$month,
                                             data_in$practice
                                           ),
                                           FUN=sum)
  colnames(practice_items_sum_Monthly) <- c("Year", "Month", "Practice", "Total_Items") 
  
  ##### end summing practices
  
  print(file_out)
  
  write.csv(file=file_out, x=practice_items_sum_Monthly, quote=TRUE, row.names = FALSE)

  if (file.exists(file_csv)) 
     #Delete file if it exists
     file.remove(file_csv)

} # end of function


# run the function acorss the zip file list
file_in <- ldply(.data = zip_list, .fun = unzip_and_summarise, outdir = outdir, .inform = TRUE)

