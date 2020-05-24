# ca4_ni

# Several datasets required apart from the prescription data itself
# Practice data
# Post code data (for the practices geo-location)

library(tidyverse)

# --------------------------------------------------
# Get practise data
# --------------------------------------------------

docs_in20 <- read.csv("data/gp-practice-reference-file--january-2020.csv")
docs_in20$year = "2020"
col_names <- colnames(docs_in20)

docs_in19 <- read.csv("data/gp-practice-reference-file---january-2019.csv")
docs_in19$year = "2019"
colnames(docs_in19) <- col_names

docs_in18 <- read.csv("data/gp-practice-reference-file---january-2018.csv")
docs_in18$year = "2018"
colnames(docs_in18) <- col_names

docs_in17 <- read.csv("data/gp-practice-reference-file---january-2017.csv")
docs_in17$year = "2017"
colnames(docs_in17) <- col_names

docs_in16 <- read.csv("data/gp-practice-reference-file---january-2016.csv")
docs_in16$year = "2016"
colnames(docs_in16) <- col_names

# July 2015 the oldest data
docs_in15 <- read.csv("data/gp-practice-reference-file---july-2015.csv")
docs_in15$year = "2015"
colnames(docs_in15) <- col_names

col_names

# bind order important to keep the year
docs_in <- rbind(docs_in20, docs_in19, docs_in18, docs_in17, docs_in16, docs_in15)
str(docs_in)

docs_in$PracticeName[docs_in$PracticeName == "NULL"] <- NA

colSums(is.na(docs_in))
chk <- docs_in[!complete.cases(docs_in), ]

# too many bad records to just let pass?
# deduplicate, to see what remains after
chk <- subset(docs_in, !duplicated(docs_in$PracNo))
colSums(is.na(chk))

chk$PracNo <- as.factor(chk$PracNo)
str(chk)
docs_in$PracNo <- as.factor(docs_in$PracNo)
str(docs_in)

# the deduplication clears out the bad data
# here and the checked NA should be removed.

docs_in <- docs_in[complete.cases(docs_in), ]
colSums(is.na(docs_in))
str(docs_in)

docs_in <- subset(docs_in, !duplicated(docs_in$PracNo))
str(docs_in)

docs_in$year <- as.factor(docs_in$year)
str(docs_in)

head(docs_in)

# Examine structure of the file
str(docs_in)

# Required fields - 
# PracNo 
# Name
# Address 1 for identification
# Postcode
# LCG (Local Commissioning Group) - group responsible for addressing health and social care needs
# Registered Patients

docs_in <- docs_in[, -c(4,5)]

docs_in <- droplevels(docs_in)

# Want to join this with postcode data to have town identifier

# --------------------------------------------------
# Read Postcode data
# --------------------------------------------------
postcodes_in <- read.csv("data/CleanNIPostcodeData.csv", header=TRUE, sep=",")
str(postcodes_in)

# Don't need all the post code detail, only higher level info. 
# Retain some columns - Postcode, country, town - not x/y coords
# Note, though co-orinate info may not be required later depending on 
# the location system to be used it will be retained, as a low cost operation
postcodes_clean <- postcodes_in[,c("Postcode","Town","County")] # , "x.coordinates", "y.coordinates")]
str(postcodes_clean)

# Remove blank Towns (a small proportion of the data)
# This to minimise the number of postcode references with a blank town value
postcodes_clean$Town <- as.character(postcodes_clean$Town)
postcodes_clean$Town[postcodes_clean$Town == ""] <- NA

postcodes_clean <- postcodes_clean[complete.cases(postcodes_clean),]
str(postcodes_clean)

# Then de-duplicate the remaining data by postcode
postcodes_clean <- postcodes_clean[!duplicated(postcodes_clean$Postcode), ] 
str(postcodes_clean)

# This has reduced post code list from almost a million rows down 
# to approx 47k without reducing the level of useful data

# Deal with missing data
# NOTE: There should be a better way of doing this than manually, API sources do not
# easily yield teh post town name though with the small amount of data, its not 
# onerous and its quicker to manually search.
# The  x/y co-ordinates are not important at this time, if
# they are required later, then will retrieve that from an API reference source
# Create fake data fame of missing data to be appended to the postcode info.
temp_lookup <- data.frame(
  c("BT41NS" , "BT308RD", "BT294LN", "BT414BS", 
    "BT388TP","BT399HL", "BT670LQ", "BT670DD", "BT670QA"),
  c("BELFAST", "DOWNPATRICK", "CRUMLIN", "ANTRIM",
    "GREENISLAND", "BALLYCLARE", "MOIRA", "AGHALEE", "MOIRA"),
  c("DOWN", "DOWN", "ANTRIM", "ANTRIM",
    "ANTRIM", "ANTRIM", "DOWN", "ANTRIM", "DOWN")
)

colnames(temp_lookup) <- colnames(postcodes_clean)

postcodes_clean <- bind_rows(postcodes_clean, temp_lookup)

# Noting the format difference between the 2 datasets, practices data includes a 
# space in the postcode
# Normalise this to same format as postcodes dataset, ie; remove the space.
docs_in$Postcode <-   gsub('\\s+', '',docs_in$Postcode)

# docs_in$Postcode <- as.factor(docs_in$Postcode)
docs_in$Postcode <- as.character(docs_in$Postcode)

docs_in <- left_join(docs_in, postcodes_clean, 
                     by = c("Postcode" = "Postcode")
)
docs_in[,"Town"] = toupper(docs_in[,"Town"])

str(docs_in)

# The postcode database has found all practice address?
# Id the missing values

docs_in$Town <- as.character(docs_in$Town)
docs_in$Town[docs_in$Town == ""] <- NA

# junk <- docs_in[!complete.cases(docs_in),]
colSums(is.na(docs_in))

# --------------------------------------------------
# Save clean practice dataset to file
docs_in <- subset(docs_in, !duplicated(docs_in$PracNo))

write.csv(file="data/cleaned_practice_data.csv", x=docs_in, quote=TRUE, row.names = FALSE)

# Group the practices
# Grouping based on county bundaries
# Previous caveats still apply
docs_in$C_Group <- "g2"
docs_in$C_Group[docs_in$County == "ANTRIM"] <- "g1"
docs_in$C_Group[docs_in$County == "LONDONDERRY"] <- "g1"
docs_in$C_Group[docs_in$County == "DOWN"] <- "g1"

write.csv(file="data/cleaned_practice_data.csv", x=docs_in, quote=TRUE, row.names = FALSE)
