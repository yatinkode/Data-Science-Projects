
######################## Start of Part 1 #########################################

# Checkpoints - Part 1

######################## Start of Checkpoint 1:  Data Cleaning 1 #######################

# Reading Company details and funding round details into dataframe

companies <- read.delim("companies.txt", sep = "\t", stringsAsFactors = FALSE, 
                                fill = TRUE, quote = "")
rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

# for companies.txt, was getting error "eof within quoted string", 
# may be because of entries such as "1,2,3 Listo", hence disabling quote (changed default value)
# Since we are using RStudio, we are not using summary(), head() to understand data

str(companies) # for type of columns in companies df
str(rounds2) # for type of columns in rounds2 df

# All, but one, columns in two dataframes are chr type. Only rounds2$raised_amound_usd is num
# Based on the requirement at later stage, we may convert few chr columns to factor type. 

# Table 1.1 Understand the Data set

# Calculating non-empty cells column-wise
rounds2_nonempty <- sapply(rounds2, function(x) sum(x != ""))
companies_nonempty <- sapply(companies, function(x) sum(x != ""))

rounds2_nonempty
companies_nonempty

# temp_companies <- companies[duplicated(companies$name),]
# temp_companies <- companies[companies$name == 'Agora',]

# Calculating unique companies: Using permalink in companies df to calculate unique companies
# name column not unique. For example, there are two companies with name Agora
rounds2$company_permalink <- tolower(rounds2$company_permalink)
companies$permalink <- tolower(companies$permalink)

rounds2_unique_companies <- length(unique(rounds2$company_permalink))
companies_unique_companies <- length(unique(companies$permalink))

# There are 66,368 unique companies in both rounds2 df and companies df
# and 66,368 unique companies in companies df

# Using permalink as the primary key in companies df
# merging two dataframes

master_frame <- merge(rounds2, companies, 
                      by.x = c("company_permalink"), by.y = c("permalink"),
                      all.x = TRUE)

######################## End of Checkpoint 1:  Data Cleaning 1 #######################

######################## Start of Checkpoint 2:  Funding Type Analysis #######################

unique(master_frame$funding_round_type)
# There are 13 unique values in funding_round_type, our interest is only four
# Subsetting base on venture, seed, angel and private_equity

funding_type_desired <- subset(master_frame,
                                   master_frame$funding_round_type %in% c('venture','seed','angel','private_equity'))

# Drop all rows where raised_amount_usd not available
funding_type_desired <- subset(funding_type_desired, funding_type_desired$raised_amount_usd != "")

# from dplyr library, group_by followed by summarize on mean will give us average investment type
library(dplyr)
funding_type_desired_group <- group_by(funding_type_desired, funding_round_type)
average_funding_mean <- summarise(funding_type_desired_group, mean(raised_amount_usd))
average_funding_mean

# Since spark funds is looking for 5mn to 15mn investment
suitable_funding_type <- subset(average_funding_mean,
                                average_funding_mean$`mean(raised_amount_usd)` >= 5000000 
                                & average_funding_mean$`mean(raised_amount_usd)`<= 15000000)
# Since there is only one row, we are not arranging the tibble
suitable_funding_type

# Venture type is most suitable for Sparks Fund since it has average funding of USD 11.75 Mn

######################## End of Checkpoint 2:  Funding Type Analysis #######################


######################## Start of Part 2 #########################################

# Checkpoints - Part 2

######################## Start of Checkpoint 3:  Country Analysis #######################

# Subsetting the dataframe with only venture type funding and removing rows with no country code

country_analysis_data <- subset(funding_type_desired, 
                                funding_type_desired$funding_round_type == toString(suitable_funding_type[1,1]) 
                                & funding_type_desired$country_code != "")

# Grouping and summing up by country code
country_preference <- group_by(country_analysis_data, country_code)
aggregrate_byCountry <- summarise(country_preference, sum(raised_amount_usd))

# Sorting by sum of investment
aggregrate_byCountry_sorted <- aggregrate_byCountry[order(aggregrate_byCountry$`sum(raised_amount_usd)`
                                                   ,decreasing = TRUE),]

top9 <- aggregrate_byCountry_sorted[1:9,]
top9

######################## End of Checkpoint 3:  Country Analysis #######################

######################## Start of Checkpoint 4: Sector Analysis 1 #######################

# Importing the mapping file
mapping <- read.csv("mapping.csv", stringsAsFactors = FALSE)
# We see that the mapping dataframe is currenlty in wide format
# This needs to be converted to long format for merging
# We will be usig tidyr library for the same
library('tidyr')

mapping_long <- gather(mapping, key = 'Sector', value = 'Value',
                       Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping_long <- subset(mapping_long, mapping_long$Value == 1)
mapping_long <- select(mapping_long, category_list, Sector)

# Cleaning the mapping_long dataframe replacing 0 by na
library(stringr)
zero_to_na <- function(x){
  str_replace(x, '0', 'na')
}
mapping_long$category_list <- sapply(mapping_long$category_list, FUN = zero_to_na)

# For primary sector, we are only considering the dataframe country_analysis_data that has
# only Venture type funding and non-empty country code

# Creating different dataset for sector analysis
sector_analysis_master <- country_analysis_data

# Function to extract primary sector as per the business rule defined in document
primary_sector <- function(x) {
  if (is.na(str_locate(x ,fixed("|")))[1]) {
    y <- x
  }
  else {
    y <- substr(x, 1, str_locate(x ,fixed("|"))-1 )
  }
    return (y)
}

# Applying primary_sector function to category_list column
sector_analysis_master$category_list <- sapply(sector_analysis_master$category_list, FUN = primary_sector)

# Removing rows with no defined primary sector
sector_analysis_master <- subset(sector_analysis_master,
                                 sector_analysis_master$category_list != "")
# Out of 48,111 observations, we are now left with  47,809 observations

# Mapping primary sectors with main sectors
sector_analysis_master <- merge(sector_analysis_master, mapping_long,
                                by.x = c("category_list"), by.y = c("category_list"))

######################## End of Checkpoint 4: Sector Analysis 1 #######################

######################## Start of Checkpoint 5: Sector Analysis 2 #######################

# Creating the data frame with investment range of 5-15 million USD
# Using sector_analytis_master that only contains investment of type venture

sector_analysis_cp5 <- subset(sector_analysis_master, 
                              sector_analysis_master$raised_amount_usd >= 5000000 & 
                                sector_analysis_master$raised_amount_usd <= 15000000) 

# Top three English speaking countries: USA, GBR and IND
D1_USA <- subset(sector_analysis_cp5, sector_analysis_cp5$country_code == 'USA')
D2_GBR <- subset(sector_analysis_cp5, sector_analysis_cp5$country_code == 'GBR')
D3_IND <- subset(sector_analysis_cp5, sector_analysis_cp5$country_code == 'IND')

# total number of investment in USA, GBR and IND
count(D1_USA)
count(D2_GBR)
count(D3_IND)

# Total amount invested in USA, GBR and IND in Mn USD
sum(D1_USA$raised_amount_usd)
sum(D2_GBR$raised_amount_usd)
sum(D3_IND$raised_amount_usd)

# Top sectors in USA based on count of investment
D1_USA_sector <- group_by(D1_USA, Sector)
D1_USA_sector_num <- summarize(D1_USA_sector, n())
D1_USA_sector_num <- D1_USA_sector_num[order(D1_USA_sector_num$`n()`
                                                 ,decreasing = TRUE),]
D1_USA_sector_value <- summarize(D1_USA_sector, sum(raised_amount_usd))

# Creating D1 for USA with count of investments and amount invested as added columns
D1 <- merge(D1_USA_sector, D1_USA_sector_num, by.x = c('Sector'), by.y = c('Sector'))
D1 <- merge(D1, D1_USA_sector_value, by.x = c('Sector'), by.y = c('Sector'))

# Top sector for USA based on number of investments
top_sector_USA <- D1_USA_sector_num [1,]
top_sector_USA

# Second best sector for USA and number of investments
secondBest_sector_USA <- D1_USA_sector_num [2,]
secondBest_sector_USA

# Third best sector for USA and number of investments
thirdBest_sector_USA <- D1_USA_sector_num [3,]
thirdBest_sector_USA

# company receiving highest investment for top sector in USA
D1_USA_company_topSector <- subset(D1_USA, D1_USA$Sector == toString(D1_USA_sector_num[1,1]))
D1_USA_company_topSector <- group_by(D1_USA_company_topSector, D1_USA_company_topSector$name)
D1_USA_company_topSector <- summarize(D1_USA_company_topSector, sum(raised_amount_usd))
D1_USA_company_topSector <- D1_USA_company_topSector[order(D1_USA_company_topSector$`sum(raised_amount_usd)`
                                             ,decreasing = TRUE),]
D1_USA_company_topSector <- D1_USA_company_topSector [1,1]
D1_USA_company_topSector

# company receiving highest investment for Second best sector in USA
D1_USA_company_2ndSector <- subset(D1_USA, D1_USA$Sector == toString(D1_USA_sector_num[2,1]))
D1_USA_company_2ndSector <- group_by(D1_USA_company_2ndSector, D1_USA_company_2ndSector$name)
D1_USA_company_2ndSector <- summarize(D1_USA_company_2ndSector, sum(raised_amount_usd))
D1_USA_company_2ndSector <- D1_USA_company_2ndSector[order(D1_USA_company_2ndSector$`sum(raised_amount_usd)`
                                                           ,decreasing = TRUE),]
D1_USA_company_2ndSector <- D1_USA_company_2ndSector [1,1]
D1_USA_company_2ndSector

# Top sectors in GBR based on number of investments
D2_GBR_sector <- group_by(D2_GBR, Sector)
D2_GBR_sector_num <- summarize(D2_GBR_sector, n())
D2_GBR_sector_num <- D2_GBR_sector_num[order(D2_GBR_sector_num$`n()`
                                             ,decreasing = TRUE),]
D2_GBR_sector_value <- summarize(D2_GBR_sector, sum(raised_amount_usd)) 

# Creating D2 for GBR with count of investments and amount invested as added columns
D2 <- merge(D2_GBR_sector, D2_GBR_sector_num, by.x = c('Sector'), by.y = c('Sector'))
D2 <- merge(D2, D2_GBR_sector_value, by.x = c('Sector'), by.y = c('Sector'))

# Top sector for GBR based on number of investments
top_sector_GBR <- D2_GBR_sector_num [1,]
top_sector_GBR

# Second best sector for GBR and number of investments
secondBest_sector_GBR <- D2_GBR_sector_num [2,]
secondBest_sector_GBR

# Third best sector for GBR and number of investments
thirdBest_sector_GBR <- D2_GBR_sector_num [3,]
thirdBest_sector_GBR

# company receiving highest investment for top sector in GBR
D2_GBR_company_topSector <- subset(D2_GBR, D2_GBR$Sector == toString(D2_GBR_sector_num[1,1]))
D2_GBR_company_topSector <- group_by(D2_GBR_company_topSector, D2_GBR_company_topSector$name)
D2_GBR_company_topSector <- summarize(D2_GBR_company_topSector, sum(raised_amount_usd))
D2_GBR_company_topSector <- D2_GBR_company_topSector[order(D2_GBR_company_topSector$`sum(raised_amount_usd)`
                                                           ,decreasing = TRUE),]
D2_GBR_company_topSector <- D2_GBR_company_topSector [1,1]
D2_GBR_company_topSector

# company receiving highest investment for Second best sector in GBR
D2_GBR_company_2ndSector <- subset(D2_GBR, D2_GBR$Sector == toString(D2_GBR_sector_num[2,1]))
D2_GBR_company_2ndSector <- group_by(D2_GBR_company_2ndSector, D2_GBR_company_2ndSector$name)
D2_GBR_company_2ndSector <- summarize(D2_GBR_company_2ndSector, sum(raised_amount_usd))
D2_GBR_company_2ndSector <- D2_GBR_company_2ndSector[order(D2_GBR_company_2ndSector$`sum(raised_amount_usd)`
                                                           ,decreasing = TRUE),]
D2_GBR_company_2ndSector <- D2_GBR_company_2ndSector [1,1]
D2_GBR_company_2ndSector


# Top sectors in IND based on number of investments
D3_IND_sector <- group_by(D3_IND, Sector)
D3_IND_sector_num <- summarize(D3_IND_sector, n())
D3_IND_sector_num <- D3_IND_sector_num[order(D3_IND_sector_num$`n()`
                                             ,decreasing = TRUE),]
D3_IND_sector_value <- summarize(D3_IND_sector, sum(raised_amount_usd)) 

# Creating D3 for IND with count of investments and amount invested as added columns
D3 <- merge(D3_IND_sector, D3_IND_sector_num, by.x = c('Sector'), by.y = c('Sector'))
D3 <- merge(D3, D3_IND_sector_value, by.x = c('Sector'), by.y = c('Sector'))

# Top sector for IND based on number of investments
top_sector_IND <- D3_IND_sector_num [1,]
top_sector_IND

# Second best sector for IND and number of investments
secondBest_sector_IND <- D3_IND_sector_num [2,]
secondBest_sector_IND

# Third best sector for IND and number of investments
thirdBest_sector_IND <- D3_IND_sector_num [3,]
thirdBest_sector_IND

# company receiving highest investment for top sector in IND
D3_IND_company_topSector <- subset(D3_IND, D3_IND$Sector == toString(D3_IND_sector_num[1,1]))
D3_IND_company_topSector <- group_by(D3_IND_company_topSector, D3_IND_company_topSector$name)
D3_IND_company_topSector <- summarize(D3_IND_company_topSector, sum(raised_amount_usd))
D3_IND_company_topSector <- D3_IND_company_topSector[order(D3_IND_company_topSector$`sum(raised_amount_usd)`
                                                           ,decreasing = TRUE),]
D3_IND_company_topSector <- D3_IND_company_topSector [1,1]
D3_IND_company_topSector

# company receiving highest investment for Second best sector in IND
D3_IND_company_2ndSector <- subset(D3_IND, D3_IND$Sector == toString(D3_IND_sector_num[2,1]))
D3_IND_company_2ndSector <- group_by(D3_IND_company_2ndSector, D3_IND_company_2ndSector$name)
D3_IND_company_2ndSector <- summarize(D3_IND_company_2ndSector, sum(raised_amount_usd))
D3_IND_company_2ndSector <- D3_IND_company_2ndSector[order(D3_IND_company_2ndSector$`sum(raised_amount_usd)`
                                                           ,decreasing = TRUE),]
D3_IND_company_2ndSector <- D3_IND_company_2ndSector [1,1]
D3_IND_company_2ndSector

######################## End of Checkpoint 5: Sector Analysis 2 #######################

################# Start of Checkpoint 6 in R: Exporting data for Tableau ###################

# Exporting dataframes average_funding_mean and top9
# getwd()
write.csv(average_funding_mean, "/Users/Sameer/Documents/OneDrive/IIIT B/Course 1. Introduction to Data management/Group Project 1/Submissions/Tableau charts/average_funding.csv")
write.csv(top9, "/Users/Sameer/Documents/OneDrive/IIIT B/Course 1. Introduction to Data management/Group Project 1/Submissions/Tableau charts/top9.csv")

# Praparing dataframes for top 3 sectors in top 3 countries for import
D1_USA_sector_num_country <- as.data.frame(D1_USA_sector_num[1:3,])
USA <- rep('USA', 3)
D1_USA_sector_num_country$country <- USA
D2_GBR_sector_num_country <- as.data.frame(D2_GBR_sector_num[1:3,])
GBR <- rep('GBR', 3)
D2_GBR_sector_num_country$country <- GBR
D3_IND_sector_num_country <- as.data.frame(D3_IND_sector_num[1:3,])
IND <- rep('IND', 3)
D3_IND_sector_num_country$country <- IND

country_sector <- rbind(D1_USA_sector_num_country, D2_GBR_sector_num_country, D3_IND_sector_num_country)
write.csv(country_sector, "/Users/Sameer/Documents/OneDrive/IIIT B/Course 1. Introduction to Data management/Group Project 1/Submissions/Tableau charts/country_sector.csv")
# write.csv(master_frame, "/Users/Sameer/Documents/OneDrive/IIIT B/Course 1. Introduction to Data management/Group Project 1/Submissions/Tableau charts/master_frame.csv")

################# End of Checkpoint 6 in R: Exporting data for Tableau ###################
