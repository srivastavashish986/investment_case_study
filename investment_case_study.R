#########################################################################################################################################################################

#Checkpoint1

#########################################################################################################################################################################

#Load the companies and rounds data (provided on the previous page) into two data frames and name them companies and rounds2 respectively.
companies <- read.csv ("companies.txt",sep="\t",stringsAsFactors = FALSE)
rounds2 <- read.csv ("rounds2.csv",stringsAsFactors = FALSE)
#converting keys(permlink) in same case; CAPS
companies$permalink <- toupper(companies$permalink)
rounds2$company_permalink <- toupper(rounds2$company_permalink)
#making permlink name same; company_permlink -> permlink in rounds2
names(rounds2)[1] <- "permalink"
#Table 1.1: Understand the Data Set  : COMMANDS
##1.How many unique companies are present in rounds2?
unique_rounds2 <- length(unique(rounds2$permalink))
unique_companies <- length(unique(companies$permalink))
##2.In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#permlink
##3.Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
diffCtoR2 <- setdiff(companies$permalink,rounds2$permalink)
##Answer:N as diffCtoR2 is 0.
##4. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?
master_frame <- merge(x=rounds2,y=companies,by="permalink")
#unique records in master_frame : 66368
unique_master_frame <- length(master_frame$permalink)

#########################################################################################################################################################################

#Checkpoint 2: Funding Type Analysis

#########################################################################################################################################################################

#Table 2.1: Average Values of Investments for Each of these Funding Types 
##checking NA values in master_frame
sum(is.na(master_frame$raised_amount_usd))
#replacing NA values with numeric 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0
##Calculate the average investment amount for each of the four funding types (venture, angel, seed, and private equity) and report the answers in Table 2.1
fund_type_group <- dplyr::group_by(master_frame,funding_round_type)
summary_fund_group <- dplyr::summarise(fund_type_group,avg_amount_raised=mean(raised_amount_usd, na.rm = T))
##Average funding amount of venture type : 10634054.4 acquired by using: summary_fund_group[which(summary_fund_group$funding_round_type == "venture"),2]
##Average funding amount of angel type : 764564.3
##Average funding amount of seed type : 556606.7
##Average funding amount of private equity type : 62111788.2
##Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
summary_fund_arranged <- dplyr::arrange(summary_fund_group_1,avg_amount_raised)
#####Venture

#########################################################################################################################################################################

#Checkpoint 3: Country Analysis

#########################################################################################################################################################################
#Group by country, and calculating mean(raised_amount_type) for Venture funding type.
country_code_group <- dplyr::group_by(master_frame,country_code)
country_group_venture <- dplyr::filter(country_code_group,funding_round_type=="venture")
top9 <- dplyr::summarise(country_group_venture, total_sum=sum(raised_amount_usd, na.rm = T))
#removing columns with no country code
top9 <- top9[-which(top9$country_code == ""),]
top9 <- head(dplyr::arrange(top9, desc(total_sum)),9)

#Table 3.1: Analysing the Top 3 English-Speaking Countries
##1. Top English-speaking country  : USA [UNITED STATES]
##2. Second English-speaking country : GBR [Great Britain]
##3. Third English-speaking country : IND [INDIA]


#########################################################################################################################################################################

#Checkpoint 4: Sector Analysis 1

#########################################################################################################################################################################

#Import  mapping file
#mapping CSV file is corrected and 0  is replaced by na
mapping_file <- read.csv("mapping.csv", header=T, stringsAsFactors = F,check.names = F)
#converting to lower case
mapping_file$category_list <- tolower(mapping_file$category_list)
master_frame$category_list <- tolower(master_frame$category_list)

#Converting mapping file data frame to  long format
mapping_long <- tidyr::gather(mapping_file, main_sector, nval, 2:10)
mapping_long <- mapping_long[!(mapping_long$nval == 0), ]
mapping_long <- mapping_long[,-3]

#Extracting and adding primary sector to master frame
primary_sector_list <- str_split(master_frame$category_list, pattern="\\|")
primary_sector <- sapply(primary_sector_list, function(x) x[1][1])
master_frame[,"primary_sector"] <- primary_sector

#storing check if master frame sector matches with mapping list vector
master_frame$check = master_frame$primary_sector %in% mapping_long$category_list

#Merging the two  files
mapping_master_merged <- merge(x=master_frame,y=mapping_long,by.x = "primary_sector", by.y = "category_list")

#There are no NAs in main sector column
Nas <- subset(mapping_master_merged,is.na(mapping_master_merged$main_sector))
nrow(Nas)

#########################################################################################################################################################################

#Checkpoint 5: Sector Analysis 2

#########################################################################################################################################################################

#C1: USA
#C2: GBR
#C3: IND

USA <- subset(mapping_master_merged, mapping_master_merged$country_code=="USA" & mapping_master_merged$raised_amount_usd>=5000000 & mapping_master_merged$raised_amount_usd <=15000000)
IND <- subset(mapping_master_merged, mapping_master_merged$country_code=="IND" & mapping_master_merged$raised_amount_usd>=5000000 & mapping_master_merged$raised_amount_usd <=15000000)
GBR <- subset(mapping_master_merged, mapping_master_merged$country_code=="GBR" & mapping_master_merged$raised_amount_usd>=5000000 & mapping_master_merged$raised_amount_usd <=15000000)


USA_investment_count <- nrow(USA)
#13808

GBR_investment_count <- nrow(GBR)
#765

IND_investment_count <- nrow(IND)
#347

#2. Total amount of investment (USD)
sum(USA$raised_amount_usd) #122382876472
sum(GBR$raised_amount_usd) #6562791473
sum(IND$raised_amount_usd) #3103662363

#3. Top sector (based on count of investments)
USA$main_sector <- factor(USA$main_sector)
GBR$main_sector <- factor(GBR$main_sector)
IND$main_sector <- factor(IND$main_sector)

#USA <- Others;
#IND <- Others; 
#GBR <- Others; 

#4. Second-best sector (based on count of investments)
#USA:Social, Finance, Analytics, Advertising
#IND:News, Search and Messaging
#GBR:Social, Finance, Analytics, Advertising 

#5. Third-best sector (based on count of investments)
#USA:  News, Search and Messaging
#IND: Entertainment
#GBR: News, Search and Messaging


USA_main_sector_group <- dplyr::group_by(USA, main_sector)
USA_main_sector_summary <- dplyr::summarise(USA_main_sector_group, frequency = n())

IND_main_sc_group <- dplyr::group_by(IND, main_sector)
IND_main_sector_summary <- dplyr::summarise(IND_main_sc_group, frequency = n())

GBR_main_sec_gropu <- dplyr::group_by(GBR, main_sector)
GBR_main_sector_summary <- dplyr::summarise(GBR_main_sec_gropu, frequency = n())


#6. Number of investments in the top sector (refer to point 3)
##USA-3295, GBR-172, IND-116


#7. Number of investments in the second-best sector (refer to point 4)
##USA-2714,GBR-133,IND-60

#8. Number of investments in the third-best sector (refer to point 5)
##USA-2350,GBR-73,IND-33

#9. For the top sector count-wise (point 3), which company received the highest investment?

company_highest_investment <-  function(p,n){
  topSector <- subset(p,p$main_sector==n)
  topsector_group <- dplyr::group_by(topSector,name)
  company_wise_sum <- dplyr::summarise(topsector_group, sum=sum(raised_amount_usd))
  company_wise_arranged <- dplyr::arrange(company_wise_sum, desc(sum))
  return(head(company_wise_arranged,1))
  
}
topCompany_USA <- company_highest_investment(USA,"Others")
topCompany_IND <- company_highest_investment(IND,"others")
topCompany_GBR <- company_highest_investment(GBR,"others")

##USA-"SST Inc. (Formerly ShotSpotter)", GBR-"Celltick Technologies", IND-"Manthan Systems"

#10. For the second-best sector count-wise (point 4), which company received the highest investment?
topCompany2_USA <- company_highest_investment(USA,"Social, Finance, Analytics, Advertising")
topCompany2_IND <- company_highest_investment(IND,"Social, Finance, Analytics, Advertising")
topCompany2_GBR <- company_highest_investment(GBR,"Cleantech / Semiconductors")

#USA-Biodesix
#IND-Gupshup
#GBR-EUSA Pharma

write.csv(mapping_master_merged,"company_mapping.csv")
