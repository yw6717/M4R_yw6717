library(data.table)
library(ggplot2)


explore.cellphone.interstate.data.201021 <- function()
  
{
  
  # read mobility data
  
  infile1 <- "data/US_county_to_county_2020_08_16.csv"
  
  infile2 <- "data/US_county_to_county_2020_08_01_2020_10_13.csv"
  
  mi1 <- as.data.table(read.csv(infile1, stringsAsFactors = FALSE))
  
  mi2 <- as.data.table(read.csv(infile2, stringsAsFactors = FALSE))
  
  mi2 <- subset(mi2,day != '2020-07-31')
  
  mi <- unique(rbind(mi1,mi2))
  
  setnames(mi, colnames(mi), toupper(colnames(mi)))
  
  set(mi, NULL, 'DAY', mi[, as.Date(DAY)])
  
  set(mi, NULL, c('NAME','NEXT_NAME'), NULL)
  
  
  
  # investigate how many individuals sampled, and at sampling coverage
  
  si <- unique(subset(mi, select=c(DAY, FIPS, CNT_IDS)))
  
  infile.county.pop <- 'data/covid_county_population_usafacts.csv'
  
  tmp <- as.data.table(read.csv(infile.county.pop, stringsAsFactors = FALSE))
  
  setnames(tmp,'ï..countyFIPS','countyFIPS') ########################
  
  tmp <- subset(tmp, select=c(countyFIPS, population))
  
  setnames(tmp, c('countyFIPS','population'), c('FIPS','POP'))
  
  si <- merge(si, tmp, by='FIPS', all.x=TRUE)
  
  
  
  tmp <- subset(si, is.na(POP))[, unique(FIPS)]
  
  warning('Unknown FIPS:', paste(tmp, collapse=', '))
  
  si <- subset(si, !is.na(POP))
  
  si[, COV:= CNT_IDS/POP]
  
  set(si, NULL, 'CNT_IDS', NULL)
  
  
  
  # adjust travellers by sampling coverage
  
  mi <- merge(mi, si, by=c('DAY','FIPS'))
  
  mi[, CNT_TRAVELERS_ADJ:= CNT_TRAVELERS/COV]
  
  
  
  # count outgoing travellers per day
  
  mi_outtravel <- mi[ FIPS!=NEXT_FIPS, 
                      
                      list(CNT_TRAVELERS_ADJ_OUT= sum(CNT_TRAVELERS_ADJ)), 
                      
                      by=c('STATE_NAME','STATEFP','FIPS','DAY')]
  
  
  
  # plot on county map
  
  last_day <- mi[, max(DAY)]
  
  
  file <- 'data/US_county_to_county_2020-01-30_2020-10-14_outtravel.rds'
  
  saveRDS(mi_outtravel, file=file)
  
}