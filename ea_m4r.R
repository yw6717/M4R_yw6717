library(data.table)
library(ggplot2)
library(usmap)
library(maps)
library(fitdistrplus)

infile <- 'data/US_county_to_county_2020-01-30_2021-01-05.rds'
mi <- readRDS(infile)
infile.county.pop <- 'data/covid_county_population_usafacts.csv'
pop <- as.data.table(read.csv(infile.county.pop, stringsAsFactors = FALSE))
setnames(pop, 'ï..countyFIPS', 'fips')
pop2 <- subset(pop, select = c(fips, population))

last_day <- mi[, max(DAY)]
first_day <- mi[, min(DAY)] 

ny.out <- mi[STATE_NAME == 'New York']
ny.in <- mi[NEXT_STATE_NAME == 'New York']

ny.out$FIPS <- as.factor(ny.out$FIPS)
ny.out$NEXT_FIPS <- as.factor(ny.out$NEXT_FIPS)
ny.in$FIPS <- as.factor(ny.in$FIPS)
ny.in$NEXT_FIPS <- as.factor(ny.in$NEXT_FIPS)
pop$fips <- as.factor(pop$fips)

num <- as.numeric(ceiling((last_day-first_day)/7))



heatmap.out.ny.20210601 <- function()
{
  
  ny.out <- as.data.table(aggregate(CNT_TRAVELERS_ADJ ~ FIPS,
                                    data = ny.out, sum))
  ny.out[, log_cnt_travelers_adj := log(CNT_TRAVELERS_ADJ)]
  setnames(ny.out,'FIPS','fips')
  
  p = plot_usmap("counties",include = c("NY"),exclude = c(),
                 data = ny.out, value = "log_cnt_travelers_adj",
                 label = TRUE, label_color = 'black')
  p$layers[[2]]$aes_params$size <- 3 # change the size of label
  p <- p + scale_fill_continuous(low = "white", high ="cornflowerblue", 
                                 name = "number of people 
                                 going out on log scale",
                                 label = scales::comma) +
    theme(legend.position = "top") + 
    labs(title="Distribution of overall outflow in New York State 
         on log scale")
}



heatmap.in.ny.20210601 <- function()
{
  
  ny.in <- as.data.table(aggregate(CNT_TRAVELERS_ADJ ~ FIPS, data = ny.in, sum))
  ny.in[, log_cnt_travelers_adj := log(CNT_TRAVELERS_ADJ)]
  setnames(ny.in,'FIPS','fips')
  
  p = plot_usmap("counties",include = c("NY"),exclude = c(),data = ny.in,
                 value = "log_cnt_travelers_adj",label = TRUE, label_color = 'black')
  p$layers[[2]]$aes_params$size <- 3 # change the size of label
  p <- p + scale_fill_continuous(low = "white", high ="cornflowerblue", 
                                 name = "number of people visiting on log scale",label = scales::comma) +
    theme(legend.position = "top") + 
    labs(title="Distribution of overall inflow in New York State on log scale")
}



heatblock.out.ny.20210601 <- function()
{
  setnames(pop,'fips','FIPS')
  pop <- subset(pop,select=c('FIPS','County.Name'))
  ny.out <- merge(ny.out,pop,by = 'FIPS',all.x=TRUE)
  setnames(ny.out,'County.Name','NAME')
  
  setnames(pop,'FIPS','NEXT_FIPS')
  ny.out <- merge(ny.out,pop,by = 'NEXT_FIPS',all.x = TRUE)
  setnames(ny.out,'County.Name','NEXT_NAME')
  
  ny.out <- as.data.table(aggregate(CNT_TRAVELERS_ADJ ~ NAME + NEXT_NAME,
                                    data = ny.out,sum))
  ny.out[, log_cnt_travelers_adj := log(CNT_TRAVELERS_ADJ)]
  
  p <- ggplot(ny.out,aes(NAME,NEXT_NAME)) + theme_bw() + 
    theme(panel.grid.major = element_blank()) + 
    theme(legend.key = element_blank()) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
    theme(legend.position = "top") + 
    geom_tile(aes(fill=CNT_TRAVELERS_ADJ)) +
    scale_fill_gradient(low = "white", high = "cornflowerblue",
                        name = 'number of people traveling out') + 
    labs(
      title = "Heat map of overall human mobility flows in New York State 
      with the starting county belongs to New York State",
      subtitle = "2020/01/30-2021/01/05",
      x = "starting county",
      y = "destination county"
    )
}



heatblock.in.ny.20210601 <- function()
{
  setnames(pop,'fips','FIPS')
  pop <- subset(pop,select=c('FIPS','County.Name'))
  ny.in <- merge(ny.in,pop,by = 'FIPS',all.x=TRUE)
  setnames(ny.in,'County.Name','NAME')
  
  setnames(pop,'FIPS','NEXT_FIPS')
  ny.in <- merge(ny.in,pop,by = 'NEXT_FIPS',all.x = TRUE)
  setnames(ny.in,'County.Name','NEXT_NAME')
  
  ny.in <- as.data.table(aggregate(CNT_TRAVELERS_ADJ ~ NAME + NEXT_NAME, data = ny.in, sum))
  ny.in[, log_cnt_travelers_adj := log(CNT_TRAVELERS_ADJ)]
  
  p <- ggplot(ny.in,aes(NAME,NEXT_NAME)) + theme_bw() + theme(panel.grid.major = element_blank()) + 
    theme(legend.key = element_blank()) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + 
    theme(legend.position = "top") + geom_tile(aes(fill=CNT_TRAVELERS_ADJ)) +
    scale_fill_gradient(low = "white", high = "cornflowerblue",name = 'number of people visiting') + 
    labs(
      title = "Heat map of overall human mobility flows in New York State 
      with the destination county belongs to New York State",
      subtitle = "2020/01/30-2021/01/05",
      x = "starting county",
      y = "destination county"
    )
}



ny.wk.outflow.rate.20201103 <- function() 
{
  l = data.table()
  for (i in 1:num){
    if (i == num){
      ny.out.wk <- ny.out[DAY <= last_day & 
                            DAY >= first_day + (i-1)*7]
    }
    else{
      ny.out.wk <- ny.out[DAY >= first_day + (i-1)*7 & 
                            DAY <= first_day + i*7]
    }
    
    ny.out.wk[,outflow_rate := CNT_TRAVELERS_ADJ/POP]
    w <- as.data.table(aggregate(outflow_rate ~ FIPS, 
                                 data = ny.out.wk, sum))
    
    w <- cbind(i,w)
    l <- rbind(l,w)
  }
  
  p <- ggplot(l,aes(i,outflow_rate,color = FIPS)) + geom_line() + 
    geom_point() + 
    labs(title = "Comparison of average outflow rate", 
         subtitle = "2020/01/31-2021/01/05",
         x = "week",y = "average outflow rate") 
  
  p1 <- ggplot(l,aes(i,outflow_rate,color = FIPS)) + geom_line() + 
    geom_point() + 
    facet_wrap(~ FIPS, ncol=8) + 
    labs(title = "average outflow rate of each state", 
         subtitle = "2020/01/31-2021/01/05",
         x = "week",y = "average outflow rate") # separate plot
  
  print(p)
  print(p1)
}



fit.dist.ny.out.20201110 <- function()
{
  bo <- subset(ny.out,FIPS == 36047)
  bo <- aggregate(CNT_TRAVELERS_ADJ ~ DAY, data = bo, sum)
  bo1 <- subset(bo,DAY >= '2020-06-05')
  
  normMLE <- fitdist(bo1$CNT_TRAVELERS_ADJ,"norm",method = "mle") 
  plot(normMLE)
  summary(normMLE)
  
  lognormalMLE <- fitdist(log(bo1$CNT_TRAVELERS_ADJ),distr = 'norm',
                          method='mle')
  plot(lognormalMLE)
  summary(lognormalMLE)
  
  gammaMLE <- fitdist(log(bo1$CNT_TRAVELERS_ADJ),"gamma",
                      method = "mle")
  plot(gammaMLE)
  summary(gammaMLE)
}