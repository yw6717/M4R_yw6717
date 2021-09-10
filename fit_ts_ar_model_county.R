

mobility.ts.exploration.210113 <- function()
{
	require(data.table)
	require(ggplot2)
	require(ggsci)
	
	infile <- 'data/US_county_to_county_2020-01-30_2021-01-05_outtravel.rds'
	outdir <- 'mobility_forecasts'
	
	mi <- readRDS(infile)
	mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	mi[, WDAY2 := strftime(DAY,'%A')]
	mi[, WDAY3 := strftime(DAY,'%u')]
	mi[, WEEK := as.integer(strftime(DAY,'%V'))]
	tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
	setkey(tmp, WDAY3)
	set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
	mi[, WDAY2:=NULL]
	mi <- merge(mi, tmp, by='WDAY3')
	mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
	set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
	setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
	
	#	select NYC 
	fips.select <- c('36005', '36047', '36061', '36081', '36085')
	mis <- subset(mi, FIPS%in%fips.select)
	
	p <- ggplot(mis, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
		geom_point() +
		scale_x_date(breaks='2 weeks') +
		ggsci::scale_color_lancet() +
		theme_bw() +
		facet_wrap(~FIPS, ncol=2, scales='free_y') +
		labs(x='', y='expected number travel journeys from county', colour='', 
		     name='Mobility outflows stratified by week days and weekends from counties in New YorkCity') +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir , 'fc_outofcounty_201014_nyc.pdf'), p, w=10, h=10)

	# group 1
	fips.select3 <- c('36001', '36003', '36007', '36009', '36011','36013','36015',
	                  '36017','36019','36021','36023','36025','36027','36029','36031',
	                  '36033','36035','36037','36039','36041')
	mis3 <- subset(mi, FIPS%in%fips.select3)
	p <- ggplot(mis3, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	ggsave(file=file.path(outdir, 'fc_outofcounty_210105_nysgrp1.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	# group 2
	fips.select4 <- c('36043', '36045', '36049', '36051', '36053','36055','36057',
	                  '36059','36063','36065','36067','36069','36071','36073','36075',
	                  '36077','36079','36083','36087','36089')
	mis4 <- subset(mi, FIPS%in%fips.select4)
	p <- ggplot(mis4, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	ggsave(file=file.path(outdir, 'fc_outofcounty_210105_nysgrp2.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	# group 3
	fips.select5 <- c('36091', '36093', '36095', '36097', '36099','36101','36103',
	                  '36105','36107','36109','36111','36113','36115','36117','36119',
	                  '36121','36123')
	mis5 <- subset(mi, FIPS%in%fips.select5)
	p <- ggplot(mis5, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	ggsave(file=file.path(outdir, 'fc_outofcounty_210105_nysgrp3.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	#	select NY State and Connecticut
	fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
	mis2 <- subset(mi, FIPS%in%fips.select2)	
	p <- ggplot(mis2, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY_LABEL)) +
		geom_point() +
		scale_x_date(breaks='2 weeks') +
		ggsci::scale_color_lancet() +
		theme_bw() +
		facet_wrap(~FIPS, ncol=4, scales='free_y') +
		labs(x='', y='expected number travel journeys from county', colour='') +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	p <- ggplot(mis2, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
			geom_point() +
			scale_x_date(breaks='2 weeks') +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_wday.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	p <- ggplot(mis3, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	
	p <- ggplot(mis4, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	
	p <- ggplot(mis5, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	
	p <- ggplot(mis, aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2)) +
	  geom_point() +
	  scale_x_date(breaks='2 weeks') +
	  ggsci::scale_color_lancet() +
	  theme_bw() +
	  facet_wrap(~FIPS, ncol=4, scales='free_y') +
	  labs(x='', y='expected number travel journeys from county', colour='') +
	  theme(
	    legend.position = 'bottom', 
	    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
	  )
	
	p <- ggplot(mis2, aes(x=DAY, y=log(CNT_TRAVELERS_ADJ_OUT), group=FIPS, colour=WDAY2)) +
			geom_point() +
			scale_x_date(breaks='2 weeks') +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	ggsave(file=file.path(outdir, 'fc_outofcounty_log_201014_nysct_wday.pdf'), p, w=12, h=50, limitsize = FALSE)
	
	#	first observations:
	#	out travel follows the same overall trend across counties, 
	#	but overall magnitude differs
	#	there is nothing particular about weekends
	#	most counties do not have substantial weekday effects, but there may be some smaller ones

	
	#	ok
	#	let me just look at weekdays, and standardise to the same number of out travels at the start	
	mis2b <- subset(mis2, DAY>='2020-06-05')	
	tmp <- subset(mis2b, DAY=='2020-06-05', select=c(FIPS, CNT_TRAVELERS_ADJ_OUT))
	setnames(tmp, 'CNT_TRAVELERS_ADJ_OUT', 'CNT_TRAVELERS_ADJ_OUT_1')
	mis2b <- merge(mis2b, tmp, by='FIPS')
	mis2b[, CNT_TRAVELERS_ADJ_OUT_M := CNT_TRAVELERS_ADJ_OUT / CNT_TRAVELERS_ADJ_OUT_1]
	tmp <- subset(mis2b, WDAY==1)
	p <- ggplot(tmp, aes(x= DAY, y= CNT_TRAVELERS_ADJ_OUT_M, colour= FIPS, group=FIPS)) +
			geom_line() +
			scale_x_date(breaks='2 weeks') +
			theme_bw() +		
			labs(x='', y='expected number travel journeys from county', colour='county') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)	
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_multtrend.pdf'), p, w=12, h=12, limitsize = FALSE)
	
	#
	#	how strong are day of the week effects?
	mis3 <- subset(mis2b, select=c(FIPS, DAY, CNT_TRAVELERS_ADJ_OUT, WDAY2))
	mis3[, WEEK := strftime(DAY,'%V')]
	mis3 <- subset(mis3, WEEK>23)
	tmp <- subset(mis3, WDAY2=='Monday')
	setnames(tmp, 'CNT_TRAVELERS_ADJ_OUT', 'CNT_TRAVELERS_ADJ_OUT_1')
	tmp <- subset(tmp, select=c(FIPS, WEEK, CNT_TRAVELERS_ADJ_OUT_1))
	mis3 <- merge(mis3, tmp, by=c('FIPS','WEEK'))
	mis3[, CNT_TRAVELERS_ADJ_OUT_M := CNT_TRAVELERS_ADJ_OUT / CNT_TRAVELERS_ADJ_OUT_1]
	
	
	p <- ggplot(mis3, aes(x=WEEK, y= CNT_TRAVELERS_ADJ_OUT_M, colour=WDAY2)) +
		geom_boxplot() +		
		ggsci::scale_color_lancet() +
		labs(x='calendar week', y='relative number travel journeys from county\ncompared to Monday same week', colour='weekday') +
		theme_bw() +
		theme(
			legend.position = 'bottom', 
			axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
		)
	ggsave(file=file.path(outdir, 'fc_outofcounty_201014_nysct_weekdaytrend.pdf'), p, w=15, h=8, limitsize = FALSE)
	#	second observations: 
	#	out travel follows the same overall trend across counties on a multiplicative scale
	#	weekday effects:
	#		typically there increases Monday - Friday and a drop Sat-Sun,
	#		but the magnitude differs by week
	#	it may not be worth to model weekday effects explicitly
	

	#	start with log normal model on outflows
	#	autocorrelated random effect for each week
}



mobility.ts.lognormal.ar.week.model.210113 <- function(f)
{
	require(data.table)
	require(ggplot2)
	require(ggsci)
	require(rstan)
	require(bayesplot)
	require(pammtools)
	
	infile <- 'data/US_county_to_county_2020-01-30_2021-01-05_outtravel.rds'
	outdir <- 'mobility_forecasts'
	
	mi <- readRDS(infile)
	mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	mi[, WDAY2 := strftime(DAY,'%A')]
	mi[, WDAY3 := strftime(DAY,'%u')]
	mi[, WEEK := as.integer(strftime(DAY,'%V'))]
	tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
	setkey(tmp, WDAY3)
	set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
	mi[, WDAY2:=NULL]
	mi <- merge(mi, tmp, by='WDAY3')
	mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
	set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
	setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
		
	#	select NY State and Connecticut
	fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
	mis2 <- subset(mi, FIPS%in%fips.select2)	
	
	 #	subset data to one FIPS
	df <- subset(mis2, FIPS==36061)
	setkey(df, DAY)
	df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
	df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
	###################### need to change here --Yifei for different dataset
	df[DAY >= '2021-01-04']$WEEK_IDX <- 27L 
	df[, DAY_IDX:= 1:nrow(df)]
	
	#	make forecast data.table
	dff <- data.table(
		STATE_NAME = df$STATE_NAME[1], 
		STATEFP = df$STATEFP[1],	
		FIPS = df$FIPS[1],
		DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
		DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
		)
	dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
	dff[, WDAY3 := strftime(DAY,'%u')]
	dff[, WEEK := as.integer(strftime(DAY,'%V'))]
	dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
	setkey(dff, DAY)
	dff[, WEEK_IDX := as.integer(ceiling((WEEK + 1L)/2))]
	# dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
	tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
	set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
	
	# define normal regression using Stan syntax
	lognormal_m1_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];	
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
}
parameters{
	real beta0;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;
		mu = beta0 + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	beta0 ~ normal( 0 , 5 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2:WK_FIT_M1] ~ normal(wk_effect[1:WK_FIT_M2],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	{	
		vector[WK_PRED] wk_effect_pred;
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		for(i in 2:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( wk_effect_pred[i-1], wk_effect_sigma );
		}
		mu_pred = beta0 + wk_effect_pred[ day_to_wk_idx_pred ];
	}
}
"
	# compile model
	lognormal_m1_c <- rstan::stan_model(
		model_name= 'lognormal_m1', 
		model_code = gsub('\t',' ',lognormal_m1_txt)
		)
	
	# prepare stan_data
	stan_data <- list()
	stan_data$N <- nrow(df)	
	stan_data$WK_FIT <- max(df$WEEK_IDX)
	stan_data$y <- df$Y
	stan_data$day_to_wk_idx <- df$WEEK_IDX
	stan_data$N_PRED <- nrow(dff)
	stan_data$WK_PRED <- max(dff$WEEK_IDX)	
	stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
	
	stan_inits <- list()
	stan_inits$beta0 <- round(stan_data$y[1])
	stan_inits$sigma <- .5
	stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
	stan_inits$wk_effect_sigma <- .5
	
	# run
	lognormal_m1_fit <- rstan::sampling(lognormal_m1_c, 
			data=stan_data, 
			warmup=5e2, iter=5e3, chains=4,
			init= list(stan_inits, stan_inits, stan_inits, stan_inits)
			)
	# saveRDS(lognormal_m1_fit, file=file.path(outdir, 'lognormal_m1_fit_',f,'.rds'))
			
	# convergence and mixing
	m1_su <- summary(lognormal_m1_fit)$summary
	m1_su <- m1_su[grepl("beta|sigma|wk_effect|lp",rownames(m1_su)), ] 
	max(m1_su[, 'Rhat'])
	min(m1_su[, 'n_eff'])
	
	# trace plots
	po <- rstan:::extract(lognormal_m1_fit, inc_warmup = TRUE, permuted = FALSE)
	bayesplot:::color_scheme_set("mix-blue-pink")
	p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|lp", n_warmup = 5e2,
			facet_args = list(ncol = 1, labeller = label_parsed))
  pdf(file=file.path(outdir,'lognormal_m1_fit_traces_36047.pdf'), w=10, h=100)
	print(p)
	dev.off()
	
	# pair plot of first 10 parameters
	po <- as.array(lognormal_m1_fit)
	po <- po[,,1:10]
	p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
	# ggsave(file=file.path(outdir,'lognormal_m1_fit_pairplot_36123.pdf'), p, w=20, h=20)

	# show fit
	po <- rstan::extract(lognormal_m1_fit)
	dfp <- as.data.table(reshape2::melt(po$mu))
	setnames(dfp, 2L, 'DAY_IDX')
	tmp <- as.data.table(reshape2::melt(po$mu_pred))
	setnames(tmp, 2L, 'DAY_IDX')
	set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
	dfp <- rbind(dfp, tmp)
	
	dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
	dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
	tmp <- rbind(df, dff, fill=TRUE)
	dfp <- merge(tmp, dfp, by='DAY_IDX')
	
	p <- ggplot(dfp) +
			pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
			geom_step( aes(x=DAY, y= MU_M), colour='black') +
			geom_point( data=subset(dfp, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
			scale_x_date(breaks='2 weeks', expand=c(0,0)) +
			ggsci::scale_color_lancet() +
			theme_bw() +
			facet_wrap(~FIPS, ncol=4, scales='free_y') +
			labs(x='', y='expected number travel journeys from county', colour='') +
			theme(
					legend.position = 'bottom', 
					axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
			)
	# ggsave(file=file.path(outdir, 'lognormal_m1_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)
	ggsave(file=file.path(outdir, paste0("lognormal_m1_fit_forecast", f,".png")), p, w=8, h=6, limitsize = FALSE)
	
	
	
	
}



mobility.ts.lognormal.ar.week.model.with.week.effects.210128 <- function()
{
  require(data.table)
  require(ggplot2)
  require(ggsci)
  require(rstan)
  require(bayesplot)
  require(pammtools)
  
  infile <- 'data/US_county_to_county_2020-01-30_2021-01-05_outtravel.rds'
  outdir <- 'mobility_forecasts'
  
  mi <- readRDS(infile)
  mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  mi[, WDAY2 := strftime(DAY,'%A')]
  mi[, WDAY3 := strftime(DAY,'%u')]
  mi[, WEEK := as.integer(strftime(DAY,'%V'))]
  tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
  setkey(tmp, WDAY3)
  set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
  mi[, WDAY2:=NULL]
  mi <- merge(mi, tmp, by='WDAY3')
  mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
  set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
  setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
  
  #	select NY State and Connecticut
  fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
  mis2 <- subset(mi, FIPS%in%fips.select2)	
  
  #	subset data to one FIPS
  df <- subset(mis2, FIPS=='36123')
  setkey(df, DAY)
  df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
  df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
  ###################### need to change here --Yifei for different dataset
  df$WEEK_IDX[(length(df$WEEK_IDX)-1):length(df$WEEK_IDX)] <- 27L 
  df[, DAY_IDX:= 1:nrow(df)]
  
  #	make forecast data.table
  dff <- data.table(
    STATE_NAME = df$STATE_NAME[1], 
    STATEFP = df$STATEFP[1],	
    FIPS = df$FIPS[1],
    DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
    DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
  )
  dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  dff[, WDAY3 := strftime(DAY,'%u')]
  dff[, WEEK := as.integer(strftime(DAY,'%V'))]
  dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
  setkey(dff, DAY)
  dff[, WEEK_IDX := as.integer(ceiling((WEEK + 1L)/2))]
  #dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
  tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
  set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
    
  # define normal regression using Stan syntax
  lognormal_m2_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;

		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;
		
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;

		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2:WK_FIT_M1] ~ normal(wk_effect[1:WK_FIT_M2],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;

	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;

		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		for(i in 2:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( wk_effect_pred[i-1], wk_effect_sigma );
		}
		
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;

		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
}
"
# compile model
  lognormal_m2_c <- rstan::stan_model(
    model_name= 'lognormal_m2', 
    model_code = gsub('\t',' ',lognormal_m2_txt)
  )

# prepare stan_data
  stan_data <- list()
  stan_data$N <- nrow(df)	
  stan_data$WK_FIT <- max(df$WEEK_IDX)
  stan_data$y <- df$Y
  stan_data$day_to_wk_idx <- df$WEEK_IDX
  stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
  stan_data$N_PRED <- nrow(dff)
  stan_data$WK_PRED <- max(dff$WEEK_IDX)	
  stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
  stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)

  stan_inits <- list()
  stan_inits$beta0 <- round(stan_data$y[1])
  stan_inits$sigma <- .5
  stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
  stan_inits$wk_effect_sigma <- .5
  stan_inits$beta_wkday <- rep(0, 6L)

# run
  lognormal_m2_fit <- rstan::sampling(lognormal_m2_c, 
                                      data=stan_data, 
                                      warmup=5e2, iter=5e3, chains=4,
                                      init= list(stan_inits, stan_inits, stan_inits, stan_inits)
  )
  #saveRDS(lognormal_m2_fit, file=file.path(outdir, 'lognormal_m2_fit_36123.rds'))

# convergence and mixing
  m1_su <- summary(lognormal_m2_fit)$summary
  m1_su <- m1_su[grepl("beta|sigma|wk_effect|lp",rownames(m1_su)), ] 
  max(m1_su[, 'Rhat'])
  min(m1_su[, 'n_eff'])

# trace plots
  po <- rstan:::extract(lognormal_m2_fit, inc_warmup = TRUE, permuted = FALSE)
  bayesplot:::color_scheme_set("mix-blue-pink")
  p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|lp", n_warmup = 5e2,
                              facet_args = list(ncol = 1, labeller = label_parsed))
  #pdf(file=file.path(outdir,'lognormal_m2_fit_36123_traces.pdf'), w=10, h=100)
  print(p)
  dev.off()

# pair plot of first 10 parameters
  po <- as.array(lognormal_m2_fit)
  po <- po[,,1:10]
  p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
  #ggsave(file=file.path(outdir,'lognormal_m2_fit_36123_pairplot.pdf'), p, w=20, h=20)

# show fit
  po <- rstan::extract(lognormal_m2_fit)
  dfp <- as.data.table(reshape2::melt(po$mu))
  setnames(dfp, 2L, 'DAY_IDX')
  tmp <- as.data.table(reshape2::melt(po$mu_pred))
  setnames(tmp, 2L, 'DAY_IDX')
  set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
  dfp <- rbind(dfp, tmp)

  dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
  dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
  tmp <- rbind(df, dff, fill=TRUE)
  dfp <- merge(tmp, dfp, by='DAY_IDX')

  p <- ggplot(dfp) +
    pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
    geom_step( aes(x=DAY, y= MU_M), colour='black') +
    geom_point( data=subset(dfp, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
    scale_x_date(breaks='2 weeks', expand=c(0,0)) +
    ggsci::scale_color_lancet() +
    theme_bw() +
    facet_wrap(~FIPS, ncol=4, scales='free_y') +
    labs(x='', y='expected number travel journeys from county', colour='') +
    theme(
      legend.position = 'bottom', 
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  ggsave(file=file.path(outdir, 'lognormal_m2_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)



# calculate MSE
  po <- rstan::extract(lognormal_m1_fit)
  po$mu_pred	
  dfp <- as.data.table(reshape2::melt(po$mu_pred))
  setnames(dfp, 2L, 'DAY_IDX')
  set(dfp, NULL, 'value', exp(dfp$value))

# calculate the posterior mean estimate
  dfp <- dfp[, list(outflows_pred_med=mean(value)), by='DAY_IDX']

}



mobility.ts.lognormal.fullar1.week.model.with.week.effects.210219 <- function()
{
  require(data.table)
  require(ggplot2)
  require(ggsci)
  require(rstan)
  require(bayesplot)
  require(pammtools)
  
  infile <- 'data/US_county_to_county_2020-01-30_2021-01-05_outtravel.rds'
  outdir <- 'mobility_forecasts'
  
  mi <- readRDS(infile)
  mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  mi[, WDAY2 := strftime(DAY,'%A')]
  mi[, WDAY3 := strftime(DAY,'%u')]
  mi[, WEEK := as.integer(strftime(DAY,'%V'))]
  tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
  setkey(tmp, WDAY3)
  set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
  mi[, WDAY2:=NULL]
  mi <- merge(mi, tmp, by='WDAY3')
  mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
  set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
  setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
  
  #	select NY State and Connecticut
  fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
  mis2 <- subset(mi, FIPS%in%fips.select2)	
  
  #	subset data to one FIPS
  df <- subset(mis2, FIPS=='36047')
  setkey(df, DAY)
  df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
  df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
  # df[DAY >= '2021-01-04']$WEEK_IDX <- 27L
  df[, DAY_IDX:= 1:nrow(df)]
  
  #	make forecast data.table
  dff <- data.table(
    STATE_NAME = df$STATE_NAME[1], 
    STATEFP = df$STATEFP[1],	
    FIPS = df$FIPS[1],
    DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
    DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
  )
  dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  dff[, WDAY3 := strftime(DAY,'%u')]
  dff[, WEEK := as.integer(strftime(DAY,'%V'))]
  dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
  setkey(dff, DAY)
  # dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
  dff[, WEEK_IDX := as.integer(ceiling((WEEK + 1L)/2))]
  tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
  set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
  
  # define normal regression using Stan syntax
  lognormal_m3_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real phi;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;			
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;			
		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	phi ~ normal( 0, 0.5 );
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2:WK_FIT_M1] ~ normal(phi*wk_effect[1:WK_FIT_M2],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	vector[N] log_lik;
			
	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;
			
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		for(i in 2:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( phi*wk_effect_pred[i-1], wk_effect_sigma );
		}
			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;
			
		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
	
	for(j in 1:N)
	{
	  log_lik[j] = normal_lpdf(y[j] | mu[j],sigma);
	}
}
"
# compile model
lognormal_m3_c <- rstan::stan_model(
  model_name= 'lognormal_m3', 
  model_code = gsub('\t',' ',lognormal_m3_txt)
)

# prepare stan_data
stan_data <- list()
stan_data$N <- nrow(df)	
stan_data$WK_FIT <- max(df$WEEK_IDX)
stan_data$y <- df$Y
stan_data$day_to_wk_idx <- df$WEEK_IDX
stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
stan_data$N_PRED <- nrow(dff)
stan_data$WK_PRED <- max(dff$WEEK_IDX)	
stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)

stan_inits <- list()
stan_inits$beta0 <- round(stan_data$y[1])
stan_inits$phi <- 0.5
stan_inits$sigma <- .5
stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
stan_inits$wk_effect_sigma <- .5
stan_inits$beta_wkday <- rep(0, 6L)

# run
lognormal_m3_fit <- rstan::sampling(lognormal_m3_c, 
                                    data=stan_data, 
                                    warmup=5e2, iter=5e3, chains=4,
                                    init= list(stan_inits, stan_inits, stan_inits, stan_inits)
)
saveRDS(lognormal_m3_fit, file=file.path(outdir, 'lognormal_m3_fit_36123.rds'))

# convergence and mixing
m1_su <- summary(lognormal_m3_fit)$summary
m1_su <- m1_su[grepl("beta|sigma|wk_effect|lp",rownames(m1_su)), ] 
max(m1_su[, 'Rhat'])
min(m1_su[, 'n_eff'])

# trace plots
po <- rstan:::extract(lognormal_m3_fit, inc_warmup = TRUE, permuted = FALSE)
bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|phi|lp", n_warmup = 5e2,
                            facet_args = list(ncol = 1, labeller = label_parsed))
pdf(file=file.path(outdir,'lognormal_m3_fit_36047_traces.pdf'), w=10, h=100)
print(p)
dev.off()

# pair plot of first 10 parameters
#pdf(file=file.path(outdir,'lognormal_m3_fit_36123_pairplot_b.pdf'), w=20, h=20)
#pairs(lognormal_m3_fit, pars=c("beta0","phi","sigma","wk_effect[1]","wk_effect[2]","wk_effect[3]","wk_effect[4]","wk_effect[5]","wk_effect[6]","wk_effect[7]","wk_effect[8]","lp__"))	
#dev.off()

# pair plot of first 10 parameters
po <- as.array(lognormal_m3_fit)
po <- po[,,c(1:11,dim(po)[3])]
p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
ggsave(file=file.path(outdir,'lognormal_m3_fit_36123_pairplot.pdf'), p, w=20, h=20)

# show fit
po <- rstan::extract(lognormal_m3_fit)
dfp <- as.data.table(reshape2::melt(po$mu))
setnames(dfp, 2L, 'DAY_IDX')
tmp <- as.data.table(reshape2::melt(po$mu_pred))
setnames(tmp, 2L, 'DAY_IDX')
set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
dfp <- rbind(dfp, tmp)

dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
tmp <- rbind(df, dff, fill=TRUE)
dfp <- merge(tmp, dfp, by='DAY_IDX')

p <- ggplot(dfp14) +
  pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
  geom_step( aes(x=DAY, y= MU_M), colour='black') +
  geom_point( data=subset(dfp14, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
  scale_x_date(breaks='2 weeks', expand=c(0,0)) +
  ggsci::scale_color_lancet() +
  theme_bw() +
  facet_wrap(~FIPS, ncol=4, scales='free_y') +
  labs(x='', y='expected number travel journeys from county', colour='') +
  theme(
    legend.position = 'bottom', 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
ggsave(file=file.path(outdir, 'lognormal_m3_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)



# calculate MSE
po <- rstan::extract(lognormal_m1_fit)
po$mu_pred	
dfp <- as.data.table(reshape2::melt(po$mu_pred))
setnames(dfp, 2L, 'DAY_IDX')
set(dfp, NULL, 'value', exp(dfp$value))

# calculate the posterior mean estimate
dfp <- dfp[, list(outflows_pred_med=mean(value)), by='DAY_IDX']

}



mobility.ts.lognormal.fullar2.week.model.with.week.effects.210219 <- function()
{
  require(data.table)
  require(ggplot2)
  require(ggsci)
  require(rstan)
  require(bayesplot)
  require(pammtools)
  
  infile <- 'data/US_county_to_county_2020-01-30_2021-01-05_outtravel.rds'
  outdir <- 'mobility_forecasts'
  
  mi <- readRDS(infile)
  mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  mi[, WDAY2 := strftime(DAY,'%A')]
  mi[, WDAY3 := strftime(DAY,'%u')]
  mi[, WEEK := as.integer(strftime(DAY,'%V'))]
  tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
  setkey(tmp, WDAY3)
  set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
  mi[, WDAY2:=NULL]
  mi <- merge(mi, tmp, by='WDAY3')
  mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
  set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
  setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
  
  #	select NY State and Connecticut
  fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
  mis2 <- subset(mi, FIPS%in%fips.select2)	
  
  #	subset data to one FIPS
  df <- subset(mis2, FIPS=='36047')
  setkey(df, DAY)
  df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
  df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
  df[, DAY_IDX:= 1:nrow(df)]
  
  #	make forecast data.table
  dff <- data.table(
    STATE_NAME = df$STATE_NAME[1], 
    STATEFP = df$STATEFP[1],	
    FIPS = df$FIPS[1],
    DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
    DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
  )
  dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  dff[, WDAY3 := strftime(DAY,'%u')]
  dff[, WEEK := as.integer(strftime(DAY,'%V'))]
  dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
  setkey(dff, DAY)
  dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
  tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
  set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
  
  # define normal regression using Stan syntax
  lognormal_m4_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_FIT_M3 = WK_FIT - 3;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real phi1;
	real phi2;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;			
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;			
		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	phi1 ~ normal( 0, 0.5 );
	phi2 ~ normal( 0, 0.25 );
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2] ~ normal(phi1*wk_effect[1],  wk_effect_sigma);
	wk_effect[3:WK_FIT_M1] ~ normal(phi1*wk_effect[2:WK_FIT_M2] + phi2*wk_effect[1:WK_FIT_M3],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	vector[N] log_lik;
			
	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;
			
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		wk_effect_pred[2] = normal_rng( phi1*wk_effect_pred[1], wk_effect_sigma );
		for(i in 3:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( phi1*wk_effect_pred[i-1] + phi2*wk_effect_pred[i-2], wk_effect_sigma );
		}
			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;
			
		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
	
	for (j in 1:N)
  {
    log_lik[j] = normal_lpdf(y[j] | mu[j],sigma);
  }
}
"
# compile model
lognormal_m4_c <- rstan::stan_model(
  model_name= 'lognormal_m4', 
  model_code = gsub('\t',' ',lognormal_m4_txt)
)

# prepare stan_data
stan_data <- list()
stan_data$N <- nrow(df)	
stan_data$WK_FIT <- max(df$WEEK_IDX)
stan_data$y <- df$Y
stan_data$day_to_wk_idx <- df$WEEK_IDX
stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
stan_data$N_PRED <- nrow(dff)
stan_data$WK_PRED <- max(dff$WEEK_IDX)	
stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)

stan_inits <- list()
stan_inits$beta0 <- round(stan_data$y[1])
stan_inits$phi1 <- 0.5
stan_inits$phi2 <- 0.5
stan_inits$sigma <- .5
stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
stan_inits$wk_effect_sigma <- .5
stan_inits$beta_wkday <- rep(0, 6L)

# run
lognormal_m4_fit <- rstan::sampling(lognormal_m4_c, 
                                    data=stan_data, 
                                    warmup=5e2, iter=5e3, chains=4,
                                    init= list(stan_inits, stan_inits, stan_inits, stan_inits)
)
saveRDS(lognormal_m4_fit, file=file.path(outdir, 'lognormal_m4_fit_36047.rds'))

# convergence and mixing
m1_su <- summary(lognormal_m4_fit)$summary
m1_su <- m1_su[grepl("beta|sigma|wk_effect|phi|lp",rownames(m1_su)), ] 
max(m1_su[, 'Rhat'])
min(m1_su[, 'n_eff'])

# trace plots
po <- rstan:::extract(lognormal_m4_fit, inc_warmup = TRUE, permuted = FALSE)
bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|phi|lp", n_warmup = 5e2,
                            facet_args = list(ncol = 1, labeller = label_parsed))
pdf(file=file.path(outdir,'lognormal_m4_fit_36047_traces.pdf'), w=10, h=100)
print(p)
dev.off()

# pair plot of first 10 parameters
po <- as.array(lognormal_m4_fit)
po <- po[,,c(1:11,dim(po)[3])]
p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
ggsave(file=file.path(outdir,'lognormal_m4_fit_36047_pairplot.pdf'), p, w=20, h=20)

# show fit
po <- rstan::extract(lognormal_m4_fit)
dfp <- as.data.table(reshape2::melt(po$mu))
setnames(dfp, 2L, 'DAY_IDX')
tmp <- as.data.table(reshape2::melt(po$mu_pred))
setnames(tmp, 2L, 'DAY_IDX')
set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
dfp <- rbind(dfp, tmp)	
dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
tmp <- rbind(df, dff, fill=TRUE)
dfp <- merge(tmp, dfp, by='DAY_IDX')	
p <- ggplot(dfp4) +
  pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
  geom_step( aes(x=DAY, y= MU_M), colour='black') +
  geom_point( data=subset(dfp4, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
  scale_x_date(breaks='2 weeks', expand=c(0,0)) +
  ggsci::scale_color_lancet() +
  theme_bw() +
  facet_wrap(~FIPS, ncol=4, scales='free_y') +
  labs(x='', y='expected number travel journeys from county', colour='') +
  theme(
    legend.position = 'bottom', 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )
ggsave(file=file.path(outdir, 'lognormal_m4_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)		
}



mobility.ts.lognormal.fullar2.week.model.with.week.effects.county.to.county.210219 <- function()
{
  require(data.table)
  require(ggplot2)
  require(ggsci)
  require(rstan)
  require(bayesplot)
  require(pammtools)
  
  infile <- 'data/US_county_to_county_2020-01-30_2021-01-05.rds'
  outdir <- 'mobility_forecasts'
  
  mi <- readRDS(infile)
  mi <- mi[ FIPS!=NEXT_FIPS, 
            list(CNT_TRAVELERS_ADJ_OUT= sum(CNT_TRAVELERS_ADJ)), 
            by=c('STATE_NAME','NEXT_STATE_NAME','STATEFP','FIPS','NEXT_FIPS','DAY')]
  mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  mi[, WDAY2 := strftime(DAY,'%A')]
  mi[, WDAY3 := strftime(DAY,'%u')]
  mi[, WEEK := as.integer(strftime(DAY,'%V'))]
  tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
  setkey(tmp, WDAY3)
  set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
  mi[, WDAY2:=NULL]
  mi <- merge(mi, tmp, by='WDAY3')
  mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
  set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
  setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)
  
  #	select NY State and Connecticut
  fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
  next.fips.select2 <- subset(mi,NEXT_STATE_NAME%in%c("New York","Connecticut"))[,unique(NEXT_FIPS)]
  mis2 <- subset(mi, FIPS%in%fips.select2 & NEXT_FIPS%in%next.fips.select2)	
  
  #	subset data to one FIPS
  df <- subset(mis2, FIPS=='36005' & NEXT_FIPS=='36071')
  setkey(df, DAY)
  df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
  df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
  df[DAY >= '2021-01-04']$WEEK_IDX <- 27L
  df[, DAY_IDX:= 1:nrow(df)]
  
  #	make forecast data.table
  dff <- data.table(
    STATE_NAME = df$STATE_NAME[1], 
    STATEFP = df$STATEFP[1],	
    FIPS = df$FIPS[1],
    DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
    DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
  )
  dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
  dff[, WDAY3 := strftime(DAY,'%u')]
  dff[, WEEK := as.integer(strftime(DAY,'%V'))]
  dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
  setkey(dff, DAY)
  # dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
  dff[, WEEK_IDX := as.integer(ceiling((WEEK + 1L)/2))]
  tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
  set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
  
  # define normal regression using Stan syntax
  lognormal_m4_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_FIT_M3 = WK_FIT - 3;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real phi1;
	real phi2;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;			
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;			
		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	phi1 ~ normal( 0, 0.5 );
	phi2 ~ normal( 0, 0.25 );
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2] ~ normal(phi1*wk_effect[1],  wk_effect_sigma);
	wk_effect[3:WK_FIT_M1] ~ normal(phi1*wk_effect[2:WK_FIT_M2] + phi2*wk_effect[1:WK_FIT_M3],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	vector[N] log_lik;
			
	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;
			
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		wk_effect_pred[2] = normal_rng( phi1*wk_effect_pred[1], wk_effect_sigma );
		for(i in 3:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( phi1*wk_effect_pred[i-1] + phi2*wk_effect_pred[i-2], wk_effect_sigma );
		}
			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;
			
		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
	
	for (j in 1:N)
  {
    log_lik[j] = normal_lpdf(y[j] | mu[j],sigma);
  }
}
"
# compile model
lognormal_m4_c <- rstan::stan_model(
  model_name= 'lognormal_m4', 
  model_code = gsub('\t',' ',lognormal_m4_txt)
)

# prepare stan_data
stan_data <- list()
stan_data$N <- nrow(df)	
stan_data$WK_FIT <- max(df$WEEK_IDX)
stan_data$y <- df$Y
stan_data$day_to_wk_idx <- df$WEEK_IDX
stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
stan_data$N_PRED <- nrow(dff)
stan_data$WK_PRED <- max(dff$WEEK_IDX)	
stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)

stan_inits <- list()
stan_inits$beta0 <- round(stan_data$y[1])
stan_inits$phi1 <- 0.5
stan_inits$phi2 <- 0.5
stan_inits$sigma <- .5
stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
stan_inits$wk_effect_sigma <- .5
stan_inits$beta_wkday <- rep(0, 6L)

# run
lognormal_m4_fit <- rstan::sampling(lognormal_m4_c, 
                                    data=stan_data, 
                                    warmup=5e2, iter=5e3, chains=4,
                                    init= list(stan_inits, stan_inits, stan_inits, stan_inits)
)
saveRDS(lognormal_m4_fit, file=file.path(outdir, 'lognormal_m4_fit_36123.rds'))

# convergence and mixing
m1_su <- summary(lognormal_m4_fit)$summary
m1_su <- m1_su[grepl("beta|sigma|wk_effect|phi|lp",rownames(m1_su)), ] 
max(m1_su[, 'Rhat'])
min(m1_su[, 'n_eff'])

# trace plots
po <- rstan:::extract(lognormal_m4_fit, inc_warmup = TRUE, permuted = FALSE)
bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_trace(po,  regex_pars ="beta|sigma|wk_effect|phi|lp", n_warmup = 5e2,
                            facet_args = list(ncol = 1, labeller = label_parsed))
pdf(file=file.path(outdir,'lognormal_m4_fit_36123_traces.pdf'), w=10, h=100)
print(p)
dev.off()

# pair plot of first 10 parameters
po <- as.array(lognormal_m4_fit)
po <- po[,,c(1:11,dim(po)[3])]
p <- bayesplot::mcmc_pairs(po, diag_fun = "dens", off_diag_fun = "hex")
ggsave(file=file.path(outdir,'lognormal_m4_fit_36123_pairplot.pdf'), p, w=20, h=20)

# show fit
po <- rstan::extract(lognormal_m4_fit)
dfp <- as.data.table(reshape2::melt(po$mu))
setnames(dfp, 2L, 'DAY_IDX')
tmp <- as.data.table(reshape2::melt(po$mu_pred))
setnames(tmp, 2L, 'DAY_IDX')
set(tmp, NULL, 'DAY_IDX', tmp$DAY_IDX+max(dfp$DAY_IDX))
dfp <- rbind(dfp, tmp)	
dfp <- dfp[, list(value=quantile(exp(value), p=c(0.5,0.025,0.975)), variable= c('MU_M','MU_CL','MU_CU')), by='DAY_IDX']
dfp <- dcast.data.table(dfp, DAY_IDX~variable, value.var='value')
tmp <- rbind(df, dff, fill=TRUE)
dfp <- merge(tmp, dfp, by='DAY_IDX')	
p <- ggplot(dfp) +
  pammtools:::geom_stepribbon( aes(x=DAY, ymin=MU_CL, ymax=MU_CU), fill='black', alpha=.5) +
  geom_step( aes(x=DAY, y= MU_M), colour='black') +
  geom_point( data=subset(dfp, !is.na(CNT_TRAVELERS_ADJ_OUT)), aes(x=DAY, y=CNT_TRAVELERS_ADJ_OUT, group=FIPS, colour=WDAY2) ) +
  scale_x_date(breaks='2 weeks', expand=c(0,0)) +
  ggsci::scale_color_lancet() +
  theme_bw() +
  facet_wrap(~FIPS, ncol=4, scales='free_y') +
  labs(x='', y='expected number travel journeys from county', colour='') +
  theme(
    legend.position = 'bottom', 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) #+
  #ggtitle(paste0('from',fips,'to',next_fips,'.png'))
ggsave(file=file.path(outdir, 'lognormal_m4_fit_36123_forecast.pdf'), p, w=8, h=6, limitsize = FALSE)		
}



elpd.ts.lognormal.fullar1.2.week.model.with.week.effects.20210323 <- function()
{
  # complie stan models with log lik
  # define normal regression using Stan syntax
  lognormal_m4_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_FIT_M3 = WK_FIT - 3;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real phi1;
	real phi2;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;			
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;			
		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	phi1 ~ normal( 0, 0.5 );
	phi2 ~ normal( 0, 0.25 );
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2] ~ normal(phi1*wk_effect[1],  wk_effect_sigma);
	wk_effect[3:WK_FIT_M1] ~ normal(phi1*wk_effect[2:WK_FIT_M2] + phi2*wk_effect[1:WK_FIT_M3],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	vector[N] log_lik;
			
	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;
			
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		wk_effect_pred[2] = normal_rng( phi1*wk_effect_pred[1], wk_effect_sigma );
		for(i in 3:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( phi1*wk_effect_pred[i-1] + phi2*wk_effect_pred[i-2], wk_effect_sigma );
		}
			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;
			
		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
	
	for (j in 1:N)
  {
    log_lik[j] = normal_lpdf(y[j] | mu[j],sigma);
  }
}
"
# compile model
  lognormal_m4_c <- rstan::stan_model(
    model_name= 'lognormal_m4', 
    model_code = gsub('\t',' ',lognormal_m4_txt)
  )

# full ar 1
lognormal_m3_txt <- "
data{
	int<lower=1> N;
	int<lower=1> WK_FIT;	
	vector[N] y;
	int<lower=1, upper=WK_FIT> day_to_wk_idx[N];
	int<lower=1, upper=7> day_to_wkday_idx[N];
	int<lower=1> N_PRED;
	int<lower=1> WK_PRED;
	int<lower=1, upper=WK_PRED> day_to_wk_idx_pred[N_PRED];
	int<lower=1, upper=7> day_to_wkday_idx_pred[N_PRED];
}
transformed data{
	int<lower=1> WK_FIT_M1 = WK_FIT - 1;
	int<lower=1> WK_FIT_M2 = WK_FIT - 2;
	int<lower=1> WK_DAY_N = 7;
	int<lower=1> WK_DAY_N_M1 = 6;
}
parameters{
	real beta0;	
	real phi;
	real<lower=0> sigma;
	vector[WK_FIT_M1] wk_effect;
	real<lower=0> wk_effect_sigma;
	vector[WK_DAY_N_M1] beta_wkday;
}
transformed parameters{
	vector[N] mu;
	{
		vector[WK_FIT] wk_effect_ext;
		vector[WK_DAY_N] beta_wkday_ext;			
		wk_effect_ext[1] = 0;
		wk_effect_ext[2:WK_FIT] = wk_effect;			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;			
		mu = beta0 + beta_wkday_ext[ day_to_wkday_idx ] + wk_effect_ext[ day_to_wk_idx ];
	}	
}
model{
	phi ~ normal( 0, 0.5 );
	beta0 ~ normal( 0 , 5 );
	beta_wkday ~ normal( 0, 0.2 );
	wk_effect[1] ~ normal( 0 , 0.3 );
	wk_effect[2:WK_FIT_M1] ~ normal(phi*wk_effect[1:WK_FIT_M2],  wk_effect_sigma);
	wk_effect_sigma ~ cauchy( 0, 1);
	sigma ~ cauchy( 0, 1);
	y ~ normal(mu, sigma);	
}
generated quantities{
	vector[N_PRED] mu_pred;
	vector[N] log_lik;
			
	{	
		vector[WK_PRED] wk_effect_pred;
		vector[WK_DAY_N] beta_wkday_ext;
			
		wk_effect_pred[1] = wk_effect[WK_FIT_M1];
		for(i in 2:WK_PRED)
		{
			wk_effect_pred[i] = normal_rng( phi*wk_effect_pred[i-1], wk_effect_sigma );
		}
			
		beta_wkday_ext[1] = 0;
		beta_wkday_ext[2:WK_DAY_N] = beta_wkday;
			
		mu_pred = beta0 + beta_wkday_ext[ day_to_wkday_idx_pred ] + wk_effect_pred[ day_to_wk_idx_pred ];
	}
	
	for(j in 1:N)
	{
	  log_lik[j] = normal_lpdf(y[j] | mu[j],sigma);
	}
}
"
# compile model
lognormal_m3_c <- rstan::stan_model(
  model_name= 'lognormal_m3', 
  model_code = gsub('\t',' ',lognormal_m3_txt)
)


# prepare county to county data.
require(data.table)
require(ggplot2)
require(ggsci)
require(rstan)
require(bayesplot)
require(pammtools)

infile <- 'data/US_county_to_county_2020-01-30_2021-01-05.rds'
outdir <- 'mobility_forecasts'

mi <- readRDS(infile)
mi <- mi[ FIPS!=NEXT_FIPS, 
          list(CNT_TRAVELERS_ADJ_OUT= sum(CNT_TRAVELERS_ADJ)), 
          by=c('STATE_NAME','NEXT_STATE_NAME','STATEFP','FIPS','NEXT_FIPS','DAY')]
mi[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
mi[, WDAY2 := strftime(DAY,'%A')]
mi[, WDAY3 := strftime(DAY,'%u')]
mi[, WEEK := as.integer(strftime(DAY,'%V'))]
tmp <- unique(subset(mi, select=c(WDAY2,WDAY3)))
setkey(tmp, WDAY3)
set(tmp, NULL, 'WDAY2', tmp[, factor(WDAY3, levels=WDAY3, labels=WDAY2)])
mi[, WDAY2:=NULL]
mi <- merge(mi, tmp, by='WDAY3')
mi[, WDAY_LABEL := factor(WDAY, levels=c(0,1), labels=c('weekend','weekday'))]
set(mi, NULL, 'FIPS', mi[, factor(FIPS)])
setkey(mi, STATE_NAME, STATEFP, FIPS, DAY)

#	select NY State and Connecticut
fips.select2 <- subset(mi, STATE_NAME%in%c("New York", "Connecticut"))[, unique(FIPS)]
next.fips.select2 <- subset(mi,NEXT_STATE_NAME%in%c("New York","Connecticut"))[,unique(NEXT_FIPS)]
mis2 <- subset(mi, FIPS%in%fips.select2 & NEXT_FIPS%in%next.fips.select2)	

# create empty data table to store values later
elpd <- data.table()
# create fips in NYS 
fips.list <- unique(as.numeric(as.character(mis2$FIPS)))
ny.fips <- fips.list[9:length(fips.list)]



# run ar1 and ar2 using a loop.
for ( fips in ny.fips){
  for( next_fips in ny.fips){
    df <- subset(mis2, FIPS==fips & NEXT_FIPS==next_fips)
    setkey(df, DAY)
    df[, Y := log(CNT_TRAVELERS_ADJ_OUT)]
    df[, WEEK_IDX := as.integer(ceiling((WEEK - min(WEEK) + 1L)/2))]
    df[DAY >= '2021-01-04']$WEEK_IDX <- 27L
    df[, DAY_IDX:= 1:nrow(df)]
    if (nrow(df)<30){
      print(paste0('no enough data to forecast flow from',fips,'to',next_fips))
    }else{
      #	make forecast data.table
      dff <- data.table(
        STATE_NAME = df$STATE_NAME[1], 
        STATEFP = df$STATEFP[1],	
        FIPS = df$FIPS[1],
        DAY= seq.int(max(df$DAY)+1L, by=1L, length.out=30L),
        DAY_IDX= seq.int(max(df$DAY_IDX)+1L, by=1L, length.out=30L)
      )
      dff[, WDAY := as.integer( !strftime(DAY,'%A') %in% c('Saturday','Sunday') )]
      dff[, WDAY3 := strftime(DAY,'%u')]
      dff[, WEEK := as.integer(strftime(DAY,'%V'))]
      dff <- merge(dff, unique(subset(df, select=c(WDAY3, WDAY2))), by='WDAY3')	
      setkey(dff, DAY)
      # dff[, WEEK_IDX := as.integer(ceiling((WEEK - min(df$WEEK) + 1L)/2))]
      dff[, WEEK_IDX := as.integer(ceiling((WEEK + 1L)/2))]
      tmp <- ifelse( max(df$WEEK_IDX) < min(dff$WEEK_IDX), 2L, 1L )
      set(dff, NULL, 'WEEK_IDX', dff[, WEEK_IDX-min(WEEK_IDX)+tmp])
      
      # prepare stan_data for ar1
      stan_data <- list()
      stan_data$N <- nrow(df)	
      stan_data$WK_FIT <- max(df$WEEK_IDX)
      stan_data$y <- df$Y
      stan_data$day_to_wk_idx <- df$WEEK_IDX
      stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
      stan_data$N_PRED <- nrow(dff)
      stan_data$WK_PRED <- max(dff$WEEK_IDX)	
      stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
      stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)
      
      stan_inits <- list()
      stan_inits$beta0 <- round(stan_data$y[1])
      stan_inits$phi <- 0.5
      stan_inits$sigma <- .5
      stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
      stan_inits$wk_effect_sigma <- .5
      stan_inits$beta_wkday <- rep(0, 6L)
      
      # run ar 1
      lognormal_m3_fit <- rstan::sampling(lognormal_m3_c, 
                                          data=stan_data, 
                                          warmup=5e2, iter=5e3, chains=4,
                                          init= list(stan_inits, stan_inits, stan_inits, stan_inits)
      )
      
      # prepare stan_data for ar 2
      stan_data <- list()
      stan_data$N <- nrow(df)	
      stan_data$WK_FIT <- max(df$WEEK_IDX)
      stan_data$y <- df$Y
      stan_data$day_to_wk_idx <- df$WEEK_IDX
      stan_data$day_to_wkday_idx <- as.integer(df$WDAY3)
      stan_data$N_PRED <- nrow(dff)
      stan_data$WK_PRED <- max(dff$WEEK_IDX)	
      stan_data$day_to_wk_idx_pred <- dff$WEEK_IDX
      stan_data$day_to_wkday_idx_pred <- as.integer(dff$WDAY3)
      
      stan_inits <- list()
      stan_inits$beta0 <- round(stan_data$y[1])
      stan_inits$phi1 <- 0.5
      stan_inits$phi2 <- 0.5
      stan_inits$sigma <- .5
      stan_inits$wk_effect <- rep(0, stan_data$WK_FIT-1L)
      stan_inits$wk_effect_sigma <- .5
      stan_inits$beta_wkday <- rep(0, 6L)
      
      # run ar2
      lognormal_m4_fit <- rstan::sampling(lognormal_m4_c, 
                                          data=stan_data, 
                                          warmup=5e2, iter=5e3, chains=4,
                                          init= list(stan_inits, stan_inits, stan_inits, stan_inits)
      )
      
      # compute elpd
      lognormal_m3_fit_loo <- loo::loo(lognormal_m3_fit)
      lognormal_m4_fit_loo <- loo::loo(lognormal_m4_fit)
      elpd_ar1 <- lognormal_m3_fit_loo$elpd_loo
      elpd_ar2 <- lognormal_m4_fit_loo$elpd_loo
      
      # compare, give p values
      comp <- loo::loo_compare(lognormal_m3_fit_loo,lognormal_m4_fit_loo)
      p <- pnorm(comp[2, 'elpd_diff'], 0,  comp[2, 'se_diff'])
      
      # put in the empty data table created before.
      tmp <- cbind(fips,next_fips,elpd_ar1,elpd_ar2,p)
      elpd <- rbind(elpd,tmp)
    }
  }
}


}