library(rstan)
library(gtools)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)
library(TeachingDemos)
library(COVID19)

# StanModel = 'mixed_effects'
StanModel = 'no_regions'

print(sprintf("Running %s",StanModel))

pale_t <- c(
  rgb(red=68, green=34, blue=136, alpha=100, maxColorValue = 255),
  rgb(red=108, green=162, blue=234, alpha=100, maxColorValue = 255),
  rgb(red=181, green=211, blue=61, alpha=100, maxColorValue = 255),
  rgb(red=254, green=210, blue=63, alpha=100, maxColorValue = 255),
  rgb(red=235, green=125, blue=91, alpha=100, maxColorValue = 255),
  rgb(red=207, green=72, blue=41, alpha=100, maxColorValue = 255)
)
pale <- c(
  rgb(red=68, green=34, blue=136, alpha=255, maxColorValue = 255),
  rgb(red=108, green=162, blue=234, alpha=255, maxColorValue = 255),
  rgb(red=181, green=211, blue=61, alpha=255, maxColorValue = 255),
  rgb(red=254, green=210, blue=63, alpha=255, maxColorValue = 255),
  rgb(red=235, green=125, blue=91, alpha=255, maxColorValue = 255),
  rgb(red=207, green=72, blue=41, alpha=255, maxColorValue = 255)
)

## Reading all data
d = covid19("ITA", level = 2, verbose = FALSE)
dd = covid19("ITA", verbose = FALSE)

d = data.frame("date" = d$date, 
               "Cases_cum" = d$confirmed,
               "Deaths_cum" = d$deaths,
               "Country" = d$administrative_area_level_2)
dd = data.frame("date" = dd$date,
                "cases_ita" = dd$confirmed,
                "deaths_ita" = dd$deaths
                )
dd <- dd[dd$date>as.Date("2020-02-14") & dd$date<=as.Date(max(d$date)),]

d$Country <- as.character(d$Country)
d$Country[d$Country == "Friuli Venezia Giulia"] <- "Friuli-Venezia_Giulia"
d$Country[d$Country == "Valle d'Aosta"] <- "Aosta"
d$Country[d$Country == "P.A. Trento"] <- "Trentino-AA"

d$Cases_cum[d$Country == "Trentino-AA"] <- d$Cases_cum[d$Country == "Trentino-AA"] + 
  d$Cases[d$Country == "P.A. Bolzano"]
d$Deaths_cum[d$Country == "Trentino-AA"] <- d$Deaths_cum[d$Country == "Trentino-AA"] + 
  d$Cases[d$Country == "P.A. Bolzano"]
d = d[d$Country != "P.A. Bolzano",]


countries = unique(as.character(d$Country))
countries_sub <- c("Lombardia", "Emilia-Romagna", "Lazio", "Umbria")

### RIEMPIMENTO DEI PRIMI GIORNI
###################################
ddd <- by(d, d$Country, function(x) merge(x, dd[,c("date","cases_ita", "deaths_ita")], by = "date", all = TRUE))
d = NULL
for(cc in 1:length(ddd)){
  ddd[[cc]]$Country = as.character(ddd[[cc]]$Country)
  ddd[[cc]]$Country[1:10] = ddd[[cc]]$Country[nrow(ddd[[cc]])-1]
  d = rbind(d, ddd[[cc]])
}

d$Cases_cum[is.na(d$Cases_cum)] = 0
d$Deaths_cum[is.na(d$Deaths_cum)] = 0

d$Country <- factor(d$Country, levels = countries)

##### LOMBARDIA
d$Cases_cum[(d$Country == "Lombardia") & d$date<as.Date("2020-02-21")] = 0
d$Cases_cum[(d$Country == "Lombardia") & d$date == as.Date("2020-02-21")] = 16
d$Cases_cum[(d$Country == "Lombardia") & d$date == as.Date("2020-02-22")] = 54
d$Cases_cum[(d$Country == "Lombardia") & d$date == as.Date("2020-02-23")] = 110 

d$Deaths_cum[(d$Country == "Lombardia") & d$date<=as.Date("2020-02-21")] = 0
d$Deaths_cum[(d$Country == "Lombardia") & d$date == as.Date("2020-02-22")] = 1
d$Deaths_cum[(d$Country == "Lombardia") & d$date == as.Date("2020-02-23")] = 2

##### VENETO
d$Cases_cum[(d$Country == "Veneto") & d$date == as.Date("2020-02-21")] = 2
d$Cases_cum[(d$Country == "Veneto") & d$date == as.Date("2020-02-22")] = 17
d$Cases_cum[(d$Country == "Veneto") & d$date == as.Date("2020-02-23")] = 24

d$Deaths_cum[(d$Country == "Veneto") & d$date == as.Date("2020-02-22")] = 1
d$Deaths_cum[(d$Country == "Veneto") & d$date == as.Date("2020-02-23")] = 1

##### PIEMONTE
d$Cases_cum[(d$Country == "Piemonte") & d$date == as.Date("2020-02-22")] = 1
d$Cases_cum[(d$Country == "Piemonte") & d$date == as.Date("2020-02-23")] = 6

##### EMILIA ROMAGNA
d$Cases_cum[(d$Country == "Emilia-Romagna") & d$date == as.Date("2020-02-22")] = 2
d$Cases_cum[(d$Country == "Emilia-Romagna") & d$date == as.Date("2020-02-23")] = 9

##### LAZIO
d$Cases_cum[(d$Country == "Lazio") & (d$date <= as.Date("2020-02-23"))] = 3
###################################
## FINE RIEMPIMENTO PRIMI GIORNI

# Modifico le variabili di interesse nel numero giornaliero anzichè cumulato
d$Deaths <- NULL
d$Cases <- NULL
deaths <- tapply(d$Deaths_cum, d$Country, function(x) c(0, diff(x)))
cases <- tapply(d$Cases_cum, d$Country, function(x) c(0, diff(x)))
for (cc in countries){
  d$Deaths[d$Country == cc] <- deaths[[cc]]
  d$Cases[d$Country == cc] <- cases[[cc]]
}
d$Deaths[d$Deaths < 0] <- 0
d$Cases[d$Cases < 0] <- 0

d$Deaths_smooth <- 0
for (cc in countries){
  minidat = d[d$Country == cc, ]
  d$Deaths_smooth[d$Country == cc] = smooth.spline(minidat$date, minidat$Deaths, df = 15)$y
}

dat <- d[d$Country %in% countries_sub, ]
dat$Country <- factor(dat$Country, levels=countries_sub)
ggplot(dat[dat$date > '2020-02-14',], aes(x = date, y = Deaths, col = Country )) +
  geom_line(size=1.2, alpha=0.9) +
  # geom_point(size=1.5, alpha=1) +
  # geom_line(size=1, alpha=0.3) +
  # geom_point(size = 1, alpha=0.3) +
  # geom_line(aes(x = date, y = Deaths_smooth, col = Country ), size = 1.5) +
  theme_classic(base_size = 24) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.15,0.8),
        plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("Date") +
  ylab("Daily Deaths") +
  # ggtitle("Daily deaths in some Italian regions") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%d %b") +
  scale_color_manual(values = pale[-c(4)])


d = d[,c(1,4,7,8)]
rm(list = c('deaths', 'cases', 'dd', 'ddd', 'minidat', 'dat'))

#---------# #---------# #---------# #---------# #---------# #---------# #---------# #---------#

## get CFR 
# infection-fatality-rate by country
# = prob di morire una volta infetti, pesato per le classi di età di quel paese
cfr.by.country = utils::read.csv("data/weighted_fatality_regions.csv", header = TRUE)


## tau(g)
# the average time from infection of one person to the time at which they infect another
serial.interval = utils::read.csv("data/serial_interval.csv")
# questo lo teniamo tale e quale

#---------# #---------# #---------# #---------# #---------# #---------# #---------# #---------#

# Importo dati mobilità
# covar = utils::read.csv('~/Documents/Dottorato/2.09 Statistical Consulting/StatisticalConsulting/covid19model-1.0_regions/data/Global_Mobility_Report.csv', stringsAsFactors = FALSE)
covar = utils::read.csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', stringsAsFactors = FALSE)
covar = covar[covar$country_region == 'Italy', ]
covar = covar[!(covar$sub_region_1 == ""), ]

covar$sub_region_1[covar$sub_region_1 == "Friuli-Venezia Giulia"] <- "Friuli-Venezia_Giulia"
covar$sub_region_1[covar$sub_region_1 == "Apulia"] <- "Puglia"
covar$sub_region_1[covar$sub_region_1 == "Lombardy"] <- "Lombardia"
covar$sub_region_1[covar$sub_region_1 == "Piedmont"] <- "Piemonte"
covar$sub_region_1[covar$sub_region_1 == "Sardinia"] <- "Sardegna"
covar$sub_region_1[covar$sub_region_1 == "Sicily"] <- "Sicilia"
covar$sub_region_1[covar$sub_region_1 == "Trentino-South Tyrol"] <- "Trentino-AA"
covar$sub_region_1[covar$sub_region_1 == "Tuscany"] <- "Toscana"

#covar[covar$country_region=="Italy",c(2,3,4,5,6)]
covar = data.frame("Country" = covar$sub_region_1,
                   "date" = covar$date,
                   "retail" = covar$retail_and_recreation_percent_change_from_baseline,
                   "essential" = covar$grocery_and_pharmacy_percent_change_from_baseline,
                   "parks" = covar$parks_percent_change_from_baseline,
                   "workplaces" = covar$workplaces_percent_change_from_baseline,
                   "residential" = covar$residential_percent_change_from_baseline,
                   "transit" = covar$transit_stations_percent_change_from_baseline)

covar$Country <- as.character(covar$Country)
covar$date = as.Date(as.character(covar$date))
# covar = covar[covar$Country != "",]
# covar$Country = as.factor(covar$Country)
### volendo si potrebbero aggiungere anche delle indicatrici dei vari provvedimenti 
# e.g. stai a casa se malato, chiusura scuole, ecc

covariates <- data.frame("date" = as.Date(covar$date), 
                         "Country" = covar$Country,
                         "retail" = covar$retail,
                         "essential" = covar$essential,
                         "parks" = covar$parks,
                         "workplaces" = covar$workplaces,
                         "residential" = covar$residential,
                         "transit" = covar$transit)
for (cc in countries){
  country.data <- covar[covar$Country == cc,]
  for (colname in colnames(country.data)[c(3:8)]){
    for (i in 1:nrow(country.data)) {
      if (is.na(country.data[i,colname])) country.data[i, colname] = mean(country.data[c(i-1,i+1), colname], na.rm = TRUE)
    }
  }
  country.ts <- apply(country.data[, -c(1:2)], 2, 
                      function(x) decompose(ts(x, frequency=7, start=as.Date("2020-02-15")))$trend)
  covariates[covariates$Country == cc, c(3:8)] <- country.ts
}

rownames(covariates) <- NULL
# head(covariates)
covariates$Country = as.character(covariates$Country)

#---------# #---------# #---------# #---------# #---------# #---------# #---------# #---------#
# Modifico i dati sulla mobilità per semplificare ed evidenziare il trend: uso una smoothing spline con bassi df

for(cc in countries){
  index_series = covariates$Country == cc
  # riempio i primi e ultimi valori (NA) con la prima/ultima osservazione disponibile
  covariates[index_series,][1:3, c(3:8)] <- covariates[index_series,][4, c(3:8)]
  n <- nrow(covariates[index_series,])
  covariates[index_series,][(n-2):n, c(3:8)] <- covariates[index_series,][(n-3), c(3:8)]

  covariates[index_series, c(3:8)] <- apply(covariates[index_series, c(3:8)], 2,
                                                         function(val) smooth.spline(covariates$date[index_series],
                                                                                   val, df = 30)$y )
}

covariates$retail_travel <- (covariates$retail + covariates$transit)/2
covar$retail_travel <- (covar$retail + covar$transit)/2

dat = data.frame("date" = covariates$date,
                "Country" = covariates$Country,
                "retail_travel" = covar$retail_travel,
                "retail_travel_smooth" = covariates$retail_travel)

dat <- dat[dat$Country %in% countries_sub, ]
dat$Country <- factor(dat$Country, levels=countries_sub)

ggplot(dat, aes(x = date, y = retail_travel, col = Country )) +
  geom_hline(yintercept = 0, size=0.5, linetype="dashed") +
  # geom_line(size=1.2, alpha=0.9) +
  # geom_point(size=1.5, alpha=1) +
  geom_line(size=1, alpha=0.3) +
  # geom_point(size = 1, alpha=0.3) +
  geom_line(aes(x = date, y = retail_travel_smooth, col = Country ), size = 1.5) +
  theme_classic(base_size = 22) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.15,0.15),
        plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("Date") +
  ylab("Percentage change w.r.t baseline") +
  # ggtitle("Movement for retail and travels in some Italian regions") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%d %b") +
  scale_color_manual(values = pale)




covariates <- covariates[,c(1,2,6,7,9)]
covariates[,3:5] <- covariates[,3:5]/100 # le covariate devono essere in (0,1)
# covariates <- covariates[,c(1,2,4,5)]

rm(list = c("covar", "country.data", "country.ts", "index_series", "n", "cc", "colname", "i"))

d = d[(d$date>=min(covariates$date)) & (d$date<=max(covariates$date)),]
d = d[d$date < "2020-05-30",]
covariates = covariates[covariates$date < "2020-05-30",]
#---------# #---------# #---------# #---------# #---------# #---------# #---------# #---------#
# countries = countries_sub
m = length(countries)
p = ncol(covariates) - 2
forecast = 0

DEBUG = F
N2 = 105

# prepara liste vuote in cui poi aggiunge i parametri che dipendono dalle countries selezionate
stan_data = list(M = m, # number of countries
                 P = p, # number of covariates
                 N0 = 6, # number of days for which to impute infections
                 N = NULL, # days of observed data for each country. Each entry must be <= N2
                 N2 = N2, # days of observed data + # of days to forecast
                 cases = NULL, # reported cases -- the rows with i > N contain -1 and should be ignored
                 deaths = NULL, # reported deaths -- the rows with i > N contain -1 and should be ignored
                 f = NULL, # matrix N2 x M that contains h * s
                 covariate = array(0, dim = c(m, N2, p) ), # covariates: array of size M that contains matrices N2 x P
                 EpidemicStart = NULL, # vector length M
                 SI = serial.interval$fit[1:N2] # fixed pre-calculated SI using empirical data from Neil
                 )

for(Country in countries) {
  CFR = cfr.by.country$weighted_fatality[cfr.by.country$country == Country]  # tmp: specifico per Country
  
  covariates1 <- covariates[covariates$Country == Country,]  # tmp: specifico per Country
  
  d1 = d[d$Country==Country,]  # tmp: specifico per Country
  
  # df con date, casi e morti
  d1$date = as.Date(d1$date, format = '%d/%m/%Y')
  d1$t = decimal_date(d1$date) # cosa strana di cui non capisco il senso, lo usa per riordinare il df, boh (?)
  d1 = d1[order(d1$t),]
  
  index = which(d1$Cases>0)[1] # indice del primo giorno con un caso
  index1 = which(cumsum(d1$Deaths)>=5)[1] # also 5  # indice del primo giorno in cui somma(morti)>=10
  index2 = max(index1-30, 1)  # indice del primo giorno in cui somma(morti)>=10 - 30 giorni
  # di questi c'è la spiegazione nel report
  
  print(sprintf("First non-zero cases is on day %d, and 30 days before 10 deaths is day %d", index, index2))
  
  
  d1 = d1[index2:nrow(d1),]
  # considero il dataset da (giorno con 10 morti - 30) a oggi
  
  stan_data$EpidemicStart = c(stan_data$EpidemicStart, index1+1-index2)
  # data inizio epidemia 
  
  d1 <- merge(d1, covariates1, by = c("date", "Country"))
  
  N = nrow(d1)
  print(sprintf("%s has %d days of data", Country, N))
  forecast = N2 - N
  if (forecast < 0) {
    print(sprintf("%s: %d", Country, N))
    print("ERROR!!!! increasing N2")
    N2 = N
    forecast = N2 - N
  }
  
  # hazard estimation
  ####################################
  h = rep(0, N2) # discrete hazard rate from time t = 1, ..., 100
  if (DEBUG)
  {
    # OLD -- but faster for testing this part of the code
    mean = 18.8
    cv = 0.45
    
    for (i in 1:length(h)) 
    {
      h[i] = (CFR * pgammaAlt(i, mean = mean, cv = cv) - 
                CFR * pgammaAlt(i - 1, mean = mean, cv = cv)) / 
        (1 - CFR * pgammaAlt(i - 1, mean = mean, cv = cv))
    }
  } else {
    # NEW
    # infection to onset
    mean1 = 5.1
    cv1 = 0.86
    # onset to death
    mean2 = 18.8
    cv2 = 0.45 
    ## assume that CFR is probability of dying given infection
    x1 = rgammaAlt(5e6, mean1, cv1) # infection-to-onset ----> do all people who are infected get to onset?
    x2 = rgammaAlt(5e6, mean2, cv2) # onset-to-death
    f = ecdf(x1 + x2)
    convolution = function(u) (CFR * f(u))
    
    h[1] = (convolution(1.5) - convolution(0))
    for (i in 2:length(h)) 
    {
      h[i] = (convolution(i + .5) - convolution(i - .5)) / (1 - convolution(i - .5))
    }
  }
  
  s = rep(1, N2)
  for (i in 2:N2)
  {
    s[i] = s[i - 1] * (1 - h[i - 1])
  }
  f = s * h
  ####################################
  # fine di hazard estimation
  
  deaths = c(as.vector(as.numeric(d1$Deaths)),rep(-1,forecast))
  cases = c(as.vector(as.numeric(d1$Cases)),rep(-1,forecast))
  
  # covariates2 df con (N + forecast) righe, le covariate per il futuro sono = all'ultima osservazione
  covariates2 <- as.data.frame(d1[, colnames(covariates1)])
  covariates2[N:(N+forecast),] <- covariates2[N,]
  
  ## append data
  stan_data$N = c(stan_data$N,N)
  stan_data$covariate[match(Country, countries),,] <- as.matrix(covariates2[,-c(1,2)])
  stan_data$f = cbind(stan_data$f,f)
  stan_data$deaths = cbind(stan_data$deaths,deaths)
  stan_data$cases = cbind(stan_data$cases,cases)
  
  if(length(stan_data$N) == 1) 
  {
    stan_data$N = as.array(stan_data$N)
  }
}


save(d, covariates, countries, stan_data, file='data/stan_data_regioni.Rdata')
rm(list=c("covariates1", "covariates2", "d1", "cases", "deaths", "f", "h", "s", "m", "p", "Country"))

load('data/stan_data_regioni.Rdata')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
m = stan_model(paste0('stan/',StanModel,'.stan'))

stanfit = sampling(m, data = stan_data,
               iter = 1500, warmup = 500,
               chains = 4, thin = 2,
               control = list(adapt_delta = 0.9, max_treedepth = 12))
save(stanfit, file = paste0('data/',StanModel,'_regioni_2.Rdata'))
load(paste0('data/',StanModel,'_regioni.Rdata'))
out = rstan::extract(stanfit)
prediction = out$prediction # prediction di cosa? contagi? 
estimated.deaths = out$E_deaths


#------------# #------------# #------------# #------------#
######################## Risultati ########################
#------------# #------------# #------------# #------------#


#------------# Morti stimate dal modello #------------#

countries_sub <- c("Lombardia", "Emilia-Romagna", "Piemonte", "Lazio", "Toscana", "Umbria")
countries_sub <- countries
df = data.frame("date" = d$date[d$Country %in% countries_sub],
                "country" = d$Country[d$Country %in% countries_sub],
                "deaths" = d$Deaths[d$Country %in% countries_sub],
                "est_deaths" = 0,
                "low_ci" = 0,
                "upp_ci" = 0,
                "n_bin" = 0,
                "low_ci_bin" = 0,
                "upp_ci_bin" = 0
                )

for (cc in 1:length(countries_sub)){
  reg = countries_sub[cc]
  tmp = d[d$Country == reg,]
  k = match(reg, countries)
  
  ic = apply(estimated.deaths[,,k], 2, emp.hpd)
  df$est_deaths[df$country == reg] = colMeans(estimated.deaths[,,k])
  df$low_ci[df$country == reg] = ic[1,]
  df$upp_ci[df$country == reg] = ic[2,]
  
  nbin = apply(estimated.deaths[,,k], 2, function(x) rnbinom(1000, out$psi, 1-x/(out$psi+x)) )
  ic_nbin = apply(nbin, 2, emp.hpd, 0.85)
  df$n_bin[df$country == reg] = colMeans(nbin)
  df$low_ci_bin[df$country == reg] = ic_nbin[1,]
  df$upp_ci_bin[df$country == reg] = ic_nbin[2,]
  
}

df$country <- factor(df$country, levels = countries_sub)

ggplot(df) + 
  geom_line(aes(x = date, y = deaths), size=0.5, alpha=0.3) +#, col = pale[cc]) +
  geom_point(aes(x = date, y = deaths), size = 2) +#, col = pale[cc]) +
  geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci, ymax = upp_ci),
              stat = "identity", size = 0.7, fill = "blue", alpha = 0.2) +
  geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci_bin, ymax = upp_ci_bin),
              stat = "identity", size = 0.7) +
  # geom_line(aes(x = date, y = est_deaths), size = 0.4, col = pale[cc]) +
  theme_classic(base_size = 22) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.1,0.15),
        plot.title = element_text(lineheight=.8, face="bold")) +
  facet_wrap(. ~ country, ncol = 3, scales = 'free_y') +
  xlab("") +
  ylab("") +
  xlab("Date") +
  ylab("Daily Deaths") +
  # ggtitle("Daily death count and model estimate in italian regions") +
  #scale_color_manual(values = pal) +
  scale_x_date(date_breaks = "1 month" , date_labels = "%d %b")
  


for(cc in 1:length(countries_sub)){
  reg = countries_sub[cc]
  tmp = d[d$Country == reg,]
  k = match(reg, countries)
  
  ic = apply(estimated.deaths[,,k], 2, emp.hpd, 0.85)
  est = colMeans(estimated.deaths[,,k])
  nbin = apply(estimated.deaths[,,k], 2, function(x) rnbinom(700, out$psi, 1-x/(out$psi+x)) )
  ic_nbin = apply(nbin, 2, emp.hpd)
  
  # df = data.frame("date" = c(tmp$date, max(tmp$date) + c(1 : (N2 - length(tmp$date))) ),
  #                 "deaths" = c(tmp$Deaths, rep(NA, N2-length(tmp$date))),
  #                 "est_deaths" = colMeans(estimated.deaths[,,cc]),
  #                 "low_ci" = ic[1,],
  #                 "upp_ci" = ic[2,])
  df = data.frame("date" = tmp$date,
                  "deaths" = tmp$Deaths,
                  "est_deaths" = est,
                  "low_ci" = ic[1,],
                  "upp_ci" = ic[2,],
                  "nbin" = colMeans(nbin),
                  "low_ci_bin" = ic_nbin[1,],
                  "upp_ci_bin" = ic_nbin[2,]
                  )
  
  print(
    ggplot(df) + 
      geom_line(aes(x = date, y = deaths), size=0.5, alpha=0.3) +#, col = pale[cc]) +
      geom_point(aes(x = date, y = deaths), size = 2) +#, col = pale[cc]) +
      geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci, ymax = upp_ci),
                  stat = "identity", size = 0.7, fill = "blue", alpha = 0.2) +
      geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci_bin, ymax = upp_ci_bin),
                  stat = "identity", size = 0.7) +
      # geom_line(aes(x = date, y = est_deaths), size = 0.4, col = pale[cc]) +
      theme_classic(base_size = 22) +
      theme(axis.line.y.left = element_line(size=0.5),
            axis.line.x.bottom = element_line(size=0.5),
            legend.position = c(0.1,0.15),
            plot.title = element_text(lineheight=.8, face="bold")) +
      xlab("Date") +
      ylab("Daily Deaths") +
      scale_x_date(date_breaks = "1 month" , date_labels = "%d %b") #+
  )
}
#------------# #------------# #------------# #------------#
#------------# Modello Italia #------------# #------------#
nr = length(unique(d$date))
est_ita <- numeric(nr)
ic_ita <- matrix(NA, nr, 2)
ic_ita_bin <- matrix(NA, nr, 2)
nbin <- NULL

for(i in 1:nr){
  est_ita[i] = sum(colMeans(estimated.deaths[,i,]))
  ic_ita[i,] = emp.hpd(apply(estimated.deaths[,i,], 1, sum))
  
  nbin = rnbinom(700, out$psi, 1- rowSums(estimated.deaths[,i,])/(out$psi + rowSums(estimated.deaths[,i,]))) 
  ic_ita_bin[i,] = emp.hpd(nbin, conf = 0.85)
}

df = data.frame("date" = unique(d$date),
                "deaths" = tapply(d$Deaths, d$date, sum),
                "est_deaths" = est_ita,
                "low_ci" = ic_ita[,1],
                "upp_ci" = ic_ita[,2],
                "low_ci_n" = ic_ita_bin[,1],
                "upp_ci_n" = ic_ita_bin[,2]
)

ggplot(df) + 
  geom_line(aes(x = date, y = deaths), size=0.5, alpha=0.3) +#, col = pale[cc]) +
  geom_point(aes(x = date, y = deaths), size = 2) +#, col = pale[cc]) +
  geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci, ymax = upp_ci),
              stat = "identity", size = 0.7, fill = "blue", alpha = 0.2) +
  geom_line(aes(x = date, y = est_deaths), size = 0.4, col = "dodgerblue4") +
  geom_smooth(aes(x = date, y = est_deaths, ymin = low_ci_n, ymax = upp_ci_n),
              stat = "identity", size = 0.7) +
  theme_classic(base_size = 22) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.1,0.15),
        plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("Date") +
  ylab("Deaths") +
  # ggtitle("Daily death observations and model estimate for Italy") +
  # scale_y_log10() +
  scale_x_date(date_breaks = "1 month" , date_labels = "%d %b") 

#------------# Effetto della mobilità: coefficienti #------------#
covs = colnames(covariates)[c(3:5)]

hpd = matrix(NA, length(covs), 2)
for (i in c(1:length(covs))){hpd[i,] <- emp.hpd(out$beta[,i])}


betas_1 = data.frame("beta" = mean(out$beta[,1]),
                     "lo" = hpd[1,1],
                     "hi" = hpd[1,2],
                     "covariates" = covs[1]
)
betas_2 = data.frame("beta" = mean(out$beta[,2]),
                     "lo" = hpd[2,1],
                     "hi" = hpd[2,2],
                     "covariates" = covs[2]
)
betas_3 = data.frame("beta" = mean(out$beta[,3]),
                     "lo" = hpd[3,1],
                     "hi" = hpd[3,2],
                     "covariates" = covs[3]
)


betas = rbind(betas_1, betas_2, betas_3)
betas = betas_2

ggplot(betas, aes(y = beta, x = covariates, 
                  ymin = lo, ymax = hi)) +
  geom_pointrange(col= "dodgerblue4") + 
  coord_flip() +
  # facet_grid(. ~ covariates) +
  # scale_y_continuous(limits = colMeans(out$alpha)[1] + c(-2,2)) +
  theme_classic(base_size = 22) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.1,0.15),
        plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, size=0.5, alpha=0.6) #+
  # geom_hline(aes(yintercept = alpha), size=0.5, alpha=0.6, col= "dodgerblue4", linetype='dashed')

#------------# #------------# #------------# #------------#





#------------# Effetto della mobilità: R_tm #------------#
par(mfrow=c(2,3))
df_r = data.frame("date" = d$date[d$Country %in% countries], "Country" = as.character(d$Country[d$Country %in% countries]), "R" = NA)

for(cc in 1:length(countries)){
  reg = countries[cc]
  #plot(1:length(colMeans(out$Rt[,,cc])), colMeans(out$Rt[,,cc]), type = "l", ylim = c(0.5,4), main = reg)
  df_r$R[df_r$Country == reg] = colMeans(out$Rt[,,cc])[1:nrow(df_r[df_r$Country == reg,])]
}

df_r$Country = factor(df_r$Country, levels = countries)

#pal <- wes_palette("GrandBudapest1", 3)
# pal = c("#d27e1a", "#e79738",# "#eeaf65",
#         "#fa4d51", "#fc7e81", #"#feb0b1")
#         "#501716", "#792220")
# 
# pal = c("#29777c", "#318894",
#         "#4a3aaf" , "#3e3091",
#         "#339933", "#52ac39")
pal = pale

ggplot(df_r, aes(x = date, y = R, col = Country )) + 
  geom_line() + 
  geom_point(size = 0.7) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank()) +
  xlab("Date") +
  ylab(expression(R[t])) +
  scale_x_date(date_breaks = "2 weeks" , date_labels = "%d %b") #+
  # scale_color_manual(values = pale)




#------------# #------------# #------------# #------------#
reg = "Lombardia"

df_rv = df_r[df_r$Country==reg,]
cov_v = covariates[covariates$Country == reg,]
# plot(df_rv$date, cov_v$retail_travel, type = "l", ylim = c(-1,0))
# abline(v = min(cov_v$date[cov_v$stay_home==1]), lty = 3 )

new_ret_tr05 = cov_v$retail_travel
new_ret_tr05[new_ret_tr05< (-0.5)] = -0.5

new_ret_tr09 = cov_v$retail_travel
new_ret_tr09[new_ret_tr09< (-0.7)] = -0.9
new_ret_tr09[50:length(new_ret_tr09)] = -0.9

new_ret_5g = c(cov_v$retail_travel[5:length(cov_v$retail_travel)],
               rep(cov_v$retail_travel[length(cov_v$retail_travel)], 4))

new_ret_tr_aumento = cov_v$retail_travel
new_ret_tr_aumento[50:92] = -0.5


# change covariate values and see the number of deaths
retail_new_deaths <- function(new_retail)
{
  df_rv = df_r[df_r$Country==reg,]
  cov_v = covariates[covariates$Country == reg,]
  
  beta_v = out$beta[,]
  mu_v = out$mu[,match(reg, countries)]
  R_vt = out$Rt[,,match(reg, countries)]
  cov_v$retail_travel = new_retail
  
  R_vt_newcov = matrix(0, nrow(R_vt), nrow(cov_v))
  
  for(i in 1:ncol(R_vt_newcov))
  {
    R_vt_newcov[,i] <- mu_v * 2 * c(inv.logit( as.matrix(cov_v[i,c(3:5)]) %*% t(beta_v) ))
  }
  
  conv_v = numeric(ncol(R_vt_newcov))
  
  pred_v2 = matrix(NA, nrow(R_vt_newcov), ncol(R_vt_newcov))
  pred_v2[,1:stan_data$N0] = out$y[,match(reg, countries)]
  
  for(i in (stan_data$N0+1):(stan_data$N[match(reg,countries)])) 
  {
    convolution = rep(0, nrow(pred_v2))
    for(j in 1:(i-1)) 
    {
      convolution = convolution + pred_v2[,j] * stan_data$SI[i-j]
    }
    pred_v2[,i] = R_vt_newcov[,i] * convolution
  }
  
  e_deaths_v = matrix(NA, nrow(R_vt_newcov), ncol(R_vt_newcov))
  e_deaths_v[,1]= 1e-9;
  
  for (i in 2:ncol(R_vt_newcov))
  {
    e_deaths_v[,i] = 0
    for(j in 1:(i-1))
    {
      e_deaths_v[,i] = e_deaths_v[,i] + pred_v2[,j] * stan_data$f[i-j,match(reg, countries)];
    }
  }
  return("newdeaths" = e_deaths_v)
}

deaths_newcov05 = retail_new_deaths(new_ret_tr05)
deaths_newcov09 = retail_new_deaths(new_ret_tr09)
deaths_newcov5g = retail_new_deaths(new_ret_5g)
deaths_newcov_a = retail_new_deaths(new_ret_tr_aumento)

df_deaths = data.frame("date" = d$date[d$Country == reg], 
                       "deaths_obs" =  d$Deaths[d$Country == reg],
                       "deaths_original" = colMeans(out$E_deaths[,,match(reg,countries)])[1:length(d$date[d$Country == reg])],
                       "deaths_newcov05" = colMeans(deaths_newcov05),
                       "deaths_newcov09" = colMeans(deaths_newcov09),
                       "deaths_newcov5g" = colMeans(deaths_newcov5g),
                       "deaths_newcov_a" = colMeans(deaths_newcov_a))

pal = pale
ggplot(df_deaths) +
  geom_line(aes(x = date, y = deaths_obs), alpha = 0.3) +
  geom_point(aes(x = date, y = deaths_obs), size = 1.2, alpha = 0.7) +
  theme_classic(base_size = 22) +
  theme(axis.line.y.left = element_line(size=0.5),
        axis.line.x.bottom = element_line(size=0.5),
        legend.position = c(0.15,0.8),
        plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("Date") +
  ylab("Daily deaths") +
  ggtitle(paste0("Effect of Retail and Transit mobility on deaths in ", reg)) +
  ylim(c(0,600)) +
  scale_x_date(date_breaks = "1 month" , date_labels = "%d %b") +
  scale_color_manual("Retail and transit mobility", values = pale, labels = c("Original model (~80% reduction)", "Original, 5 days advance", "Early partial reopening", "50% reduction", "90% reduction") ) +
  scale_fill_manual(values = pale) +
  ## covar1
  geom_line(aes(x = date, y = deaths_newcov_a, col = pale[3]), size=1.5) +
  ## covar2
  geom_line(aes(x = date, y = deaths_newcov05, col = pale[5]), size=1.5) +
  ## covar3
  geom_line(aes(x = date, y = deaths_newcov5g, col = pale[2]), size=1.5) +
  ## covar4
  geom_line(aes(x = date, y = deaths_newcov09, col = pale[4]), size=1.5) +
  geom_line(aes(x = date, y = deaths_original, col = pale[1]), size=1.5) 

