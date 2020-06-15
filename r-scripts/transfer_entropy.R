# install.packages("RTransferEntropy")
# install.packages("VLTimeCausality")
# install.packages("COVID19")
# install.packages("future")

library(RTransferEntropy)
library(VLTimeCausality)
library(COVID19)
library(future)
library(reshape2)
library(ggplot2)
library(stats)
library(vars)

plan(multiprocess)

cup = rgb(red=68, green=34, blue=136, alpha=255, maxColorValue = 255)
clo = 'grey'
cmi = rgb(red=108, green=162, blue=234, alpha=255, maxColorValue = 255)


Linear.GC <- function(X, Y, lag=1){
    n <- length(X)
    dfX <- data.frame(c(NA, X[1:(n-1)]))
    dfY <- data.frame(c(NA, Y[1:(n-1)]))
    if (lag > 1){
        for (i in 2:lag){
            dfX <- cbind(dfX, c(rep(NA, i), X[1:(n-i)]))
            dfY <- cbind(dfY, c(rep(NA, i), Y[1:(n-i)]))
        }
    }
    colnames(dfY) <- 1:lag
    colnames(dfX) <- (lag+1):(2*lag)
    
    regression.uni <- lm(Y ~ ., data=cbind(Y, dfY))
    regression.mul <- lm(Y ~ ., data=cbind(Y, dfY, dfX))
    
    var.eps.uni <- (summary(regression.uni)$sigma)^2
    var.eps.mul <- (summary(regression.mul)$sigma)^2
    
    GC <- log(var.eps.uni / var.eps.mul)
    return(GC)
}

Test.GC <- function(X, Y, lag=1){
    res <- VLGrangerFunc(Y, X, maxLag = lag)
    return(res$BICDiffRatio)
}

Shannon.TE <- function(X, Y, lag=1){
    TE <- calc_ete(X, Y, lx=lag, ly=lag)
    return(TE)
}

FApply.Pairwise <- function(X, D.Func, lag=1){
    n = seq_len(ncol(X))
    
    ff.TE.value = function(a, b) {
        if (a == b){
            ret = NA
        } else {
            ret = D.Func(X[,a], X[,b], lag)
        }
        return(ret)
    }
    return(outer(n, n, Vectorize(ff.TE.value)))
}

plot.matrix <- function(matrix, title){
    matrix[upper.tri(matrix)] <- NA
    # matrix[matrix < 0.001] <- 0
    melted <- melt(matrix)
    names(melted) <- c('Cause', 'Effect', 'Causality')
    mx <- max(matrix, na.rm = TRUE)
    mn <- min(matrix, na.rm = TRUE)
    md <- (mx + mn) / 2
    cat(mn, '\t', md, '\t', mx, '\n')
    ggplot(data = melted, aes(x=Cause, y=Effect, fill=Causality)) + 
        geom_tile(color='white') +
        scale_fill_gradient2(low = clo, high = cup, mid = cmi, 
                             midpoint = md, limit = c(mn,mx), space = "Lab", 
                             na.value="transparent",
                             name=title) +
        theme_classic(base_size = 24) +
        theme(axis.line.y.left = element_line(size=0.5),
              axis.line.x.bottom = element_line(size=0.5),
              axis.text.x = element_text(angle = 45, vjust = 1, 
                                         size = 12, hjust = 1)) +
        coord_fixed()
}

############################################### 
# LOAD DATA
############################################### 
d = covid19("ITA", verbose = FALSE)
d = data.frame("date" = as.Date(d$date), 
               "Deaths" = c(NA, diff(d$deaths, 1)),
               "Cases" = c(NA, diff(d$confirmed, 1))
)

covar = utils::read.csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', 
                        stringsAsFactors = FALSE)
covar = covar[covar$country_region == 'Italy' & covar$sub_region_1 == "", ]
covariates = data.frame("date" = as.Date(covar$date),
                   "Retail_Transit" = (covar$retail_and_recreation_percent_change_from_baseline +
                                   covar$transit_stations_percent_change_from_baseline) / 2,
                   "Workplace" = covar$workplaces_percent_change_from_baseline,
                   "Residential" = covar$residential_percent_change_from_baseline)

d <- d[d$date >= min(covariates$date) & d$date <= max(covariates$date), ]

dat <- merge(d, covariates)

############################################### 
# DATA CLEANING (DESEASONALIZATION)
############################################### 
dat_ts <- ts(dat[,-c(1)], frequency = 7, start = dat$date[1])
dat_dec <- decompose(dat_ts)
dat_trend <- data.frame(dat_dec$trend)
colnames(dat_trend) <- colnames(dat)[-c(1)]

dat_cl <- scale(dat_trend)[complete.cases(dat_trend),]
dat_cl <- dat_cl[c(1:75),]

plotTimeSeries(dat_cl[,'Retail_Transit'], dat_cl[,'Deaths'])
plotTimeSeries(dat_cl[,'Retail_Transit'], dat_cl[,'Cases'])
plotTimeSeries(dat_cl[,'Workplace'], dat_cl[,'Deaths'])
plotTimeSeries(dat_cl[,'Residential'], dat_cl[,'Deaths'])
plotTimeSeries(dat_cl[,'Workplace'], dat_cl[,'Retail_Transit'])

############################################### 
# CAUSALITY MEASURES
############################################### 
GC.matrix <- FApply.Pairwise(dat_cl, Test.GC, lag=14)
rownames(GC.matrix) <- colnames(GC.matrix) <- colnames(dat_cl)
plot.matrix(GC.matrix, title = 'BIC\nratio')

# GC.matrix.7 <- FApply.Pairwise(dat, Linear.GC, 7)
# rownames(GC.matrix.7) <- colnames(GC.matrix.7) <- c('covid', names(mobility_data[,-c(1)]))
# plot_matrix(GC.matrix.7)
# 
# GC.matrix.14 <- FApply.Pairwise(dat, Linear.GC, 14)
# rownames(GC.matrix.14) <- colnames(GC.matrix.14) <- c('covid', names(mobility_data[,-c(1)]))
# plot_matrix(GC.matrix.14)

TE.matrix <- FApply.Pairwise(dat_cl, Shannon.TE, lag=2)
rownames(TE.matrix) <- colnames(TE.matrix) <- colnames(dat_cl)
TE.flux <- TE.matrix - t(TE.matrix)
plot.matrix(TE.flux, title = 'Transfer\nEntropy')
