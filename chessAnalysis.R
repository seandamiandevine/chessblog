
# Load --------------------------------------------------------------------

source('preprocess.R')

dat <- preprocess('data/pgn/lichess_db_standard_rated_2014-07.pgn', writeIn = T,
                  writeOut = T, outdir = 'data/csv', nIter = 10)

# dat <- read.csv('data/csv/lichess_db_standard_rated_2013-01_processed.csv', stringsAsFactors = F)


# Prepare data ------------------------------------------------------------

dat_nodraw = dat[dat$uOutcome!=0.5, ]                         # no draws for now
dat_nodraw$last_uOutcome = dplyr::lag(dat_nodraw$uOutcome)    # last game played...
dat_nodraw$last_uOutcome = factor(dat_nodraw$last_uOutcome)   # ... as a factor
dat_nodraw$last_uOutcome[dat_nodraw$gameNum==1] = NA          # first recorded game by this user
dat_nodraw = dat_nodraw[!is.na(dat_nodraw$ELO),]              # skip mystery players
dat_nodraw$ELO_c = dat_nodraw$ELO-mean(dat_nodraw$ELO)        # center ELO 
dat_nodraw$time_elapsed_c = dat_nodraw$time_elapsed-
  mean(dat_nodraw$time_elapsed, na.rm=T)                      # center time_elapsed
contrasts(dat_nodraw$last_uOutcome) = -contr.sum(2)
dat_nodraw$RatingDiff = ifelse(dat_nodraw$White==dat_nodraw$User, 
                               dat_nodraw$WhiteElo-dat_nodraw$BlackElo, 
                               dat_nodraw$BlackElo-dat_nodraw$WhiteElo)


nGames = nrow(dat_nodraw[dat_nodraw$])

# GLMM on sequence effects --------------------------------------------------

library(lme4)

linMod = glm(uOutcome ~ last_uOutcome, data=dat_nodraw, family='binomial') # just to see
mlm0 = glmer(uOutcome ~ 1 + (1|User), data=dat_nodraw[!is.na(dat_nodraw$last_uOutcome),], family='binomial', nAGQ = 0)
mlm1 = glmer(uOutcome ~ last_uOutcome + (1|User), data=dat_nodraw, family='binomial', nAGQ=0)
mlm2 = glmer(uOutcome ~ last_uOutcome + RatingDiff + (1|User), data=dat_nodraw, family='binomial', nAGQ=0)
#mlm3 = glmer(uOutcome ~ last_uOutcome * RatingDiff + (1|User), data=dat_nodraw, family='binomial', nAGQ=0)
mlm3 = glmer(uOutcome ~ last_uOutcome + RatingDiff + ELO_c + (1|User), data=dat_nodraw, family='binomial', nAGQ=0)
mlm4 =  glmer(uOutcome ~ last_uOutcome*time_elapsed_c + RatingDiff + ELO_c + (1|User), data=dat_nodraw, family='binomial', nAGQ=0)
anova(mlm4, mlm3, mlm2, mlm1, mlm0)

# Plot sequence effects ---------------------------------------------------

# How does the last game influence the outcome of the current game?

pWin = tapply(dat_nodraw$uOutcome, dat_nodraw$last_uOutcome, mean, na.rm=T)
seWin =  tapply(dat_nodraw$uOutcome, dat_nodraw$last_uOutcome, plotrix::std.error, na.rm=T)
names(pWin) = c('Lose', 'Win')
thisbar = barplot(pWin, beside=T, xpd=F,
                  ylab = 'p(win current game)', xlab = 'Outcome of last game', 
                  main='The last game influences\nthe outcome of the current game',
                  ylim = c(.4,.6))

arrows(thisbar, pWin-seWin, thisbar, pWin+seWin, length=0)
g00 = mean(dat_nodraw$uOutcome)
abline(h=g00, col='red', lty='dashed') # outcome-independent average


# How far back does the influence of past games go?

# insepct distirbution of games played per player
# hist(aggregate(gameNum~User, dat_nodraw, max)[,2], xlab='max recorded games', main='')

# get regression coeffiencet k games back
k = 6 # num of games back + 1 (can be tweaked)
glm1 = glm(uOutcome~last_uOutcome,data=dat_nodraw,family = 'binomial')
intercepts=c(coef(glm1)[1])
b =c(coef(glm1)[2])
glms = list(glm1)
for(i in 1:k) {
  gamesAgo = dplyr::lag(dat_nodraw$last_uOutcome, i)
  gamesAgo = factor(gamesAgo)
  contrasts(gamesAgo)=-contr.sum(2)
  thisGlm = glm(dat_nodraw$uOutcome~gamesAgo, family='binomial')
  intercepts[length(intercepts)+1] = coef(thisGlm)[1]
  b[length(b)+1] = coef(thisGlm)[2]
  glms[[length(glms)+1]] = thisGlm
  }

plot(-(1:(k+1)), b , type='b', pch=16, ylim=range(pretty(b)), 
     xlab = 'games ago', ylab='regression coefficient', xaxt='n', 
     main='Up to 7 games ago\ninfluences the outcome of the\ncurrent game')
axis(1, at=-(1:(k+1)), labels = 1:(k+1))

# Does taking longer breaks help? 

# take median split (two most extremes)
timebin = dplyr::ntile(dat_nodraw$time_elapsed,3) 
extremes = dat_nodraw[timebin != 2,]
extremes$timebin = timebin[timebin!=2]
pWin = tapply(extremes$uOutcome, list(extremes$timebin, extremes$last_uOutcome), mean, na.rm=T)
seWin =  tapply(extremes$uOutcome,  list(extremes$timebin, extremes$last_uOutcome), plotrix::std.error, na.rm=T)
colnames(pWin) = c('Lose', 'Win')
thisbar = barplot(pWin, beside=T, xpd=F,
                  ylab = 'p(win current game)', xlab = 'Outcome of last game', 
                  main='Breaks help a little',
                  ylim = c(.35,.65), 
                  legend.text = T,
                  args.legend = list(x='topleft', bty='n',
                                     legend=c('Short', 'Long'), title='Time between games'))

arrows(thisbar, pWin-seWin, thisbar, pWin+seWin, length=0)
g00 = mean(dat_nodraw$uOutcome)
abline(h=g00, col='red', lty='dashed') # outcome-independent average

# Does experience decrease the tilt/hype?

elobin = Hmisc::cut2(dat_nodraw$ELO,g=5,levels.mean = T)
pWin = tapply(dat_nodraw$uOutcome, list(dat_nodraw$last_uOutcome, elobin), mean)

plot(pWin[1,], type='n',
     xlab='ELO', ylab='p(Win)', 
     xlim=range(pretty(as.numeric(colnames(pWin)))),
     ylim=range(pretty(pWin)), 
     main='Tilt & hype happen across ELO levels')
cols=c('red','green')
for(i in 1:nrow(pWin)){
  lines(as.numeric(colnames(pWin)), pWin[i,], col=cols[i])
}
legend('topleft', lty=1,col=cols,legend=c('Lost', 'Won'), title = 'Last Game')


# Rating difference and tilt/hype

diffbin = Hmisc::cut2(dat_nodraw$RatingDiff, g=3, levels.mean = T)
pWin = tapply(dat_nodraw$uOutcome, list(dat_nodraw$last_uOutcome, diffbin), mean)

plot(pWin[1,], type='n', xaxt='n',
     xlab='', ylab='p(Win)', 
     ylim=range(pretty(pWin)), 
     main='Tilt & hype still present\nacross Rating Differences')
axis(1, at=1:ncol(pWin), labels=c('Player is worse',
                                  'Players the same', 
                                  'Player is better'))
cols=c('red','green')
for(i in 1:nrow(pWin)){
  lines(pWin[i,], col=cols[i])
}
legend('topleft', lty=1,col=cols,legend=c('Lost', 'Won'), title = 'Last Game')

# Case example

datex=dat_nodraw[dat_nodraw$User=='800',] # biggest eb

rollmean = zoo::rollmean(datex$uOutcome, k=5)
plot(rollmean, type='l', xlab='Games Played', ylab='Rolling Win Rate (k=5)', xaxt='n', 
     main='Case example')
axis(1, at=seq(1,length(rollmean),by=20), labels=seq(1,length(rollmean),by=20)+5)


# Non-linear model --------------------------------------------------------------

# scale to be between -1 and 1 to not underflow floating point
dat_nodraw = dat_nodraw[!is.na(dat_nodraw$RatingDiff), ] # remove NAs
dat_nodraw$ELO0 = 1 - 2*((dat_nodraw$ELO - min(dat_nodraw$ELO))/(max(dat_nodraw$ELO) - min(dat_nodraw$ELO)))
dat_nodraw$RatingDiff0 = x=1 - 2*((dat_nodraw$RatingDiff - min(dat_nodraw$RatingDiff))/(max(dat_nodraw$RatingDiff) - min(dat_nodraw$RatingDiff)))

# fit model to each user and predict game outcomes using these params
source('model/fitthMod.R')
source('model/thMod.R')

fits = list()
predictions = list()
nSimpred = 1000 # number of samples to takes for predictions

for(id in unique(dat_nodraw$User)){
  cat('--------------USER:', 
      match(id, unique(dat_nodraw$User)), '/', length(unique(dat_nodraw$User)),
      '--------------\n')
  # fit
  thisUser = dat_nodraw[dat_nodraw$User==id,]
  thisFit = fitmod(ELO=thisUser$ELO0, 
                   diff=thisUser$RatingDiff0, 
                   outcomes = thisUser$uOutcome,
                   nIter=1000, verbose=F)
  fits[[id]] = thisFit
  
  # predict
  thisCP = thmod(params=thisFit$par, 
                 ELO=thisUser$ELO0, 
                 diff=thisUser$RatingDiff0, 
                 outcomes = thisUser$uOutcome,
                 toreturn = 'cp')
  cat('~simulating~\n')
  sim = replicate(sapply(thisCP, function(x) sample(c(1,0), prob=c(x, 1-x), size=1)), n=nSimpred)
  if(!is.matrix(sim)) sim=t(matrix(sim)) # for people with only two games
  predictions[[id]] = round(rowMeans(sim))
  
  cat('empirical:',sum(thisUser$uOutcome), '\n')
  cat('predicted', sum(predictions[[id]]), '\n')
  
}

saveRDS(fits, paste0('model/fits_',nrow(dat_nodraw),'.rds'))
saveRDS(predictions,  paste0('model/predictions_',nrow(dat_nodraw),'.rds'))


# Visualize model fits and predictions ----------------------------------------------------

# Show distribution of parameters
npar = length(fits[[1]]$par)

layout(matrix(1:(npar+1), ncol=2, byrow=T))
for(p in 1:length(fits[[1]]$par)) {
  thispar = sapply(fits, function(x) x$par[p])
  hist(thispar, xlab=names(fits[[1]]$par)[p], main=names(fits[[1]]$par)[p])
}

lambdas = thispar # assuming there are no later params
pLambdaAbove0 = sum(lambdas > 0)/length(lambdas)

# Visualize predictions
# empirical
x=dat_nodraw[dat_nodraw$User %in% names(fits),] # in case you're visualizing an incomplete dataset
pWin = tapply(x$uOutcome, x$last_uOutcome, mean, na.rm=T)
seWin =  tapply(x$uOutcome, x$last_uOutcome, plotrix::std.error, na.rm=T)
names(pWin) = c('Lose', 'Win')
thisbar = barplot(pWin, beside=T, xpd=F,
                  ylab = 'p(win current game)', xlab = 'Outcome of last game', 
                  main='Model predicts tilt & hype',
                  ylim=c(0,1))

arrows(thisbar, pWin-seWin, thisbar, pWin+seWin, length=0)
g00 = mean(x$uOutcome)
abline(h=g00, col='red', lty='dashed') # outcome-independent average

# simulated
simWin = unlist(predictions)
# simlast = dplyr::lag(simWin)       

simpWin = tapply(simWin, x$last_uOutcome[-1], mean)
lines(thisbar, simpWin, type='b', pch=18, col='red', cex=2)

legend('topleft', pch=18, lty=1, col='red', legend='Model predictions', pt.cex=2, bty='n')
