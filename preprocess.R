preprocess <- function(file, writeIn=F, writeOut=F, outdir='', cols=NULL, nIter=200, nLines=NULL) {
 # Loads in pgn file with bigchess and
 # computes variables needed for the analysis
  
  source('parsePGN.R')
 
  # check if dplyr is installed (for lag function)
  if(!'dplyr' %in% rownames(installed.packages())) {
    stop('R package "dplyr" is required to process the data.\nInstall with install.packages("dplyr") to continue.')
  }
  
  # Load in the raw pgn data
  filename = tail(strsplit(file, '/')[[1]],1)
  
  cat('reading number of lines in raw PGN\n')
  if(is.null(nLines)) nLines = as.numeric(gsub(file, '', system(paste('wc -l', file), intern = T)))
  cat('raw PGN has', nLines, 'lines.\n')
  iterSize = round(nLines/nIter)
  iterSeq = seq(1, nLines, by=iterSize)
  if(tail(iterSeq,1) != nLines) iterSeq[length(iterSeq)] = nLines # expand/shrink the last batch 
  
  raw = data.frame(stringsAsFactors = F)
  for(start in iterSeq[-length(iterSeq)]) {
    con  <- file(file, open = "r")
    idx = match(start, iterSeq)
    n = iterSeq[idx+1]-start
    thisraw = scan(con, what=character(), n=n, skip=start-1, sep='\n', quiet = T)
    chunk = parsePGN(thisraw, cols=cols)
    raw = rbind(raw, chunk)
    cat('read in batch', match(start, iterSeq), '/', length(iterSeq), '\n')
    close(con)
  }
  # Remove duplicate games 
  raw = raw[!duplicated(raw$Site), ]
  cat('imported', nrow(raw), 'games.\n')
  if(writeIn) write.csv(raw, paste0(outdir, '/', gsub('.pgn', '.csv', filename)))
    
  cat('Computing useful variables...\n')
  
  # Compute useful variables
  if(is.null(cols)){
    cols = c('User', 'gameNum', 'ELO', 'White', 'Black', 'Result', 'WhiteElo', 'BlackElo', 
             'time_elapsed', 'uOutcome', 'combinedTime', 'TimeControl', 'Site')
  }
  
  # Machine-readable time (date and time)
  raw$combinedTime = paste0(raw$UTCDate, ' ', raw$UTCTime)
  
  # games users played as white
  white = raw
  white$User = white$White
  
  # games user played as black = raw
  black = raw
  black$User = black$Black
  
  # combine by user and order by user and time
  dat = rbind(white, black)
  dat = dat[order(dat$User, dat$combinedTime), ]
  
  # append useful variables
  # number of games
  dat$gameNum = ave(dat$X, dat$User, FUN=function(x) 1:length(x))
  
  # user ELO
  dat$ELO = ifelse(dat$User==dat$White, dat$WhiteElo, dat$BlackElo)
  
  # time between games
  this_time = as.POSIXct(dat$combinedTime, format='%Y.%m.%d %H:%M:%S', tz='GMC')
  last_time = dplyr::lag(this_time)
  dat$time_elapsed = as.numeric(difftime(this_time,last_time, units='min'))
  dat$time_elapsed[dat$gameNum==1] = NA # no last game prior to recordings
  
  # result relative to that user
  isWhite = ifelse(dat$White==dat$User,1,0)
  dat$uOutcome = ifelse(dat$Result=='1/2-1/2', 0.5, 
                             ifelse(isWhite==1 & dat$Result=='1-0', 1, 
                                    ifelse(isWhite==0 & dat$Result=='0-1', 1, 
                                           0)))
  # only take columns of interest
  dat = dat[,cols]
  
  # remove users with only one game
  ngames = table(dat$User)
  dat = dat[!dat$User %in% names(ngames)[ngames==1], ]
  
  # x = dat[dat$User == unique(dat$User)[103], ] # check for one user if debugging
  
  if(writeOut) write.csv(dat, paste0(paste0(outdir, '/'), gsub('.pgn', '_processed.csv', filename)))
  return(dat)
}
