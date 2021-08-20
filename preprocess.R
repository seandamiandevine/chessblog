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
  # Machine-readable time (date and time)
  raw$combinedTime = paste0(raw$UTCDate, ' ', raw$UTCTime)
  
  # List of unique players (whether white or black)
  uniqueUsers = unique(c(as.character(raw$White), as.character(raw$Black)))
  
  # Initialize output dataframe
  if(is.null(cols)){
    cols = c('User', 'gameNum', 'ELO', 'White', 'Black', 'Result', 'WhiteElo', 'BlackElo', 
             'time_elapsed', 'uOutcome', 'combinedTime')
  }
 
  dat = list()
  for (user in uniqueUsers){
    # keep track (this can take awhile with thousands of players...)
    progress=match(user, uniqueUsers)/length(uniqueUsers)
    cat('data parsing for user', match(user, uniqueUsers), '/', length(uniqueUsers), '\n')
   
    # if(round(progress) %in% seq(.1,1,by=.1)){
    #   cat('data parsing is', round(progress)*10, '% complete.\n') 
    # }
    
    # iterate through players 
    thisUser=raw[raw$White==user | raw$Black==user, ]
    thisUser = thisUser[!is.na(thisUser$Site), ]                   # remove blank cells
    if(nrow(thisUser)==1) next                                     # skip players who only played once 
    
    thisUser=thisUser[order(thisUser$UTCDate, thisUser$UTCTime), ] # sort by date and time
    thisUser$User = user
    thisUser$gameNum = 1:nrow(thisUser)
    thisUser$ELO = ifelse(thisUser[1,'White']==user, thisUser[1,'WhiteElo'], thisUser[1,'BlackElo'] )
    
    # compute time between games and outcomes
    this_time = as.POSIXct(thisUser$combinedTime, format='%Y.%m.%d %H:%M:%S', tz='GMC')
    last_time = dplyr::lag(this_time)
    thisUser$time_elapsed = difftime(this_time,last_time, units='min')
    
    # compute result for this user
    isWhite = ifelse(thisUser$White==user,1,0)
    thisUser$uOutcome = ifelse(thisUser$Result=='1/2-1/2', 0.5, 
                               ifelse(isWhite==1 & thisUser$Result=='1-0', 1, 
                                      ifelse(isWhite==0 & thisUser$Result=='0-1', 1, 
                                             0)))
    
    dat[[user]] = thisUser[,cols]
  }
  
  cat('formatting output...\n')
  out = do.call(rbind.data.frame, dat)
  rownames(out) = NULL
  
  if(writeOut) write.csv(out, paste0(paste0(outdir, '/'), gsub('.pgn', '_processed.csv', filename)))
  return(out)
}
