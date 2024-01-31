preprocess <- function(file, writeIn=F, writeOut=F, outdir='', cols=NULL, nIter=200, nLines=NULL) {
 # Loads in pgn file with bigchess and
 # computes variables needed for the analysis
  
  source('parsePGN.R')
 
  # check if dplyr is installed (for lag function)
  if(!'dplyr' %in% rownames(installed.packages())) {
    stop('R package "dplyr" is required to process the data.\nInstall with install.packages("dplyr") to continue.')
  }
  
  # Load in the raw pgn data
  filename = strsplit(file, '/')[[1]][2]
  
  cat('reading number of lines in raw PGN\n')
  if(is.null(nLines)) nLines = as.numeric(gsub(file, '', system(paste('wc -l', file), intern = T))) # count rows with wc -l
  cat('raw PGN has', nLines, 'lines.\n')
  # determine how to iterate through rows
  iterSize = round(nLines/nIter)
  iterSeq = seq(1, nLines, by=iterSize)
  # expand/shrink the last batch 
  if(tail(iterSeq,1) != nLines) {
    iterSeq[length(iterSeq)] = nLines
    }
  
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
  # Remove NA games
  raw = raw[complete.cases(raw), ]
  # Remove duplicate games 
  raw = raw[!duplicated(raw$Site), ]
  cat('imported', nrow(raw), 'games.\n')
  if(writeIn) write.csv(raw, paste0(outdir, '/', gsub('.pgn', '.csv', filename)))
    
  cat('Computing useful variables...\n')
  
  # Compute useful variables
  # base time and increment
  raw <- raw %>% 
    filter(TimeControl != "-") %>% # first, remove rows that don't have time-control
    tidyr::separate_wider_delim(cols = TimeControl, 
                                             delim = "+", 
                                             names = c("TimeBase", "TimeInc"),
                                             too_few = "align_start",
                                             cols_remove = FALSE) # keep TimeControl variable
  
  # white wins and black wins columns
  raw <- raw %>% 
    tidyr::separate_wider_delim(cols = Result,
                                      delim = "-",
                                      names = c("WhiteWins", "BlackWins"),
                                      cols_remove = FALSE) %>% 
    mutate(WhiteWins = ifelse(WhiteWins == "1/2", 0.5, WhiteWins),
           BlackWins = ifelse(BlackWins == "1/2", 0.5, BlackWins))
  
  # adjust column types
  raw$WhiteWins <- as.numeric(raw$WhiteWins)
  raw$BlackWins <- as.numeric(raw$BlackWins)
  raw$WhiteElo <- as.numeric(raw$WhiteElo)
  raw$BlackElo <- as.numeric(raw$BlackElo)
  raw$TimeBase <- as.numeric(raw$TimeBase)
  raw$TimeInc <- as.numeric(raw$TimeInc)
  
  if(writeOut) write.csv(raw, paste0(paste0(outdir, '/'), gsub('.pgn', '_processed.csv', filename)))
  return(raw)
}
