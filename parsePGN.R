parsePGN <- function(pgn,cols=NULL) {
  
  if(is.null(cols)) cols = c("Event", "Site", "White", "Black", "Result", "UTCDate", "UTCTime", 
                             "WhiteElo", "BlackElo", "ECO", "Opening", "TimeControl", "Termination",
                             "Movetext")
  
  reg <- "^\\[([\\S]+)\\s\"([\\S\\s]+|\\B)\"\\]$"
  tag = gsub(reg, '\\1', pgn, perl=T)
  tag[startsWith(tag, '1. ')] = 'Movetext'
  content = gsub(reg, '\\2', pgn, perl=T)
  
  content=content[tag%in%cols]
  tag = tag[tag%in%cols]
  
  out = list()
  for(t in unique(tag)) {
    thisContent = content[tag==t]
    out[[t]] <- thisContent
  }
  
  # pad missing with NA in the case of incomplete PGN
  maxL = max(sapply(out, length))
  
  for(t in names(out)){
    if(length(out[[t]]) < maxL) out[[t]] <- append(out[[t]], rep(x = NA, times = maxL - length(out[[t]])))
  }
  
  out = data.frame(out)
  out = out[,cols] # order columns
  
  return(out)
  
}