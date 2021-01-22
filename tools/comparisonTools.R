library(htmltools)

splitToVector <- function(s) { unlist(strsplit(s, '')) }

delimit <- function(alignment, consensus) {
  alignment <- splitToVector(alignment)
  consensus <- splitToVector(consensus)

  isContinuation <- function(isMatch, index) {
    if (index == 1) {
      return(isMatch)
    }
    
    # Beyond here we check if the current comparison is consistent with
    # the preceding comparison.
    isCurrentMatch <- function() { consensus[index] == alignment[index] }
    isCurrentMismatch <- function() { !isCurrentMatch() }

    if (isMatch) {
      isPreviousMatch <- consensus[index - 1] == alignment[index - 1];
      isConsecutiveMatch <- function() { isPreviousMatch && isCurrentMatch() }
      
      return(isConsecutiveMatch())
    }
    
    isPreviousComparisonSame <-  alignment[index] == alignment[index - 1]
    isConsecutiveMismatch <- function() { isPreviousComparisonSame && isCurrentMismatch() }
    
    return(isConsecutiveMismatch())
  }
    
  limit <- min(length(consensus), length(alignment));
  results <- list()
  
  i <- 1
  while(i <= limit + 1) {
    isMatch <- consensus[i] == alignment[i]
    
    while (i <= limit && isContinuation(isMatch, i)) {
      i <- i + 1
    }

    results[i] <- i
    i <- i + 1
  }
  
  return(unlist(results))
}

defineSegments <- function(endpoints) {
  regions <- list()
  start <- 1

  
  for(idx in 1:length(endpoints)) {
    end <- endpoints[idx]
    regions[[idx]] <- list(start = start, end = end)
    start <- end;
  }
  
  return(regions)
}

rowHtml <- function(comparisonRegions, alignment, consensus) {

  segmentHtml <- function(region) {
    start <- region$start
    end <- region$end
    conSeg <- substring(consensus, start, end - 1)
    alSeg <- substring(alignment, start, end - 1)

    className <- if(all(conSeg == alSeg)) {
      'match'
    } else {
      indel <- '-'
      alignmentCharacter <- splitToVector(alSeg)[1]
      paste(c('mismatch-', ifelse(alignmentCharacter == indel, 'indel', alignmentCharacter)),
            collapse = '')
    }
    
    return(span(alSeg, class = className))
  }

  spans <- lapply(comparisonRegions, segmentHtml)

  return(div(spans, class = 'alignment-row'))
}

columnHtml <- function(innerHtml, className) {
  return(div(innerHtml, class = className))
}

placeholder <- function() {
  print('calling the placeholder method')
  consensus <- 'AAAACCCCGGGGTTTTAAA'
  alignment <- 'CCCCACTGACTGACTGAAC'
  endpoints <- delimit(alignment, consensus)
  comparisonRegions <- defineSegments(endpoints)
  innerHtml <- rowHtml(comparisonRegions, alignment, consensus)
  column <- columnHtml(innerHtml, "sequence-column")
  
  return(column)
}
