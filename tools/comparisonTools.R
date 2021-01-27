library(htmltools)

# TODO: Consider moving shared functions to a common folder if they
# are to be used across multiple files.
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
    isCurrentMatch <- consensus[index] == alignment[index]
    isCurrentMismatch <- !isCurrentMatch

    if (isMatch) {
      isPreviousMatch <- consensus[index - 1] == alignment[index - 1];
      isConsecutiveMatch <- isPreviousMatch && isCurrentMatch
      
      return(isConsecutiveMatch)
    }
    
    isPreviousComparisonSame <-  alignment[index] == alignment[index - 1]
    isConsecutiveMismatch <- isPreviousComparisonSame && isCurrentMismatch
    
    return(isConsecutiveMismatch)
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

columnHtml <- function(innerHtml, className = "sequence-column") {
  return(div(innerHtml, class = className))
}

tally <- function(voters) {
  INDEL <- '-'
  ALPHABET <-  c('A', 'C', 'G', 'T', INDEL)
  
  # Initialize vote counts
  results <- list()
  for(a in ALPHABET) {
    results[[a]] <- 0
  }
  
  for(v in voters) {
    results[[v]] <- results[[v]] + 1
  }
  
  results <- unlist(results)
  
  # TODO: Implement tie-breaking logic.
  winner <- names(results)[results == max(results)]
  
  return(winner[1])
}

consensusBuilder <- function(alignments) {
  alignmentVectors <- lapply(alignments, splitToVector)  
  votingRounds <- max(unlist(lapply(alignmentVectors, length)))
  
  results <- c()
  
  for (idx in 1:votingRounds) {
    hasCharacter <- function(vec) { idx <= length(vec) }
    eligible <- sapply(alignmentVectors, hasCharacter)
    voters <- alignmentVectors[eligible]
    votes <- sapply(voters, FUN = function(voter) { voter[idx] })
    results <- c(results, tally(votes))
  }
  
  concatToVector <- function(v) { paste(v, collapse = '')}
  consensus <- concatToVector(results)
  
  return(consensus)
}

wraparoundHtml <- function(alignments) {
  consensus <- consensusBuilder(alignments)
  
  buildRows <- function(alignment) {
    endpoints <- delimit(alignment, consensus)
    comparisonRegions <- defineSegments(endpoints)

    return(rowHtml(comparisonRegions, alignment, consensus))
  }

  return(columnHtml(lapply(alignments, buildRows)))
}
