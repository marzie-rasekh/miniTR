source("tools/helpers.R")

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
  
  consensus <- joinToString(results)
  
  return(consensus)
}
