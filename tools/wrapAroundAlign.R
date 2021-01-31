#' Wrap around alignment
#'
#' This code aligns a sequence to a pattern and finds a tandem repeat with that functions
#' 
#' @param pattern The pattern of the repeat [character]
#' @param sequence The sequence to search for tandem repeats [character]
#' @param MATCH The match score, default 2 [numeric]
#' @param MISMATCH The mismatch penalty, should be negative, default -5 [numeric]
#' @param GAP The mismatch penalty, should be negative, default -7 [numeric]
#'
#' @return A list containing: copy_number: number of copies, 
#'                            max_score: the alignment score
#'                            alignment: an array of characters consisting of the alignment of each repeat unit
#'                            left_flank: the flank before the tandem repeat
#'                            right_flank: the flank after the tandem repeat
#'                            start: the starting position of the tandem repeat in the sequence
#'                            end: the ending position of the tandem repeat in the sequence
#'
#' @examples
#' wrapAroundAlign(pattern = "ACCT", sequence = "LLLACCTACTACGTACCTRRR", 2, -3, -5)
#' 
wrapAroundAlign <- function(pattern, 
                            sequence, 
                            MATCH = 2, 
                            MISMATCH = -5, 
                            GAP = -7) {
  require(stringr)
  source("tools/consensusBuilder.R")
  pattern = str_split(string = (paste0(" ", toupper(pattern))), pattern = "")[[1]]
  sequence = str_split(string = paste0(" ", toupper(sequence)), pattern = "")[[1]]
  
  M = length(pattern)
  N = length(sequence)
  
  align = matrix(data = 0, nrow = N, ncol = M)
  path  = matrix(data = "stop", nrow = N, ncol = M) 
  
  max_score = 0
  max_i = 0
  max_j = 0
  for (i in 2:N) {
    for (j in 2:M ) {
      if (j == 2) {
        diagonal = align[i-1,M] + 
          ifelse(pattern[j] == sequence[i], MATCH, MISMATCH);
        up = align[i-1,j] + GAP;
        left = align[i-1,M] + GAP;
      } else {
        diagonal = align[i-1,j-1] + 
          ifelse(pattern[j] == sequence[i], MATCH, MISMATCH);
        up = align[i-1,j] + GAP;
        left = align[i,j-1] + GAP;
      }
      if (diagonal <= 0 && up <= 0 && left <= 0) {
        path[i,j] = "stop";
        align[i,j] = 0;
      }
      else if (diagonal > left) {
        if (diagonal > up) {
          # the path came from diagonal
          path[i,j] = 'diagonal';
          align[i,j] = diagonal;
        } else {
          path[i,j] = 'up';
          align[i,j] = up;
        }
      } else {
        if (left > up) {
          path[i,j] = 'left';
          align[i,j] = left;
        } else {
          path[i,j] = 'up';
          align[i,j] = up;
        }
      }
      if (align[i,j] > max_score) {
        max_score = align[i,j];
        max_i = i;
        max_j = j;
      }
    }
  }
  
  i = max_i;
  j = max_j;
  action = ""
  alignment = list()
  repeat_unit = ""
  copy_number = 0
  indel = c()
  indel_at = c()
  while (action != 'stop') {
    action = path[i,j];
    if(action == "up") {
      repeat_unit = paste0(sequence[i], repeat_unit)
      indel = c(indel, j)
      indel_at = c(indel_at, copy_number)
      i = i - 1
    } else if (action == "left") {
      repeat_unit = paste0("-", repeat_unit)
      j = j - 1
    } else if (action == "diagonal") {
      repeat_unit = paste0(sequence[i], repeat_unit)
      i = i - 1
      j = j - 1
    }
    if (j == 1) {
      j = M
      repeat_unit = paste0(" ", repeat_unit)
      copy_number = copy_number + 1
    }
  } 
  min_i = i
  indel_at = copy_number - indel_at
  alignment = str_split(string = str_trim(repeat_unit, "both"), pattern = " ")[[1]]
  N = length(alignment)
  if(length(indel) > 0) {
    for (i in unique(indel)) {
      idx = setdiff(1:N, indel_at[which(indel == i)])
      for (j in idx) {
        alignment[j] = 
          paste(substring(text = alignment[j], first = 1, last = i - 1), 
                substring(text = alignment[j], first = i ), sep = "-")
      }
    }
  }
  
  # check beginning and end
  length = max(str_length(string = alignment))
  if (str_length(string = alignment[1]) < length) {
    padding = paste0(rep(" ", length - str_length(string = alignment[1])), collapse = "")
    alignment[1] = paste0(padding, alignment[1])
  }
  if (str_length(string = alignment[N]) < length) {
    padding = paste0(rep(" ", length - str_length(string = alignment[N])), collapse = "")
    alignment[N] = paste0(alignment[N], padding)
  }
  
  list(copy_number = copy_number,
       max_score = max_score, 
       consensus = consensusBuilder(alignment),
       alignment = alignment,
       left_flank = str_trim(string = paste0(sequence[1:(min_i)],
                           collapse = "")),
       right_flank = paste0(sequence[-(1:max_i)],
                            collapse = ""),
       start = min_i,
       end = max_i)
}

