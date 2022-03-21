# clean_names_lcfm is stupid slow maybe talk to ben s about making it run faster?
# BUGS - names_fml when passed "SIEU, MARY" returns "sieu,mary sieu,," should return "sieu,mary,,"

# Sample implementation code
# library(tidyverse)
# x <- c("the quick brown fox", "jumped over the lazy", "black dog word word")
# df <- as.tibble(x)
# df %>% 
#   rowwise() %>%  
#   mutate(temp = clean_names_fml(`value`,suffix=suffix)) %>% 
#   separate(temp, into=c("name_first", "name_middle", "name_last", "name_other"), sep=",")


suffix = c("jr","sr", "ii", "iii", "iv")

#' split a "first middle last" name into 4 elements
#' The evaluated string is converted to lower case broken into four parts and punctuation is stripped out.
#' The four parts are returned as a csv character string in order: name_first, name_middle, name_last, name_other
#' I parse the string and look for suffixes which are separated out and assigned to name_other
#' All characters before the first space are assigned to the first_name
#' All characters after the last space are assigned to the last name
#' All character between the first and last space are assigned to the middle name
#' @param x A character string
#' @param suffix a list of common suffixes
#'
#' @return A csv character string containing four elements, name_first name_middle name_last name_other
#'
#' @examples
#' ## Two simple examples showing punctuation getting removed
#' clean_names_fml(x="John Jones Jr", suffix=suffix)
#' clean_names_fml(x="John J. Jones Jr", suffix=suffix)
#' clean_names_fml(x="John J. Jones Jr", suffix=suffix)
#' ## Examples with multiple middle names and different positioning of the suffix
#' clean_names_fml(x="John J.P. Jones Jr", suffix=suffix)
#' clean_names_fml(x="Jr John J P Jones", suffix=suffix)
#' ## A case when a lcfm name is passed through, note that the comma is ignored
#' clean_names_fml(x="Jones, John J P Jr", suffix=suffix)
clean_names_fml <- function(x, suffix, print=FALSE) {
  if (print==TRUE) {
    print(x)
  }
  x0 <- tolower(x)
  x1 <- as.list(strsplit(x0, " "))
  x2 <- lapply(x1, function(y) gsub("[[:punct:]]", "", y))
  x3 <- separateSuffix(x2[[1]], suffix=suffix)
  
  name_other <- paste(x3[[1]], collapse = " ")
  name_first <- paste(x3[[2]][1], collapse = " ")
  
  len <- length(x3[[2]])
  name_last <- paste(x3[[2]][len], collapse = " ")
  if (len > 2) { 
    name_middle <- paste(x3[[2]][2:(len-1)], collapse = " ")
  } else {
    name_middle <- c()
  }
  
  return <- paste(name_first,name_middle,name_last,name_other,sep=",")
  
  return(return)
}



#' split a "last, first middle" name into 4 elements
#' The evaluated string is converted to lower case broken into four parts and punctuation is stripped out.
#' The four parts are returned as a csv character string in order: name_first, name_middle, name_last, name_other
#' I parse the string and look for suffixes which are separated out and assigned to name_other
#' All characters before the first comma are assigned to the last_name
#' All characters after the first comma and before the first space are assigned to the first_name
#' All characters after the first space after the first comma are assigned to the middle name
#' All commas other than the first are ignored
#' If no commas are present, all characters (other than suffixes) will be assigned to the last name
#' @param x A character string
#' @param suffix a list of common suffixes default is c("jr","sr", "ii", "iii", "iv", "v")
#'
#' @return A list containing four elements, name_first name_middle name_last name_other
#'
#' @examples
#' ## Simple example
#' clean_names_lcfm("Jones, John J", suffix=suffix) == "
#' clean_names_lcfm("Jones, John J. Jr", suffix=suffix)
#' ## Suffixes in different positions
#' clean_names_lcfm(x="Jones, John J P Jr", suffix=suffix)
#' clean_names_lcfm(x="Jones Jr, John J P", suffix=suffix)
#' ## a really weird case
#' clean_names_lcfm("Jensen James II, John J Patrick, jr", suffix=suffix)
#' # No comma case
#' clean_names_lcfm("Jones John J Jr", suffix=suffix)
clean_names_lcfm <- function(x, suffix, print=FALSE) {
  name_last <- c()
  name_first <- c()
  name_middle <- c()
  other1 <- c()
  other2 <- c()
  name_other <- c()
  
  if (print==TRUE) {
    print(x)
  }
  x0 <- tolower(x)
  x1 <- as.list(strsplit(x0, ","))
  x2 <- lapply(x1, trimws)
  x3 <- lapply(x2, function(y) gsub("[[:punct:]]", "", y))

  last_1 <- x3[[1]][1]
  last_2 <- strsplit(last_1, " ")
  if(!(identical(last_2[[1]], character(0)))) {
    last_sepsuf <- separateSuffix(last_2[[1]], suffix=suffix)
    other1 <- last_sepsuf[[1]]
    name_last <- paste(last_sepsuf[[2]], collapse=" ")
  }
  if(!(identical(x3[[1]][-1], character(0)))) {
    if(x3[[1]][-1]!="") {
      rest1 <- x3[[1]][-1]
      rest2 <- paste(rest1, collapse= " ") # need this line in case there are >1 commas
      rest3 <- strsplit(rest2, " ")
      
      rest_sepsuf <- separateSuffix(rest3[[1]], suffix=suffix)
      other2 <- rest_sepsuf[[1]]
      name_first <- paste(rest_sepsuf[[2]][1], collapse=" ")
      name_middle <- paste(rest_sepsuf[[2]][-1], collapse=" ")
      name_other <- trimws(paste(other1, other2, collapse= " "))
    } 
  } 
  
  return <- paste(name_first,name_middle,name_last,name_other,sep=",")
  
  return(return)
}



#' Separates suffix strings from a list of strings
#' The evaluated list is checked for suffixes, suffixes are moved to a separate list
#' @param x A list of character strings
#' @param suffs A list of suffixes default is suffix string passed from 
#'
#' @return
#'
#' @examples
separateSuffix <- function(x, suffix) {
  other <- ""
  for(i in 1:length(x)) {
    if(x[i] %in% suffix) {
      other <- paste(other,x[i], sep=" ")
      x[i] <- NA
    } 
  }
  return(list(trimws(other), na.omit(x)))
}

