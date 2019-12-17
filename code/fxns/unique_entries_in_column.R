# function: unique_entries_in_column
# inputs: a `dataset` and a `string` to look for (as well as a 
#   `perfect_match` string if an exact match is unlikely, but 
#   some abbreviation of it is useful)
# output: a vector containing the unique values in the most 
#   relevant column (or NULL)
unique_entries_in_column <- function(dataset, string, perfect_match = NULL) {
  
  # check that the dataset argument is actually a dataset
  if (!"data.frame" %in% class(dataset)) {
    return(NULL)
  }
  
  # clean/sanitize the dataset column names
  dataset <- janitor::clean_names(dataset, case = "snake")
  
  # decide which column is most likely to contain relevant information, if any.
  # first, coming up with some conditions...
  detect_string <- str_detect(colnames(dataset), string)
  
  # the subsetting syntax is different for data.tables, sooo
  dataset <- as.data.frame(dataset)
  
  # select off only the columns in the dataset that contain the string.
  # if none do, just return NULL
  if (TRUE %in% detect_string) {
    dataset <- dataset %m>%
      dplyr::select(base::which(detect_string, TRUE))
  } else {
    return(NULL)
  }
  
  # if there are issues with the dataset after only selecting
  # relevant columns, just return NULL
  if (!("data.frame" %in% class(dataset))) {
    return(NULL)
  }
  
  # if the dataset does have a some sort of relevant column, though, move on.
  # first, look for a perfect match
  if (!is.null(perfect_match)) {
    detect_perfect_match <- str_detect(colnames(dataset), perfect_match)
  } else {
    detect_perfect_match <- rep(FALSE, ncol(dataset))
  }
  
  # and check if any are characters or factors
  has_character <- TRUE %in% lapply(dataset, is.character)
  has_factor <- TRUE %in% lapply(dataset, is.factor)
  
  # if there are character columns, identify the position of the first
  if (has_character) {
    character_col <- suppressWarnings(which(unlist(lapply(dataset, 
                                                          is.character)), 
                                            TRUE)[1])
  } else {
    character_col <- NULL
  } 
  
  # if there are factor columns, identify the position of the first
  if (has_factor) {
    factor_col <- suppressWarnings(which(unlist(lapply(dataset, 
                                                       is.factor)), 
                                         TRUE)[1])  
  } else {
    factor_col <- NULL
  } 
  
  # ...and now actually selecting the "best" column
  best_column <- if (sum(colnames(dataset) == perfect_match) == 1) {
    which(colnames(dataset) == perfect_match, TRUE)
  } else if (sum(colnames(dataset) == string) == 1) {
    which(colnames(dataset) == string, TRUE)
  } else if (sum(detect_perfect_match) == 1) {
    which(str_detect(colnames(dataset), perfect_match), TRUE)
  } else if (sum(detect_string) == 1) {
    which(str_detect(colnames(dataset), string), TRUE)
  } else if (sum(detect_string) > 1) {
    if (has_character) {
      character_col
    } else if (has_factor) {
      factor_col 
    } else {
      1
    }
  } else {
    1
  }
  
  # some of the loaded libraries have been overwriting the "bang-bang"
  # operator, so load it explicitly
  library(rlang)
  
  # extract the actual name at the column index
  best_column_name <- colnames(dataset)[best_column]
  
  # ...and quote it
  best_column_name <- enquo(best_column_name)
  
  # grab the unique values and number of occurences
  entry_counts <- dataset %m>%
    # where the column name is 'unquoted'
    dplyr::group_by_at(dplyr::vars(!!best_column_name)) %m>%
    # count the number of unique entries
    dplyr::summarize(n = dplyr::n()) %m>%
    # add on the column name containing the entries
    dplyr::mutate(actual_column = !!best_column_name,
                  column = string) %m>%
    # rename the first column, containing the unique entries, to "entry"
    dplyr::rename("entry" = 1) %m>%
    dplyr::mutate(entry = as.character(entry))
  
}
