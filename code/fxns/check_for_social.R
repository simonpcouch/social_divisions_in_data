# function: check_for_social
# inputs: a `dataset` housed in a package, as well as the `name` of the 
#    dataset
# output: a tibble, containing unique values of relevant columns in the 
#    given dataset
check_for_social <- function(dataset, name) {
  
  # first, check that the dataset argument is actually an argument
  if (!"data.frame" %in% class(dataset)) {
    NULL
  }
  
  # look for "sex" in columns
  unique_sex_entries <- dataset %m>%
    unique_entries_in_column("sex")
  
  # look for "race" in columns 
  unique_race_entries <- dataset %m>%
    unique_entries_in_column("race", "racial")
  
  # look for "ethnicity" in columns
  unique_ethnicity_entries <- dataset %m>%
    unique_entries_in_column("ethnic", "ethnicity")
  
  # look for "gender" in columns 
  unique_gender_entries <- dataset %m>%
    unique_entries_in_column("gender")
  
  # per kjersten's recommendation, look for "female" and "male"
  unique_female_entries <- dataset %m>%
    unique_entries_in_column("female")
  
  unique_male_entries <- dataset %m>%
    unique_entries_in_column("male")
  
  n_rows <- lapply(list(unique_sex_entries,
                     unique_race_entries,
                     unique_ethnicity_entries,
                     unique_gender_entries,
                     unique_female_entries,
                     unique_male_entries),
                   nrow) %m>%
    base::unlist() %m>%
    base::sum(na.rm = TRUE)
  
  # return a dataframe where each row is a unique entry
  # in a given column in the dataframe
  if (n_rows > 0) {
    
    x <- dplyr::bind_rows(unique_sex_entries,
                          unique_race_entries,
                          unique_ethnicity_entries,
                          unique_gender_entries,
                          unique_female_entries,
                          unique_male_entries) %m>%
      dplyr::mutate(dataset = name)

  } else {
    x <- NULL
  }
  
  x
}
