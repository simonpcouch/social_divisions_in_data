# function: extract_pkg_data
# inputs: the `package` name as a character string
# output: a tibble, containing unique values of relevant columns of every 
#     dataset in the package, as well as which dataset and package the 
#     values came from
extract_pkg_data <- function(package) {
  
  # print the package so that progress is easier to track
  base::print(package)
  
  # the object that will eventually be the output--if the package
  # contains relevant columns, this will be a tibble containing the
  # unique entries in those columns (and where they came from)
  dataset_entries <- NULL
  
  # load the package, if possible
  package_loaded <- tryCatch(library(package,
                                     character.only = TRUE,
                                     quietly = TRUE),
                             error = function(e) {e})
  
  # test for an error while loading the package
  if ("error" %in% class(package_loaded)) {
    return(NULL)
  }
  
  # check if the package contains datasets
  package_has_datasets <- (dim(data(package = package)[[3]])[1] != 0)
  
  # if the package doesn't have datasets, return NULL
  if (!package_has_datasets) {
    return(NULL)
  }
  
  # if the package has datasets, pull out their unique values  
  
  # check if vcdExtra will work on the package
  data_will_load <- tryCatch(vcdExtra::datasets(package),
                             error = function(e) {e})
  
  # if dataset loading doesn't work, return NULL
  if ("error" %in% class(data_will_load)) {
    return(NULL)
  }
  
  # initialize a variable containing all of the names in the package
  dataset_names <- NULL
  
  # if the datasets will load, load them!
  if (is.data.frame(data_will_load[1])) {
    dataset_names <- vcdExtra::datasets(package) 
  }
  
  # check that the data looks like it should. if it does, just pull out
  # a character vector of the names of the datasets
  if (is.data.frame(dataset_names) & "Item" %in% colnames(dataset_names)) {
    dataset_names <- dataset_names$Item
  }
  
  # actually load the datasets
  datasets <- tryCatch(lapply(dataset_names, get), 
                       error = function(e) {e})
  
  # check that the datasets were loaded successfully 
  if ((!("error" %in% class(package_loaded))) & 
      is.data.frame(datasets[[1]]) & 
      (!is.null(dataset_names))) {
    
    # ...if they were, look for social division data in each of the datasets
    dataset_entries_list <- purrr::map2(datasets, 
                                        dataset_names,
                                        check_for_social)
    
    # and then smush all of the data drawn from the datasets in 
    # the package together
    dataset_entries <- dplyr::bind_rows(dataset_entries_list)
  }
  
  # check that the data looks like it should
  if (is.null(dataset_entries) |
      !"data.frame" %in% class(dataset_entries)) {
    return(NULL)
  }
  
  # it looks like the result is a dataset, buuut
  if (nrow(dataset_entries) == 0) {
    return(NULL)
  }
  
  # unload the package so that it's namespace won't conflict in later trials
  tryCatch(R.utils::detachPackage(package), 
           error = function(e) {e})
  
  # otherwise, good to go!
  dataset_entries %m>%
    dplyr::mutate(package = package)
}
