setwd("/cloud/project")

download_csv <- function(url, file_name) {
  
  dest_folder <- "data"
  dest_file <- file.path(dest_folder, file_name)
  
  if (!dir.exists(dest_folder)) {
    dir.create(dest_folder)
  }
  
  download.file(url, dest_file, mode = "wb", quiet = TRUE)
}

download_csv("https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals/sites/default/files/hpai-mammals.csv",
             "mammal.csv")