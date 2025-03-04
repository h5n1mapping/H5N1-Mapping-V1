setwd("/cloud/project")

url <- "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals/sites/default/files/hpai-mammals.csv"
dest_folder <- "data"
dest_file <- file.path(dest_folder, "mammal.csv")

if (!dir.exists(dest_folder)) {
  dir.create(dest_folder)
}

download.file(url, dest_file, mode = "wb", quiet = TRUE)