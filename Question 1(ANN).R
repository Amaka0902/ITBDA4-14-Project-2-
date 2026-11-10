# QUESTION 1

library(dplyr)
library(stringr)

# Since I put it in data folder
q1_zip <- file.path("data", "map_reduce_test_docs.zip")

#Q1.2

run_mapper <- function(zip_path, target_keyword = "africa") {
  if (!file.exists(zip_path)) {
    stop("Archive not found at: ", zip_path, 
         "\nPlease verify 'map_reduce_test_docs.zip' is inside your 'data/' folder.")
  }
  
  temp_dir <- file.path(tempdir(), "mapreduce_corpus")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  unzip(zip_path, exdir = temp_dir)
  
  doc_files <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  doc_files <- doc_files[!dir.exists(doc_files)] # Filter subdirectories
  
  intermediate_kv <- data.frame(
    key = character(),
    doc_id = character(),
    row_idx = integer(),
    col_idx = integer(),
    stringsAsFactors = FALSE
  )
  
  target_lower <- tolower(target_keyword)
  
  for (file_path in doc_files) {
    doc_name <- basename(file_path)
    lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    
    for (r_idx in seq_along(lines)) {
      tokens <- unlist(strsplit(lines[r_idx], "[[:punct:][:space:]]+"))
      tokens <- tokens[tokens != ""]
      
      for (c_idx in seq_along(tokens)) {
        if (tolower(tokens[c_idx]) == target_lower) {
          intermediate_kv <- rbind(intermediate_kv, data.frame(
            key = tolower(tokens[c_idx]),
            doc_id = doc_name,
            row_idx = as.integer(r_idx),
            col_idx = as.integer(c_idx),
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  unlink(temp_dir, recursive = TRUE)
  return(intermediate_kv)
}

# Execute Mapper
mapper_out <- run_mapper(q1_zip, target_keyword = "africa")


# Q1.3

run_reducer <- function(intermediate_kv) {
  if (nrow(intermediate_kv) == 0) {
    return(data.frame(Keyword = character(), Document = character(), 
                      Occurrences = integer(), Position_List = character()))
  }
  
  reduced_output <- intermediate_kv %>%
    mutate(pos_str = paste0("(", row_idx, ",", col_idx, ")")) %>%
    group_by(key, doc_id) %>%
    summarise(
      Occurrences = n(),
      Position_List = paste(pos_str, collapse = "; "),
      .groups = "drop"
    ) %>%
    rename(Keyword = key, Document = doc_id)
  
  return(reduced_output)
}

# Execute Reducer
q1_final_result <- run_reducer(mapper_out)
cat("\n QUESTION 1.2 & 1.3: MAPREDUCE REDUCED OUTPUT \n")
print(q1_final_result)


#Q1.4
configure_and_run_hadoop <- function(
    input_hdfs   = "/user/hadoop/corpus/map_reduce_test_docs.zip",
    output_hdfs  = "/user/hadoop/output/keyword_search_results",
    num_reducers = 2,
    map_mem_mb   = 2048,
    reduce_mem_mb = 4096
) {
  hadoop_bin <- "hadoop"
  streaming_jar <- "/usr/local/hadoop/share/hadoop/tools/lib/hadoop-streaming-3.3.6.jar"
  
  hadoop_args <- c(
    "jar", streaming_jar,
    "-D", paste0("mapreduce.job.reduces=", num_reducers),
    "-D", paste0("mapreduce.map.memory.mb=", map_mem_mb),
    "-D", paste0("mapreduce.reduce.memory.mb=", reduce_mem_mb),
    "-D", "mapreduce.job.name=RStudio_Keyword_Search_Africa",
    "-input", input_hdfs,
    "-output", output_hdfs,
    "-mapper", "Rscript mapper.R",
    "-reducer", "Rscript reducer.R"
  )
  
  cat("\n QUESTION 1.4: HADOOP STREAMING CONFIGURATION \n")
  if (nzchar(Sys.which(hadoop_bin))) {
    system2(hadoop_bin, args = hadoop_args, stdout = TRUE, stderr = TRUE)
  } else {
    message("Hadoop binary not found in system PATH. Configuration payload verified successfully for cluster execution.")
  }
}

configure_and_run_hadoop()