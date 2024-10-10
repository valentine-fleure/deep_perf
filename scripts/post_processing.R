post_processing <- function(summary, safe_predict_rate){
  summary$post_pro = NA
  for (i in 1:nrow(summary)){
    if (!is.na(summary[i,]$verif)){
      if (isTRUE(summary[i,]$verif)){
        safe_predict = stats::rbinom(1, 1, 1-safe_predict_rate)
      }
      else{
        safe_predict = stats::rbinom(1, 1, safe_predict_rate)
      }
      if (safe_predict){
        summary[i,]$post_pro = "unsure"
      }
      else{
        summary[i,]$post_pro = summary[i,]$pred
      }
    }
  }
  return(summary)
}

in_path = "Simu_20231113/res_simu/"
out_path = "Simu_20231113/res_post_processing/"

files = list.files(in_path)

scores = c(0.8, 0.85, 0.9, 0.95, 0.99)
for (file in files){
  print(Sys.time())
  
  load(paste0(in_path,file))
  
  summary_post_processing = summary_post_identification
  file_info = substr(file, 1 , nchar(file)-6)
  file_name = paste0(out_path, file_info, "_0sp.Rdata")
  save(summary_post_processing, file = file_name)
  
  print(file_info)
  
  for (score in scores){
    print(paste0("score : ", score))
    summary_post_processing = post_processing(summary_post_identification, score)
    file_name = paste0(out_path, file_info, "_", score,"sp.Rdata")
    save(summary_post_processing, file = file_name)
  }
  print(Sys.time())
}

in_path = "Simu_20231129/res_simu/"
out_path = "Simu_20231129/res_post_processing/"

files = list.files(in_path)

scores = c(0.8, 0.85, 0.9, 0.95, 0.99)
for (file in files){
  print(Sys.time())
  
  load(paste0(in_path,file))
  
  summary_post_processing = summary_post_identification
  file_info = substr(file, 1 , nchar(file)-6)
  file_name = paste0(out_path, file_info, "_0sp.Rdata")
  save(summary_post_processing, file = file_name)
  
  print(file_info)
  
  for (score in scores){
    print(paste0("score : ", score))
    summary_post_processing = post_processing(summary_post_identification, score)
    file_name = paste0(out_path, file_info, "_", score,"sp.Rdata")
    save(summary_post_processing, file = file_name)
  }
  print(Sys.time())
}

in_path = "Simu_20231201/res_simu/"
out_path = "Simu_20231201/res_post_processing/"

files = list.files(in_path)

scores = c(0.8, 0.85, 0.9, 0.95, 0.99)
for (file in files){
  print(Sys.time())
  
  load(paste0(in_path,file))
  
  summary_post_processing = summary_post_identification
  file_info = substr(file, 1 , nchar(file)-6)
  file_name = paste0(out_path, file_info, "_0sp.Rdata")
  save(summary_post_processing, file = file_name)
  
  print(file_info)
  
  for (score in scores){
    print(paste0("score : ", score))
    summary_post_processing = post_processing(summary_post_identification, score)
    file_name = paste0(out_path, file_info, "_", score,"sp.Rdata")
    save(summary_post_processing, file = file_name)
  }
  print(Sys.time())
}

in_path = "Simu_20231204/res_simu/"
out_path = "Simu_20231204/res_post_processing/"

files = list.files(in_path)

scores = c(0.8, 0.85, 0.9, 0.95, 0.99)
for (file in files){
  print(Sys.time())
  
  load(paste0(in_path,file))
  
  summary_post_processing = summary_post_identification
  file_info = substr(file, 1 , nchar(file)-6)
  file_name = paste0(out_path, file_info, "_0sp.Rdata")
  save(summary_post_processing, file = file_name)
  
  print(file_info)
  
  for (score in scores){
    print(paste0("score : ", score))
    summary_post_processing = post_processing(summary_post_identification, score)
    file_name = paste0(out_path, file_info, "_", score,"sp.Rdata")
    save(summary_post_processing, file = file_name)
  }
  print(Sys.time())
}