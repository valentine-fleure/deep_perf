load_database <- function(path, time_steps = 600, duplicate = 30){
  data = read.csv(path)
  
  nbr_species = sum(data$esp_total)
  matrix_species = as.data.frame(matrix(0, nrow=time_steps, ncol=nbr_species))
  names(matrix_species) = paste0("species_", 1:nbr_species)
  
  i = 1
  while (i <=nbr_species){
    for (type in 1:nrow(data)){
      nbr_indiv = data[type,]$nbr_indiv
      tps_presence = data[type,]$tps_presence/100
      nbr_pics = data[type,]$nbr_pics
      esp_video = data[type,]$esp_video
      esp_total = data[type,]$esp_total
      
      for (esp_v in 1:esp_video){
        a = sim_one_species(nbr_indiv, tps_presence, nbr_pics)
        matrix_species[i] = a
        i = i+1
      }
      i = i + (esp_total - esp_video)
    }
  }
  
  ### Replication pour avoir *duplicate* frames seconde
  matrix_species = matrix_species |>
    dplyr::slice(rep(1:dplyr::n(), each = duplicate))
  
  return(matrix_species)
}

sim_one_species <- function(nbr_indiv, tps_presence, nbr_pics, time_steps = 600){
  species = as.data.frame(matrix(0, nrow=time_steps, ncol=1))
  
  nbr_pic_remain = nbr_pics
  mean_duration_pic = (tps_presence*time_steps)/nbr_pics
  duration_pic = (round(rnorm(nbr_pics, mean_duration_pic, sqrt(mean_duration_pic))))
  duration_pic = ifelse(duration_pic < 1, 1, duration_pic)
  duration_gap_available = time_steps - sum(duration_pic)
  previous_start_pic = 1
  for (j in 1:nbr_pics){
    mean_rnorm = duration_gap_available/nbr_pic_remain
    duration_gap = round(runif(1, 1, mean_rnorm))
    start_pic = previous_start_pic + duration_gap
    duration = duration_pic[j]
    ech = rbinom(duration, nbr_indiv, 0.9)
    previous_start_pic = start_pic + duration
    nbr_pic_remain = nbr_pic_remain - 1
    duration_gap_available = duration_gap_available - duration_gap
    for (k in 0:(duration-1)){
      species[start_pic+k,] = species[start_pic+k,] + ech[k+1]
    }
  }
  
  return(species)
}

detect_indiv <- function(rate, matrix_species, time_steps = 600){
  # Selectionne combien d'individus sont detectés par frame
  matrix_recall = as.data.frame(matrix(0, nrow=time_steps, ncol=ncol(matrix_species)))
  names(matrix_recall) = paste0("species_", 1:ncol(matrix_species))
  summary = as.data.frame(matrix(ncol=3)) |>
    tidyr::drop_na() |>
    dplyr::rename("frame" = V1,
                  "true" = V2,
                  "bb" = V3) |>
    dplyr::mutate(across(true, as.character))
  for (i in 1:nrow(matrix_species)){
    for (j in 1:ncol(matrix_species)){
      tot = 0
      if (matrix_species[i,j] != 0){
        tot_seen = 0
        for (k in 1:matrix_species[i,j]){
          seen = rbinom(1, 1,rate)
          tot = tot + seen
          if (seen != 0){
            tot_seen = tot_seen + 1
            summary = summary |>
              dplyr::add_row(frame = i,
                             true = as.character(j),
                             bb = tot_seen)
          }
          else {
            summary = summary |>
              dplyr::add_row(frame = i,
                             true = as.character(j),
                             bb = NA)
          }
        }
      }
      matrix_recall[i,j] = tot
    }
  }
  return(summary)
}

create_false_bb <- function(summary, accuracy, nbr_species = 30){
  nbr_pred_bb = nrow(tidyr::drop_na(summary))
  all_bb = round(nbr_pred_bb/accuracy)
  to_add_bb = all_bb - nbr_pred_bb
  
  # Tirage frame false detection
  for (i in 1:to_add_bb){
    frame = sample(1:nbr_species, 1)
    summary = summary |>
      dplyr::add_row(frame = frame,
                     true = "no_fish",
                     bb = 0)
  }
  
  return(summary)
}

identification <- function(summary, accuracy, nbr_species = 30){
  summary$pred = NA
  summary$verif = NA
  for (i in 1:nrow(summary)){
    if (is.na(summary[i,]$bb)){
      summary[i,]$pred = NA 
      summary[i,]$verif = NA
    }
    else{
      if (summary[i,]$true == "no_fish"){
        pred_species = sample(1:nbr_species, 1)
        summary[i,]$pred = pred_species 
        summary[i,]$verif = FALSE
      }
      else{
        verif = stats::rbinom(1, 1, accuracy)
        if (verif){
          summary[i,]$pred = summary[i,]$true
          summary[i,]$verif = TRUE
        }
        else{
          pred_species = sample(1:nbr_species, 1)
          summary[i,]$pred = pred_species 
          summary[i,]$verif = FALSE
        }
      }
    }
  }
  return(summary)
}

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

data_path = "data/data.csv"
fps = c(1,2,5,10,30)
duplicate = 30
models_scores = c(0.6, 0.7, 0.8, 0.85, 0.9, 0.95, 0.99)
post_processing_scores = c(0.8, 0.85, 0.9, 0.95, 0.99)
dates = c("20240103", "20240108", "20240112",  "20240115",
          "20240118", "20240121", "20240124", "20240126", "20240128", "20240131")
for (date in dates) {
  save_path = paste0("Working_10_sim/Simu_", date, "/res_post_processing/")
  if (!dir.exists(save_path)){
    dir.create(save_path, recursive = TRUE)
  }
  matrix_species = load_database(data_path)
  save(matrix_species, file = paste0("Working_10_sim/Simu_", date, "/matrix_species.Rdata"))
  for (frame_rate in fps){
    for (recall in models_scores){
      for (det_acc in models_scores){
        for (identif_acc in models_scores){
          print(Sys.time())
          print(paste0("fps : ", frame_rate))
          matrix = matrix_species |>
            dplyr::slice(seq(1, dplyr::n(), by = as.integer(duplicate/10)))
          
          print(paste0("recall : ", recall))
          summary_post_recall = detect_indiv(recall, matrix)
          
          print(paste0("Acc detection : ", det_acc))
          summary_post_detection = create_false_bb(summary_post_recall, det_acc)
          
          print(paste0("Acc identif : ", identif_acc))
          summary_post_identification = identification(summary_post_detection, identif_acc)
          
          #No post-processing
          summary_post_processing = summary_post_identification
          file_name = paste0(save_path, frame_rate, "fps_", recall, "R_", det_acc, "accD_", identif_acc, "accI_0sp.Rdata")
          save(post_processing_result, file = file_name)
          
          for (safe_predict in post_processing_scores){
            print(paste0("Safe predict : ", safe_predict))
            post_processing_result = post_processing(summary_post_identification, safe_predict)
            
            file_name = paste0(save_path, frame_rate,"fps_", recall, "R_", det_acc, "accD_", identif_acc, "accI_", safe_predict, "sp.Rdata")
            save(post_processing_result, file = file_name)
            print(Sys.time())
          }
        }
      }
    }
  }
}

