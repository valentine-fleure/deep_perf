library(patchwork)

colMax <- function(data) sapply( data, max, na.rm=TRUE)

maxN = function (data, col){
  cols = c("frame", 1:30)
  a = data |>
    dplyr::group_by(frame, .data[[col]]) |>
    dplyr::summarise(count = dplyr::n()) |>
    dplyr::filter(.data[[col]] != "no_fish") |>
    dplyr::filter(.data[[col]] != "unsure") |>
    dplyr::arrange(as.integer(.data[[col]])) |>
    tidyr::pivot_wider(names_from = .data[[col]], values_from = count) 
  a[cols[!(cols %in% colnames(a))]] = NA
  a = a[, cols]
  a[is.na(a)] <- 0
  return(colMax(a)[2:31])
}

dates = c("20240103", "20240108", "20240112",  "20240115",
  "20240118", "20240121", "20240124", "20240126", "20240128", "20240131")

for (date in dates){
  files = list.files(paste0("Working_10_sim/Simu_", date, "/res_post_processing/"))
  all_data = tibble::tibble(fps = integer(),
                            recall = double(),
                            acc_det = double(),
                            acc_id = double(),
                            safe_predict = double(),
                            indiv_true = integer(),
                            indiv_pred = integer(),
                            sp_true = integer(),
                            sp_pred = integer(),
                            maxN_true = list(),
                            maxN_pred = list(),
                            abund_sp_true = list(),
                            abund_sp_pred = list(), 
                            pres_true = list(),
                            pres_pred = list())
  
  for (file in files){
    fps = as.integer(stringr::str_sub(unlist(stringr::str_split(file, "_"))[1], 1, -4))
    recall = as.double(stringr::str_sub(unlist(stringr::str_split(file, "_"))[2], 1, -2))
    acc_det = as.double(stringr::str_sub(unlist(stringr::str_split(file, "_"))[3], 1, -5))
    acc_id = as.double(stringr::str_sub(unlist(stringr::str_split(file, "_"))[4], 1, -5))
    safe_predict = as.double(stringr::str_sub(unlist(stringr::str_split(file, "_"))[5], 1, -9))
    
    load(paste0("Working_10_sim/Simu_", date, "/res_post_processing/", file))
    post_processing_result = summary_post_processing
    indiv_true = sum(post_processing_result$true != "no_fish")
    sp_true = length(unique(post_processing_result$true)) -1 #Pour supprimer le no_fish
    
    maxN_true_var = maxN(summary_post_processing, "true")
    
    if (safe_predict == 0){
      indiv_pred = sum(post_processing_result$pred != "NA", na.rm = TRUE)
      sp_pred = length(unique(post_processing_result$pred)) - 1 #Pour supprimer le NA
      
      abund_sp_true_var = c()
      abund_sp_pred_var = c()
      
      for (i in 1:30){
        abund_true = sum(post_processing_result$true == i)
        abund_sp_true_var = append(abund_sp_true_var, abund_true)
        
        abund_pred = sum(post_processing_result$pred == i, na.rm = TRUE)
        abund_sp_pred_var = append(abund_sp_pred_var, abund_pred)
      }
      
      maxN_pred_var = maxN(summary_post_processing, "pred")
      
      all_data = all_data |>
        dplyr::add_row(fps = fps, 
                       recall = recall,
                       acc_det = acc_det, 
                       acc_id = acc_id, 
                       safe_predict = safe_predict, 
                       indiv_true = indiv_true, 
                       indiv_pred = indiv_pred, 
                       sp_true = sp_true,
                       sp_pred = sp_pred,
                       maxN_true = list(maxN_true_var),
                       maxN_pred = list(maxN_pred_var), 
                       abund_sp_true = list(abund_sp_true_var),
                       abund_sp_pred = list(abund_sp_pred_var), 
                       pres_true = list(replace(abund_sp_true_var, abund_sp_true_var>0, 1)),
                       pres_pred = list(replace(abund_sp_pred_var, abund_sp_pred_var>0, 1)))
    } 
    
    else {
      nbr_na_post_pro = sum(is.na(post_processing_result$post_pro))
      nbr_unsure = sum(post_processing_result$post_pro == "unsure", na.rm = TRUE)
      indiv_pred = nrow(post_processing_result) - nbr_na_post_pro - nbr_unsure
      sp_pred = length(unique(post_processing_result$post_pro)) -2 #Pour supprimer le unsure et NA  
      
      abund_sp_true_var = c()
      abund_sp_pred_post_pro = c()
      abund_sp_pred_no_post_pro = c()
      
      for (i in 1:30){
        abund_true = sum(post_processing_result$true == i)
        abund_sp_true_var = append(abund_sp_true_var, abund_true)
        
        abund_pred_pp = sum(post_processing_result$post_pro == i, na.rm = TRUE)
        abund_sp_pred_post_pro = append(abund_sp_pred_post_pro, abund_pred_pp)
      }
      
      maxN_pred_var = maxN(summary_post_processing, "post_pro")
      
      all_data = all_data |>
        dplyr::add_row(fps = fps, 
                       recall = recall,
                       acc_det = acc_det, 
                       acc_id = acc_id, 
                       safe_predict = safe_predict, 
                       indiv_true = indiv_true, 
                       indiv_pred = indiv_pred, 
                       sp_true = sp_true,
                       sp_pred = sp_pred,
                       maxN_true = list(maxN_true_var),
                       maxN_pred = list(maxN_pred_var),
                       abund_sp_true = list(abund_sp_true_var),
                       abund_sp_pred = list(abund_sp_pred_post_pro), 
                       pres_true = list(replace(abund_sp_true_var, abund_sp_true_var>0, 1)),
                       pres_pred = list(replace(abund_sp_pred_post_pro, abund_sp_pred_post_pro>0, 1)))
    }
  }
  all_data = all_data |>
    tidyr::drop_na()
  save(all_data, file = paste0("Working_10_sim/Simu_", date, "/all_data.Rdata"))
  print(Sys.time())
}





