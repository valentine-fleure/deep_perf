library(patchwork)

indice <- function(a, b, indice) {
  to_calc = t(data.frame(a, b))
  indice = vegan::vegdist(to_calc, indice)
  return(indice)
}

plot_indice = function(to_plot, indice){
  minmin = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.8)
  
  minmax = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.99) 
  
  maxmin = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.8) 
  
  maxmax = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.99) 
  
  minmin_plot = ggplot2::ggplot(minmin, ggplot2::aes(x= fps, y = .data[[indice]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot[[indice]])-0.005, max(to_plot[[indice]])+0.005) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.8") 
  
  minmax_plot = ggplot2::ggplot(minmax, ggplot2::aes(x= fps, y = .data[[indice]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot[[indice]])-0.005, max(to_plot[[indice]])+0.005) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.99") 
  
  maxmin_plot = ggplot2::ggplot(maxmin, ggplot2::aes(x= fps, y = .data[[indice]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot[[indice]])-0.005, max(to_plot[[indice]])+0.005) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.80") 
  
  maxmax_plot = ggplot2::ggplot(maxmax, ggplot2::aes(x= fps, y = .data[[indice]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot[[indice]])-0.005, max(to_plot[[indice]])+0.005) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.99")
  
  fig <- (minmin_plot | minmax_plot) / (maxmin_plot | maxmax_plot)
  
  return(fig)
}

plot_true_pred = function(to_plot, true, pred){
  minmin = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.8)
  
  minmax = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.99) 
  
  maxmin = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.8) 
  
  maxmax = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.99) 
  
  minmin_plot = ggplot2::ggplot(minmin, ggplot2::aes(x= fps, y = .data[[pred]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(min(to_plot[[true]]), min(to_plot[[pred]]))-1, max(max(to_plot[[true]]), max(to_plot[[pred]]))+1) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.8") +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = .data[[true]], yend = .data[[true]]),
                          color = "red")
  
  minmax_plot = ggplot2::ggplot(minmax, ggplot2::aes(x= fps, y = .data[[pred]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(min(to_plot[[true]]), min(to_plot[[pred]]))-1, max(max(to_plot[[true]]), max(to_plot[[pred]]))+1) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.99") +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = .data[[true]], yend = .data[[true]]),
                          color = "red")
  
  maxmin_plot = ggplot2::ggplot(maxmin, ggplot2::aes(x= fps, y = .data[[pred]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(min(to_plot[[true]]), min(to_plot[[pred]]))-1, max(max(to_plot[[true]]), max(to_plot[[pred]]))+1) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.80") +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = .data[[true]], yend = .data[[true]]),
                          color = "red")
  
  maxmax_plot = ggplot2::ggplot(maxmax, ggplot2::aes(x= fps, y = .data[[pred]], colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(min(to_plot[[true]]), min(to_plot[[pred]]))-1, max(max(to_plot[[true]]), max(to_plot[[pred]]))+1) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.99") +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = .data[[true]], yend = .data[[true]]),
                          color = "red")
  
  fig <- (minmin_plot | minmax_plot) / (maxmin_plot | maxmax_plot)
  
  return(fig)
}

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
    
    # nbr_na_no_post_pro = sum(is.na(post_processing_result$pred))
    # indiv_pred_no_post_pro = nrow(post_processing_result) - nbr_na_no_post_pro
    # sp_pred_no_post_pro = length(unique(post_processing_result$pred)) -1 #Pour supprimer NA
    # 
    # all_data = all_data |>
    #   dplyr::add_row(fps = fps, 
    #                  recall = recall,
    #                  acc_det = acc_det, 
    #                  acc_id = acc_id, 
    #                  safe_predict = 0, 
    #                  indiv_true = indiv_true, 
    #                  indiv_pred = indiv_pred_no_post_pro, 
    #                  sp_true = sp_true,
    #                  sp_pred = sp_pred_no_post_pro, 
    #                  abund_sp_true = list(abund_sp_true_var),
    #                  abund_sp_pred = list(abund_sp_pred_no_post_pro), 
    #                  pres_true = list(replace(abund_sp_true_var, abund_sp_true_var>0, 1)),
    #                  pres_pred = list(replace(abund_sp_pred_no_post_pro, abund_sp_pred_no_post_pro>0, 1)))
  }
  all_data = all_data |>
    tidyr::drop_na()
  save(all_data, file = paste0("Working_10_sim/Simu_", date, "/all_data.Rdata"))
  print(Sys.time())
}





load("Simu_20240103/all_data.Rdata")
calc = all_data |>
  dplyr::rowwise() |>
  dplyr::mutate(jaccard = indice(pres_true, pres_pred, "jaccard")) |>
  dplyr::mutate(bray_curtis = indice(maxN_true, maxN_pred, "bray")) |>
  dplyr::mutate(sum_MaxN_pred = sum(maxN_pred)) |>
  dplyr::mutate(sum_MaxN_true = sum(maxN_true))

to_plot = calc |>
  dplyr::filter(safe_predict == 0.9 | safe_predict == 0 | safe_predict == 0.99) |>
  dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
  dplyr::filter(recall == 0.8 | recall == 0.99) |>
  dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
  dplyr::mutate(safe_predict = factor(safe_predict, levels = c("0", "0.9", "0.99"))) |>
  dplyr::mutate(fps = factor(fps, levels = c("1", "2", "5", "10", "30"))) |>
  dplyr::mutate(sp_true = as.integer(sp_true))
  

plot_jaccard = plot_indice(to_plot, "jaccard")
plot_braycurtis = plot_indice(to_plot, "bray_curtis")
plot_sp = plot_true_pred(to_plot, "sp_true", "sp_pred")
plot_indiv = plot_true_pred(to_plot,"indiv_true", "indiv_pred") 
plot_maxN = plot_true_pred(to_plot,"sum_MaxN_true", "sum_MaxN_pred") 

ggplot2::ggsave("Simu_20240103/plot_jaccard.png", plot = plot_jaccard, width = 30, height = 20, units = "cm")
ggplot2::ggsave("Simu_20240103/plot_braycurtis.png", plot = plot_braycurtis, width = 30, height = 20, units = "cm")
ggplot2::ggsave("Simu_20240103/plot_sp.png", plot = plot_sp, width = 30, height = 20, units = "cm")
ggplot2::ggsave("Simu_20240103/plot_indiv.png", plot = plot_indiv, width = 30, height = 20, units = "cm")
ggplot2::ggsave("Simu_20240103/plot_maxN.png", plot = plot_maxN, width = 30, height = 20, units = "cm")


plot_species_time = function(matrix, duplicate = 1){
  # matrix_to_plot = dplyr::mutate(matrix, time = 1:(time_steps*duplicate)) |>
  #   tidyr::pivot_longer(cols = c(1:nbr_species), names_to = "species", values_to = "count")
  
  matrix_to_plot = matrix |>
    dplyr::group_by(frame, pred) |>
    dplyr::summarise(count = dplyr::n())
  
  matrix_to_plot[matrix_to_plot == 0] = NA
  
  ggplot2::ggplot(matrix_to_plot, ggplot2::aes(x = frame, y = factor(pred), fill = count)) + 
    ggplot2::geom_tile() + 
    ggplot2::theme_classic() +
    # ggplot2::scale_fill_gradient2(low = "white", high = "darkgreen")
    ggplot2::scale_fill_gradient(low = "#e5efe5", high = "darkgreen", na.value = "white")
  # #e5efe5
}

plot_species_time_post_pro = function(matrix, duplicate = 1){
  # matrix_to_plot = dplyr::mutate(matrix, time = 1:(time_steps*duplicate)) |>
  #   tidyr::pivot_longer(cols = c(1:nbr_species), names_to = "species", values_to = "count")
  
  matrix_to_plot = matrix |>
    dplyr::group_by(frame, post_pro) |>
    dplyr::summarise(count = dplyr::n())
  
  matrix_to_plot[matrix_to_plot == 0] = NA
  
  ggplot2::ggplot(matrix_to_plot, ggplot2::aes(x = frame, y = factor(post_pro), fill = count)) + 
    ggplot2::geom_tile() + 
    ggplot2::theme_classic() +
    # ggplot2::scale_fill_gradient2(low = "white", high = "darkgreen")
    ggplot2::scale_fill_gradient(low = "#e5efe5", high = "darkgreen", na.value = "white")
  # #e5efe5
}

load("Simu_20231204/res_post_processing/10fps_0.85R_0.95accD_0.95accI_0sp.Rdata")
plot_species_time(summary_post_processing)

load("Simu_20231204/res_post_processing/10fps_0.85R_0.95accD_0.95accI_0.85sp.Rdata")
plot_species_time_post_pro(summary_post_processing)

load("Simu_20231204/res_simu/10fps_0.85R_0.95accD_0.95accI.Rdata")
plot_species_time(summary_post_identification)




