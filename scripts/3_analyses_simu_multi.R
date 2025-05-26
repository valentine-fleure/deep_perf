indice <- function(a, b, indice) {
  to_calc = t(data.frame(a, b))
  indice = vegan::vegdist(to_calc, indice)
  return(indice)
}

create_all_data <- function(all_data){
  new = all_data |>
    dplyr::rowwise() |>
    dplyr::mutate(jaccard = indice(pres_true, pres_pred, "jaccard")) |>
    dplyr::mutate(bray_curtis = indice(maxN_true, maxN_pred, "bray")) |>
    dplyr::mutate(sum_MaxN_pred = sum(maxN_pred)) |>
    dplyr::mutate(sum_MaxN_true = sum(maxN_true)) |>
    dplyr::mutate(shannon = vegan::diversity(maxN_pred)) |>
    dplyr::mutate(hill = exp(shannon))
  
  return(new)
}

colMax <- function(data) sapply( data, max, na.rm=TRUE)

dates = c("20240103", "20240108", "20240112", "20240115", "20240118", "20240121",
          "20240124", "20240126", "20240128", "20240131")
data = c("fps", "recall", "acc_det", "acc_id", "safe_predict")

for (date in dates){
  file = paste0("Working_10_sim/Simu_", date, "/all_data.Rdata")
  load(file)
  
  matrix_species_file = paste0("Working_10_sim/Simu_", date, "/matrix_species.Rdata")
  load(matrix_species_file)

  indiv_true_v = sum(matrix_species)
  sp_true_v = 15
  maxN_true_v = colMax(matrix_species)
  abund_sp_true_v = colSums(matrix_species)
  pres_true_v = replace(abund_sp_true_v, abund_sp_true_v>0, 1)

  all_data = all_data |>
    dplyr::mutate(indiv_true = indiv_true_v) |>
    dplyr::mutate(sp_true = sp_true_v) |>
    dplyr::mutate(maxN_true = list(maxN_true_v)) |>
    dplyr::mutate(abund_sp_true = list(abund_sp_true_v)) |>
    dplyr::mutate(pres_true = list(pres_true_v))

  all_data = all_data |>
    dplyr::rowwise() |>
    dplyr::mutate(seen_among_to_see = sum(unlist(pres_pred) & unlist(pres_true))) |>
    dplyr::mutate(missed = sum(unlist(pres_true)) - seen_among_to_see) |>
    dplyr::mutate(invented = sum(unlist(pres_pred)) - seen_among_to_see)
  save(all_data, file = file)

  
  eval(parse(text=paste0("all_data_",date,"= create_all_data(all_data)")))
  eval(parse(text = paste0("all_data = all_data_",date)))
  save(all_data, file = file)
  
  if (date == "20240103"){
    jaccard = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, jaccard) |>
      dplyr::rename(jaccard_20240103 = jaccard)

    bray_curtis = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, bray_curtis) |>
      dplyr::rename(bray_curtis_20240103 = bray_curtis)

    missed = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, missed) |>
      dplyr::rename(missed_20240103 = missed)

    invented = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, invented) |>
      dplyr::rename(invented_20240103 = invented)

    nb_species = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, sp_pred)|>
      dplyr::rename(sp_20240103 = sp_pred)

    abundance = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, indiv_pred)|>
      dplyr::rename(abund_20240103 = indiv_pred)
    
    sum_maxN = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, sum_MaxN_pred)|>
      dplyr::rename(sum_maxN_20240103 = sum_MaxN_pred)
    
    hill_number = all_data_20240103 |>
      dplyr::select(fps, recall, acc_det, acc_id, safe_predict, hill)|>
      dplyr::rename(hill_20240103 = hill)
  }
  else{
    cmd = paste0("jaccard = jaccard |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, jaccard),
                                    by = data) |>
                   dplyr::rename(jaccard_", date, " = jaccard)")
    eval(parse(text=cmd))

    cmd = paste0("bray_curtis = bray_curtis |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, bray_curtis),
                                    by = data) |>
                   dplyr::rename(bray_curtis_", date, " = bray_curtis)")
    eval(parse(text=cmd))

    cmd = paste0("missed = missed |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, missed),
                                    by = data) |>
                   dplyr::rename(missed_", date, " = missed)")
    eval(parse(text=cmd))

    cmd = paste0("invented = invented |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, invented),
                                    by = data) |>
                   dplyr::rename(invented_", date, " = invented)")
    eval(parse(text=cmd))

    cmd = paste0("nb_species = nb_species |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, sp_pred),
                                    by = data) |>
                   dplyr::rename(sp_", date, " = sp_pred)")
    eval(parse(text=cmd))

    cmd = paste0("abundance = abundance |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, indiv_pred),
                                    by = data) |>
                   dplyr::rename(abund_", date, " = indiv_pred)")
    eval(parse(text=cmd))
    
    cmd = paste0("sum_maxN = sum_maxN |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, sum_MaxN_pred),
                                    by = data) |>
                   dplyr::rename(sum_maxN_", date, " = sum_MaxN_pred)")
    eval(parse(text=cmd))
    
    cmd = paste0("hill_number = hill_number |>
                   dplyr::left_join(all_data_", date," |> dplyr::select(data, hill),
                                    by = data) |>
                   dplyr::rename(hill_", date, " = hill)")
    eval(parse(text=cmd))
  }
}

jaccard = jaccard |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("jaccard", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("jaccard", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("jaccard", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("jaccard", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("jaccard", dates, sep = "_")))) 


bray_curtis = bray_curtis |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("bray_curtis", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("bray_curtis", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("bray_curtis", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("bray_curtis", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("bray_curtis", dates, sep = "_")))) 

missed = missed |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("missed", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("missed", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("missed", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("missed", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("missed", dates, sep = "_")))) 

invented = invented |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("invented", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("invented", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("invented", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("invented", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("invented", dates, sep = "_")))) 

nb_species = nb_species|>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("sp", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("sp", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("sp", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("sp", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("sp", dates, sep = "_")))) 

abundance = abundance |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("abund", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("abund", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("abund", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("abund", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("abund", dates, sep = "_")))) 

sum_maxN = sum_maxN |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("sum_maxN", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("sum_maxN", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("sum_maxN", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("sum_maxN", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("sum_maxN", dates, sep = "_")))) 

hill_number = hill_number |>
  dplyr::rowwise() |>
  dplyr::mutate(min = min(dplyr::c_across(paste("hill", dates, sep = "_")))) |>
  dplyr::mutate(max = max(dplyr::c_across(paste("hill", dates, sep = "_")))) |>
  dplyr::mutate(med = median(dplyr::c_across(paste("hill", dates, sep = "_")))) |>
  dplyr::mutate(mean = mean(dplyr::c_across(paste("hill", dates, sep = "_")))) |>
  dplyr::mutate(sd = sd(dplyr::c_across(paste("hill", dates, sep = "_")))) 


save(jaccard, file = "Working_10_sim/jaccard.Rdata")
save(bray_curtis, file = "Working_10_sim/bray_curtis.Rdata")
save(missed, file = "Working_10_sim/missed.Rdata")
save(invented, file = "Working_10_sim/invented.Rdata")
save(nb_species, file = "Working_10_sim/nb_species.Rdata")
save(abundance, file = "Working_10_sim/abundance.Rdata")
save(sum_maxN, file = "Working_10_sim/sum_maxN.Rdata")
save(hill_number, file = "Working_10_sim/hill_number.Rdata")
