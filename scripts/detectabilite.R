library(patchwork)

colMax <- function(data) sapply( data, max, na.rm=TRUE)

indice <- function(a, b, indice) {
  to_calc = t(data.frame(a, b))
  indice = vegan::vegdist(to_calc, indice)
  return(indice)
}

rename_date <- function(data, new_name, old_name, date){
  
  new_name = paste0(new_name, date)
  data = data |>
    dplyr::rename_at(old_name, ~new_name)
  
  return(data)
  
}

analyse <- function(data, col_name, dates){
  data = data |>
    dplyr::mutate(fps = factor(fps, levels = c("0.25", "0.5", "1", "2", "5", "10", "30"))) |>
    dplyr::rowwise() |>
    dplyr::mutate(min = min(dplyr::c_across(paste(col_name, dates, sep = "_")))) |>
    dplyr::mutate(max = max(dplyr::c_across(paste(col_name, dates, sep = "_")))) |>
    dplyr::mutate(med = median(dplyr::c_across(paste(col_name, dates, sep = "_")))) |>
    dplyr::mutate(mean = mean(dplyr::c_across(paste(col_name, dates, sep = "_")))) |>
    dplyr::mutate(sd = sd(dplyr::c_across(paste(col_name, dates, sep = "_")))) 
  
  return(data)
}

plot_detect <- function(data, name){
  p = ggplot2::ggplot(data, ggplot2::aes(x= fps, y = mean)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(floor(min(data$mean-data$sd)) + 0.5, ceiling(max(data$mean+data$sd)) - 0.5) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(name)
  
  return(p)
}

sum_maxN = tibble::tibble(fps = c(0.25,0.5,1,2,5,10,30))
nb_sp = tibble::tibble(fps = c(0.25,0.5,1,2,5,10,30))
jacc = tibble::tibble(fps = c(0.25,0.5,1,2,5,10,30))
bray = tibble::tibble(fps = c(0.25,0.5,1,2,5,10,30))

dates = c("20240103", "20240108", "20240112", "20240115", "20240118", "20240121",
          "20240124", "20240126", "20240128", "20240131")

for (date in dates){
  file = paste0("Working_10_sim/Simu_", date, "/matrix_species.Rdata")
  load(file)
  maxN_true = colMax(matrix_species)
  # sum_maxN_true = sum(maxN_true) # tjrs 65
  pres_true = replace(maxN_true, maxN_true>0, 1)
  # nb_sp_true = sum(pres_true) # tjrs 15
  
  for (i in 1:nrow(sum_maxN)){
    
    fps = sum_maxN$fps[i]
    
    matrix = matrix_species |>
      dplyr::slice(seq(1, dplyr::n(), by = as.integer(30/fps)))
    
    maxN_detec = colMax(matrix)
    sum_maxN_detec = sum(maxN_detec)
    pres_detec = replace(maxN_detec, maxN_detec>0, 1)
    nb_sp_detec = sum(pres_detec)
    
    jaccard = indice(pres_true, pres_detec, "jaccard")
    bray_curtis = indice(maxN_true, maxN_detec, "bray")
    
    sum_maxN$sum_maxN[i] = sum_maxN_detec
    nb_sp$nb_sp[i] = nb_sp_detec
    jacc$jaccard[i] = jaccard
    bray$bray[i] = bray_curtis
    
  }
  
  sum_maxN = rename_date(sum_maxN, "sum_maxN_", "sum_maxN", date)
  nb_sp = rename_date(nb_sp, "nb_sp_", "nb_sp", date)
  jacc = rename_date(jacc, "jaccard_", "jaccard", date)
  bray = rename_date(bray, "bray_", "bray", date)

}

sum_maxN = analyse(sum_maxN, 'sum_maxN', dates)
nb_sp = analyse(nb_sp, "nb_sp", dates)
jacc = analyse(jacc, "jaccard", dates)
bray = analyse(bray, "bray", dates)

plot_sum_maxN = plot_detect(sum_maxN, "abundance")
plot_nb_sp = plot_detect(nb_sp, "nb_species")
plot_jacc = plot_detect(jacc,"jaccard")
plot_bray = plot_detect(bray, "bray_curtis")

fig = (plot_nb_sp + plot_jacc) / (plot_sum_maxN + plot_bray) ; fig
ggplot2::ggsave("Working_10_sim/fig0.png", plot = fig, width = 30, height = 20, units = "cm")
