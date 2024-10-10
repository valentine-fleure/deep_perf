facet_h_1 = function(to_plot, name){
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= safe_predict, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::guides(colour = "none")+
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ .) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(safe_predict)-0.25, xend = as.numeric(safe_predict)+0.25, y = true, yend = true),
                          color = "red") +
    ggplot2::theme_bw() + 
    ggplot2::theme(strip.background = ggplot2::element_blank(),               
                   strip.text.y = ggplot2::element_blank(), 
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.x = ggplot2::element_blank()) + 
    ggplot2::labs(y = "Biodiversity index") +
    ggplot2::ggtitle(name)
}

facet_h_middle = function(to_plot, name){
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= safe_predict, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::guides(colour = "none")+
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ .) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(safe_predict)-0.25, xend = as.numeric(safe_predict)+0.25, y = true, yend = true),
                          color = "red") +
    ggplot2::theme_bw() + 
    ggplot2::theme(strip.background = ggplot2::element_blank(),               
                   strip.text.y = ggplot2::element_blank(), 
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.y = ggplot2::element_blank()) +  
    ggplot2::labs(x = "Post-processing threshold") + 
    ggplot2::ggtitle(name)
}

facet_h_last = function(to_plot, name){
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= safe_predict, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ .) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(safe_predict)-0.25, xend = as.numeric(safe_predict)+0.25, y = true, yend = true),
                          color = "red") +
    ggplot2::ggtitle(name) +   
    ggplot2::theme_bw() + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())
}


prepare_data = function(data_to_prepare, type, true) {
  data = data_to_prepare |>
    dplyr::filter(fps == 1) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
    dplyr::filter(recall == 0.8 | recall == 0.99) |>
    dplyr::select(acc_det, acc_id, recall,safe_predict, mean, sd) |>
    dplyr::mutate(detecteur = stringr::str_c(recall, acc_det, sep = "_")) |>
    dplyr::mutate(det_type = dplyr::case_when(
      detecteur == "0.8_0.8" ~ "det_1",
      detecteur == "0.8_0.99" ~ "det_3",
      detecteur == "0.99_0.8" ~ "det_2",
      detecteur == "0.99_0.99" ~ "det_4")) |> 
    dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::mutate(safe_predict = factor(safe_predict, levels = c("0", "0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::rename(values = mean) |>
    dplyr::rename(cla_acc = acc_id) |>
    dplyr::mutate(type = type) |>
    dplyr::mutate(true = true)
  
  return(data)
}

load("Working_10_sim/nb_species.Rdata")
load("Working_10_sim/sum_maxN.Rdata")
load("Working_10_sim/hill_number.Rdata")

true_abund = 65
to_plot_big_sum_maxN = prepare_data(sum_maxN, 'Abundance', true_abund)
abundance_plot = facet_h_1(to_plot_big_sum_maxN, 'Total Abundance') ; abundance_plot

true_richness = 15
to_plot_big_nb_species = prepare_data(nb_species, 'Species richness', true_richness)
richness_plot = facet_h_middle(to_plot_big_nb_species, 'Species richness') ; richness_plot

true_div = 8.713867
to_plot_big_hill = prepare_data(hill_number, 'Species diversity', true_div)
diversity_plot = facet_h_last(to_plot_big_hill, 'Species diversity'); diversity_plot

library(patchwork)
fig = abundance_plot + richness_plot + diversity_plot ; fig
ggplot2::ggsave("fig/fig3.jpeg", plot = fig, width = 30, height = 20, units = "cm")

