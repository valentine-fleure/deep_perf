#abondance, richesse, diversité (hill)
# facet_plot_big = function(to_plot){
#   
#   p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = values, colour = acc_id)) +
#     ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
#     ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
#     ggplot2::ylim(min(to_plot$values)-0.005, max(to_plot$values)+0.005) +
#     ggplot2::facet_grid(det_type ~ type, scales = "free_y")
# 
#   return(p)
# }

facet_h_1 = function(to_plot, name){
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::guides(colour = "none") +
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ .) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = true, yend = true),
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
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::guides(colour = "none") +
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ .) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = true, yend = true),
                          color = "red") +
    ggplot2::theme_bw() + 
    ggplot2::theme(strip.background = ggplot2::element_blank(),               
                   strip.text.y = ggplot2::element_blank(), 
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.y = ggplot2::element_blank()) +  
    ggplot2::labs(x = "Processing rate (fps)") +
    ggplot2::ggtitle(name)
}

facet_h_last = function(to_plot, name){
  labels = ggplot2::as_labeller(c("det_1" = "Det 1", "det_2" = "Det 2", "det_3" = "Det 3", "det_4" = "Det 4"))
  
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = values, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=values-sd, ymax=values+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$true)-5, max(to_plot$values)+5) +
    ggplot2::facet_grid(det_type ~ ., labeller = (labels)) +
    ggplot2::geom_segment(ggplot2::aes(x = as.numeric(fps)-0.25, xend = as.numeric(fps)+0.25, y = true, yend = true),
                          color = "red") +
    ggplot2::ggtitle(name) +  
    ggplot2::theme_bw() + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank())
}

prepare_data = function(data_to_prepare, type, true) {
  data = data_to_prepare |>
    dplyr::filter(safe_predict == 0) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
    dplyr::filter(recall == 0.8 | recall == 0.99) |>
    dplyr::select(fps, acc_det, acc_id, recall, mean, sd) |>
    dplyr::mutate(detecteur = stringr::str_c(recall, acc_det, sep = "_")) |>
    dplyr::mutate(det_type = dplyr::case_when(
      detecteur == "0.8_0.8" ~ "det_1",
      detecteur == "0.8_0.99" ~ "det_3",
      detecteur == "0.99_0.8" ~ "det_2",
      detecteur == "0.99_0.99" ~ "det_4")) |> 
    dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::mutate(fps = factor(fps, levels = c("0.25", "0.5", "1", "2", "5", "10", "30"))) |>
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
abundance_plot = facet_h_1(to_plot_big_sum_maxN, 'Total abundance') ; abundance_plot

true_richness = 15
to_plot_big_nb_species = prepare_data(nb_species, 'Species richness', true_richness)
richness_plot = facet_h_middle(to_plot_big_nb_species, 'Species richness') ; richness_plot

true_div = 8.713867
to_plot_big_hill = prepare_data(hill_number, 'Species diversity', true_div)
diversity_plot = facet_h_last(to_plot_big_hill, 'Species diversity'); diversity_plot

library(patchwork)
fig = abundance_plot + richness_plot + diversity_plot; fig 
ggplot2::ggsave("fig/fig1.jpeg", plot = fig, width = 30, height = 20, units = "cm")

# to_plot_big = to_plot_big_sum_maxN |>
#   dplyr::bind_rows(to_plot_big_nb_species) |> 
#   dplyr::bind_rows(to_plot_big_hill)
# 
# p = facet_plot_big(to_plot_big); p  
