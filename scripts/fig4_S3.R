prepare_data = function(data){
  data = data |>
    dplyr::filter(fps == 1) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99 | acc_det == 0.9) |>
    dplyr::filter(recall == 0.8 | recall == 0.99 | recall == 0.9) |>
    dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::mutate(safe_predict = factor(safe_predict, levels = c("0","0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::rename(det_recall = recall) |>
    dplyr::rename(cla_acc = acc_id) |>
    dplyr::rename(det_prec = acc_det) |>
    dplyr::mutate(mean = 1- mean)
  
  return(data)
}

facet_plot = function(to_plot, y){
  
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= safe_predict, y = mean, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(0, 1.005) +
    ggplot2::labs(y = y) +
    ggplot2::labs(x = "Post-processing threshold") + 
    ggplot2::facet_grid(det_prec ~ det_recall, labeller = ggplot2::label_both) + 
    ggplot2::theme_bw()
  
  return(p)
}

load("Working_10_sim/bray_curtis.Rdata")
to_plot_bray = prepare_data(bray_curtis)
fig = facet_plot(to_plot_bray, "Bray Curtis similarity") ; fig
ggplot2::ggsave("fig/fig4.jpeg", plot = fig, width = 30, height = 20, units = "cm")

load("Working_10_sim/jaccard.Rdata")
to_plot_jaccard = prepare_data(jaccard)
fig = facet_plot(to_plot_jaccard, "Jaccard index") ; fig
ggplot2::ggsave("fig/figS3.jpeg", plot = fig, width = 30, height = 20, units = "cm")

