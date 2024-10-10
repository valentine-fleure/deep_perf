prepare_data = function(data){
  data = data |>
    dplyr::filter(safe_predict == 0.9 | safe_predict == 0 | safe_predict == 0.99) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99 | acc_det == 0.9) |>
    dplyr::filter(recall == 0.8 | recall == 0.99 | recall == 0.9) |>
    dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::mutate(safe_predict = factor(safe_predict, levels = c("0", "0.9", "0.99"))) |>
    dplyr::mutate(fps = factor(fps, levels = c("0.25", "0.5", "1", "2", "5", "10", "30"))) |>
    dplyr::rename(det_recall = recall) |>
    dplyr::rename(cla_acc = acc_id) |>
    dplyr::rename(det_prec = acc_det) |>
    dplyr::mutate(mean = 1 - mean)
  
  return(data)
}

facet_plot = function(to_plot, y){
  to_plot = to_plot |>
    dplyr::filter(safe_predict == 0)
  
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = mean, colour = cla_acc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, 1) +
    ggplot2::labs(y = y) +
    ggplot2::labs(x = "Processing rate (fps)") + 
    ggplot2::facet_grid(det_prec ~ det_recall, labeller = ggplot2::label_both) + 
    ggplot2::theme_bw()
  
  return(p)
}

load("Working_10_sim/bray_curtis.Rdata")
to_plot_bray = prepare_data(bray_curtis)
fig = facet_plot(to_plot_bray, "Bray Curtis similarity")
ggplot2::ggsave("fig/fig2.jpeg", plot = fig, width = 30, height = 20, units = "cm")

load("Working_10_sim/jaccard.Rdata")
to_plot_jaccard = prepare_data(jaccard)
fig = facet_plot(to_plot_jaccard, "Jaccard index")
ggplot2::ggsave("fig/figS2.jpeg", plot = fig, width = 30, height = 20, units = "cm")
