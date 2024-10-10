library(patchwork)

plot = function(to_plot){
  minmin = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.8 & safe_predict == 0)
  
  minmax = to_plot |>
    dplyr::filter(acc_det == 0.8 & recall == 0.99 & safe_predict == 0) 
  
  maxmin = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.8 & safe_predict == 0) 
  
  maxmax = to_plot |>
    dplyr::filter(acc_det == 0.99 & recall == 0.99 & safe_predict == 0) 
  
  minmin_plot = ggplot2::ggplot(minmin, ggplot2::aes(x= fps, y = mean, colour = acc_id)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.8") 
  
  minmax_plot = ggplot2::ggplot(minmax, ggplot2::aes(x= fps, y = mean, colour = acc_id)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.8\nrecall = 0.99") 
  
  maxmin_plot = ggplot2::ggplot(maxmin, ggplot2::aes(x= fps, y = mean, colour = acc_id)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.80") 
  
  maxmax_plot = ggplot2::ggplot(maxmax, ggplot2::aes(x= fps, y = mean, colour = acc_id)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::annotate("label", x = Inf, y = Inf, vjust=1, hjust=1, label = "acc_det = 0.99\nrecall = 0.99")
  
  fig <- (minmin_plot | minmax_plot) / (maxmin_plot | maxmax_plot)
  
  return(fig)
}

prepare_data = function(data){
  data = data |>
    dplyr::filter(safe_predict == 0.9 | safe_predict == 0 | safe_predict == 0.99) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
    dplyr::filter(recall == 0.8 | recall == 0.99) |>
    dplyr::mutate(acc_id = factor(acc_id, levels = c("0.8", "0.85", "0.9", "0.95", "0.99"))) |>
    dplyr::mutate(safe_predict = factor(safe_predict, levels = c("0", "0.9", "0.99"))) |>
    dplyr::mutate(fps = factor(fps, levels = c("0.25", "0.5", "1", "2", "5", "10", "30")))
  
  return(data)
}

facet_plot = function(to_plot, name){
  to_plot = to_plot |>
    dplyr::filter(safe_predict == 0) |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
    dplyr::filter(recall == 0.8 | recall == 0.99)
  
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = mean, colour = acc_id)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::ggtitle(name) +
    ggplot2::facet_grid(acc_det ~ recall, labeller = ggplot2::label_both)
  
  return(p)
}

facet_plot_sp = function(to_plot, name){
  to_plot = to_plot |>
    dplyr::filter(acc_det == 0.8 | acc_det == 0.99) |>
    dplyr::filter(recall == 0.8 | recall == 0.99)
  
  p = ggplot2::ggplot(to_plot, ggplot2::aes(x= fps, y = mean, colour = acc_id, shape = safe_predict, group = interaction(fps, safe_predict))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.1, position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::geom_point(position = ggplot2::position_dodge(width=0.75)) + 
    ggplot2::ylim(min(to_plot$mean)-0.005, max(to_plot$mean)+0.005) +
    ggplot2::ggtitle(name) +
    ggplot2::facet_grid(acc_det ~ recall, labeller = ggplot2::label_both)
  
  return(p)
}

load("Working_10_sim/bray_curtis.Rdata")
load("Working_10_sim/jaccard.Rdata")
load("Working_10_sim/missed.Rdata")
load("Working_10_sim/invented.Rdata")
load("Working_10_sim/nb_species.Rdata")
load("Working_10_sim/sum_maxN.Rdata")
load("Working_10_sim/hill_number.Rdata")

to_plot_jaccard = prepare_data(jaccard)
to_plot_bray = prepare_data(bray_curtis)
to_plot_missed = prepare_data(missed)
to_plot_invented = prepare_data(invented)
to_plot_sp = prepare_data(nb_species)
to_plot_abund = prepare_data(abundance)
to_plot_sum_maxN = prepare_data(sum_maxN)
to_plot_hill = prepare_data(hill_number)

fig_jaccard = facet_plot(to_plot_jaccard, "jaccard")
fig_bray = facet_plot(to_plot_bray, "bray_curtis")
# ggplot2::ggsave("Working_10_sim/bray_curtis.png", plot = fig_bray, width = 30, height = 20, units = "cm")
# fig1a = fig_jaccard + fig_bray
# ggplot2::ggsave("Working_10_sim/fig1a.png", plot = fig1a, width = 30, height = 20, units = "cm")

fig_sp = facet_plot(to_plot_sp, "nb_species")
fig_sum_maxN = facet_plot(to_plot_sum_maxN, "abundance")
fig_indiv = facet_plot(to_plot_abund, "nb_indiv")
# fig1b = fig_sp + fig_abund
# ggplot2::ggsave("Working_10_sim/fig1b.png", plot = fig1b, width = 30, height = 20, units = "cm")

fig1a = fig_sp + fig_jaccard
fig1b = fig_sum_maxN+ fig_bray
ggplot2::ggsave("Working_10_sim/fig1a.png", plot = fig1a, width = 30, height = 20, units = "cm")
ggplot2::ggsave("Working_10_sim/fig1b.png", plot = fig1b, width = 30, height = 20, units = "cm")

fig_missed = facet_plot(to_plot_missed, "missed")
fig_invented = facet_plot(to_plot_invented, "invented")


fig2 = fig_missed + fig_invented
ggplot2::ggsave("Working_10_sim/missed_invented.png", plot = fig2, width = 30, height = 20, units = "cm")

fig_missed_sp = facet_plot_sp(to_plot_missed, "missed")
fig_invented_sp = facet_plot_sp(to_plot_invented, "invented")

fig3 = fig_missed_sp + fig_invented_sp

fig_sp = facet_plot(to_plot_sp, "nb_species")
fig_abund = facet_plot(to_plot_abund, "abundance")
