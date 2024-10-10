####### Fig S1

plot_species_time = function(matrix, duplicate = 30, time_steps = 600){
  matrix_to_plot = dplyr::mutate(matrix, time = 1:(time_steps*duplicate)) |>
    tidyr::pivot_longer(cols = c(1:ncol(matrix)), names_to = "species", values_to = "count")
  
  matrix_to_plot[matrix_to_plot == 0] = NA
  
  ggplot2::ggplot(matrix_to_plot, ggplot2::aes(x = time, y = factor(species, level = level_order), fill = count)) + 
    ggplot2::geom_tile() + 
    ggplot2::theme_classic() +
    ggplot2::scale_fill_gradient(low = "#e5efe5", high = "darkgreen", na.value = "white") + 
    ggplot2::labs(fill = "number of \nindividuals", 
                  x = "time (s)",
                  y = "")
}

load("Working_10_sim/Simu_20240103/matrix_species.Rdata")
nbr_species = ncol(matrix_species)
level_order = paste0("species_", 1:nbr_species)
plot_species_time(matrix_species)
