

dataset_names <- gsub("RMSE_|.csv", "", list.files("Output/N1_Results/Rescaled_results/HMM_kalman_filter_results/RMSE/"))

var_lowest_rmse <- find_lowest_rmse(path_list = list("Output/N1_Results/Rescaled_results/VAR_kalman_filter_RMSE/",
                                                     "Output/N1_Results/Rescaled_results/VAR_listwise_deletion_RMSE/"),
                                    dataset_names = dataset_names, 
                                    model_names = c("kalman_filter", "listwise_deletion"), 
                                    show_equal_results = TRUE,
                                    format_output = "matrix")


var_lowest_rmse <- var_lowest_rmse[,order(var_lowest_rmse[1,], decreasing = TRUE)]



palette <-   wesanderson::wes_palette("Darjeeling1", 3)



pdf(file = "Output/Figures/var_methods_lowest_rmse.pdf", 
    width = standard_figure_with,
    height = 5)

lowest_rmse_percentage_barplot(var_lowest_rmse, 
                               palette, 
                               split = 2, 
                               margin_bottom_graphs = 1.5, 
                               margin_dataset_labels = 6,
                               dataset_labels_position = -3.5,
                               labels_width = 1,
                               barplot_width = 1.3,
                               vline_colour = "black")

dev.off()


lowest_rmse_percentage_barplot <- function(matrix_input, 
                                           palatte, 
                                           #clean_labels, 
                                           split = 1, 
                                           margin_bottom_graphs = 2, 
                                           margin_dataset_labels = 8, 
                                           dataset_labels_position = -1, 
                                           labels_width = 1,
                                           barplot_width = 3, 
                                           vline_colour = "red"){
  
  
  
  if(split == 2){
    layout(matrix(c(1:4,rep(5, 4)), nrow = 2, byrow = TRUE), 
           widths = c(labels_width, barplot_width, labels_width, barplot_width),
           heights = c(12,1))
    par(oma = c(.5, .5, .5, 1.5)) 
    
    matrix_input_1 <- matrix_input[,as.integer(n_bars/2):1]
    
    
  } else {
    
    layout(matrix(c(1:2,rep(3, 2)), nrow = 2, byrow = TRUE), 
           widths = c(labels_width, barplot_width),
           heights = c(12,1))
    par(oma = c(1, 1, 1, 1)) 
    matrix_input_1 <- matrix_input
    
  }
  
  clean_labels <- clean_dataset_names(colnames(matrix_input_1))
  n_bars_1 <- ncol(matrix_input_1)
  par(mar = c(margin_bottom_graphs,margin_dataset_labels,0,0), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0,0), ylim = c(0,n_bars_1))
  axis(2, at = (1:n_bars_1 -.5), labels = clean_labels, 
       las = 1, tick = FALSE, line = dataset_labels_position)
  
  par(mar = c(margin_bottom_graphs,0.1,0,0), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0,1), ylim = c(0,n_bars_1))
  axis(1, at = seq(0, 1, length.out = 5), labels = paste0(seq(0, 100, length.out = 5), "%"))
  barplot(matrix_input_1, 
          width = 1,
          space = 0,
          horiz = TRUE,
          beside = FALSE, 
          axes = FALSE,
          axisnames = FALSE,
          col = palette, 
          add = TRUE
  )
  abline(v=.5, col = vline_colour)
  
  if(split == 2){
    matrix_input_2 <- matrix_input[,n_bars:as.integer(n_bars/2)]
    clean_labels <- clean_dataset_names(colnames(matrix_input_2))
    
    n_bars_2 <- ncol(matrix_input_2)
    par(mar = c(margin_bottom_graphs,margin_dataset_labels,0,0), xaxs = "i", yaxs = "i")
    plot.new()
    plot.window(xlim = c(0,0), ylim = c(0,n_bars_2))
    axis(2, at = (1:n_bars_2 -.5), labels = clean_labels, 
         las = 1, tick = FALSE, line = dataset_labels_position)
    
    par(mar = c(margin_bottom_graphs,0.1,0,0), xaxs = "i", yaxs = "i")
    plot.new()
    plot.window(xlim = c(0,1), ylim = c(0,n_bars_2))
    axis(1, at = seq(0, 1, length.out = 5), labels = paste0(seq(0, 100, length.out = 5), "%"))
    barplot(matrix_input_2, 
            width = 1,
            space = 0,
            horiz = TRUE,
            beside = FALSE, 
            axes = FALSE,
            axisnames = FALSE,
            col =palette, 
            add = TRUE
    )
    abline(v=.5, col = vline_colour)
  }
  par(mar = c(0,0,0,0))
  plot.new()
  legend("bottom", 
         legend = clean_model_names(row.names(matrix_input)), 
         fill = palette, 
         horiz = TRUE, inset = c(0, 0), bty = "n")
  

}








layout(matrix(1:20, nrow = 4, byrow = TRUE))

palette_names <- c("BottleRocket1", "BottleRocket2", "Rushmore1", "Royal1", "Royal2", 
                   "Zissou1", "Darjeeling1", "Darjeeling2", "Chevalier1" , "FantasticFox1" , "Moonrise1", 
                   "Moonrise2", "Moonrise3", "Cavalcanti1", "GrandBudapest1", "GrandBudapest2", "IsleofDogs1", 
                   "IsleofDogs2", "FrenchDispatch", "AsteroidCity2", "AsteroidCity2", "AsteroidCity3")


wesanderson::wes_palette(palette_names[1])
wesanderson::wes_palette(palette_names[2])
wesanderson::wes_palette(palette_names[3])
wesanderson::wes_palette(palette_names[4])
wesanderson::wes_palette(palette_names[5])
wesanderson::wes_palette(palette_names[6])
wesanderson::wes_palette(palette_names[7])
wesanderson::wes_palette(palette_names[8])
wesanderson::wes_palette(palette_names[9])
wesanderson::wes_palette(palette_names[10])
wesanderson::wes_palette(palette_names[11])
wesanderson::wes_palette(palette_names[12])
wesanderson::wes_palette(palette_names[13])
wesanderson::wes_palette(palette_names[14])
wesanderson::wes_palette(palette_names[15])
wesanderson::wes_palette(palette_names[16])
wesanderson::wes_palette(palette_names[17])
wesanderson::wes_palette(palette_names[18])
wesanderson::wes_palette(palette_names[19])
wesanderson::wes_palette(palette_names[20])

wesanderson::wes_palette(palette_names[22])

graphics.off() 