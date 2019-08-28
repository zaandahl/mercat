#' m_spol_freq
#'
#' Displays the spoligotype patterns in a data set. 
#' Black represents the presence and white the absence of a spacer. 
#' The spacer frequencies are summarised in a heat plot at the bottom.
#' @param merdata A merdata object
#' @param title the title of the graph
#' @param title_size the font size of the title
#' @param xaxis_size the font size of the X-axis
#' @param yaxis_size the font size of the Y-axis
#' @param label_size the font size for labels
#' @param summarylab the label of the last row
#' @param xlab the X-axis label
#' @param ylab the Y-axis label
#' @param col_high colour for spacer presence
#' @param col_low colour for spacer absence
#' @examples
#' m_spol_freq(monteserin)
#' @keywords spoligotype SPOL merdata
#' @export
m_spol_freq <- function(merdata, 
                        title = "Spoligotype frequency", 
                        title_size = 16,
                        xaxis_size = 12,
                        yaxis_size = 12,
                        label_size = 12,
                        summarylab = "Summary",
                        xlab = "Spacer",
                        ylab = "Count",
                        col_high = "black",
                        col_low = "white") {
  variable <- rnum <- value <- NULL # appease the build
	if(class(merdata) != "merdata")
    stop("The merdata object is not valid.", call. = FALSE)
  s_type <- m_get_first_type(merdata, "SPOL")
  merdata <- m_aggregate(merdata, s_type)
  merdata_sum <- summary(merdata)
	spolmat <- m_spol(merdata)[, -1]
  space_sum <- unlist(merdata_sum[[s_type]]$spacer_freq)
  blank_row <- matrix(NA, nrow = 1, ncol = ncol(spolmat))
  spolmat <- rbind(spolmat, blank_row, space_sum)
  rownames(spolmat) <- c(merdata$data$COUNT, "", summarylab)
  spoldf <- as.data.frame(spolmat)
  rnames <- rev(rownames(spolmat))
  spoldf$rnum <- seq(nrow(spoldf), 1)
  spol_plot <- spoldf %>%
    reshape2::melt(id.vars = "rnum") %>%
    tidyr::replace_na(list(value = 0)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = variable, y = factor(rnum), fill = value)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::scale_fill_gradient(low = col_low, high = col_high) +
      ggplot2::guides(fill = FALSE) +
      ggplot2::scale_y_discrete(labels = rnames) + 
      ggplot2::ggtitle(title) +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) +
      ggplot2::theme_minimal() +
      ggplot2::theme(title = ggplot2::element_text(size = title_size),
        axis.text = ggplot2::element_text(size = label_size),
        axis.title.x = ggplot2::element_text(size = xaxis_size, face = "bold"),
        axis.title.y = ggplot2::element_text(size = yaxis_size, face = "bold"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black"))
  return(spol_plot)
}


#' m_res_freq
#'
#' Calculates a frequency histogram of drug resistant states using data of type RES.
#' @param merdata a merdata object
#' @param title title of the graph
#' @param title_size font size for main title
#' @param xaxis_size X axis font size
#' @param yaxis_size Y axis font size
#' @param label_size font size for resistance labels
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param fill Colour of resistance bars
#' @param orderbynames TRUE to order by names. FALSE to order by counts.
#' @param reverseorder Reverse bar order
#' @param legend Add a legend 
#' @param legend_x Legend X position (0-1)
#' @param legend_y Legend Y position (0-1)
#' @param legend_size Size of legend text
#' @param flip TRUE for horizontal bars FALSE for vertical bars
#' @examples
#' m_res_freq(monteserin)
#' # omit xlabel
#' m_res_freq(monteserin, xlab = "")
#' # change label size
#' m_res_freq(monteserin, label_size = 16)
#' @keywords RES resistance susceptible merdata
#' @export
m_res_freq <- function(merdata,
                       title = "Drug resistance frequency",
                       title_size = 16,
                       xaxis_size = 12,
                       yaxis_size = 12,
                       label_size = 12,
                       xlab = NULL,
                       ylab = "Count",
                       fill = "steelblue",
                       orderbynames = TRUE,
                       reverseorder = FALSE,
                       legend = TRUE,
                       legend_x = 0.5,
                       legend_y = 1,
                       legend_size = 4,
                       flip = TRUE) {
  x <- y <- label <- NULL  # appease the build
	if(class(merdata) != "merdata")
    stop("The merdata object is not valid.", call. = FALSE)
	r_type <- m_get_first_type(merdata, "RES")
  merdata <- m_aggregate(merdata, r_type)
	if(is.null(xlab)) xlab <- m_get_details(merdata, r_type)
  drug_abb <- m_get_details(merdata, r_type, split = TRUE)
  drug_full <- drug_abb %>% m_antibiotic_full_names()
  drug_leg <- paste0(drug_full, " (", drug_abb, ")\n", collapse = "")
  drug_leg <- drug_leg %>% substr(1, nchar(drug_leg) - 1)

	names <- apply(merdata$data[r_type], 1, paste, collapse = "")
  counts <- merdata$data$COUNT
  if(!orderbynames) {
    name_order <- order(counts)
    if(reverseorder) name_order <- rev(order(counts))
  } else {
    name_order <- order(names)
    if(reverseorder) name_order <- rev(order(names))
  }
  names <- factor(names, levels = names[name_order])
  res_df <- data.frame(x = names, y = counts)

  leg_df <- data.frame(label = drug_leg,
                       x = round(length(names) * legend_x),
                       y = round(max(res_df$y) * legend_y))

  resfreqplot <- res_df %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) + 
    ggplot2::geom_bar(stat = "identity", fill = fill) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) + 
    ggplot2::ylab(ylab) + 
    ggplot2::theme_minimal() + 
    ggplot2::theme(title = ggplot2::element_text(size = title_size),
          axis.text.x = ggplot2::element_text(size = label_size, angle = 90),
          axis.text.y = ggplot2::element_text(size = label_size),
          axis.title.x = ggplot2::element_text(size = xaxis_size, face = "bold"),
          axis.title.y = ggplot2::element_text(size = yaxis_size, face = "bold"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(colour = "black"))
  if(legend) resfreqplot <- resfreqplot +
    ggplot2::geom_label(data = leg_df,
                        ggplot2::aes(x = x, y = y, label = label),
                        hjust = 1,
                        vjust = 1,
                        size = legend_size)
  if(flip) resfreqplot <- resfreqplot +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = label_size, angle = 0)) +
    ggplot2::coord_flip() 
  resfreqplot
}


#' m_mlva_freq
#'
#' Plots the frequency of VNTR repeat numbers as a heat plot.
#' @param merdata a merdata object
#' @param title an overall title for the plot
#' @param title_size main font size
#' @param xaxis_size X axis font size
#' @param yaxis_size Y axis font size
#' @param label_size The size of the tile labels
#' @param text_size The size of text for the tiles
#' @param legend_size the size of the legend text
#' @param xlab a label for the x axis
#' @param ylab a label for the y axis
#' @param col_high colour for high frequency tiles
#' @param col_low colour for low frequency tiles
#' @param values display frequency values on tiles
#' @param legend display a legend
#' @references {Aminian et al. (2009). Determination of Major Lineages of Mycobacterium tuberculosis Complex using Mycobacterial Interspersed Repetitive Units. IEEE Int Conf on Bioinformatics Biomed, 1;2009:338-343.}
#' @references {Aandahl et al. (2012). A model-based Bayesian estimation of the rate of evolution of VNTR loci in Mycobacterium tuberculosis. PLoS Comput Biol. 2012;8(6):e1002573.}
#' @examples
#' m_mlva_freq(monteserin)
#' @keywords MLVA MIRU VNTR merdata
#' @export
m_mlva_freq <- function(merdata,
                        title = "MLVA frequency",
                        title_size = 16,
                        xaxis_size = 12,
                        yaxis_size = 12,
                        label_size = 12,
                        text_size = 6,
                        legend_size = 12,
                        xlab = "VNTR locus",
                        ylab = "Repeats",
                        col_high = "steelblue",
                        col_low = "white",
                        values = TRUE,
                        legend = FALSE) {
  value <- repeats <- locus <- value_na <- NULL # appease the build
	if(class(merdata) != "merdata")
    stop("The merdata object is not valid.", call. = FALSE)
  max_repeats <- merdata %>% m_mlva() %>% nrow()
  mheat <- m_mlva(merdata) %>%
    reshape2::melt() %>%
    dplyr::mutate(value_na = value) %>%
    tidyr::replace_na(list(value = 0)) 
  colnames(mheat) <- c("locus", "repeats", "value", "value_na")
  mlva_plot <- mheat %>% ggplot2::ggplot(ggplot2::aes(x = repeats, y = locus, fill = value)) +
    ggplot2::geom_tile(colour = "white") +
    # ggplot2::geom_text(ggplot2::aes(label = value_na), na.rm = TRUE) +
    ggplot2::scale_fill_gradient(low = col_low, high = col_high) +
    ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1),
                                breaks = seq(1, max_repeats)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(xlab) + 
    ggplot2::ylab(ylab) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(title = ggplot2::element_text(size = title_size),
      axis.text = ggplot2::element_text(size = label_size),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.title.x = ggplot2::element_text(size = xaxis_size, face = "bold"),
      axis.title.y = ggplot2::element_text(size = yaxis_size, face = "bold"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"))
  if(legend) {
    mlva_plot <- mlva_plot + 
    ggplot2::guides(fill = ggplot2::guide_colorbar(name = "")) + 
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = legend_size))
  } else {
    mlva_plot <- mlva_plot + 
    ggplot2::guides(fill = FALSE) 
  }
  if(values) mlva_plot <- mlva_plot +
    ggplot2::geom_text(ggplot2::aes(x = repeats, y = locus, label = value_na),
              color = "black",
              size = text_size,
              na.rm = TRUE) 
  return(mlva_plot)
}

#' m_res_corr_mat
#'
#' Plots the linkage disequilibrium (LD) between pairs of antibiotic resistance frequencies. Linkage disequilibrium is computed as 
#' \deqn{r^2 = \frac{D_{AB}^2}{p_A(1-p_A)p_B(1-p_B)} }where \deqn{D_{AB} = p_{AB} - (p_A*p_B)} and pA and pB are the frequencies of resistance to antibiotics A and B respectively. 
#' @param merdata A merdata object
#' @param title The title of the graph
#' @param title_size The font size of main
#' @param xaxis_size The font size of the x axis
#' @param yaxis_size The font size of the y axis
#' @param label_size The size of the tile labels
#' @param text_size The font size for text on labels
#' @param legend_size The size of the legend text
#' @param xlab The label of the x axis
#' @param ylab The label of the y axis
#' @param col_high Colour for high frequency tiles
#' @param col_low Colour for low frequency tiles
#' @param col_na Colour for tiles without information
#' @param values Display correlation values on tiles
#' @param legend Display a legend
#' @param symmetric Display a symmetric graph
#' @param digits the significant digits to show in the plot
#' @references {Slatkin (2008). Linkage disequilibrium-understanding the evolutionary past and mapping the medical future. Nature Reviews Genetics, 9(6):477-485, 2008.}
#' @examples
#' m_res_corr_mat(monteserin)
#' @keywords RES resistance susceptible merdata
#' @export
m_res_corr_mat <- function(merdata,
                           title = "Resistance Correlation",
                           title_size = 16,
                           xaxis_size = 12,
                           yaxis_size = 12,
                           label_size = 12,
                           text_size = 6,
                           legend_size = 12,
                           xlab = "Antibiotic",
                           ylab = "Antibiotic",
                           col_high = "steelblue",
                           col_low = "white",
                           col_na = "white",
                           values = TRUE,
                           legend = FALSE, 
                           symmetric = FALSE,
                           digits = 2) { 
  Var1 <- Var2 <- value <- NULL # appease the build
  if(class(merdata) != "merdata")
    stop("The merdata object is not valid.", call. = FALSE)

  r_square <- m_res_correlation(merdata)
  r_square <- round(r_square, digits)
  r_square[upper.tri(r_square, diag = FALSE)] <- NA
  if(symmetric) 
    r_square[upper.tri(r_square)] <- t(r_square)[upper.tri(r_square)]
  cor_plot <- r_square %>%
    reshape2::melt() %>%
    ggplot2::ggplot(ggplot2::aes(Var2, forcats::fct_rev(Var1), fill = value)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient(
        low = col_low, 
        high = col_high, 
        na.value = col_na) +
      ggplot2::xlab(xlab) + 
      ggplot2::ylab(ylab) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::theme(title = ggplot2::element_text(size = title_size),
        axis.text = ggplot2::element_text(size = label_size),
        axis.title.x = ggplot2::element_text(size = xaxis_size, face = "bold"),
        axis.title.y = ggplot2::element_text(size = yaxis_size, face = "bold"),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")) 
                          
  if(legend) {
    cor_plot <- cor_plot + 
    ggplot2::guides(fill = ggplot2::guide_colorbar(name = "")) + 
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = legend_size))
  } else {
    cor_plot <- cor_plot + 
    ggplot2::guides(fill = FALSE) 
  }

  if(values) cor_plot <- cor_plot +
    ggplot2::geom_text(ggplot2::aes(Var2, Var1, label = value),
              color = "black",
              size = text_size,
              na.rm = TRUE) 

  return(cor_plot)  
}

