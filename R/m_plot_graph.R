#' m_spolforest
#'
#' Draws a spoligoforest using spoligotypes, incorporating drug resistance status of isolates if present.
#' @param merdata A merdata object
#' @param tree_seed A seed for the random number generation. 
#' @param \dots Additonal plotting parameters passed to the underlying graphing function \code{m_plot_graph}
#' @return A ggplot2 object
#' @references {Reyes et al. (2008). Models of deletion for visualizing bacterial variation: an application to tuberculosis spoligotypes. BMC Bioinformatics. 2008 Nov 27;9:496. doi: 10.1186/1471-2105-9-496.}
#' @references {Tang et al. (2008). spolTools: online utilities for analyzing spoligotypes of the Mycobacterium tuberculosis complex. Bioinformatics. 2008 Oct 15;24(20):2414-5.}
#' @examples
#' m_spolforest(monteserin)
#' @keywords spoligotype SPOL resistance RES merdata
#' @export
m_spolforest <- function(merdata, tree_seed = 1, ...) {
  m_plot_graph(merdata,
    tree_seed = tree_seed,
    plot_type = "spolforest",
    ...)
}

#' m_mst 
#'
#' Plots a Minimum Spanning Tree (MST) by computing weighted distances between molecular types.
#' @param merdata A merdata object
#' @param weights A named list of weights for the different molecular types. e.g. c("MLVA" = 1, "SPOL" = 1.2); by default all markers are given weights of 1. 
#' @param distances A named list of distance types (default c("MLVA" = "L1", "SPOL" = "spol"). Valid distances are "L1" for L1 or Manhattan distance, "hamming" for Hamming distance, 
#' @param tree_seed A seed for the random number generation. Change the value to generate a MST with a different layout.
#' @param \dots Additonal plotting parameters passed to the underlying graphing function \code{m_plot_graph}
#' @return A ggplot2 object
#' @references {Reyes et al. (2008). Models of deletion for visualizing bacterial variation: an application to tuberculosis spoligotypes. BMC Bioinformatics. 2008 Nov 27;9:496. doi: 10.1186/1471-2105-9-496.}
#' @examples
#' m_mst(monteserin)
#' @keywords spoligotype SPOL resistance RES MST merdata
#' @export
m_mst <- function(merdata, weights = NULL, distances = NULL, tree_seed = 1, ...) {
  m_plot_graph(merdata,
    weights = weights,
    distances = distances,
    tree_seed = tree_seed,
    plot_type = "mst",
    ...)
}

#' m_spolforest_nodes
#' 
#' Prints corresponding nodes for a spoligoforest plot. This function is a wrapper for \code{m_print_nodes}
#' @param merdata A merdata object
#' @param tree_seed A seed for the random number generation. 
#' @examples
#' m_spolforest_nodes(monteserin)
#' @keywords spoligotype SPOL resistance RES merdata
#' @export
m_spolforest_nodes <- function(merdata, tree_seed = 1) {
  m_print_nodes(merdata, plot_type = "spolforest", tree_seed = tree_seed)
}

#' m_mst_nodes
#' 
#' Prints corresponding nodes for MST plots. This function is a wrapper for \code{m_print_nodes}
#' @param merdata A merdata object
#' @param weights A named vector of weights for the different molecular types. e.g. c("MLVA" = 1, "SPOL" = 1.2); by default all markers are given weights of 1. 
#' @param distances A named list of distance types (default c("MLVA" = "L1", "SPOL" = "spol"). Valid distances are "L1" for L1 or Manhattan distance, "hamming" for Hamming distance, 
#' @param tree_seed A seed for the random number generation. Change the value to generate a MST with a different layout.
#' @return A data frame containing node information.
#' @examples
#' m_mst_nodes(monteserin)
#' @keywords spoligotype SPOL resistance RES MST merdata
#' @export
m_mst_nodes <- function(merdata,
                        weights = NULL,
                        distances = NULL,
                        tree_seed = 1) {
  m_print_nodes(merdata,
    plot_type = "mst",
    weights = weights,
    distances = distances,
    tree_seed = tree_seed)
}

#' m_print_nodes
#' 
#' Prints corresponding nodes for MST or spoligoforest plots.
#' @param merdata A merdata object
#' @param plot_type The type of plot to print node information from: "spolforest" or "mst"
#' @param weights A named vector of weights for the different molecular types. e.g. c("MLVA" = 1, "SPOL" = 1.2); by default all markers are given weights of 1. 
#' @param distances A named list of distance types (default c("MLVA" = "L1", "SPOL" = "spol"). Valid distances are "L1" for L1 or Manhattan distance, "hamming" for Hamming distance, 
#' @param tree_seed A seed for the random number generation. Change the value to generate a MST with a different layout.
#' @return A data frame containing node information.
#' @examples
#' m_print_nodes(monteserin)
#' @keywords spoligotype SPOL resistance RES MST merdata
#' @export
m_print_nodes <- function(merdata,
                         plot_type = "spolforest",
                         weights = NULL,
                         distances = NULL,
                         tree_seed = 1) {
  layoutX <- layoutY <- name <- COUNT <- ID <- groupID <- NODE <- NULL # appease the build
  if(class(merdata) != "merdata") 
  stop("The merdata object is not valid.", call. = FALSE)

  if(plot_type == "spolforest") {
    plotgraph <- m_res_spol_forest(merdata, 
     rooted = TRUE,
     tree_seed = tree_seed)
  } else if(plot_type == "mst") {
    plotgraph <- m_mst_from_data(merdata,
      weights = weights,
      distances = distances,
      noise = TRUE,
      tree_seed = tree_seed)
  } else stop("The plot_type must be 'mst' or 'spolforest'.", call. = FALSE)

  edge_df <- igraph::as_data_frame(plotgraph, what = "edges")
  vert_df <- igraph::as_data_frame(plotgraph, what = "vertices")

  # create a pretty data frame for printing
  pretty_df <- vert_df %>%
    dplyr::select(-c(layoutX, layoutY, name)) %>%
    dplyr::mutate(groupID = ifelse(dplyr::lag(groupID) != groupID, groupID, "")) %>%
    dplyr::mutate(NODE = ifelse(is.na(groupID), 1, groupID)) %>%
    dplyr::select(-groupID) %>%
    dplyr::select(NODE, COUNT, dplyr::everything(), ID)
  return(pretty_df)
}

#' m_plot_graph 
#'
#' Plots a spoligoforest or minimum spanning tree (MST).
#' @param merdata A merdata object
#' @param plot_type Either "spolforest" or "mst" to choose the plot type
#' @param weights A named vector of weights for the different molecular types. e.g. c("MLVA" = 1, "SPOL" = 1.2); by default all markers are given weights of 1. 
#' @param distances A named list of distance types (default c("MLVA" = "L1", "SPOL" = "spol"). Valid distances are "L1" for L1 or Manhattan distance, "hamming" for Hamming distance, 
#' @param tree_seed A seed for the random number generation. Change the value to generate a MST with a different layout.
#' @param title A title for the plot
#' @param title_size The text size for the title
#' @param node_label_size The text size for the node labels
#' @param core_rad The size of the inner nodes
#' @param ring_rad The base size of the outer rings
#' @param spacer_rad The gap between outer rings
#' @param ring_scale A factor to scale the rings by (uses percentage of the highest COUNT in the sample)
#' @param plot_scale_x A factor to scale the x-axis of the plot (1 is default)
#' @param plot_scale_y A factor to scale the y-axis of the plot (1 is default)
#' @param show_legend Display the plot resistance legend (meaningless if there are no resistance data in the merdata object)
#' @param legend_text_size The text size for the resistance legend
#' @param legend_x_location The x-axis location of the resistance legend on the plot (0-1)
#' @param legend_y_location The y-axis location of the resistance legend on the plot (0-1)
#' @param sensitive_colour The colour for drug sensitive arc segments
#' @param resistant_colour The colour for drug resistant arc segments
#' @param unknown_colour The colour for arc segments missing drug resistance information
#' @param core_colour The colour of the center of the nodes
#' @param outline_colour The colour outlining the nodes
#' @param segment_colour The colour of line segments between nodes
#' @param segment_label_colour The colour of the segment weight text (only valid for MST)
#' @param show_segment_label Display the segment weight (only valid for MST)
#' @param show_node_label Display the node ID information
#' @param arrow_size The size of the segment arrow (only valid for spoligoforest)
#' @return A ggplot2 object
#' @references {Reyes et al. (2008). Models of deletion for visualizing bacterial variation: an application to tuberculosis spoligotypes. BMC Bioinformatics. 2008 Nov 27;9:496. doi: 10.1186/1471-2105-9-496.}
#' @examples
#' m_plot_graph(monteserin, plot_type = "spolforest")
#' @keywords spoligotype SPOL spolforest resistance RES MST merdata
#' @export
m_plot_graph <- function(merdata,
                        plot_type = "spolforest",
                        weights = NULL,
                        distances = NULL,
                        tree_seed = 1,
                        title = NULL, 
                        title_size = 16,
                        node_label_size = 2,
                        core_rad = 0.3,
                        ring_rad = 0.05,
                        spacer_rad = 0.02,
                        ring_scale = 3,
                        plot_scale_x = 1,
                        plot_scale_y = 1,
                        show_legend = TRUE,
                        legend_text_size = 2,
                        legend_x_location = 0.98,
                        legend_y_location = 0.08,
                        sensitive_colour = "steelblue",
                        resistant_colour = "orange",
                        unknown_colour = "grey",
                        core_colour = "white",
                        outline_colour = "black",
                        segment_colour = "darkgrey",
                        segment_label_colour = "darkgrey",
                        show_segment_label = TRUE,
                        show_node_label = TRUE,
                        arrow_size = 0.01) {
  x <- y <- xend <- yend <- layoutX <- layoutY <- NULL # appease the build
  r0 <- r <- r_start <- r_end <- weight <- RES <- groupID <- NULL
  if(class(merdata) != "merdata") 
  stop("The merdata object is not valid.", call. = FALSE)

  if(plot_type == "spolforest") {
    plotgraph <- m_res_spol_forest(merdata,
     rooted = TRUE,
     tree_seed = tree_seed)
    arrow_type <- ggplot2::arrow(length = ggplot2::unit(arrow_size, "npc"),
                                 type = "closed")
  } else if(plot_type == "mst") {
    plotgraph <- m_mst_from_data(merdata,
      weights = weights,
      distances = distances,
      noise = TRUE,
      tree_seed = tree_seed)
    arrow_type <- NULL
  } else stop("The plot_type must be 'mst' or 'spolforest'.", call. = FALSE)

  edge_df <- igraph::as_data_frame(plotgraph, what = "edges")
  vert_df <- igraph::as_data_frame(plotgraph, what = "vertices")

  # determine whether to show resistance info and/or legend
  res_vec <- m_get_res_vec(merdata)
  show_res <- TRUE
  if(is.null(res_vec)) {
    show_legend <- FALSE
    show_res <- FALSE
  }

  # add colour scale for plot
  if(show_res)
    colour_scale <- c(sensitive_colour,
                      resistant_colour,
                      unknown_colour,
                      core_colour,
                      outline_colour)
  else 
    colour_scale <- c(sensitive_colour,
                      core_colour,
                      outline_colour)

  # add plot scaling, radius info, arc length
  vert_df <- vert_df %>%
    m_plot_scale(plot_scale_x, plot_scale_y) %>%
    m_add_radius(
      core_rad = core_rad,
      ring_rad = ring_rad,
      spacer_rad = spacer_rad,
      ring_scale = ring_scale) %>%
    m_add_arc_length(show_res = show_res) %>%
    m_beautify_nodes(core_rad = core_rad, spacer_rad = spacer_rad)
  
  # get plot extents for legend
  plot_extents <- vert_df %>%
    m_get_plot_extents()
  
  # add layout information to the edge_df
  edge_df <- edge_df %>%
    m_add_edge_layout(vert_df = vert_df,
                      plot_type = plot_type,
                      spacer_rad = spacer_rad)
  
  # create data frame for text labels (groupIDs)
  text_df <- vert_df %>%
    m_get_text_df()

  # create data frame for edge weight labels (if MST)
  if(plot_type == "mst" && show_segment_label) 
    edge_weight_df <- edge_df %>%
      m_get_edge_weight_df()
  else edge_weight_df <- NULL

  # draw main plot
  spolplot <- vert_df %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_df,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = segment_colour,
      arrow = arrow_type
    ) 
  if(!is.null(edge_weight_df)) spolplot <- spolplot +  # only show for mst
    ggplot2::geom_point(
      data = edge_weight_df,
      ggplot2::aes(x = x, y = y),
      size = node_label_size,
      color = "white"
    ) +
    ggplot2::geom_text(
      data = edge_weight_df,
      ggplot2::aes(x = x, y = y, label = weight),
      size = node_label_size,
      color = segment_label_colour
    ) 
  spolplot <- spolplot + 
    ggforce::geom_arc_bar(
      ggplot2::aes(x0 = layoutX,
          y0 = layoutY,
          r0 = r0,
          r = r,
          start = r_start,
          end = r_end,
          fill = RES),
      color = "transparent"
    ) + 
    ggplot2::scale_fill_manual(
      values = colour_scale,
      breaks = c("Sensitive", "Resistant", "Unknown")
    ) 
  if(show_node_label) 
  spolplot <- spolplot + 
    ggplot2::geom_text(
      data = text_df,
      ggplot2::aes(x = layoutX, y = layoutY, label = groupID),
      size = node_label_size
    ) 
  spolplot <- spolplot + 
    ggplot2::coord_fixed() +
    ggplot2::ggtitle(title) + 
    ggplot2::labs(fill = ggplot2::element_blank()) +
    ggforce::theme_no_axes() +
  # add legend (adds NULL if not available)
    ggplot2::theme(
      title = ggplot2::element_text(size = title_size),
      legend.position = "bottom") +
    m_plot_legend(
      res_vec,
      plot_extents = plot_extents, 
      show_legend = show_legend,
      core_rad = core_rad,
      ring_rad = ring_rad,
      sensitive_colour = sensitive_colour,
      legend_x_location = legend_x_location,
      legend_y_location = legend_y_location) +
    m_plot_legend_text(
      res_vec,
      plot_extents = plot_extents,
      show_legend = show_legend,
      core_rad = core_rad,
      ring_rad = ring_rad,
      legend_text_size = legend_text_size,
      legend_x_location = legend_x_location,
      legend_y_location = legend_y_location)  
  # remove any legend if it is not being shown
  if(!show_legend) spolplot <- spolplot + 
    ggplot2::theme(legend.position = "none")
  return(spolplot)
}



## FUNCTIONS THAT ARE NOT EXPORTED BELOW HERE

# return a vector of resistance names from header information
m_get_res_vec <- function(merdata) {
  FIELDS <- VALUES <- NULL # appease the build
  res_vec <- merdata$details %>%
      dplyr::filter(FIELDS == "RES") %>%
      dplyr::pull(VALUES) %>%
      unlist() %>%
      stringr::str_split(",") %>%
      unlist() %>%
      stringr::str_trim(side = "both") 
  if(length(res_vec) == 0) return(NULL)
  return(res_vec)
}

# function to scale a plot by a specified amount
m_plot_scale <- function(vert_df, plot_scale_x = 1, plot_scale_y = 1) {
  layoutX <- layoutY <- NULL  # appease the build
  if(!is.numeric(plot_scale_x) ||
    !is.numeric(plot_scale_y) ||
    plot_scale_x <= 0 ||
    plot_scale_y <= 0) return(vert_df)
  vert_df <- vert_df %>%
    dplyr::mutate(layoutX = layoutX * plot_scale_x,
                  layoutY = layoutY * plot_scale_y)
  return(vert_df)
}

# function to get extents of the plotting area
m_get_plot_extents <- function(vert_df) {
  layoutX <- layoutY <- NULL  # appease the build
  plot_extent <- vert_df %>% 
    dplyr::summarise(min_x = min(layoutX),
                     max_x = max(layoutX),
                     min_y = min(layoutY),
                     max_y = max(layoutY))
  return(plot_extent)
}

m_add_radius <- function(vert_df,
                         core_rad = 0.3,
                         ring_rad = 0.05,
                         spacer_rad = 0.02,
                         ring_scale = 3) {
  groupID <- COUNT <- prop <- group_num <- NULL
  exp_rad <- sum_rad <- order_num <- NULL # appease the build
  vert_df <- vert_df %>% 
    dplyr::mutate(groupID = as.integer(groupID),
                  COUNT = as.integer(COUNT),
                  prop = COUNT / max(COUNT)) %>%
    dplyr::group_by(groupID) %>%
    dplyr::mutate(order_num = dplyr::row_number()) %>%
    dplyr::arrange(groupID, dplyr::desc(order_num)) %>%  # preserve ring ordering
    dplyr::mutate(
      group_num = dplyr::row_number(),
      exp_rad = ring_rad + ring_scale * (ring_rad * prop),
      sum_rad = cumsum(exp_rad),
      r = core_rad + dplyr::lag(sum_rad) %>%
        tidyr::replace_na(0) + ((group_num - 1) * spacer_rad),
      r0 = core_rad + sum_rad + ((group_num - 1) * spacer_rad)) %>%
    dplyr::select(-c(exp_rad, sum_rad, group_num, prop)) %>%
    dplyr::ungroup()
  return(vert_df)
}

# adds arc length info to vertices df (r_num, r_start, r_end)
m_add_arc_length <- function(vert_df, show_res = TRUE) {
  RES <- r_num <- name <- NULL # appease the build
  if(show_res) {
    vert_df <- vert_df %>% 
      dplyr::mutate(RES = strsplit(as.character(RES), "")) %>%
      tidyr::unnest(RES) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(
        r_num = dplyr::row_number(),
        r_start = ((r_num - 1) * (2 * pi / max(r_num))) + (2 * pi / 360),
        r_end = (r_num * (2 * pi / max(r_num))) - (2 * pi / 360)) %>%
      dplyr::ungroup()
  } else {
    vert_df <- vert_df %>%
      dplyr::mutate(r_num = 1, r_start = 0, r_end = 2 * pi, RES = "S")
  }
  return(vert_df)
}

# add a background disc and inner and outer spacers
m_beautify_nodes <- function(vert_df, core_rad = 0.3, spacer_rad = 0.02) {
  groupID <- name <- layoutX <- layoutY <- r_num <- NULL
  r <- r0 <- r_start <- r_end <- RES <- NULL # appease the build
  white_disc <- vert_df %>% m_white_disc()
  inside_spacer <- vert_df %>% m_inside_spacer(core_rad, spacer_rad)
  outside_spacer <- vert_df %>% m_outside_spacer(spacer_rad)
  vert_df <- vert_df %>%
    dplyr::select(groupID, name, layoutX, layoutY,
                  r_num, r, r0, r_start, r_end, RES) %>%
    rbind(white_disc, inside_spacer, outside_spacer) %>%
    dplyr::arrange(groupID, r_num) %>%
    dplyr::mutate(RES = factor(RES,
      levels = c("S", "R", "-", "W", "B"),
      labels = c("Sensitive", "Resistant", "Unknown", "White", "Black")))
  return(vert_df)
}

# creates white background discs for all nodes
m_white_disc <- function(vert_df) {
  groupID <- name <- layoutX <- layoutY <- r <- r0 <- NULL  # appease the build
  white_disc <- vert_df %>%
    dplyr::select(groupID, name, layoutX, layoutY, r, r0) %>%
    dplyr::group_by(groupID) %>%
    dplyr::slice(which.max(r0)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      r_num = 0,
      r = 0,
      r_start = 0,
      r_end = 2 * pi,
      RES = "W") %>%
    dplyr::ungroup()
  return(white_disc)
}

# creates a black spacer on the inside of the core of the node
m_inside_spacer <- function(vert_df, core_rad = 0.3, spacer_rad = 0.02) {
  groupID <- name <- layoutX <- layoutY <- r <- r0 <- NULL # appease the build
  inside_spacer <- vert_df %>% 
    dplyr::select(groupID, name, layoutX, layoutY, r, r0) %>%
    dplyr::group_by(groupID) %>%
    dplyr::slice(which.max(r0)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      r_num = 0,
      r = core_rad - spacer_rad,
      r0 = core_rad,
      r_start = 0,
      r_end = 2 * pi,
      RES = "B") %>%
    dplyr::ungroup()
  return(inside_spacer)
}

# create a black spacer on the outside of the node
m_outside_spacer <- function(vert_df, spacer_rad = 0.02) {
  groupID <- layoutX <- layoutY <- name <- NULL
  r <- r0 <- r_num <- max_r_num <- NULL # appease the build
  outside_spacer <- vert_df %>%
    dplyr::select(groupID, name, layoutX, layoutY, r, r0, r_num) %>%
    dplyr::group_by(groupID) %>%
    dplyr::mutate(max_r_num = max(r_num)) %>%
    dplyr::slice(which.max(r0)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      r_num = max_r_num + 1,
      r = r0,
      r0 = r0 + spacer_rad,
      r_start = 0,
      r_end = 2 * pi,
      RES = "B") %>%
    dplyr::select(-max_r_num) %>%
    dplyr::ungroup()
  return(outside_spacer)
}

# add layout coords to the edge df
m_add_edge_layout <- function(edge_df,
                              vert_df,
                              plot_type = "spolforest",
                              spacer_rad = 0.02) {
  groupID <- name <- layoutX <- layoutY <- r0 <- max_r0 <- NULL
  x <- y <- xend <- yend <- NULL
  x1 <- x2 <- y1 <- y2 <- dmp <- outputs <- NULL # appease the build
  # create data frame with name info only to join with edge data frame
  vert_df <- vert_df %>%
    dplyr::select(groupID, name, layoutX, layoutY, r0) %>% 
    dplyr::group_by(groupID) %>%
    dplyr::mutate(max_r0 = max(r0)) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-c(groupID, r0)) %>%
    dplyr::distinct()
  # create edge data frame with layout coordinates
  edge_df <- edge_df %>%
    dplyr::inner_join(vert_df, by = c("from" = "name")) %>%
    dplyr::rename(x = layoutX, y = layoutY) %>% 
    dplyr::select(-max_r0) %>%
    dplyr::inner_join(vert_df, by = c("to" = "name")) %>%
    dplyr::rename(xend = layoutX, yend = layoutY) %>%
    dplyr::distinct()
  if(plot_type == "spolforest") {
    edge_df <- edge_df %>%
      dplyr::mutate(outputs = purrr::pmap(
        list(xend, x, yend, y, max_r0 + (2 * spacer_rad)),
        m_get_mid_point)) %>%
      dplyr::select(-c(xend, yend)) %>%
      tidyr::unnest(outputs)
  }
  return(edge_df)
}

# find a mid point to (x1, y1) and (x2, y2) dmp distance
m_get_mid_point <- function(x1, x2, y1, y2, dmp = NULL) {
  dtot <- sqrt((x1 - x2)^2 + (y1 - y2)^2)
   if(is.null(dmp)) dmp <- dtot / 2
  if(dmp >= dtot) { # return the second point if dmp is greater 
    out_frame <- data.frame(xend = x2, yend = y2)
  } else { 
    xend <- x1 - (dmp * (x1 - x2)) / dtot
    yend <- y1 - (dmp * (y1 - y2)) / dtot
    out_frame <- data.frame(xend = xend, yend = yend)
  }
  return(out_frame)
}

# return a simplified dataframe for text labels
m_get_text_df <- function(vert_df) {
  groupID <- layoutX <- layoutY <- NULL
  text_df <- vert_df %>%
    dplyr::select(groupID, layoutX, layoutY) %>%
    dplyr::distinct()
  return(text_df)
}

# return a simplified dataframe for edge weight labels
m_get_edge_weight_df <- function(edge_df) {
  xend <- yend <- x <- y <- outputs <- weight <- NULL # appease the build
  edge_weight_df <- edge_df %>%
    dplyr::mutate(outputs = purrr::pmap(
      list(xend, x, yend, y),
      m_get_mid_point)) %>%
    dplyr::select(-c(xend, yend)) %>%
    tidyr::unnest(outputs) %>%
    dplyr::select(xend, yend, weight) %>%
    dplyr::rename(x = xend, y = yend)
  return(edge_weight_df)
}

# get a dataframe for the resistance legend
m_get_res_df <- function(res_vec) {
  r_start <- r_end <- middle <- hjust <- NULL
  num_ab <- length(res_vec)
  res_df <- data.frame(
    drug = res_vec,
    r_start = (2 * pi / 360) + seq(0, num_ab - 1) * 2 * pi / num_ab,
    r_end = (seq(1, num_ab) * 2 * pi / num_ab) - (2 * pi / 360)) %>% 
    dplyr::mutate(
      middle = 0.5 * (r_start + r_end),
      hjust = ifelse(middle > pi, 1, 0),
      hjust = ifelse(middle == pi, 0.5, hjust),
      vjust = ifelse(middle < pi/2 | middle > 3 * pi / 2, 0, 1))
  return(res_df)
}

# creates a resistance legend (ggproto object) 
m_plot_legend <- function(res_vec,
                          plot_extents,
                          show_legend = TRUE,
                          core_rad = 0.3,
                          ring_rad = 0.03,
                          sensitive_colour = "steelblue",
                          legend_x_location = 0.98,
                          legend_y_location = 0.08) {
  r_start <- r_end <- NULL
  if(!show_legend) return(NULL)
  res_df <- m_get_res_df(res_vec)
  leg_x <- (plot_extents$min_x +
    (plot_extents$max_x - plot_extents$min_x) * legend_x_location)
  leg_y <- (plot_extents$min_y +
    (plot_extents$max_y - plot_extents$min_y) * legend_y_location)
  leg_proto <- ggforce::geom_arc_bar(data = res_df, 
      ggplot2::aes(
        x0 = leg_x,
        y0 = leg_y,
        r0 = core_rad,
        r = core_rad + (4 * ring_rad),
        start = r_start,
        end = r_end),
      color = "transparent",
      fill = sensitive_colour)
  return(leg_proto)
}

# creates resistance text for legend (ggproto object)
m_plot_legend_text <- function(res_vec,
                               plot_extents,
                               show_legend = TRUE,
                               core_rad = 0.3,
                               ring_rad = 0.03,
                               legend_text_size = 2,
                               legend_x_location = 0.98,
                               legend_y_location = 0.08) {
  middle <- drug <- hjust <- vjust <- NULL
  if(!show_legend) return(NULL)
  res_df <- m_get_res_df(res_vec)
  leg_x <- (plot_extents$min_x +
    (plot_extents$max_x - plot_extents$min_x) * legend_x_location)
  leg_y <- (plot_extents$min_y +
    (plot_extents$max_y - plot_extents$min_y) * legend_y_location)
  leg_text_proto <- ggplot2::geom_text(data = res_df,
      ggplot2::aes(
        x = leg_x + (core_rad + 6 * ring_rad) * sin(middle),
        y = leg_y + (core_rad + 6 * ring_rad) * cos(middle),
        label = drug,
        hjust = hjust,
        vjust = vjust),
      size = legend_text_size)
}