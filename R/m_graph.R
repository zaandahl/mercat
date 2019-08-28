## FUNCTIONS THAT ARE NOT EXPORTED BELOW HERE

# Calculates the forest based on "SPOL", "MLVA" if present using 
# Ozcaglar et al. algorithm
# references {Ozcaglar et al. (2012). Inferred Spoligoforest Topology Unravels 
# Spatially Bimodal Distribution of Mutations in the DR Region. IEEE 
# Transactions on Nanobioscience}
# uses m_get_candidate_parents, m_get_reduced_set, 
# m_get_unique_random_parent,m_expand_by_res
m_res_spol_forest <- function(merdata, rooted = TRUE, tree_seed = 1) { 
  set.seed(tree_seed)
  m_graph <- igraph::graph.empty()
  m_graph <- m_get_candidate_parents(merdata)
  s_type <- m_get_first_type(merdata, "SPOL") 
  m_type <- m_get_first_type(merdata, "MLVA", halt = FALSE)
  if(!is.null(m_type))
    m_graph <- m_get_reduced_set(merdata, m_graph, m_type, "hamming")
  m_graph <- m_get_reduced_set(merdata, m_graph, s_type, "hamming")
  if(!is.null(m_type)) 
    m_graph <- m_get_reduced_set(merdata, m_graph, m_type, "L1")
  m_graph <- m_get_unique_random_parent(m_graph)
  m_graph <- m_expand_by_res(merdata, m_graph, s_type, rooted)
  igraph::V(m_graph)$name <- c(1:igraph::vcount(m_graph))
  return(m_graph)
}

# Gets the set of candidate parents based on "SPOL" using the contiguous 
# deletion assumption
# references {Reyes et al. (2008). Models of deletion for visualizing bacterial
# variation: an application to tuberculosis spoligotypes. 
# BMC Bioinformatics, 9:496, 2008.}
# references {Warren et al. (2002). Microevolution of the Direct Repeat Region 
# of Mycobacterium tuberculosis: Implications for Interpretation of 
# Spoligotyping Data. JClin Microbiol. Dec 2002; 40(12): 4457-4465.}
# uses: m_get_first_type,m_aggregate,getm_spol_dist
m_get_candidate_parents <- function(merdata) {
  s_type <- m_get_first_type(merdata, "SPOL")
  merdata <- m_aggregate(merdata, s_type)
  genotypes <- nrow(merdata$data)
  s_graph <- igraph::graph.empty(genotypes)
  if(genotypes > 1) {
      grid <- t(utils::combn(1:genotypes, 2))
    for(n in 1:nrow(grid)) {
      A <- grid[n, 1]
      B <- grid[n, 2]
      if(m_spol_dist(merdata$data[[s_type]][A],
                     merdata$data[[s_type]][B]) == 1) {
        if(stringr::str_count(merdata$data[[s_type]][A], "1") >
           stringr::str_count(merdata$data[[s_type]][B], "1"))
          s_graph[A, B] <- TRUE
        else s_graph[B, A] <- TRUE
      }
    }
  }
  for(type in colnames(merdata$data))
    s_graph <- igraph::set.vertex.attribute(s_graph, type, value = merdata$data[[type]])
  return(s_graph)
}

# Reduces the number of elements from the candidate parents
# graph the set of parents to reduce
# dist_type the distance in which to redeuce the set of candidate parents (hamming distance or L1 distance)
# type the type of data to reduce the graph (ie. "SPOL" or "MLVA")
# returns The graph with the updated edges
# uses m_get_dist, m_get_min_dist
m_get_reduced_set <- function(merdata, m_graph, type, dist_type) {
  for(v in igraph::V(m_graph)) {
    if(length(igraph::neighbors(m_graph, v, "in")) >= 2) {
      parents <- igraph::neighbors(m_graph, v, "in")
      if(grepl("SPOL",type)) {
        data <- igraph::get.data.frame(m_graph, "vertices")[type]
        par <- parents[[1]]
        min <- m_get_dist(as.character(data[[1]][v]),
                          as.character(data[[1]][par]),
                          dist_type)
        for(p in 2:length(parents)) {
          dist <- m_get_dist(as.character(data[[1]][v]),
                             as.character(data[[1]][parents[[p]]]),
                             dist_type)
          if(dist < min) {
            min <- dist
            par <- parents[[p]]
          } else if(dist == min) par <- c(par, parents[[p]])
        }
        m_graph[-par, v] <- FALSE
      } else if(grepl("MLVA",type)) {
        minimum <- array(length(parents))
        for(p in 1:length(parents))
          minimum[p] <- m_get_min_dist(merdata,parents[p], v, type, m_graph, dist_type)
        m_graph[parents[which(minimum != min(minimum))], v] <- FALSE
      }
    }
  }
  return(m_graph)
}

# Random pick of parent vertex - Reduces the number of parents of a vertex to be unique
# graph the set of parents to reduce
# returns the graph with every vertex with at most one parent
m_get_unique_random_parent <- function(m_graph) {
  for(v in igraph::V(m_graph)) {
    if(length(igraph::neighbors(m_graph, v, "in")) > 1) {
      parents <- igraph::neighbors(m_graph, v, "in")
      par <- sample(parents, 1)  #random pick
      m_graph[-par, v] <- FALSE
    }
  }
  return(m_graph)
}

# Get Spoligotype Distance -  Calculates the spoligotype distance based on 
# the contiguous deletion assumption, where block deletions are count 
# as distance one regardless of their length
# string_a the string containing the spoligotype pattern in binary
# string_b the other string containing the spoligotype pattern in binary
# returns The calculated distance between string_a and string_b
# ues m_get_spol_dist_from_ancestor
m_spol_dist <- function(string_a, string_b) {
  string_a <- as.numeric(unlist(strsplit(string_a, split = "")))
  string_b <- as.numeric(unlist(strsplit(string_b, split = "")))
  most_common_ancestor <- as.numeric(string_a|string_b) 
  # 0+0=0,0+1=1,1+0=1,1+1=1
  distance <- m_get_spol_dist_from_ancestor(most_common_ancestor, string_a) + 
              m_get_spol_dist_from_ancestor(most_common_ancestor, string_b)
  return(distance)
}

#
# gets the hamming distance
m_hamming_dist <- function(string_a, string_b) {
  dist <- V1 <- V2 <- V3 <- NULL # appease the build
  ham_dist <- stringr::str_split(c(string_a, string_b),
                                 pattern = "",
                                 simplify = TRUE) #%>%
  ham_dist <- sum(ham_dist[1, ] != ham_dist[2, ], na.rm = TRUE)
  return(ham_dist)
}

# 
# gets the L1 distance
m_l1_dist <- function(string_a, string_b) {
  dist <- V1 <- V2 <- V3 <- NULL # appease the build
  l1_dist <- stringr::str_split_fixed(c(string_a, string_b),
    pattern = "",
    n = stringr::str_length(string_a)) %>%
    apply(c(1,2), strtoi, base = 36) %>% 
    apply(2, diff) %>%
    abs() %>%
    sum(na.rm = TRUE)
  return(l1_dist)
}

# Calculates the distance between two string of characters according 
# to dist_type
# string_a the string to be compared
# string_b the other string to be compared
# dist_type hamming distance or L1 distance
# The distance between the strings
# modularised the function; added new distance methods
# uses m_spol_dist
m_get_dist <- function(string_a, string_b, dist_type) {
  #string_a <- as.character(string_a)
  #string_b <- as.character(string_b)
  if(dist_type == "hamming") dist <- m_hamming_dist(string_a, string_b)
  else if(dist_type == "L1") dist <- m_l1_dist(string_a, string_b) 
  else if(dist_type == "spol") dist <- m_spol_dist(string_a, string_b)
  else if(dist_type == "note") dist <- sum(string_a != string_b, na.rm = TRUE)
  else {
    warning("dist_type must be one of hamming, L1, spol or note. Returning 0.")
    dist <- 0
  }
  return(dist)
}

# Get Minimum Distance - Calculates the minimum distance between a set of 
# parents and a set of children according to data
# parent the parent of a vertex
# child the child of a vertex
# data which data type, in the algorithm is MLVA
# graph the graph containg all attributes at its vertices
# merdata the merdata object
# dist_type hamming distance or L1 distance
# The minimum distance between the parent and child
m_get_min_dist <- function(merdata, parent, child, type, m_graph, dist_type) {
  p_id <- strsplit(igraph::get.vertex.attribute(m_graph, "ID")[parent], ",")
  c_id <- strsplit(igraph::get.vertex.attribute(m_graph, "ID")[child], ",")
  names(p_id) = names(c_id) <- "ID"
  p_string <- unique(merge(merdata$data, p_id)[type])
  c_string <- unique(merge(merdata$data, c_id)[type])
  min <- m_get_dist(as.character(c_string[[1]][1]),
                    as.character(p_string[[1]][1]),
                    dist_type) #the first
  for(c_idx in 1:nrow(c_string)) {
    for(p_idx in 1:nrow(p_string)) {
      dist <- m_get_dist(as.character(c_string[[1]][c_idx]), 
                         as.character(p_string[[1]][p_idx]),
                         dist_type)
      if(dist < min) min <- dist
    }
  }
  return(min)
}



# Get the spoligotype distance from ancestor
# Calculates the distance to an ancestor, note that it shrinks ancestor 
# and descendant where the gap is, 
# to contemplate cases like 1001 -> 0000 distance = 1
# ancestor the vector containing the spoligotype pattern from the ancestor
# descendant the vector containing the spoligotype pattern from the decendant
# returns The calculated distance between ancestor and descendant
# renamed variables to give more informative names
m_get_spol_dist_from_ancestor <- function(ancestor, descendant) {
  no_gap <- which(ancestor == 1)
  # check that ancestor has more 1s that the descendant
  d_ones <- which(descendant == 1)
  if(length(d_ones) > length(no_gap)) {
    warning("Spoligotypes can lose spacers in the DR region, 
            but not gain spacers. Results might be incorrect. Check your data.")
  } 
  ancestor <- ancestor[no_gap]
  descendant <- descendant[no_gap]
  equal_runs <- rle(ancestor == descendant)$values
  distance <- length(equal_runs[equal_runs == FALSE])
  return(distance)
}

# Gets the roots from a graph
# Finds the orphan vertices (no edges) and the vertices which have no incoming edges
m_get_roots <- function(m_graph) {
  all_vertices <- igraph::V(m_graph)
  edge_from <- unique(igraph::get.edgelist(m_graph)[, 1])
  edge_to <- unique(igraph::get.edgelist(m_graph)[, 2])
  orphan_roots <- setdiff(all_vertices, unique(c(edge_from, edge_to)))
  tree_roots <- setdiff(edge_from,edge_to)
  root_list <- c(tree_roots, orphan_roots)  
  return(root_list)
}

# Expands the vertices according to columns which type is "RES" 
# (the resistance pattern)
# return The expanded graph with its attributes at the vertices
m_expand_by_res <- function(merdata, m_graph, param_names, rooted = TRUE) { 
  if(!rooted) layout <- igraph::layout.fruchterman.reingold(m_graph)
  else layout <- igraph::layout.reingold.tilford(m_graph,
                    params = list(root = m_get_roots(m_graph)))
  igraph::V(m_graph)$layoutX <- layout[, 1]
  igraph::V(m_graph)$layoutY <- layout[, 2]
  # igraph::V(m_graph)$sizes <- as.numeric(igraph::V(m_graph)$COUNT) / 
  #                     sum(as.numeric(igraph::V(m_graph)$COUNT))
  igraph::V(m_graph)$groupID <- c(1:igraph::vcount(m_graph))
  
  r_type <- m_get_first_type(merdata, "RES", halt = FALSE)  
  if(is.null(r_type)) return(m_graph) # no RES data, don't bother

  a_types <- unique(c(param_names,r_type))
  merdata <- m_aggregate(merdata, a_types)
  graph_res <- igraph::graph.empty(nrow(merdata$data))
  
  dfmerdata <- apply(merdata$data[param_names],
                     1,
                     function(x) paste(x, collapse = ""))

  dfgraph <- apply(igraph::get.data.frame(m_graph, "vertices")[param_names],
                   1,
                   function(x) paste(x, collapse = ""))
  merdata$data$groupID <- apply(as.matrix(dfmerdata),
                                1,
                                function(x) which(dfgraph == x)[[1]])
    
  #sort by res     
  for(index in 1:length(igraph::V(m_graph))) {
    group_index <- which(merdata$data$groupID == as.character(index))
    if(length(group_index) > 1) merdata <- m_sort_by_res(merdata, group_index)
  }
  
  merdata$data <- merdata$data[order(as.numeric(merdata$data$groupID)),]

  for(colnum in 1:ncol(merdata$data)) {
    values <- apply(merdata$data[colnum], 1, paste, collapse = "")
    cname <- colnames(merdata$data)[colnum]
    graph_res <- igraph::set.vertex.attribute(graph_res, cname, value = values)
  }
  
  dfgraph <- igraph::get.data.frame(m_graph, "vertices")
  dfgraph_res <- igraph::get.data.frame(graph_res, "vertices")
  edges <- igraph::get.data.frame(m_graph,"edges")
  
  #transfering edges from m_graph to graph_res
  if(nrow(edges) > 1) {
    for(e in 1:nrow(edges))
      graph_res[which(dfgraph_res["groupID"] == dfgraph[edges[e, "from"], "groupID"])[1],
                  which(dfgraph_res["groupID"] == dfgraph[edges[e, "to"], "groupID"])[1]] = TRUE
  }
  if(!is.null(igraph::E(m_graph)$weight))
    igraph::E(graph_res)$weight <- igraph::E(m_graph)$weight
  
  m_graph <- m_concentric_rings_layout_size(m_graph, graph_res)

  return(m_graph)
}

# Sort Group by Resistance pattern
# Sorts the merdata by susceptibility, 
# being the most susceptible the inner sample to the most resistant 
# the outer one, based on "RES"
# param merdata the merdata object
# param group_index the indexes of the data that are in the same group
# return The ordered merdata
m_sort_by_res <- function(merdata, group_index) {
  r_type <- m_get_first_type(merdata,"RES", halt = FALSE)
  res <- which(colnames(merdata$data) %in% r_type)
  group <- merdata$data[group_index, ]
  resistance <- apply(group[res], 1, paste, collapse = "")
  sus_count <- stringr::str_count(resistance, "S")
  res_count <- stringr::str_count(resistance, "R")
  merdata$data[group_index, ] <- group[order(sus_count, -res_count),]
  return(merdata)
}

# Concentric Rings Layout and Sizes
# Copies the position of the graph_s (small = less nodes) into the layout 
# of graph_l (large = more nodes) and increases the sizes of the nodes
# param graph_s the small graph
# param graph_l the large graph
# return The larger graph with the position on its vertices and its sizes updated
m_concentric_rings_layout_size <- function(graph_s, graph_l) {
  dfgraph_s <- igraph::get.data.frame(graph_s, "vertices")
  groupID <- igraph::get.data.frame(graph_l, "vertices")["groupID"]
  igraph::V(graph_l)$layoutX <- apply(groupID, 1, function(x) dfgraph_s[x, "layoutX"])
  igraph::V(graph_l)$layoutY <- apply(groupID, 1, function(x) dfgraph_s[x, "layoutY"])
  # igraph::V(graph_l)$sizes <- as.numeric(igraph::V(graph_l)$COUNT) / sum(as.numeric(igraph::V(graph_l)$COUNT))
  # first <- 1
  # if(length(igraph::V(graph_l)) > 1) {
  #   for(v in 2:(length(igraph::V(graph_l)))) {
  #     if(igraph::V(graph_l)$groupID[v] == igraph::V(graph_l)$groupID[first]) 
  #       igraph::V(graph_l)$sizes[first:(v - 1)] <- igraph::V(graph_l)$sizes[first:(v - 1)] + igraph::V(graph_l)$sizes[v]
  #     else first <- v
  #   }
  # }
  return(graph_l)
}


# returns default distance for types
m_default_dist <- function(types) {
  distances <- NULL
  for(type in types) {
    if(grepl("MLVA", type)) distances <- c(distances, "L1")
    else if(grepl("SNP", type)) distances <- c(distances, "hamming")
    else if(grepl("SPOL", type)) distances <- c(distances, "spol")
    else distances <- c(distances, "note")
  }
  return(distances)
}

#
# gets the weights (command line > file > default)
# 
m_process_weights <- function(merdata, weights, noise) {
  c_names <- colnames(merdata$data)
  r_types <- c_names[grepl("RES", c_names)]
  valid_types <- setdiff(colnames(merdata$data), c("COUNT", "ID", r_types))
  m_type <- m_get_first_type(merdata, "MLVA", halt = FALSE)
  s_type <- m_get_first_type(merdata, "SPOL", halt = FALSE)
  p_type <- m_get_first_type(merdata, "SNP", halt = FALSE)

  # simple data validation
  if(!is.null(weights)) {
    if(!is.numeric(weights) || any(weights < 0))
      stop("Specified weights must be numeric and >= 0", call. = FALSE)
    if(length(setdiff(names(weights),valid_types)) > 0)
      stop("Invalid weight name", call. = FALSE)  
  }
  unweighted <- setdiff(valid_types, names(weights))
  if(!is.null(unweighted)) for(weightname in unweighted) {
    detailsname <- paste(weightname, "WT", sep = "")
    weightval <- as.numeric(m_get_details(merdata, detailsname))
    if(length(weightval) == 0) {
      if(noise) message("Using default weight for ", weightname)
      if(weightname %in% c(m_type, s_type, p_type)) weightval <- 1       
      else weightval <- 0
    } else if(noise) message("Adding ", weightname, " value from smef")
    weights <- c(weights, weightval)
    names(weights)[length(weights)] <- weightname
  }
  if(noise) message("Weights specified: ",
                    paste(names(weights), "=", weights, collapse = "; "))
  weights <- weights[weights > 0]
  return(weights) 
}


# Calculate the MST
# Calculates the MST from the data provided based on weights 
# and distances
# param merdata the merdata object
# param weights the named list of weights to apply to each 
# column, e.g. c("MLVA" = 3, "SPOL" = 2), by default 
# all data are weighted 1
# param distances the distance type to use to calculate 
# the distances, by default the following 
# distances are used c("MLVA" = "L1", "SPOL" = "spol", "NOTE" = "note"). Distances available are: "L1", "hamming", "spol" and "note"
# return The minimal spanning tree as an igraph graph object
# if weights are NULL use defaults and print message
# else if the weight vector doesn't match the length of types, pad with 1 and give warning
# check that weights are numeric (and positive?), stop if not true
# if distances are NULL use defaults and print message
# else if distance vector doesn't match length of types, use defaults and give warning
# check that distance vector is valid. stop if not true
m_mst_from_data <- function(merdata,
                            weights = NULL,
                            distances = NULL,
                            noise = FALSE,
                            tree_seed = 1) {
  set.seed(tree_seed)
  weights <- m_process_weights(merdata, weights, noise)
  if(is.null(distances)) {
    distances <- m_default_dist(names(weights))
    names(distances) <- names(weights)
  }
  m_dist <- m_weighted_dist_from_data(merdata, weights, distances)
  m_graph <- m_graph_front_dist(m_dist$merdata, m_dist$weighted_mat)  
  mst <- igraph::minimum.spanning.tree(m_graph)
  mst <- m_expand_by_res(merdata, mst, names(weights), rooted = FALSE)
  igraph::V(mst)$name <- c(1:igraph::vcount(mst))
  return(mst)
}

# Weighted Distance From Data
# Computes weighted sum of pairwise distances between each isolate
# param merdata the merdata object
# param weights the named list of weights to apply to each column, 
# e.g. c(MLVA=1, SPOL=2)
# param distances the distance type to use to calculate the distances, 
# by default the following distances are used 
# c("MLVA" = "L1", "SPOL" = "spol", "NOTE" = "note"). 
# Distances available are: "L1", "hamming", "spol" and "note"
# return The distance "matrix" (as object of class dist; use as.matrix() 
# to convert to matrix) and the merdata with the aggregated information
m_weighted_dist_from_data <- function(merdata, weights, distances) {
  merdata <- m_aggregate(merdata, names(weights))
  mats <- list()
  w <- list()
  j <- 1
  for(type in names(weights)) {
    mats[[j]] <- m_get_dist_matrix(as.character(merdata$data[[type]]),
                                   distances[[type]])
    w[[j]] <- weights[[type]]
    j <- j + 1
  }
  weighted_mat <- mapply("*", w, mats, SIMPLIFY = FALSE)
  weighted_mat <- Reduce("+", weighted_mat)
  return(list(merdata = merdata, weighted_mat = weighted_mat))
}

# Get Distance Matrix
# Computes pairwise distances between rows of dataFrames (e.g. MLVA, SPOL)
# param dataFrame the marker to get the distance
# param dist_type the type of distance asociated with the type. 
# Distances available are: "L1", "hamming", "spol" and "note"
# return The matrix distance
m_get_dist_matrix <- function(marker_vec, dist_type) {
  Var1 <- Var2 <- m_dist <- NULL # appease the build
  nmarkers <- length(marker_vec)
  if(nmarkers < 1) {
    stop("At least one marker needed to get matrix", do.call = FALSE)
  }
  marker_pairs <- expand.grid(marker_vec, marker_vec)
  dist_mat <- marker_pairs %>%
    dplyr::mutate(Var1 = as.character(Var1), Var2 = as.character(Var2)) %>%
    dplyr::mutate(m_dist = purrr::pmap(list(Var1, Var2, dist_type),
                                       m_get_dist)) %>%
    dplyr::pull(m_dist) 
  dist_mat <- dist_mat %>%
    matrix(nrow = nmarkers) %>% 
    stats::as.dist(diag = FALSE, upper = FALSE)                 
  return(dist_mat)
}

# Get graph from distances 
# Returns a weighted graph whose weights are the pairwise distances
# param dists the distance matrix based on the weights
# param merdata the merdata object aggregated by the weight's names
# return The igraph object with all the attributes on each vertex
m_graph_front_dist <- function(merdata, dists) {
  dist_mat <- as.matrix(dists)
  if(nrow(dist_mat)>0)
    m_graph <- igraph::graph.adjacency(dist_mat,
                               mode = "undirected",
                               weighted = TRUE)
  else
    m_graph <- igraph::graph.adjacency(as.matrix(1),
                               mode = "undirected",
                               weighted = TRUE)
  for(cname in colnames(merdata$data)) {
    attribute <- apply(merdata$data[cname], 1, paste, collapse = "")
    m_graph <- igraph::set.vertex.attribute(m_graph,cname, value = attribute)
  }
  return(m_graph)
}
