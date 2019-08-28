#' m_aggregate
#'
#' Aggregates the data in a merdata object according to columns 
#' specified by a vector of column labels. As an example, aggregating using 
#' the spoligotype column will summarise the data so each row contains a
#' distinct spoligotype. These aggregated columns are returned in the new
#' merdata object along with two augmented columns. The data is augmented with
#' an "ID" column where each aggregated row contains a concatenated list of
#' "ID" values from the original rows in the un-aggregated data. The data is
#' also augmented with a "COUNT" column where each aggregated row contains the
#' sum of the corresponding COUNT data type from the un-aggregated data.
#' @param merdata A \code{merdata} object
#' @param types A vector of types (column names) to be aggregated
#' e.g. "SPOL", "MLVA15"
#' @return A \code{merdata} object
#' @examples
#' # Data aggregated to only show distinct spoligotypes
#' m_aggregate(monteserin, "SPOL")
#' # Data aggregated to show MIRU-VNTR types with distinct 
#' # antibiotic resistance profiles
#' m_aggregate(monteserin, c("MLVA15", "RES"))
#' @keywords aggregate select compress group
#' @export
m_aggregate <- function(merdata, types) {
	removeblanks <- TRUE
	if(class(merdata) != "merdata")
		stop("The merdata object is not valid.", call. = FALSE)
	ID <- COUNT <- FIELDS <- . <- NULL # appease the build
	types <- toupper(types)
	types <- m_get_types(merdata, types)
	ctypes <- colnames(merdata$data) %>% setdiff(c("ID", "COUNT"))
	if(length(intersect(ctypes, types)) < length(types)) {
		stop("You must aggregate using a valid type [",
			paste(ctypes, collapse = ","), "]", call. = FALSE)
	}
	types <- merdata$data %>% colnames() %>% intersect(types)
	unused_types <- setdiff(ctypes, types)
	data <- merdata$data %>%
		dplyr::group_by(!!!rlang::syms(types)) %>%
		dplyr::summarise(COUNT = sum(COUNT),
						  			 ID = paste(ID, collapse = ",")) %>%
		dplyr::arrange(dplyr::desc(COUNT))
	if(removeblanks) {
		data <- data %>% 
			dplyr::filter_at(dplyr::vars(!!!rlang::syms(types)),
											 dplyr::any_vars(!stringr::str_detect(., pattern = "^[#]+$")))
	}
	details <- merdata$details %>%
		dplyr::filter(!FIELDS %in% unused_types)
	aggregated <- m_create(details, data, validate = FALSE)
	return(aggregated)
}

#' Printing merdata
#'
#' Prints each element of a merdata object using the generic function print.
#' @param x A \code{merdata} object
#' @param \dots Further arguments passed to or from other methods.
#' @examples
#' print(monteserin)
#' @keywords print S3 merdata
#' @export
print.merdata <- function(x, ...) {
	invisible(lapply(x, print, right = FALSE, ...))
}

#' Summary merdata
#'
#' Displays summary statistics for a \code{merdata} object.
#' @param object A \code{merdata} object
#' @param \dots additional arguments
#' @return A list of summary statistics
#' @references {Tang et al. (2008). spolTools: online utilities for analyzing 
#' spoligotypes of the Mycobacterium tuberculosis complex. 
#' Bioinformatics, 24(20):2414-2415.}
#' @examples
#' summary(monteserin)
#' @keywords summary merdata
#' @export
summary.merdata <- function(object, ...) { 
	# first the common statistics 
	full <- object$data %>%
		colnames() %>%
		setdiff(c("ID", "COUNT"))
	merdata <- m_aggregate(object, types = full)
	merdata_sum <- NULL	
	merdata_sum$FULL <- m_summary_common(merdata)	
	# the following might be able to be generalised further
	s_types <- m_get_types(object, types = "SPOL")
	for(s_type in s_types) {
		merdata_s <- m_aggregate(object, types = s_type)
		merdata_sum[[s_type]] <- c(m_summary_common(merdata_s),
															 m_summary_spol(merdata_s, s_type))
	}	
	m_types <- m_get_types(object, types = "MLVA")
	for(m_type in m_types) {
		merdata_m <- m_aggregate(object, types = m_type)
		merdata_sum[[m_type]] <- c(m_summary_common(merdata_m),
															 m_summary_mlva(merdata_m, m_type))
	}
	p_types <- m_get_types(object, types = "SNP")
	for(p_type in p_types) {
		merdata_p <- m_aggregate(object, types = p_type)
		merdata_sum[[p_type]] <- c(m_summary_common(merdata_p))
	}
	r_types <- m_get_types(object, types = "RES")
	for(r_type in r_types) {
		merdata <- m_aggregate(object, types = r_type)
		merdata_sum[[r_type]] <- c(m_summary_common(merdata),
															 m_summary_res(merdata, r_type))		
	}
	n_types <- m_get_types(object, types = "NOTE")
	for(n_type in n_types) {
		merdata <- m_aggregate(object, types = n_type)
		merdata_sum[[n_type]] <- c(m_summary_common(merdata),
															 m_summary_note(merdata, n_type))		
	}
	return(merdata_sum)
}

#' m_summary
#'
#' Writes summary statistics in "pretty" output to the screen or to a text file.
#' @param merdata A \code{merdata} object
#' @param file A path and name of the output file
#' @param digits The number of significant digits to display
#' @references {Tang et al. (2008). spolTools: online utilities for analyzing 
#' spoligotypes of the Mycobacterium tuberculosis complex. 
#' Bioinformatics, 24(20):2414-2415.}
#' @examples
#' # print to screen
#' m_summary(monteserin)
#' # export to a file named "output.txt"
#' m_summary(monteserin, "output.txt")
#' @keywords merdata summary isolates genotypes Cluster Size RTI RTIn RTIn-1 
#' singleton virtual-Heterozygosity theta Hunter-Gaston
#' @export
m_summary <- function(merdata, file = NULL, digits = 3) {
	if(class(merdata) != "merdata")
		stop("The merdata object is not valid.", call. = FALSE)
	merdata_sum <- summary(merdata)
	out <- paste("")
	if(length(m_get_types(merdata, c("SPOL", "MLVA", "SNP", "RES"))) > 1)
		out <- paste(out, m_summary_format(merdata_sum$FULL,
																			 type = "ALL",
																			 digits = digits))
	s_types <- m_get_types(merdata, types = "SPOL")
	for(s_type in s_types) 
		out <- paste(out, m_summary_format(merdata_sum[[s_type]],
																			 type = s_type,
																			 digits = digits))
	m_types <- m_get_types(merdata, types = "MLVA")	
	for(m_type in m_types) 
		out <- paste(out, m_summary_format(merdata_sum[[m_type]],
																			 type = m_type,
																			 digits = digits))
	p_types <- m_get_types(merdata, types = "SNP")	
	for(p_type in p_types) 
		out <- paste(out, m_summary_format(merdata_sum[[p_type]],
																			 type = p_type,
																			 digits = digits))
	r_types <- m_get_types(merdata, types = "RES")
	for(r_type in r_types) {
		out <- paste(out, "Resistance frequencies\n")
		res_string <- m_get_details(merdata, field = r_type, split = TRUE)
		res_string <- paste(res_string, collapse = "\t")
		out <- paste(out, res_string, "\n")
		res_freqs <- as.character(round(merdata_sum[[r_type]]$res_frequency, digits))
		res_freqs <- paste(res_freqs, collapse = "\t")
		out <- paste(out, res_freqs)
	}
	out <- paste(out, "\n")
	if(is.null(file)) cat(out)
	else {
		con = file(file, "w")
		on.exit(close(con))
		write(out, file = con)
	}
}

## FUNCTIONS THAT ARE NOT EXPORTED BELOW HERE

# Helper function for m_summary
# returns key summary statistics in a pretty format
m_summary_format <- function(summary, type = "ALL", digits = 3) {
   out <- paste(
   	"Data type:", type, "\n",
	  "Number of isolates (n):", summary$isolates, "\n",
	  "Number of genotypes (g):", summary$genotypes, "\n",
	  "Average cluster size (n/g):",
	  	round(summary$mean_cluster_size, digits), "\n",
	  "RTI n-1:", round(summary$rti_n_1, digits), "\n",
	  "RTI n ('clustering rate'):", round(summary$rti_n, digits), "\n",
	  "Number of singletons (s):", summary$singletons, "\n",
	  "Virtual heterozygosity / Genotype diversity:", 
	  	round(summary$virtual_heterozygosity, digits), "\n",
	  "Maximum likelihood estimate of IAM theta:", 
	  	round(summary$theta, digits), "\n", 
	  "Hunter-Gaston Index:", round(summary$hunter_gaston, digits), "\n\n")
   return(out)
}

# Helper function for m_summary
# returns merdata details in a pretty format
m_get_details <- function(merdata, field, split = FALSE) {
	detail <- which(merdata$details$FIELDS == field)
	if(length(detail) < 1) return(NULL)
	out_string <- do.call(c, merdata$details[detail, ]$VALUES)
	if(split) out_string <- do.call(c, strsplit(out_string, ","))
	out_string <- stringr::str_trim(out_string)
	return(out_string)
}

# Calculates Theta (see Ewens 1972)
m_calc_theta <- function(isolates, genotypes) {
	theta <- 0.0001
	delta <- 0.00001
	diff <- 10
	i <- 0
	while(diff > delta && i < 10000) {
		esum <- sum(1 / (theta + seq(1, isolates - 1)))
		prev <- theta
		if(esum == 0) theta <- 0
		else theta <- (genotypes - 1) / esum
		diff <- theta - prev
		i <- i + 1 
	}
	return(theta)
}

# Calculates virtual heterozygosity
m_virtual_heterozygosity <- function(count_vector) {
  isolates = sum(count_vector)
  vhe = 1 - sum((count_vector / isolates)^2)
  return(vhe)
}

# Calculate summary statistics common to all markers
# returns a list of statistics
m_summary_common <- function(merdata) {
	COUNT <- NULL # appease the build
	m_count <- merdata$data %>% dplyr::pull(COUNT)
	isolates <- sum(m_count)
	genotypes <- merdata$data %>% nrow()
	mean_cluster_size <- isolates / genotypes
	singletons <- sum(m_count == 1)
	# Recent Transmission Index "n-1 method" , Small et al. 1994
	rti_n_1 <- 1 - (genotypes / isolates)
	# Recent Transmission Index "n method" , Alland et al. 1994
	rti_n <- 1 - (singletons / isolates)
	virtual_heterozygosity <- m_virtual_heterozygosity(m_count)
	theta <- m_calc_theta(isolates, genotypes)
	hunter_gaston <- m_hunter_gaston_index(m_count)
	summary_list <- list(isolates = isolates, 
											 genotypes = genotypes,
											 mean_cluster_size = mean_cluster_size, 
											 rti_n_1 = rti_n_1,
											 rti_n = rti_n,
											 singletons = singletons,
											 virtual_heterozygosity = virtual_heterozygosity,
											 theta = theta,
											 hunter_gaston = hunter_gaston)
	return(summary_list)
}

# Summarises SPOL information as the frequency weighted by COUNT; 
# calculates the Hunter-Gaston diversity index
m_summary_spol <- function(merdata, type) {
	COUNT <- NULL # appease the build
	m_count <- merdata$data %>% dplyr::pull(COUNT)
	m_vec <- merdata$data %>% dplyr::pull(!!rlang::enquo(type))
	spacer_freq <- m_make_matrix(m_vec) %>%
	  apply(2, function(x) stats::weighted.mean(x, m_count, na.rm = TRUE))
	spacer_hunter_gaston <- m_calc_specific_hg(m_vec, m_count)
	spol_summary <- list(spacer_freq = spacer_freq,
											 spacer_hunter_gaston = spacer_hunter_gaston)
	return(spol_summary)
}

# Summarises MLVA information as the weighted mean and the Hunter-Gaston
# diversity index weighted by COUNT
m_summary_mlva <- function(merdata, type) {
	COUNT <- NULL # appease the build
	m_count <- merdata$data %>% dplyr::pull(COUNT)
	m_vec <- merdata$data %>% dplyr::pull(!!rlang::enquo(type))
	loci_freq <- m_make_matrix(m_vec) %>% 
		apply(2, function(x) stats::weighted.mean(x, m_count, na.rm = TRUE))
	loci_hunter_gaston <- m_calc_specific_hg(m_vec, m_count)
  mlva_summary = list(loci_freq = loci_freq,
    								  loci_hunter_gaston = loci_hunter_gaston)
  return(mlva_summary)
}

# Summarises the resistance information as frequency weighted by COUNT 
m_summary_res <- function(merdata, type) {
	COUNT <- NULL # appease the build
	m_count <- merdata$data %>% dplyr::pull(COUNT)
	m_vec <- merdata$data %>% dplyr::pull(!!rlang::enquo(type))
	res_freq <- m_get_res_mat(m_vec) %>% 
		apply(2, function(x) stats::weighted.mean(x, m_count, na.rm = TRUE))
	res_summary <- list(res_frequency = res_freq)
	return(res_summary)
}

# Replaces all occurances of the letter S with 0 and that of R 
# with 1 in resistance profile data. All other characters get replaced by NA.
# Used by m_res_weighted_mean
m_get_res_mat = function(r_data) {
  n_types <- stringr::str_length(r_data[1])
	r_mat <- r_data %>% 
		stringr::str_replace_all(c("S" = "0", "R" = "1")) %>%
		stringr::str_split("") %>%
		unlist() 
	r_mat <- suppressWarnings(as.numeric(r_mat))
	r_mat <- r_mat %>% 
		matrix(ncol = n_types, byrow = TRUE)
	return(r_mat)
}

# Summarises the Note information as the number of occurences in the sample
m_summary_note <- function(merdata, type) {
	COUNT <- NULL # appease the build
	note <- merdata$data %>% 
		dplyr::select(!!rlang::enquo(type), COUNT)
	return(note)
}

# Makes a matrix with the data.frame
m_make_matrix <- function(mdata) {
	level_key <- as.character(c(0:32))
	names(level_key) <- c(0:9, letters[1:23])
	in_string <- unlist(strsplit(as.character(mdata), split = ""))
	intdata <- suppressWarnings(as.integer(dplyr::recode(in_string, !!!level_key)))
	mat <- matrix(intdata, nrow = NROW(mdata), byrow = TRUE)
	return(mat)
}

# Hunter-Gaston Diversity index requires only a count of types in each strain.
# counts_vector is a numeric vector of counts in each strain
m_hunter_gaston_index <- function(counts_vector) {
  if(!is.numeric(counts_vector) | any(counts_vector < 0)) {
  	stop("Each count should be numeric and non-negative", 
  		do.call = FALSE)
  }
  total = sum(counts_vector)
  hg = 1 - sum(sapply(counts_vector,
  									  function(x) x * (x - 1))) / (total * (total - 1))
  return(hg)
}

# Calculates the Hunter-Gaston diversity index for each column
# type_data is the spoligotypes or MLVA pattern 
m_calc_specific_hg <- function(type_data, count) {
	COUNT <- D <- NULL # appease the build
	type_data_m <- type_data %>%
		m_make_matrix()
	count <- as.numeric(unlist(count))
	nloci <- ncol(type_data_m)
	hg <- array(dim = nloci)
	for(i in 1:nloci) {
		locus_i <- data.frame(D = type_data_m[,i], COUNT = count)
		locus_summary <- locus_i %>% 
			dplyr::group_by(D) %>%
			dplyr::summarise(COUNT = sum(COUNT))
		locus_count <- locus_summary["COUNT"]
		hg[i] <- locus_count %>%
			unlist() %>%
			m_hunter_gaston_index()
	}
	return(hg)
}
