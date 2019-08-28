#' m_spol
#'
#' @param merdata A \code{merdata} object
#' @return A numeric matrix representing the spoligotype pattern
#' @examples
#' m_spol(monteserin)
#' @keywords spoligotype matrix
#' @export 
m_spol <- function(merdata) {
	s_type <- m_get_first_type(merdata, "SPOL")
	merdata <- m_aggregate(merdata, s_type)
	spolcount <- merdata$data$COUNT
	spolmat <- m_make_matrix(merdata$data[[s_type]])
	spolmat <- cbind(spolcount, spolmat)
	colnames(spolmat) <- c("COUNT", 1:(ncol(spolmat) - 1))
	return(spolmat)
}

#' m_mlva
#'
#' @param merdata A \code{merdata} object
#' @return A numeric matrix representing the MLVA pattern
#' @examples
#' m_mlva(monteserin)
#' @keywords MLVA VNTR matrix
#' @export 
m_mlva <- function(merdata) {	
	m_type <- m_get_first_type(merdata, "MLVA")	
	merdata <- m_aggregate(merdata, m_type)
	mat <- m_make_matrix(merdata$data[[m_type]])
	mmax <- max(mat, na.rm = TRUE)
	heat <- matrix(0, ncol = ncol(mat), nrow = mmax)
	for(k in 1:ncol(mat)) {
		for(j in 1:nrow(mat)) {
			index <- mat[j, k]
			heat[index, k] <- heat[index, k] + merdata$data$COUNT[j]
		}
	}
	heat <- round(heat)
	heat[heat == 0] <- NA
	loci_names <- m_get_details(merdata, m_type, split = TRUE)
	if(length(loci_names) == ncol(mat)) colnames(heat) <- loci_names
	return(heat)
}

#' m_res
#'
#' @param merdata A \code{merdata} object
#' @return A numeric matrix indicating the presence of antibiotic resistance
#' @examples
#' m_res(monteserin)
#' @keywords antibiotic drug resistance matrix
#' @export 
m_res <- function(merdata) {
	r_type <- m_get_first_type(merdata, "RES")
	merdata <- m_aggregate(merdata, r_type) 
	res_mat <- m_get_res_mat(merdata$data[[r_type]])
	res_names <- m_get_details(merdata, r_type, split = TRUE)
	if(length(res_names) == ncol(res_mat)) colnames(res_mat) <- res_names
	res_mat <- cbind(merdata$data$COUNT, res_mat)
	colnames(res_mat)[1] <- "COUNT"
	return(res_mat)
}


#' m_res_correlation
#'
#' @param merdata A \code{merdata} object
#' @return A numeric matrix representing the antibiotic resistance correlation
#' @examples
#' m_res_correlation(monteserin)
#' @keywords antibiotic drug resistance matrix
#' @export 
m_res_correlation <- function(merdata) {
	COUNT <- RES <- NULL # appease the build
	r_type <- m_get_first_type(merdata, "RES")
	merdata <- m_aggregate(merdata, r_type) 
	merdata$data <- merdata$data %>% 
		dplyr::filter(stringr::str_detect(RES, "-") == FALSE)
	resmat <- as.data.frame(m_get_res_mat(merdata$data[[r_type]]))
	ab_num <- ncol(resmat)
	if(ab_num < 2)
		stop("Need two or more antiobiotic drugs
			in RES to perform a pairwise comparison")
	a_names <- m_get_details(merdata, r_type, split = TRUE)
	if(is.null(a_names)) a_names = as.character(seq(1:ab_num))
	resmat <- resmat %>%
		dplyr::mutate(COUNT = merdata$data$COUNT) %>%
		tidyr::uncount(COUNT)
	out_mat <- matrix(0, nrow = ab_num, ncol = ab_num)
	colnames(out_mat) <- rownames(out_mat) <- a_names
	antib_pairs <- t(utils::combn(1:ncol(out_mat), 2))
	antib_pairs <- rbind(antib_pairs, matrix(rep(seq(1, ncol(out_mat)), 2),
																					 ncol = 2))
	for(i in 1:nrow(antib_pairs)) {
		probs <- m_get_res_freq(resmat, antib_pairs[i, 1], antib_pairs[i, 2])
		out_mat[antib_pairs[i, 1],
					  antib_pairs[i, 2]] <- m_get_r_squared(probs$a_freq,
					  																		  probs$b_freq,
					  																		  probs$ab_freq)
	}
	out_mat <- t(out_mat)
	return(out_mat)
}

## FUNCTIONS THAT ARE NOT EXPORTED BELOW HERE

# Correlation coeff of 1/0 indicator variables indicating the presence
# of alleles A and B. In our case, this is resistance to antiobiotics 
# A and B. 
m_get_r_squared <- function(p_a, p_b, p_ab) {
	if(p_a == p_b && p_a == p_ab) return(1)
	d_ab <- p_ab - (p_a * p_b)
	rsq <- d_ab * d_ab / (p_a * (1 - p_a) * p_b * (1 - p_b))
	if(is.nan(rsq)) return(0)
	# if(any(rsq > 1.0000000000001)) {
	# 	warning("r_squared should be less than 1. Check your resistance data")
	# }
	return(rsq)
}

# Returns marginal and joint resistance frequencies from a numeric resistance
# matrix
m_get_res_freq <- function(resmat, col1, col2) {
	c1 <- c2 <- ab <- NULL # appease the build
	# check for bad input
	if(col1 < 1 || col1 > ncol(resmat) || 
		 col2 < 1 || col2 > ncol(resmat))
			stop("Column numbers must be within bounds of the resistance matrix")
	# check to see if the column number is the same
	if(col1 == col2) {
		res_freq <- resmat %>%
			dplyr::rename(c1 = col1) %>%
			dplyr::mutate(c2 = c1) %>%
			dplyr::mutate(ab = floor((c1 + c2) / 2)) %>%
			dplyr::summarise(a_freq = sum(c1, na.rm = TRUE) / dplyr::n(),
								b_freq = sum(c2, na.rm = TRUE) / dplyr::n(),
								ab_freq = sum(ab, na.rm = TRUE) / dplyr::n() ) 
	} else {
		res_freq <- resmat %>%
			dplyr::rename(c1 = col1, c2 = col2) %>%
			dplyr::mutate(ab = floor((c1 + c2) / 2)) %>%
			dplyr::summarise(a_freq = sum(c1, na.rm = TRUE) / dplyr::n(),
								b_freq = sum(c2, na.rm = TRUE) / dplyr::n(),
								ab_freq = sum(ab, na.rm = TRUE) / dplyr::n() ) 
	}
	return(res_freq)
}