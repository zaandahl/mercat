#' m_read
#'
#' Reads a file in Simple Molecular Epidemiology Format (SMEF) 
#' and returns a \code{merdata} object.
#' @param file Name (unix style) of a file in SMEF or RSF format. The function 
#' requires a .rsf to read RSF. The default format is SMEF.
#' @param validate Check to see if the file is valid.
#' @return A \code{merdata} object
#' @references {Tang et al. (2008). spolTools: online utilities for analyzing 
#' spoligotypes of the Mycobacterium tuberculosis complex. Bioinformatics, 
#' 24(20):2414-2415.}
#' @examples
#' m_read("filename.smef")
#' @keywords smef data input
#' @export
m_read <- function(file, validate = TRUE) {
	if(file.access(file, 4) == -1) {
		stop("File does not exist or is not readable", call. = FALSE)
	}
	extension <- strsplit(file, split = "\\.") %>% 
	             unlist() %>% 
	             utils::tail(n = 1) %>% 
	             tolower()
	if(extension == "rsf") return(m_read_rsf(file, validate))
	else return(m_read_smef(file, validate))
}

#' m_create
#'
#' Creates a \code{merdata} object from molecular data including header
#' information and data types.
#' @param details A data frame containing details about Format, Author, etc...
#' @param data A data frame containing isolate marker and resistance information 
#' @param validate Check the resulting merdata object is valid
#' @return A \code{merdata} object
#' @examples
#' details <- data.frame(Format = "SMEF 1.0", Author = "John Smith", pubyear = 1900)
#' data <- data.frame(
#'   ID = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'   COUNT = c("5", "2", "3", "7", "13", "2", "9", "4"),
#'   MLVA = c("25354323", "25753323", "25353322", "25353443",
#'            "25553323", "25353923", "25353323", "28353323"),
#'   NOTE = c("1", "1", "6", "4", "5", "6", "7", "7")
#' )
#' d <- m_create(details = details, data = data)
#' @keywords merdata
#' @export
m_create <- function(details, data, validate = TRUE) {
	merdata <- list(details = details, data = data)
	class(merdata) <- "merdata"
	colnames(merdata$data) <- toupper(colnames(merdata$data))
	merdata <- m_process(merdata)
	if(validate) {
	  error <- m_validate(merdata)
	  if(error) stop("Unable to create merdata. 
	  	Error(s) encountered!", call. = FALSE)		
	}
	return(merdata)
}

#' m_export
#'
#' Exports a \code{merdata} object to a text file in Simple 
#' Molecular Epidemiology Format (SMEF).
#' @param merdata A \code{merdata} object
#' @param file The path and name of the output file
#' @param format The output file format, SMEF or RSF
#' @references {Tang et al. (2008). spolTools: online utilities for analyzing 
#' spoligotypes of the Mycobacterium tuberculosis complex. Bioinformatics,
#' 24(20):2414-2415.}
#' @examples
#' # export to a file called "filename.smef" in the default SMEF format
#' m_export(monteserin, "filename.smef","smef")
#' # export to a file called "filename.rsf" in RSF format
#' m_export(monteserin, "filename.rsf","rsf")
#' @keywords csv export smef merdata rsf
#' @export
m_export <- function(merdata, file, format = "SMEF") {
	if(class(merdata) != "merdata") stop("The merdata object is not valid.",
																			 call. = FALSE)
	format <- toupper(format)
	if(format == "SMEF") m_export_smef(merdata, file)
	else if (format == "RSF") m_export_rsf(merdata, file)
	else stop("The format must be SMEF or RSF", call. = FALSE)	
}


#' m_get_types
#' 
#' Returns a string vector containing all specific examples of the base
#' types requested. As an example if the base types "SPOL" and "MLVA" are
#' requested and the \code{merdata} object contains two "MLVA" types (15, 24)  
#' \code{m_get_types} will return "SPOL", "MLVA15", "MLVA24".
#' @param merdata A \code{merdata} object
#' @param types A string vector of base types (e.g. "SPOL", "MLVA")
#' @return A string vector of specific types (e.g. "SPOL", "MLVA15", "MLVA24")
#' @examples
#' m_get_types(monteserin, "RES")
#' @keywords merdata type
#' @export
m_get_types <- function(merdata, types) {
	types <- do.call(c, lapply(types, m_get_type, merdata = merdata))
	return(types)
}

#' m_get_first_type
#' 
#' Returns the first instance of a specific type. As an example if the base
#' type of "MLVA" is requested and there are two "MLVA" types (15, 24)
#' \code{m_get_first_type} will return "MLVA15".
#' @param merdata A \code{merdata} object
#' @param type The string base type (e.g. "MLVA")
#' @param halt Stop on error
#' @return A string containing the first instance of that type (e.g. "MLVA15")
#' @examples
#' m_get_first_type(monteserin, "RES", halt = FALSE)
#' @keywords merdata type
#' @export
m_get_first_type <- function(merdata, type, halt = TRUE) {
	types <- m_get_types(merdata, type)
	if(is.null(types)) {
		if(halt) stop("No data of type ", type, " available",
			call. = FALSE)
		else types = NULL
	}
	if(length(types) > 1) 
		warning("Multiple ", 
						type, 
						" data types found, using first instance", 
						call. = FALSE)	
	return(types[1])	
}

## FUNCTIONS THAT ARE NOT EXPORTED BELOW HERE

# used by: m_get_types 
# returns a single type
m_get_type <- function(merdata, type) {
	types <- colnames(merdata$data)
	gterm <- paste("^",type, "[0-9]*", sep = "")
	tindex <- grepl(gterm, types)
	if(sum(tindex) > 0) return(types[tindex])
	else return(NULL)
}



# Takes in a file in SMEF format and returns a merdata object
# Used by: m_read
# Uses: m_create
m_read_smef <- function(file, validate = TRUE) {
	if(con <- file(file, "r")) {
		on.exit(close(con))
		infile <- readLines(file)
		infile <- infile[sapply(infile, nchar) > 0] #delete empty lines
		rows <- length(infile)
		fields <- list()
		values <- list()
		i <- 1
		header <- TRUE
		while(header) {
			if(infile[i] != "DATA") {
				row <- strsplit(infile[i], ":")
				field <- gsub("(^[ \t]+)|([ \t]+$)", "", row[[1]][1])
				value <- gsub("(^[ \t]+)|([ \t]+$)", "", row[[1]][2])
				fields <- c(fields, field)
				values <- c(values, value)
			} else header <- FALSE
			i <- i + 1
		} # end while
		details <- matrix(cbind(fields, values), ncol = 2, nrow = length(fields))
		colnames(details) <- c("FIELDS", "VALUES")
		details <- data.frame(details)
		datafile <- infile[i:rows]
		datafile <- gsub("[[:space:]]+", "", datafile)
		data <- utils::read.csv(textConnection(datafile),
										 colClasses = "character",
										 stringsAsFactors = FALSE)
		merdata <- m_create(details = details, data = data, validate)
		return(merdata)
	} # end if
}


# Takes in a file in RSF format and returns a merdata object
# Used by: m_read
# Uses: m_create
m_read_rsf <- function(file, validate = TRUE) {
	if(con <- file(file, "r")) {
		on.exit(close(con))
		infile <- readLines(file)
		infile <- infile[sapply(infile, nchar) > 0] #delete empty lines
		rows <- length(infile)
		fields <- list()
		values <- list()
		i <- 1
		header <- TRUE
		while(header) {
			if(infile[i] != "SPOLIGOTYPE") {
				row <- strsplit(infile[i], "[ \t]+")
				field <- gsub("(^[ \t]+)|([ \t]+$)", "", row[[1]][1])
				value <- gsub("(^[ \t]+)|([ \t]+$)", "", row[[1]][-1])
				value <- paste(value, collapse = " ")
				fields <- c(fields, field)
				values <- c(values, value)
			} else header <- FALSE
			i <- i + 1
		} # end while
		details <- matrix(ncol = 2, nrow = length(fields))
		details <- cbind(fields, values)
		details <- data.frame(details)
		colnames(details) <- c("FIELDS", "VALUES")
		types <- c("ID", "SPOL", "COUNT")
		datafile <- infile[i:(rows - 1)] # last line is //
		#datafile <- gsub(":", ",", datafile)
		datafile <- gsub(" +", "", datafile)
		data <- utils::read.csv(textConnection(datafile),
						colClasses = "character",
						header = FALSE,
						col.names = paste(types, sep = ""),
						stringsAsFactors = FALSE,
						sep = ":")
		merdata <- m_create(details = details, data = data, validate)
		return(merdata)
	} # end if
}

# Automatic processing of files before validation check
# Used by: m_create
# Uses: m_process_id, m_process_count, m_process_spol, m_process_res
m_process <- function(merdata) {
	merdata <- m_process_id(merdata)	
	merdata <- m_process_count(merdata)
	merdata <- m_process_spol(merdata)
	merdata <- m_process_res(merdata)
	return(merdata)
}

# Check ID column - Checks that there is an ID column, 
# if there is not it creates it assinging seq 1..n for the row ID
# Used by: m_process
m_process_id <- function(merdata) {
	ID <- NULL # appease the build
	if(sum(colnames(merdata$data) == "ID") == 0) {
		warning("No ID column provided. Using 1..n for ID", call. = FALSE)
		merdata$data <- merdata$data %>% dplyr::mutate(ID = dplyr::row_number())
	}
	# reorder so ID is last
	non_id <- setdiff(colnames(merdata$data), "ID")
	merdata$data <- merdata$data %>% dplyr::select(!!!rlang::syms(non_id), ID)
	return(merdata)
}

# Check COUNT column - Checks that there is a COUNT column, 
# if there is not it creates it assigning count 1 to every row of data
# used by: m_process
m_process_count <- function(merdata) {
	COUNT <- ID <- NULL # appease the build
	if(sum(colnames(merdata$data) == "COUNT") == 0) {
		warning("No COUNT column provided. Setting COUNT = 1 for each row",
						 call. = FALSE)
		merdata$data <- merdata$data %>% dplyr::mutate(COUNT = 1)		
	}
	merdata$data$COUNT <- as.integer(merdata$data$COUNT)
	# reorder, COUNT is first
	non_idcount <- setdiff(colnames(merdata$data), c("ID", "COUNT"))
	merdata$data <- merdata$data %>% dplyr::select(COUNT, !!!rlang::syms(non_idcount), ID) 
	return(merdata)
}

# converts SPOL from octal and hex to binary
# need to allow more than one SPOL column!
# Used by: m_process
m_process_spol <- function(merdata) {
	s_types <- m_get_types(merdata, "SPOL")
	for(s_type in s_types) {
		len <- nchar(as.character(merdata$data[1, s_type]))
		#octal length == 15
		if(len == 15) 
			merdata$data[[s_type]] <- do.call(rbind,
																				lapply(merdata$data[[s_type]],
																							 m_oct2bin))
		#hex length == 17
		else if(len == 17) 
			merdata$data[[s_type]] <- do.call(rbind,
																				lapply(merdata$data[[s_type]],
																							 m_hex2bin))
	}	
	return(merdata)
}

# converts RES to uppercase
# Used by: m_process
m_process_res <- function(merdata) {
	r_types <- m_get_types(merdata, "RES")
	for(r_type in r_types)
		merdata$data[[r_type]] <- toupper(merdata$data[[r_type]])
	return(merdata)
}

# Takes in a merdata object and runs validation checks
# used by m_create
m_validate <- function(merdata) {
	error <- c(m_check_data_dim(merdata),
					   m_check_valid_type(merdata),	
					   m_check_id(merdata),
					   m_check_count(merdata),
					   m_check_data(merdata, types = "MLVA", pattern = "^[0-9a-zA-Z-]+$"),
					   m_check_data(merdata, types = "SPOL", pattern = "^[01]+$"),
					   m_check_data(merdata, types = "SNP", pattern = "^[gatcGATC#]+$"),
					   m_check_data(merdata, types = "RES", pattern = "^[RS-]+$"))
	error <- any(error)
	m_check_data_warn(merdata, types = "MLVA", pattern = "[xwzXWZ]")
	return(error)
}

# Validate data dimensions
# used by m_validate
m_check_data_dim <- function(merdata) {
	error <- FALSE
	data_dim <- dim(merdata$data)
	if(! data_dim[1] > 0 ) {
		message("Error: Number of data rows must be greater than zero")
		error <- TRUE
	}
	return(error)
}

# Check type list contains valid types
# Currently, ID, COUNT, MLVA[#], SPOL, SNP[#], RES, NOTE[#] 
# used by m_validate
m_check_valid_type <- function(merdata) {
	error <- FALSE
	valid_types <- c("ID", "COUNT", "MLVA", "SPOL", "SNP", "RES", "NOTE")
	if(length(colnames(merdata$data)) != length(m_get_types(merdata, valid_types))) {
		message("Error: invalid data type. 
						 Types must contain prefix [",
						 paste(valid_types, collapse = ","), "]")
		error <- TRUE
	}
	return(error)
}


# Check unique ID column - Checks that there is one ID coulumn
# used by m_validate
m_check_id <- function(merdata) {
	error <- FALSE
	if(sum(colnames(merdata$data) == "ID") != 1) {
		message("Error: there must be exactly one 'ID' column")
		error <- TRUE
	}
	return(error)
}


# Check COUNT column - Checks that there is one COUNT coulumn and it is numeric
# used by m_validate
m_check_count <- function(merdata) {
	error <- FALSE
	if(sum(colnames(merdata$data) == "COUNT") != 1) {
		message("Error: there must be exactly one 'COUNT' column")
		error <- TRUE
	}
	if(is.na(sum(suppressWarnings(merdata$data$COUNT)))) {
		message("Error: all 'COUNT' values must be numeric")
		error <- TRUE
	}
	return(error)	
}

# checks to see if a single data column has a consistent number 
# of characters and matches a pattern
m_check_data_col <- function(merdata, type, pattern) {
	error <- FALSE
	if(length(unique(nchar(as.character(merdata$data[[type]])))) > 1) {
		message("Error: data for ", type, 
			" does not contain the same number of characters 
			 across all rows")
		error <- TRUE
	}
	if(sum(grepl(pattern, 
							 merdata$data[[type]])) != length(merdata$data[[type]])) {
		message("Error: data for ", type," does not follow 
			the pattern ", pattern)
		error <- TRUE
	}
	return(error)
}

# calls m_check_data_col for multiple types. note that only one 
# pattern can be used at a time (not vectorized)
m_check_data <- function(merdata, types, pattern) {
	error <- FALSE
	types <- m_get_types(merdata, types)
	if(length(types) > 0)
		error <- all(do.call(c,lapply(types,
																	m_check_data_col,
																	merdata = merdata,
																	pattern = pattern)))	
	return(error)
}

m_check_data_col_warn <- function(merdata, type, pattern) {
	warn <- FALSE
	#if(sum(grepl(pattern, merdata$data[[type]])) != length(merdata$data[[type]]))
	if(sum(grepl(pattern, merdata$data[[type]])) > 0)
		warn <- TRUE
	return(warn)
}

m_check_data_warn <- function(merdata, types, pattern) {
	warn <- FALSE
	types <- m_get_types(merdata, types)
	if(length(types) > 0)
		warn <- all(do.call(c, lapply(types,
																	m_check_data_col_warn,
																	merdata = merdata,
																	pattern = pattern)))
	if(warn) warning(types,
							 " contains chars from string pattern ",
							 pattern,
							 ". Those values will not be used for computation.\n")
}

# Exports to a file in the SMEF format
# used by: m_export
m_export_smef <- function(merdata, file) {
	con <- file(file, "w")
	on.exit(close(con))
	write(paste(merdata$details[[1]],
							merdata$details[[2]], 
							sep = ": "), file = con)
	write("DATA", file = con, append = TRUE)
	write(paste(colnames(merdata$data), collapse = ","), 
			  file = con, 
			  append = TRUE)
	merdata$data[, "ID"] <- stringr::str_replace_all(merdata$data[, "ID"], ",", "/")
	utils::write.table(merdata$data,
						  file = con,
						  sep = " , ",
						  quote = FALSE,
							row.names = FALSE,
							col.names = FALSE,
							append = TRUE)
}


# Exports to a file in the RSF format
# used by: m_export
m_export_rsf <- function(merdata, file) {
	spol <- m_get_types(merdata, "SPOL")[1]  # only accept the first SPOL type
	if (! is.null(spol)) {
		con <- file(file, "w")
		on.exit(close(con))
		write(paste(merdata$details[[1]],
								merdata$details[[2]],
								sep = " "), file = con)
		write("SPOLIGOTYPE", file = con, append = TRUE)
		rsf <- paste(merdata$data$ID, merdata$data[[spol]], 
						merdata$data$COUNT, sep = " : ")
		utils::write.table(rsf,
								file = con,
								quote = FALSE,
								row.names = FALSE,
								col.names = FALSE,
								append = TRUE)
		write("//", file = con, append = TRUE)
	} else
			stop("There is no SPOL information to create an RSF file",call. = FALSE)
}
