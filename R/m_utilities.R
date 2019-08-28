
#' m_oct2bin
#'
#' Converts an octal spoligotype pattern to binary format.
#' For more information see Dale, J. W., et al. "Spacer oligonucleotide typing 
#' of bacteria of the Mycobacterium tuberculosis complex: recommendations for 
#' standardised nomenclature [The Language of Our Science]." The International 
#' Journal of Tuberculosis and Lung Disease 5.3 (2001): 216-219.
#' @param octal A spoligotype pattern in octal format
#' @return A spoligotype pattern in binary format
#' @examples m_oct2bin("777743677760771")
#' @keywords octal spoligotype format
#' @export
m_oct2bin <- function(octal) {
  converter <- c("000", "001", "010", "011", "100", "101", "110", "111")
  pattern <- "[0-7]{15}" # Regex for octal string of length 15
  # Matches 15 or more occurances of digits 0-7
  if(stringr::str_detect(octal, pattern)) {
    if(nchar(octal) > 15)
      warning("String of length greater than 15 entered. 
               Extra characters ignored.")  
  } else {
    stop("Improper octal string entered.", call. = FALSE) 
  } 
  binary <- as.numeric(strsplit(octal, "")[[1]])[1:14]
  binary <- paste(sapply(binary, function(x) converter[x + 1]), collapse = "")
  binary <- paste(binary, as.numeric(strsplit(octal, "")[[1]])[15], sep = "")
  return(binary)
}

#' m_hex2bin
#'
#' Converts a hex spoligotype pattern to binary format. 
#' For more information see Dale, J. W., et al. "Spacer oligonucleotide typing 
#' of bacteria of the Mycobacterium tuberculosis complex: recommendations for 
#' standardised nomenclature [The Language of Our Science]." The International 
#' Journal of Tuberculosis and Lung Disease 5.3 (2001): 216-219.
#' @param hex A spoligotype pattern in hex format
#' @return A spoligotype pattern in binary format
#' @examples m_hex2bin("7F-7E-1E-FF-F0-FF")
#' @keywords hex spoligotype format
#' @export
m_hex2bin <- function(hex) {
  converter <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
                 "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
    pattern = "[A-Fa-f0-9][A-Fa-f0-9]-[A-Fa-f0-9][A-Fa-f0-9]-[A-Fa-f0-9][A-Fa-f0-9]-[A-Fa-f0-9][A-Fa-f0-9]-[A-Fa-f0-9][A-Fa-f0-9]-[A-Fa-f0-9][A-Fa-f0-9]"
    stringr::str_detect("7F-7E-1E-FF-F0-FF", pattern = pattern)
  if(stringr::str_detect(hex, pattern)){
      if(nchar(hex) > 17){
    warning("String of length greater than 17 entered. 
             Extra characters ignored.")  
   }
  } else {
    stop("Improper hex string entered.", call. = FALSE) 
  } 
  binary <- strsplit(paste(strsplit(hex, "-")[[1]], collapse = ""), 
                     split = "")[[1]]
  binary <- paste("0x", binary, sep = "")
  binary <- sapply(as.numeric(binary) + 1, function(x) converter[x])
  rm <- c(1, 9, 17, 25, 41)
  binary <- paste(unlist(strsplit(binary, split = ""))[-rm], collapse = "")
  return(binary)
}

#' m_antibiotic_full_names
#'
#' Converts antibiotic drug three letter abbreviations to full names.
#' Antibiotic names sourced from https://jcm.asm.org/content/abbreviations-and-conventions
#' @param drug_list A string vector of three letter abbreviations
#' @return A string vector of full antibiotic names 
#' @examples m_antibiotic_full_names(c("STM", "RMP"))
#' @keywords antibiotic abbreviation drug
#' @export
m_antibiotic_full_names <- function(drug_list) {
  drug_list <- toupper(drug_list)
  drug_abbr <- c(
   "STM", "INH", "RMP", "EMB", "PZA",
   "AMK", "AMX", "AMC", "AMP", "SAM", "AZM", "AZL", "ATM",
   "CAR", "CEC", "CFR", "FAM", "CFZ", "CDR", "CDN", "FEP",
   "FET", "CFM", "CMZ", "CID", "CFP", "CTX", "CTT", "FOX",
   "CPD", "CPR", "CAZ", "CTB", "ZOX", "CRO", "CXM", "LEX",
   "CEF", "HAP", "RAD", "CHL", "CIN", "CIP", "CLR", "CLX",
   "CLI", "CST", "DAP", "DCX", "DTM", "DOX", "ENX", "ERY",
   "FLE", "FOF", "GAT", "GEN", "GRX", "IPM", "KAN", "LVX",
   "LZD", "LOM", "LOR", "MEM", "MET", "MEZ", "MIN", "MOX",
   "MXF", "NAF", "NAL", "NET", "NIT", "NOR", "OFX", "OXA",
   "PEN", "PIP", "TZP", "PMB", "Q-D", "RFB", "RIF", "RFP",
   "SPX", "SPT", "STR", "TEC", "TEL", "TET", "TIC", "TIM",
   "TGC", "TOB", "TMP", "SXT", "TVA", "VAN", "CLA", "SUL",
   "TZB", "AMB", "CLT", "5FC", "FLC", "ITC", "KTC", "NYT",
   "TRB", "VRC", "ACV", "CDV", "FCV", "FOS", "GCV", "PCV",
   "VCV", "AZT", "SM", "ETB")
  drug_name <- c(
    "streptomycin", "isoniazid", "rifampicin", "ethambutol", "pyrazinamide",
    "amikacin", "amoxicillin", "amoxicillin-clavulanic", "ampicillin",
    "ampicillin-sulbactam", "azithromycin", "azlocillin", "aztreonam",
    "carbenicillin", "cefaclor", "cefadroxil", "cefamandole",
    "cefazolin", "cefdinir", "cefditoren", "cefepime",
    "cefetamet", "cefixime", "cefmetazole", "cefonicid",
    "cefoperazone", "cefotaxime", "cefotetan", "cefoxitin",
    "cefpodoxime", "cefprozil", "ceftazidime", "ceftibuten",
    "ceftizoxime", "ceftriaxone", "cefuroxime", "cephalexin",
    "cephalothin", "cephapirin", "cephradine", "chloramphenicol",
    "cinoxacin", "ciprofloxacin", "clarithromycin", "clinafloxacin",
    "clindamycin", "colistin", "daptomycin", "dicloxacillin",
    "dirithromycin", "doxycycline", "enoxacin", "erythromycin",
    "fleroxacin", "fosfomycin", "gatifloxacin", "gentamicin",
    "grepafloxacin", "imipenem", "kanamycin", "levofloxacin",
    "linezolid", "lomefloxacin", "loracarbef", "meropenem",
    "methicillin", "mezlocillin", "minocycline", "moxalactam",
    "moxifloxacin", "nafcillin", "nalidixic", "netilmicin",
    "nitrofurantoin", "norfloxacin", "ofloxacin", "oxacillin",
    "penicillin", "piperacillin", "piperacillin-tazobactam", "polymyxin",
    "quinupristin-dalfopristin", "rifabutin", "rifampin", "rifapentine",
    "sparfloxacin", "spectinomycin", "streptomycin", "teicoplanin",
    "telithromycin", "tetracycline", "ticarcillin", "ticarcillin-clavulanic",
    "tigecycline", "tobramycin", "trimethoprim", "trimethoprim-sulfamethoxazole",
    "trovafloxacin", "vancomycin", "clavulanic", "sulbactam",
    "tazobactam", "amphotericin", "clotrimazole", "flucytosine",
    "fluconazole", "itraconazole", "ketoconazole", "nystatin",
    "terbinafine", "voriconazole", "acyclovir", "cidofovir",
    "famciclovir", "foscarnet", "ganciclovir", "penciclovir",
    "valacyclovir", "zidovudine", "streptomycin", "ethambutol")
  full_names <- NULL
  for(abbr in drug_list) {
    drug_idx <- which(drug_abbr== abbr)
    if(length(drug_idx) > 0) full_names <- c(full_names,drug_name[drug_idx])
    else full_names <- c(full_names,abbr)
  }
  return(full_names)
}

#' m_mlva_names
#'
#' Returns MIRU-VNTR standard names according to 
#' \href{http://www.miru-vntrplus.org/MIRU/miruChooser.faces}{MIRU-VNTRplus}.
#' @param merdata A \code{merdata} object
#' @return A string vector of MIRU-VNTR names
#' @references \href{http://www.miru-vntrplus.org/MIRU/miruChooser.faces}{MIRU-VNTRplus}
#' @examples m_mlva_names(monteserin)
#' @keywords MLVA VNTR MIRU merdata
#' @export
m_mlva_names <- function(merdata) {
  miru <- NULL
  mlva <- m_get_types(merdata, "MLVA")
  if(length(mlva) > 0) {
    i <- 1
    for(j in mlva) {
      n <- nchar(as.character(merdata$data[[j]][1]))
      miru[[i]] <- c("MIRU02", "Mtub04", "ETRC", "MIRU04ETRD", "MIRU40", "MIRU10",
                     "MIRU16", "Mtub21", "MIRU20", "QUB11b", "ETRA", "Mtub29",
                     "Mtub30", "ETRB", "MIRU23", "MIRU24", "MIRU26", "MIRU27QUB5",
                     "Mtub34", "MIRU31ETRE", "Mtub39", "QUB26", "QUB4156", "MIRU39")
      if(n == 12) {
        miru[[i]] <- miru[[i]][c(1, 4:7, 9, 15:18, 20, 24)]
      } else if(n == 15) {
        miru[[i]] <- miru[[i]][c(2:8, 10:11, 13, 17, 20:23)]
      } else if(n != 24) {
        miru[[i]] <- c(1:n)
      }
      i <- i + 1
    }
  }
  if(length(mlva) == 1) miru <- unlist(miru)
  return(miru)
}

#' m_export_forest
#' 
#' Returns a data frame with link or node data from a merdata spolforest.
#' @param merdata A \code{merdata} object
#' @param type The type of data to return: "links" or "nodes"
#' @return A data frame with the requested object type from the spolforest
#' @examples 
#' m_export_forest(monteserin, "links")
#' @keywords merdata spolforest export
#' @export
m_export_forest <- function(merdata, type = "links") {
  sforest <- m_res_spol_forest(merdata) #%>% add_layout_(as_tree())
  gdata_n <- igraph::get.data.frame(sforest, what = "vertices")
  gdata_l <- igraph::get.data.frame(sforest, what = "edges")
  if(type == "links") return(gdata_l)
  else if(type == "nodes") return(gdata_n)
  else return(NULL)
}

#' m_export_mst
#'
#' Returns a data frame with link or node data from a merdata mst.
#' @param merdata A \code{merdata} object
#' @param type The type of data to return: "links" or "nodes"
#' @return A data frame with the requested object type from the mst
#' @examples 
#' m_export_mst(monteserin, "links")
#' @keywords merdata mst export
#' @export
m_export_mst <- function(merdata, type = "links") {
  smst <- m_mst_from_data(merdata) #%>% add_layout_(as_tree())
  gdata_n <- igraph::get.data.frame(smst, what = "vertices")
  gdata_l <- igraph::get.data.frame(smst, what = "edges")
  if(type == "links") return(gdata_l)
  else if(type == "nodes") return(gdata_n)
  else return(NULL)
}