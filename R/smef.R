#' SMEF: Simple Molecular Epidemiology Format
#'
#' SMEF, which stands for Simple Molecular Epidemiology Format, is the
#' input data format for the MERCAT software
#' It is a plain text format making it easily readable by computers and understandable by humans.
#' 
#' The first section of a SMEF file is a preamble that contains information
#' related to the study while the second section, which starts with the
#' heading DATA, contains the genotyping patterns and other information
#' such as drug resistance phenotypes.
#' An example of the first few lines of the format is as follows.
#' 
#' \preformatted{
#' FORMAT: SMEF 1.0
#' AUTHOR1:  Monteserin
#' PUBYEAR: 2013
#' TITLE: Genotypes of Mycobacterium tuberculosis in patients at risk of drug resistance in Bolivia
#' PMID: 23603419
#' MLVA15:  2163b,4052,960,1955,2996,3690,2165,2401,802,577,424,4156,1644,3192,580
#' RES: STM, INH, RMP, EMB, PZA
#' DATA
#' COUNT,MLVA15,SPOL,RES,NOTE,ID
#' 1 , 253533233433427 , 1111111111111111111111111111110100001110111 , SSSSS , H3 , BO0091
#' 1 , 253533233433427 , 1111111111111111111111111111110100001110111 , SSSSS , H3 , BO0093
#' 1 , 143533233433527 , 1111111111111111111111111000000100001111111 , SRSSS , H1 , BO0207
#' 1 , 143533233433527 , 1111111111111111111111111000000100001111111 , SSSRS , H1 , BO0117
#' }
#' 
#' @section Preamble section:
#' The preamble section gives accompanying information such as the list of drugs 
#' in a specified order.  There is no restriction on the number of fields and 
#' most fields are optional in that none of the functions require them. The name 
#' of a field and the value of the field must be separated by a colon (note that 
#' the field cannot be named DATA because this is a reserved word).
#'
#' \emph{MLVA/VNTR}: The names of MLVA loci are given in a field called MLVA or 
#' MLVAn (where the "n" is an identifier). For example, MLVA15 names the 
#' field which gives locus names for VNTR-typing using 15 loci. The loci can be 
#' given in any order. The field name in the preamble (e.g. "MLVA15") must 
#' match the corresponding field name in the column header of the data section.
#' 
#' \emph{SNP}: The positions in the genome of the SNPs are given in a field called SNP or
#' SNPn (where the "n" is an identifer). For example SNP1 names the field which
#' gives the locus number of each SNP in the marker. The field name must
#' match the corresponding field name in the column header of the data section.
#' 
#' \emph{Drug resistance}: The RES field gives abbreviated names of
#' antimicrobial drugs. This field name ("RES") must match the
#' corresponding field name in the column header of the data section.
#' 
#' @section Data section:
#' 
#' The second section (DATA), is organised as values separated by commas -
#' akin to comma-separated variable (csv) format. The first line after the
#' DATA heading (which is compulsory) indicates the types of variables in the columns of the
#' data. For example, ID gives the labels of the isolates and
#' COUNT is used to show the frequency of isolates represented in the
#' lines. These types must contain a prefix
#' {[}ID, COUNT, MLVA, SPOL, SNP, RES, NOTE{]} and may be followed by extra characters for
#' alternative forms of the type; for example, for multiple NOTE columns
#' use NOTE1, NOTE2, etc.
#' 
#' Formats of content in the DATA section
#' 
#' \itemize{
#' \item SPOL (spoligotypes) is given in one of several formats:
#'   \itemize{
#'   \item binary (e.g. 1111111111111000111101111111111100001111111),
#'   \item octal (e.g. 777743677760771), and
#'   \item hex (e.g. 7F-7E-1E-FF-F0-FF).
#'   }
#' \item MLVAn (e.g. MLVA15 or MLVA24) follows the CDC notation under which
#' numerals 0 to 9 represent corresponding numbers of repeats, and letters 
#' A, B, C, and so on represent 10, 11, 12, 13 and so on up to W=32. A dash (-) 
#' represents a lack of amplification product. The letters X, Y, Z are reserved 
#' for anomalous states for special MIRUs. 
#' Note also that the format is case insensitive so that it may be convenient to use "i" 
#' instead of "I", "L" instead of "l" and "o" instead of "O" in order to 
#' avoid confusion among similar-looking characters. 
#' \item SNPn (e.g. SNP1 or SNP15) is a string of the same length as the number of 
#' SNPs in the sample. The string contains the characters C, A, T, G which represent 
#' the allele for the corresponding position. 
#' \item RES is a string of the same length as the number of drugs given in the
#'   RES field in the preamble section. The string contains R for resistant, S
#'   for susceptible and "-" for unknown or not tested.
#' }
#' @docType package
#' @name smef
NULL