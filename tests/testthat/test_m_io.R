library(testthat)
data_path <- "./data/"

context("Mercat IO Functions")

testthat::test_that("m_read works", {
	expect_error(m_read(""))
	expect_error(m_read("nonexistentfile.rsf"))
	expect_that(m_read(paste0(data_path, "Taype.smef")), is_a("merdata"))
	expect_that(m_read(paste0(data_path, "Aranaz.rsf")), is_a("merdata"))
	expect_that(m_read(paste0(data_path, "Aranaz.Test.If.Period.Works.rsf")), 
    is_a("merdata"))
})

# test m_create here

# Create merdata from imported file. Assuming that works ok,
# you should be able to export it.
# If exporting works, then you should be able to read it back in.
testthat::test_that("m_export works", {
	# testing SMEF
	infile <- paste0(data_path, "Taype.smef")
	outfile <- paste0(data_path, "Taype.test.smef")
	m_export(m_read(infile, validate = FALSE), file = outfile)
	expect_that(m_read(outfile), is_a("merdata"))
	if (file.exists(outfile)) file.remove(outfile) # Clean-up
	# testing RSF
	infile <- paste0(data_path, "Aranaz.rsf")
	outfile <- paste0(data_path, "Aranaz.test.rsf")
	m_export(m_read(infile, validate = FALSE), file = outfile, format = "RSF")
	expect_that(m_read(outfile), is_a("merdata"))
	if (file.exists(outfile)) file.remove(outfile) # Clean-up
})


# test m_get_types here



# test m_get_first_type here


