library(shiny)
library(shinyjs)
library(jsonlite)
library(ggthemes)
library(Cairo)
#library(grDevices)
library(tools)

ui <- bootstrapPage(
  useShinyjs(),
  htmlTemplate("www/html/index.html")
)

server <- function(input, output, session) {
  options(shiny.usecairo = T)

  spectrum_pal <- list("#DADAEB", "#BCBDDC", "#9E9AC8", "#756BB1", "#54278F", 
                       "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C", 
                       "#CCECE6", "#99D8C9", "#66C2A4", "#2CA25F", "#006D2C", 
                       "#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#198D42", 
                       "#FDD0A2", "#FDAE6B", "#FD8D3C", "#E6550D", "#A63603", 
                       "#FCBBA1", "#FC9272", "#FB6A4A", "#DE2D26", "#A50F15", 
                       "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525")

  # Main reactive data
  user_dat <- reactiveValues(
    smef_data = mercat::m_read("www/data-examples/Monteserin.smef",
                               validate = FALSE),
    file_name = "Monteserin.smef",
    input_error = NULL,
    spol_plot_height = "828px",
    res_plot_height = "540px",
    ld_plot_height = "480px",
    mlva_plot_height = "495px",
    spol_pdf_height = 8.3,
    res_pdf_height = 5.4,
    ld_pdf_height = 4.8,
    mlva_pdf_height = 5.0
  )

  # Colour palette reactive data
  colour_dat <- reactiveValues(
    resfreq_colour = "#54278f",
    resld_colour_high = "#3182bd",
    resld_colour_low = "#ffffff",
    resld_colour_na = "#ffffff",
    spolfreq_colour_high = "#000000",
    spolfreq_colour_low = "#ffffff",
    mlvafreq_colour_high = "#3182bd",
    mlvafreq_colour_low = "#ffffff"
  )

  # Checkbox reactive data
  checkbox_dat <- reactiveValues(
    resbar_orderby_box = TRUE,
    resbar_revorder_box = FALSE,
    resbar_flip_box = TRUE,
    resbar_legend_box = TRUE,
    resld_value_box = TRUE,
    resld_legend_box = FALSE,
    resld_sym_box = FALSE,
    mlvafreq_value_box = TRUE,
    mlvafreq_legend_box = FALSE
  )

  legend_dat <- reactiveValues(
    resbar_legx = 0.5,
    resbar_legy = 0.8
  )

  tmp <- tempfile()
  shiny::onSessionEnded(function(){ unlink(tmp) })

  output$smef_doc <- renderUI({
    tools::Rd2HTML("www/smef.Rd", tmp, no_links = TRUE)
    shiny::includeHTML(tmp)
  })

  # Redraw Forest
  update_forest <- function() {
    session$sendCustomMessage("forest_reset_force", TRUE)
    link_df <- mercat::m_export_forest(isolate(user_dat$smef_data), type = "links")
    node_df <- mercat::m_export_forest(isolate(user_dat$smef_data), type = "nodes")
    session$sendCustomMessage("forest_link_data", jsonlite::toJSON(link_df, pretty = TRUE))
    session$sendCustomMessage("forest_node_data", jsonlite::toJSON(node_df, pretty = TRUE))
  }

  # Redraw MST
  update_mst <- function() {
    session$sendCustomMessage("mst_reset_force", TRUE)
    link_df <- mercat::m_export_mst(isolate(user_dat$smef_data), type = "links")
    node_df <- mercat::m_export_mst(isolate(user_dat$smef_data), type = "nodes")
    session$sendCustomMessage("mst_link_data", jsonlite::toJSON(link_df, pretty = TRUE))
    session$sendCustomMessage("mst_node_data", jsonlite::toJSON(node_df, pretty = TRUE))
  }

  # Dynamic Plot Heights
  get_spol_plot_height <- function(px = TRUE) {
    old_plot_height <- isolate(user_dat$spol_plot_height)
    if(is.null(mercat::m_get_first_type(isolate(user_dat$smef_data), "SPOL", halt = FALSE)))
      return(old_plot_height)
    spol_type <- mercat::m_get_first_type(isolate(user_dat$smef_data), "SPOL")
    stypes <- summary(mercat::m_aggregate(isolate(user_dat$smef_data), spol_type))[[spol_type]]$genotypes
    if(is.null(stypes)) return(NULL)
    plot_height <- (stypes + 3) * 18
    if(px) return(paste0(plot_height, "px"))
    else return(round(plot_height / 100, 1))
  }

  get_res_plot_height <- function(px = TRUE) {
    old_plot_height <- isolate(user_dat$res_plot_height)
    if(is.null(mercat::m_get_first_type(isolate(user_dat$smef_data), "RES", halt = FALSE)))
      return(old_plot_height)
    res_type <- mercat::m_get_first_type(isolate(user_dat$smef_data), "RES")
    rtypes <- summary(mercat::m_aggregate(isolate(user_dat$smef_data), res_type))[[res_type]]$genotypes
    if(is.null(rtypes)) return(NULL)
    plot_height <- (rtypes + 1) * 30
    if(px) return(paste0(plot_height, "px"))
    else return(round(plot_height / 100, 1))
  }

  get_ld_plot_height <- function(px = TRUE) {
    old_plot_height <- isolate(user_dat$ld_plot_height)
    if(is.null(mercat::m_get_first_type(isolate(user_dat$smef_data), "RES", halt = FALSE)))
      return(old_plot_height)
    res_type <- mercat::m_get_first_type(isolate(user_dat$smef_data), "RES")
    ldtypes <- summary(mercat::m_aggregate(isolate(user_dat$smef_data), res_type))[[res_type]]$res_frequency
    if(is.null(ldtypes)) return(NULL)
    antib <- length(ldtypes)
    plot_height <- (antib + 1) * 80
    paste0(plot_height, "px")
    if(px) return(paste0(plot_height, "px"))
    else return(round(plot_height / 100, 1))
  }

  get_mlva_plot_height <- function(px = TRUE) {
    old_plot_height <- isolate(user_dat$mlva_plot_height)
    if(is.null(mercat::m_get_first_type(isolate(user_dat$smef_data), "MLVA", halt = FALSE)))
      return(old_plot_height)
    max_repeats <- isolate(user_dat$smef_data) %>% mercat::m_mlva() %>% nrow()
    if(is.null(max_repeats)) return(NULL) 
    plot_height <- (max_repeats + 1) * 45
    if(px) return(paste0(plot_height, "px"))
    else return(round(plot_height / 100, 1))
  }

  # Available types for javascript visualisation
  get_available_types <- function() {
    mdata <- isolate(user_dat$smef_data)
    available_types <- list() 
    if(!is.null(mercat::m_get_first_type(mdata, type = "SPOL", halt = FALSE))) 
      available_types <- c(available_types, c("spol"))
    if(!is.null(mercat::m_get_first_type(mdata, type = "RES", halt = FALSE))) 
      available_types <- c(available_types, c("res"))
    if(!is.null(mercat::m_get_first_type(mdata, type = "MLVA", halt = FALSE))) 
      available_types <- c(available_types, c("mlva"))
    if(!is.null(mercat::m_get_first_type(mdata, type = "SNP", halt = FALSE))) 
      available_types <- c(available_types, c("snp"))
    return(available_types)
  }

  # Resistance names for javascript visualisation
  get_ab_names <- function() {
    mdetails <- isolate(user_dat$smef_data$details) %>% as.data.frame()
    ab_names <- mdetails %>%
      dplyr::filter(FIELDS == "RES") %>%
      dplyr::pull(VALUES) %>%
      stringr::str_split(",") %>%
      unlist() %>%
      stringr::str_trim(side = "both") %>%
      mercat::m_antibiotic_full_names()
    return(ab_names)
  }

  update_all <- function(file_upload = TRUE) {
    withProgress(message = "Rendering data",
                 detail = "initialising", {
      atypes <- get_available_types()
      ab_names <- get_ab_names()
      incProgress(1/10, detail = "starting analysis")
      if(file_upload) {
        session$sendCustomMessage("smef_uploaded", "hello")
      }
      incProgress(1/10, detail = "updating genotype data")
      session$sendCustomMessage("available_types", atypes)
      incProgress(1/10, detail = "updating names")
      session$sendCustomMessage("ab_names", ab_names)
      
      if("spol" %in% atypes) {
        incProgress(1/10, detail = "processing spolforest")
        update_forest()
      }
      
      if("spol" %in% atypes | "mlva" %in% atypes | "snp" %in% atypes) {
        incProgress(3/10, detail = "processing mst")
        update_mst()
      }
      incProgress(3/10, detail = "finished processing")
      user_dat$spol_plot_height <- get_spol_plot_height()
      user_dat$res_plot_height <- get_res_plot_height()
      user_dat$ld_plot_height <- get_ld_plot_height()
      user_dat$mlva_plot_height <- get_mlva_plot_height()
      user_dat$spol_pdf_height <- get_spol_plot_height(px = FALSE)
      user_dat$res_pdf_height <- get_res_plot_height(px = FALSE)
      user_dat$ld_pdf_height <- get_ld_plot_height(px = FALSE)
      user_dat$mlva_pdf_height <- get_mlva_plot_height(px = FALSE)
    })
  }

  # run initially
  update_all(file_upload = FALSE)

  # Watch for new file input
  observeEvent(input$in_file, {
    user_dat$file_name <- input$in_file$name
    new_merdata <- tryCatch(m_read(input$in_file$datapath),
                            error = function(e) return(NULL))
    if(is.null(new_merdata)) {
      user_dat$input_error <- "Error parsing file. Please check console logs."
      return(NULL)
    }
    user_dat$smef_data <- new_merdata
    user_dat$input_error <- NULL
    update_all()
  })


  # OUTPUTS DEFINED BELOW
  # File upload error status
  output$upload_error <- renderUI({
    HTML(paste0("<font color = 'red'>", user_dat$input_error, "</font>"))
  })


  # File Upload Button
  output$button_file <- renderUI({
    fileInput("in_file", "Choose a SMEF or RSF file",
                multiple = FALSE,
                accept = c(".smef", ".rsf"))
  })

  output$spolforest_file_name <- renderUI({
    HTML(user_dat$file_name)
  })

  output$mst_file_name <- renderUI({
    HTML(user_dat$file_name)
  })

  # SMEF Summary
  # ----------------------------------------------------------------------------
  output$smef_summary <- renderPrint({
    mercat::m_summary(user_dat$smef_data)
  })
  # ----------------------------------------------------------------------------


  # SMEF Data
  # ----------------------------------------------------------------------------
  output$smef_data <- renderPrint({
    print(user_dat$smef_data)
  })
  # ----------------------------------------------------------------------------



  # Resistance Frequency Plot
  # ----------------------------------------------------------------------------
  observeEvent(input$resbar_color, {
    colour_dat$resfreq_colour <- input$resbar_color
  })

  observeEvent(input$resbar_orderby, {
    checkbox_dat$resbar_orderby_box <- input$resbar_orderby
  })

  observeEvent(input$resbar_revorder, {
    checkbox_dat$resbar_revorder_box <- input$resbar_revorder
  })

  observeEvent(input$resbar_flip, {
    checkbox_dat$resbar_flip_box <- input$resbar_flip
    tempx <- legend_dat$resbar_legx
    legend_dat$resbar_legx <- legend_dat$resbar_legy
    legend_dat$resbar_legy <- tempx
  })

  observeEvent(input$resbar_legend, {
    checkbox_dat$resbar_legend_box <- input$resbar_legend
  })

  observeEvent(input$resbar_legx, {
    if(checkbox_dat$resbar_flip_box)
      legend_dat$resbar_legy <- input$resbar_legx / 10
    else
      legend_dat$resbar_legx <- input$resbar_legx / 10
  })

  observeEvent(input$resbar_legy, {
    if(checkbox_dat$resbar_flip_box)
      legend_dat$resbar_legx <- input$resbar_legy / 10
    else
      legend_dat$resbar_legy <- input$resbar_legy / 10
  })

  get_res_pdf_height <- function() {
    if(checkbox_dat$resbar_flip_box) pdf_height <- user_dat$res_pdf_height
    else pdf_height <- 5.4
    return(pdf_height)
  }

  output$res_freq_download <- downloadHandler(
      filename =  function() {"res_freq.pdf"},
      content = function(file) {
        ggplot2::ggsave(mercat::m_res_freq(user_dat$smef_data,
            title = NULL,
            xaxis_size = 16,
            yaxis_size = 16,
            label_size = 14,
            orderbynames = checkbox_dat$resbar_orderby_box,
            reverseorder = checkbox_dat$resbar_revorder_box,
            flip = checkbox_dat$resbar_flip_box,
            legend = checkbox_dat$resbar_legend_box,
            legend_x = legend_dat$resbar_legx,
            legend_y = legend_dat$resbar_legy,
            fill = colour_dat$resfreq_colour),
          filename = file, width = 8, height = get_res_pdf_height(), device = cairo_pdf)
      },
      contentType = "application/pdf"
    )

  output$res_freq_download_ui <- renderUI({  
    downloadButton("res_freq_download", "Download PDF")
  })

  output$res_freq_plot <- renderPlot({
    mercat::m_res_freq(user_dat$smef_data,
      title = NULL,
      xaxis_size = 16,
      yaxis_size = 16,
      label_size = 14,
      orderbynames = checkbox_dat$resbar_orderby_box,
      reverseorder = checkbox_dat$resbar_revorder_box,
      flip = checkbox_dat$resbar_flip_box,
      legend = checkbox_dat$resbar_legend_box,
      legend_x = legend_dat$resbar_legx,
      legend_y = legend_dat$resbar_legy,
      fill = colour_dat$resfreq_colour)                   
  })

  output$res_freq_ui <- renderUI({
    if(checkbox_dat$resbar_flip_box) plot_height <- user_dat$res_plot_height
    else plot_height <- "540px"
    plotOutput("res_freq_plot", height = plot_height)
  })

  output$res_freq_file_name <- renderUI({
    HTML(user_dat$file_name)
  })
  # ----------------------------------------------------------------------------



  # Resistance Correlation Plot
  # ----------------------------------------------------------------------------

  observeEvent(input$resld_color_high, {
    colour_dat$resld_colour_high <- input$resld_color_high
  })

  observeEvent(input$resld_color_low, {
    colour_dat$resld_colour_low <- input$resld_color_low
  })

  observeEvent(input$resld_color_na, {
    colour_dat$resld_colour_na <- input$resld_color_na
  })

  observeEvent(input$resld_values, {
    checkbox_dat$resld_value_box <- input$resld_values
  })

  observeEvent(input$resld_legend, {
    checkbox_dat$resld_legend_box <- input$resld_legend
  })

  observeEvent(input$resld_sym, {
    checkbox_dat$resld_sym_box <- input$resld_sym
  })

  output$res_ld_download <- downloadHandler(
      filename =  function() {"res_ld.pdf"},
      content = function(file) {
        ggplot2::ggsave(mercat::m_res_corr_mat(user_dat$smef_data,
            label_size = 14,
            xaxis_size = 16,
            yaxis_size = 16,            
            col_high = colour_dat$resld_colour_high,
            col_low = colour_dat$resld_colour_low,
            col_na = colour_dat$resld_colour_na,
            values = checkbox_dat$resld_value_box,
            legend = checkbox_dat$resld_legend_box,
            symmetric = checkbox_dat$resld_sym_box),
          filename = file, width = 8, height = user_dat$ld_pdf_height, device = cairo_pdf)
      },
      contentType = "application/pdf"
    )

  output$res_ld_download_ui <- renderUI({  
    downloadButton("res_ld_download", "Download PDF")
  })

  output$res_cor_plot <- renderPlot({
    mercat::m_res_corr_mat(user_dat$smef_data,
      label_size = 14,
      xaxis_size = 16,
      yaxis_size = 16,            
      col_high = colour_dat$resld_colour_high,
      col_low = colour_dat$resld_colour_low,
      col_na = colour_dat$resld_colour_na,
      values = checkbox_dat$resld_value_box,
      legend = checkbox_dat$resld_legend_box,
      symmetric = checkbox_dat$resld_sym_box)
  })

  output$res_cor_ui <- renderUI({
    plotOutput("res_cor_plot", height = user_dat$ld_plot_height)
  })

  output$res_cor_file_name <- renderUI({
    HTML(user_dat$file_name)
  })
  # ----------------------------------------------------------------------------



  # Spoligotype Plot
  # ----------------------------------------------------------------------------
  observeEvent(input$spolfreq_color_high, {
    colour_dat$spolfreq_colour_high <- input$spolfreq_color_high
  })

  observeEvent(input$spolfreq_color_low, {
    colour_dat$spolfreq_colour_low <- input$spolfreq_color_low
  })

  output$spol_freq_download <- downloadHandler(
      filename =  function() {"spol_freq.pdf"},
      content = function(file) {
        ggplot2::ggsave(mercat::m_spol_freq(user_dat$smef_data,
            title = NULL,
            xaxis_size = 12,
            yaxis_size = 12,
            label_size = 8,
            col_high = colour_dat$spolfreq_colour_high,
            col_low = colour_dat$spolfreq_colour_low),
          filename = file, width = 8, height = user_dat$spol_pdf_height, device = cairo_pdf)
      },
      contentType = "application/pdf"
    )

  output$spol_freq_download_ui <- renderUI({  
    downloadButton("spol_freq_download", "Download PDF")
  })

  output$spol_freq_plot <- renderPlot({
    mercat::m_spol_freq(user_dat$smef_data,
      title = NULL,
      xaxis_size = 16,
      yaxis_size = 16,
      label_size = 14,
      col_high = colour_dat$spolfreq_colour_high,
      col_low = colour_dat$spolfreq_colour_low)
  })

  output$spol_freq_ui <- renderUI({
    plotOutput("spol_freq_plot", height = user_dat$spol_plot_height)
  })

  output$spol_freq_file_name <- renderUI({
    HTML(user_dat$file_name)
  })
  # ----------------------------------------------------------------------------




  # MLVA Frequency Plot
  # ----------------------------------------------------------------------------
  observeEvent(input$mlvafreq_color_high, {
    colour_dat$mlvafreq_colour_high <- input$mlvafreq_color_high
  })

  observeEvent(input$mlvafreq_color_low, {
    colour_dat$mlvafreq_colour_low <- input$mlvafreq_color_low
  })

  observeEvent(input$mlvafreq_values, {
    checkbox_dat$mlvafreq_value_box <- input$mlvafreq_values
  })

  observeEvent(input$mlvafreq_legend, {
    checkbox_dat$mlvafreq_legend_box <- input$mlvafreq_legend
  })

  output$mlva_freq_download <- downloadHandler(
      filename =  function() {"mlva_freq.pdf"},
      content = function(file) {
        ggplot2::ggsave(mercat::m_mlva_freq(user_dat$smef_data,
            title = NULL,
            xaxis_size = 16,
            yaxis_size = 16,
            col_high = colour_dat$mlvafreq_colour_high,
            col_low = colour_dat$mlvafreq_colour_low,
            values = checkbox_dat$mlvafreq_value_box,
            legend = checkbox_dat$mlvafreq_legend_box),
          filename = file, width = 8, height = user_dat$mlva_pdf_height, device = cairo_pdf)
      },
      contentType = "application/pdf"
    )

  output$mlva_freq_download_ui <- renderUI({  
    downloadButton("mlva_freq_download", "Download PDF")
  })

  output$mlva_freq_plot <- renderPlot({
    mercat::m_mlva_freq(user_dat$smef_data,
      title = NULL,
      xaxis_size = 16,
      yaxis_size = 16,
      col_high = colour_dat$mlvafreq_colour_high,
      col_low = colour_dat$mlvafreq_colour_low,
      values = checkbox_dat$mlvafreq_value_box,
      legend = checkbox_dat$mlvafreq_legend_box)
  })

  output$mlva_freq_ui <- renderUI({
    plotOutput("mlva_freq_plot", height = user_dat$mlva_plot_height)
  })

  output$mlva_freq_file_name <- renderUI({
    HTML(user_dat$file_name)
  })
  # ----------------------------------------------------------------------------

}

shinyApp(ui, server)