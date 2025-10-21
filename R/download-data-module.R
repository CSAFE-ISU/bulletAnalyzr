downloadDataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("download_data_button"), "Download Data")
}

downloadDataServer <- function(id, bullet_data = NULL, drop_x3p = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Prep data for download
    download_data <- shiny::reactive({
      shiny::req(bullet_data)
      data <- shiny::reactiveValuesToList(bullet_data)
      
      if (drop_x3p) {
        data$allbull <- NULL
        data$cbull <- NULL
        data$preCC <- NULL
        data$postCC <- NULL
        data$comparison <- NULL
      } else {
        data$allbull_export <- NULL
        data$cbull_export <- NULL
        data$preCC_export <- NULL
        data$postCC_export <- NULL
        data$comparison_export <- NULL
      }

      return(data)
    })
    
    # Data download handler
    output$download_data_button <- shiny::downloadHandler(
      filename = function() {
        paste0("bullet-comparison-data-", Sys.Date(), ".rds")
      },
      content = function(file) {
        saveRDS(download_data(), file)
      },
      contentType = "application/rds"
    )
    
  })
}
