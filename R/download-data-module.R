#' Download Data Module UI
#'
#' Creates the UI for downloading bullet comparison data.
#'
#' @param id The module namespace ID
#'
#' @returns A Shiny download button
#' @noRd
downloadDataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::downloadButton(ns("download_data_button"), "Download Data")
}

#' Download Data Module Server
#'
#' Server logic for downloading bullet comparison data as an RDS file.
#'
#' @param id The module namespace ID
#' @param bullet_data A reactive values object containing bullet comparison data for download
#' @param phase_test_results Results from `bulletxtrctr::phase_test()`
#' @param drop_x3p Logical; if TRUE, drops x3p objects to reduce file size
#'
#' @returns NULL
#' @noRd
downloadDataServer <- function(id, bullet_data = NULL, phase_test_results = NULL, drop_x3p = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Prep data for download
    download_data <- shiny::reactive({
      shiny::req(bullet_data)
      data <- shiny::reactiveValuesToList(bullet_data)
      data$phase_test_results <- phase_test_results
      
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
    
    outfile <- shiny::reactive({
      shiny::req(bullet_data)
      return(unique(bullet_data$comparison$bullets$bullet))
    })
    
    # Data download handler
    output$download_data_button <- shiny::downloadHandler(
      filename = function() {
        paste0(bullet_names[1], "_", bullet_names[2], "_", outfile(), ".rds")
      },
      content = function(file) {
        saveRDS(download_data(), file)
      },
      contentType = "application/rds"
    )
    
  })
}
