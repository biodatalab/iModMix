#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(navbarPage("iModMix",
                         tabPanel("Home", icon = icon("home", lib = "glyphicon"),
                                  tags$iframe(src = "www/IModMix_Documentation.html", height = "900px", width = "100%")
                                ),
                         tabPanel("Modules & Integration",icon = icon("tasks", lib = "glyphicon"),
                                  navbarMenu(" ",
                                             tabPanel(" ",
                                                      mod_module1_ui("module1_1")
                                             )
                                             )

                         )
                         )
      #h1("iModmix")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "iModMix"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
