#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#'
#' @return A Shiny application object that launches the iModMix interface.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
)
{  # --- START: Inicialización de iModMixData ---
  eh_cache <- tools::R_user_dir("ExperimentHub", "cache")
  if (!dir.exists(eh_cache)) dir.create(eh_cache, recursive = TRUE)
  eh <- ExperimentHub::ExperimentHub()
  # --- END: Inicialización de iModMixData ---

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

