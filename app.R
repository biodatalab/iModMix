# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: reconnect::deployApp()
# Or to use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options(golem.app.prod = TRUE)
options(shiny.autoload.r=FALSE)
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize=100*1024^2)
iModMix::run_app()
