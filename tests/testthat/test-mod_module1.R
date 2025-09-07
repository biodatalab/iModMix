test_that("module ui works", {
  ui <- mod_module1_ui(id = "test")
  golem::expect_shinytaglist(ui)

  # Check that required formals are present
  fmls <- formals(mod_module1_ui)
  expect_true("id" %in% names(fmls))
})

test_that("module server initializes correctly", {
  testServer(mod_module1_server, args = list(), {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))
  })
})

test_that("mod_module1 server produces infotable output", {
  testServer(mod_module1_server, args = list(), {
    # Fake input data (small csv-like dataframe)
    fake_data <- data.frame(
      feature = paste0("F", 1:5),
      S1 = rnorm(5),
      S2 = rnorm(5)
    )

    # Simulate upload of Data1
    session$setInputs(Data1 = fake_data)

    # Advance reactivity
    session$flushReact()

    # Check that infotable output slot is created
    expect_true("infotable" %in% names(output))
  })
})

