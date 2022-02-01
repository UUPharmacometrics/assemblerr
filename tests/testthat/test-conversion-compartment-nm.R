local_edition(3)

test_that("compartments", {
  nm <- NmModel2()
  m <- model() +
    compartment("depot") +
    compartment("central")

  nm <-  purrr::reduce(m@facets$CompartmentFacet@entries, ~convert(.x, m, .y, assemblerr_options()), .init = nm)
  expect_named(nm@model, c("depot", "central"))
})

test_that("flows", {
  nm <- NmModel2()
  m <- model() +
    compartment("central") +
    flow(~cl*A, from = "central")

  nm <-  purrr::reduce(m@facets$CompartmentFacet@entries, ~convert(.x, m, .y, assemblerr_options()), .init = nm) %>%
    convert(m, m@facets$FlowFacet, assemblerr_options())
  expect_snapshot_output(as.character(nm@des@statements))
})
