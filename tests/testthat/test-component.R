test_that("add component to slot", {
  AbsorptionComponent <- setClass("AbsorptionComponent", contains = "Component")
  FOAbsorptionComponent <- setClass("FOAbsorptionComponent", contains = "AbsorptionComponent")
  TestPKModel <- setClass("TestPKModel", contains = "Component", slots = c(absorption = "AbsorptionComponent"))

  m <- TestPKModel()
  # after creation slot should be base class
  expect_s4_class(m@absorption, "AbsorptionComponent")
  foabs <- FOAbsorptionComponent()
  # adding component does not create any massage
  expect_silent(m <- add_component(m, foabs))
  # absorption component is now FOAbsorption
  expect_s4_class(m@absorption, "FOAbsorptionComponent")
})


test_that("add component to list slot", {
  TestParameter <- setClass("TestParameter", contains = "NamedComponent")
  ParameterList <- setClass(
    "ParameterList",
    contains = "NamedComponentList",
    prototype = prototype(component_class = "TestParameter")
  )
  TestObservation <- setClass("TestObservation", contains = "NamedComponent")
  ObservationList <- setClass(
    "ObservationList",
    contains = "NamedComponentList",
    prototype = prototype(component_class = "TestObservation")
  )
  TestModel <- setClass(
    "TestModel",
    contains = "Component",
    slots = c(parameters = "ParameterList", observations = "ObservationList")
  )
  m <- TestModel()
  expect_s4_class(m@parameters, "ParameterList")
  expect_s4_class(m@observations, "ObservationList")
  prm <- TestParameter(name = "cl")
  prm2 <- TestParameter(name = "v")
  obs <- TestObservation(name = "conc")
  expect_silent(m <- add_component(m, prm))
  expect_silent(m <- add_component(m, prm2))
  expect_silent(m <- add_component(m, obs))
  expect_equal(m@parameters[[1]]@name, "cl")
  expect_equal(m@observations[[1]]@name, "conc")
  expect_equal(m@parameters[["cl"]]@name, "cl")
  expect_equal(m@parameters[["v"]]@name, "v")
})
