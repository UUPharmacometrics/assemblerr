test_that("adding of components to slots", {
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
