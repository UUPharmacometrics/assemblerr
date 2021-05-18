test_that("2 cmp, fo absorption as ADVAN 4, TRANS4", {
  m <- model() +
    prm_log_normal("KA") +
    prm_log_normal("CL") +
    prm_log_normal("V2") +
    prm_log_normal("Q") +
    prm_log_normal("V3") +
    compartment("dose") +
    compartment("central", volume = ~V2) +
    compartment("peripheral", volume = ~V3) +
    flow(from = "dose", to = "central", definition = ~KA*A ) +
    flow(from = "central", definition = ~CL/V2*A) +
    flow(from = "central", to = "peripheral", definition = ~Q/V2*A) +
    flow(from = "peripheral", to = "central", definition = ~Q/V3*A) +
    obs_additive(conc~C["central"])

  render(m) %>%
    expect_contains("$SUBROUTINES ADVAN4  TRANS4") %>%
    expect_contains("CONC = A(2)/V2") %>%
    expect_contains("Y = CONC + EPS(1)")
})


test_that("2 cmp, fo absorption, with parentheses as ADVAN 4, TRANS1", {
  m <- model() +
    prm_log_normal("KA") +
    prm_log_normal("CL") +
    prm_log_normal("V2") +
    prm_log_normal("Q") +
    prm_log_normal("V3") +
    compartment("dose") +
    compartment("central", volume = ~V2) +
    compartment("peripheral", volume = ~V3) +
    flow(from = "dose", to = "central", definition = ~KA*A )+
    flow(from = "central", definition = ~(CL/V2)*A) +
    flow(from = "central", to = "peripheral", definition = ~(Q/V2)*A) +
    flow(from = "peripheral", to = "central", definition = ~(Q/V3)*A) +
    obs_additive(conc~C["central"])

  expect_warning(render(m)) %>%
    expect_contains("$SUBROUTINES ADVAN4  TRANS1") %>%
    expect_contains("K12 = (Q/V2)") %>%
    expect_contains("K = (CL/V2)") %>%
    expect_contains("K21 = (Q/V3)") %>%
    expect_contains("CONC = A(2)/V2") %>%
    expect_contains("Y = CONC + EPS(1)")
})

test_that("2 cmp, fo absorption, micro constants as ADVAN 4, TRANS1", {
  m <- model() +
    prm_log_normal("KA") +
    prm_log_normal("K") +
    prm_log_normal("K23") +
    prm_log_normal("K32") +
    prm_log_normal("V") +
    compartment("dose") +
    compartment("central", volume = ~V) +
    compartment("peripheral") +
    flow(from = "dose", to = "central", definition = ~KA*A ) +
    flow(from = "central", definition = ~K*A) +
    flow(from = "central", to = "peripheral", definition = ~K23*A) +
    flow(from = "peripheral", to = "central", definition = ~K32*A) +
    obs_additive(conc~C["central"])

  expect_warning(render(m)) %>%
    expect_contains("$SUBROUTINES ADVAN4  TRANS1") %>%
    expect_contains("CONC = A(2)/V") %>%
    expect_contains("Y = CONC + EPS(1)")
})
