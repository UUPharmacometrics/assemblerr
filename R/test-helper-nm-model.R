create_nm_test_model <- function() {
  nm <- NmModel2() %>%
    add_component(NmDataInputLabel(name = "ID")) %>%
    add_component(NmDataInputLabel(name = "TIME")) %>%
    add_component(NmDataInputLabel(name = "DV")) %>%
    add_component(NmDataInputLabel(name = "STUDY")) %>%
    add_component(NmSubroutine(name = "ADVAN1")) %>%
    add_component(NmCompartment2(name = "CENTRAL")) %>%
    add_component(NmCompartment2(name = "DEPOT", initial_off = TRUE)) %>%
    add_component(NmPkCode(statements = statement("cl <- theta[1]*exp(eta[1])"))) %>%
    add_component(NmPkCode(statements = statement("v <- theta[2]*exp(eta[2])"))) %>%
    add_component(NmPkCode(statements = statement("k <- cl/v", "conc <- d/v*exp(-k*time)"))) %>%
    add_component(NmDesCode(statements = statement("dadt[1] <- k*A[1]")))



  nm@data@filename <- "test/path.csv"
  nm@data@ignore_character <- "@"
  nm <- nm %>%
    add_component(NmIgnoreStatement(variable = "STUDY", operator = ".AND.", value = "1"))

  nm@subroutines@tol <- 3L

  nm
}
