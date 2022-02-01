create_nm_test_model <- function() {
  NmModel2() +
      nm_problem("test model") +
      nm_input_label(name = "ID") +
      nm_input_label(name = "TIME") +
      nm_input_label(name = "DV") +
      nm_input_label(name = "STUDY") +
      nm_data_record(filename = "test/path.csv", ignore_character = "@") +
      nm_subroutines_record(subroutines = "ADVAN1", tol = 3L) +
      nm_compartment2(name = "CENTRAL") +
      nm_compartment2(name = "DEPOT") +
      nm_pk_code(statement("cl <- theta[1]*exp(eta[1])")) +
      nm_pk_code(statement("v <- theta[2]*exp(eta[2])")) +
      nm_pk_code(statement("k <- cl/v", "conc <- d/v*exp(-k*time)")) +
      nm_des_code(statement("dadt[1] <- k*A[1]")) +
      nm_error_code(statement("ipred <- A[1]")) +
      nm_estimation_record(method = "FOCE", maxeval = 99999L, interaction = TRUE) +
      nm_estimation_record(method = "IMP") +
      nm_covariance_record() +
      nm_table_record("sdtab", entries = c("PRED", "IPRED", "DV", "CWRES", "WRES", "ETA(1)", "ETA(2)")) +
      nm_theta_record("cl", 10, 0) +
      nm_theta_record("v", 6) +
      nm_omega_record("cl", 0.09) +
      nm_sigma_record("ruv", 1)
}
