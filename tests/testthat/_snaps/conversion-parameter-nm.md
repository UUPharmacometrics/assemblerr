# prm_log_normal

    {
      "type": "character",
      "attributes": {},
      "value": ["test <- theta[1] * exp(eta[1])"]
    }

# prm_normal

    {
      "type": "character",
      "attributes": {},
      "value": ["test <- theta[1] + eta[1]"]
    }

# prm_logit_normal

    {
      "type": "character",
      "attributes": {},
      "value": ["logit_test <- log(theta[1]/(1 - theta[1])) + eta[1]", "test <- exp(logit_test)/(1 + exp(logit_test))"]
    }

# prm_no_var

    {
      "type": "character",
      "attributes": {},
      "value": ["test <- theta[1]"]
    }

