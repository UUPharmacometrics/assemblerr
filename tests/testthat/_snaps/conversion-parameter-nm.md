# prm_log_normal

    [1] "test <- theta[1] * exp(eta[1])"

# prm_normal

    [1] "test <- theta[1] + eta[1]"

# prm_logit_normal

    [1] "logit_test <- log(theta[1]/(1 - theta[1])) + eta[1]"
    [2] "test <- exp(logit_test)/(1 + exp(logit_test))"      

# prm_no_var

    [1] "test <- theta[1]"

