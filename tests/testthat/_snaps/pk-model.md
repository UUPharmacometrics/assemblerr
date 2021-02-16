# check empty pk_model

    Code
      check(pk_model())
    Message <cliMessage>
      WARNING! 4 issues
        1. No parameters specified
        2. A distribution component is missing
        3. An elimination component is missing
        4. No observation specified

# check complete pk_model

    Code
      check(pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        obs_additive(conc ~ C["central"]))
    Message <cliMessage>
      SUCCESS! No issues

