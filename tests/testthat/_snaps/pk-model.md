# check empty pk_model

    Code
      check(pk_model())
    Message <cliMessage>
      WARNING! 4 issues
    Message <cliMessage>
        1. No parameters specified
    Message <cliMessage>
        2. A distribution component is missing
    Message <cliMessage>
        3. An elimination component is missing
    Message <cliMessage>
        4. No observation specified

# check complete pk_model

    Code
      check(pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        obs_additive(conc ~ C["central"]))
    Message <cliMessage>
      SUCCESS! No issues

