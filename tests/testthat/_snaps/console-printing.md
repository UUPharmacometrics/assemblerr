# prm_log_normal

    Code
      prm_log_normal("test")
    Output
      # an assemblerr building block 
      test: log-normal

# prm_logit_normal

    Code
      prm_logit_normal("test")
    Output
      # an assemblerr building block 
      test: logit-normal

# prm_no_var

    Code
      prm_no_var("test")
    Output
      # an assemblerr building block 
      test: no variability

# prm_normal

    Code
      prm_normal("test")
    Output
      # an assemblerr building block 
      test: normal

# obs_additive

    Code
      obs_additive(~test)
    Output
      # an assemblerr building block 
      test: `. ~ test`

# obs_combined

    Code
      obs_combined(~test)
    Output
      # an assemblerr building block 
      test: `. ~ test`

# obs_proportional

    Code
      obs_proportional(~test)
    Output
      # an assemblerr building block 
      test: `. ~ test`

# compartment, flow

    Code
      flow(~k, "central")
    Output
      # an assemblerr building block 
      central><out>: `. ~ k`

---

    Code
      flow(~k, "depot", "central")
    Output
      # an assemblerr building block 
      depot>central: `. ~ k`

---

    Code
      compartment("central")
    Output
      # an assemblerr building block 
      central: `. ~ 1`

# algebraics

    Code
      algebraic(k ~ cl / v)
    Output
      # an assemblerr building block 
      `k ~ cl/v`

# empty model

    Code
      model()
    Output
      # an assemblerr model 
        parameters: none
        algebraics: none
        compartments: none
        flows: none
        observations: none
      # ...2 more facets 
      ! 2 critical issues 

# prm_log_normal + obs_additive

    Code
      model() + prm_log_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_additive(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# prm_logit_normal + obs_additive

    Code
      model() + prm_logit_normal("k") + compartment("central") + flow(~k * C,
      "central") + obs_additive(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# prm_no_var + obs_additive

    Code
      model() + prm_no_var("k") + compartment("central") + flow(~k * C, "central") +
        obs_additive(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# prm_normal + obs_additive

    Code
      model() + prm_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_additive(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# prm_log_normal + obs_combined

    Code
      model() + prm_log_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_combined(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (combined)
      # ...2 more facets 

# prm_logit_normal + obs_combined

    Code
      model() + prm_logit_normal("k") + compartment("central") + flow(~k * C,
      "central") + obs_combined(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (combined)
      # ...2 more facets 

# prm_no_var + obs_combined

    Code
      model() + prm_no_var("k") + compartment("central") + flow(~k * C, "central") +
        obs_combined(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (combined)
      # ...2 more facets 

# prm_normal + obs_combined

    Code
      model() + prm_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_combined(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (combined)
      # ...2 more facets 

# prm_log_normal + obs_proportional

    Code
      model() + prm_log_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_proportional(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (proportional)
      # ...2 more facets 

# prm_logit_normal + obs_proportional

    Code
      model() + prm_logit_normal("k") + compartment("central") + flow(~k * C,
      "central") + obs_proportional(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (proportional)
      # ...2 more facets 

# prm_no_var + obs_proportional

    Code
      model() + prm_no_var("k") + compartment("central") + flow(~k * C, "central") +
        obs_proportional(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (proportional)
      # ...2 more facets 

# prm_normal + obs_proportional

    Code
      model() + prm_normal("k") + compartment("central") + flow(~k * C, "central") +
        obs_proportional(~C["central"])
    Output
      # an assemblerr model 
        parameters: k
        algebraics: none
        compartments: central
        flows: central><out>
        observations: `. ~ C["central"]` (proportional)
      # ...2 more facets 

# empty pk_model

    Code
      pk_model()
    Output
      # an assemblerr pk_model 
        parameters: none
        algebraics: none
        pk components: none
        observations: none
      # ...2 more facets 
      ! 4 critical issues 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, and mat
        algebraics: none
        pk components: 1 cmp, linear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, and mat
        algebraics: none
        pk components: 2 cmp, linear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, and mat
        algebraics: none
        pk components: 3 cmp, linear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, and mat
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, and mat
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, and mat
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, and mat
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, and mat
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, and mat
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and FO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, mdt, and mat
        algebraics: none
        pk components: 1 cmp, linear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, mdt, and mat
        algebraics: none
        pk components: 2 cmp, linear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, mdt, and mat
        algebraics: none
        pk components: 3 cmp, linear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, mdt, and mat
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, mdt, and mat
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() +
        pk_absorption_fo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, mdt, and mat
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and FO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, mdt, and mat
        algebraics: none
        pk components: 1 cmp, linear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, mdt, and mat
        algebraics: none
        pk components: 2 cmp, linear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, mdt, and mat
        algebraics: none
        pk components: 3 cmp, linear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, mdt, and mat
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, mdt, and mat
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, mdt, and mat
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_transit

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() +
        pk_absorption_fo_transit() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, mdt, and mat
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and FO abs. transit-cmps(1)
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, mat, and mdt
        algebraics: none
        pk components: 1 cmp, linear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, mat, and mdt
        algebraics: none
        pk components: 2 cmp, linear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, mat, and mdt
        algebraics: none
        pk components: 3 cmp, linear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_fo_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_fo_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, mat, and mdt
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_fo_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, mat, and mdt
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_fo_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_fo_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, mat, and mdt
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and FO abs. ZO delay
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, and mat
        algebraics: none
        pk components: 1 cmp, linear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, and mat
        algebraics: none
        pk components: 2 cmp, linear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, and mat
        algebraics: none
        pk components: 3 cmp, linear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, and mat
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, and mat
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, and mat
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() + pk_absorption_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, and mat
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() + pk_absorption_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, and mat
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_zo

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() + pk_absorption_zo() +
        obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, and mat
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and ZO abs.
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cl, mat, and mdt
        algebraics: none
        pk components: 1 cmp, linear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cl, mat, and mdt
        algebraics: none
        pk components: 2 cmp, linear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cl, mat, and mdt
        algebraics: none
        pk components: 3 cmp, linear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_linear_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 1 cmp, lin. + nonlin. elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_linear_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 2 cmp, lin. + nonlin. elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_linear_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_linear_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, cllin, vmax, km, mat, and mdt
        algebraics: none
        pk components: 3 cmp, lin. + nonlin. elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_1cmp + pk_elimination_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_1cmp() + pk_elimination_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, clmm, km, mat, and mdt
        algebraics: none
        pk components: 1 cmp, nonlinear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_2cmp + pk_elimination_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_2cmp() + pk_elimination_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp, q, clmm, km, mat, and mdt
        algebraics: none
        pk components: 2 cmp, nonlinear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

# pk_distribution_3cmp + pk_elimination_nl + pk_absorption_zo_lag

    Code
      pk_model() + pk_distribution_3cmp() + pk_elimination_nl() +
        pk_absorption_zo_lag() + obs_additive(~C["central"])
    Output
      # an assemblerr pk_model 
        parameters: vc, vp1, vp2, q1, q2, clmm, km, mat, and mdt
        algebraics: none
        pk components: 3 cmp, nonlinear elim., and ZO abs. lag-time
        observations: `. ~ C["central"]` (additive)
      # ...2 more facets 

