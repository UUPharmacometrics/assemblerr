advan_definitions <- list(
  advan1 = list(
    adjacency_matrix = matrix(
      data =
        c(0, 1,
          0, 0),
      nrow = 2,
      ncol = 2,
      byrow = TRUE,
      dimnames = list(c("central", "output"), c("central", "output"))
    ),
    parameterizations = list(
      trans1 = function(k_central_output){
        dcl_id(k_central_output) <- quote(k)
        k_central_output
      },
      trans2 = function(k_central_output){
        v <- dcl_collect_denominators(k_central_output)
        cl <- dcl_discard_denominators(k_central_output)
        dcl_id(v) <- quote(v)
        dcl_id(cl) <- quote(cl)
        vec_c(
          cl, v
        )
      }
    )
  ),
  advan2 = list(
    adjacency_matrix = matrix(
      data =
        c(0, 1, 0,
          0, 0, 1,
          0, 0, 0),
      nrow = 3,
      ncol = 3,
      byrow = TRUE,
      dimnames = list(c("depot", "central", "output"), c("depot", "central", "output"))
    ),
    parameterizations = list(
      trans1 = function(k_depot_central, k_central_output){
        dcl_id(k_depot_central) <- quote(ka)
        dcl_id(k_central_output) <- quote(k)
        vec_c(
          k_depot_central, k_central_output
        )
      },
      trans2 = function(k_depot_central, k_central_output){
        ka <- k_depot_central
        v <- dcl_collect_denominators(k_central_output)
        cl <- dcl_discard_denominators(k_central_output)
        dcl_id(ka) <- quote(ka)
        dcl_id(v) <- quote(v)
        dcl_id(cl) <- quote(cl)
        vec_c(
          ka, cl, v
        )
      }
    )
  ),
  advan3 = list(
    adjacency_matrix = matrix(
      data =
        c(0, 1, 1,
          1, 0, 0,
          0, 0, 0),
      nrow = 3,
      ncol = 3,
      byrow = TRUE,
      dimnames = list(c("central", "peripheral", "output"), c("central", "peripheral", "output"))
    ),
  parameterizations = list(
    trans1 = function(k_central_peripheral, k_central_output, k_peripheral_central){
      dcl_id(k_central_peripheral) <- quote(k12)
      dcl_id(k_central_output) <- quote(k)
      dcl_id(k_peripheral_central) <- quote(k21)
      vec_c(
        k_central_peripheral,
        k_central_output,
        k_peripheral_central
      )
    },
    trans4 = function(k_central_peripheral, k_central_output, k_peripheral_central){
      v1 <- dcl_collect_denominators(k_central_output)
      cl <- dcl_discard_denominators(k_central_output)
      q <- dcl_discard_denominators(k_peripheral_central)
      v2 <- dcl_collect_denominators(k_peripheral_central)
      dcl_id(v1) <- quote(v1)
      dcl_id(cl) <- quote(cl)
      dcl_id(v2) <- quote(v2)
      dcl_id(q) <- quote(q)
      vec_c(
        cl, v1, q, v2
      )
    }
  )
),
advan4 = list(
  adjacency_matrix = matrix(
    data =
      c(0, 1, 0, 0,
        0, 0, 1, 1,
        0, 1, 0, 0,
        0, 0, 0, 0),
    nrow = 4,
    ncol = 4,
    byrow = TRUE,
    dimnames = list(c("depot", "central", "peripheral", "output"), c("depot", "central", "peripheral", "output"))
  ),
  parameterizations = list(
    trans1 = function(k_depot_central, k_central_peripheral, k_central_output, k_peripheral_central){
      dcl_id(k_depot_central) <- quote(ka)
      dcl_id(k_central_peripheral) <- quote(k12)
      dcl_id(k_central_output) <- quote(k)
      dcl_id(k_peripheral_central) <- quote(k21)
      vec_c(
        k_depot_central,
        k_central_peripheral,
        k_central_output,
        k_peripheral_central
      )
    },
    trans4 = function(k_depot_central, k_central_peripheral, k_central_output, k_peripheral_central){
      ka <- k_depot_central
      v2 <- dcl_collect_denominators(k_central_output)
      cl <- dcl_discard_denominators(k_central_output)
      q <- dcl_discard_denominators(k_peripheral_central)
      v3 <- dcl_collect_denominators(k_peripheral_central)
      dcl_id(ka) <- quote(ka)
      dcl_id(v2) <- quote(v2)
      dcl_id(cl) <- quote(cl)
      dcl_id(v3) <- quote(v3)
      dcl_id(q) <- quote(q)
      vec_c(
        ka, cl, v2, q, v3
      )
    }
  )
),
advan11 = list(
  adjacency_matrix = matrix(
    data =
      c(0, 1, 1, 1,
        1, 0, 0, 0,
        1, 0, 0, 0,
        0, 0, 0, 0),
    nrow = 4,
    ncol = 4,
    byrow = TRUE,
    dimnames = list(c("central", "peripheral1", "peripheral2", "output"), c("central", "peripheral1", "peripheral2", "output"))
  ),
  parameterizations = list(
    trans1 = function(k_central_peripheral1,
                      k_central_peripheral2,
                      k_peripheral1_central,
                      k_peripheral2_central,
                      k_central_output){
      dcl_id(k_central_peripheral1) <- quote(k12)
      dcl_id(k_central_peripheral2) <- quote(k13)
      dcl_id(k_peripheral1_central) <- quote(k21)
      dcl_id(k_peripheral2_central) <- quote(k31)
      dcl_id(k_central_output) <- quote(k)
      vec_c(
        k_central_peripheral1,
        k_central_peripheral2,
        k_peripheral1_central,
        k_peripheral2_central,
        k_central_output
      )
    },
    trans4 = function(k_central_peripheral1,
                      k_central_peripheral2,
                      k_peripheral1_central,
                      k_peripheral2_central,
                      k_central_output){
      v1 <- dcl_collect_denominators(k_central_output)
      cl <- dcl_discard_denominators(k_central_output)
      q1 <- dcl_discard_denominators(k_peripheral1_central)
      q2 <- dcl_discard_denominators(k_peripheral2_central)
      v2 <- dcl_collect_denominators(k_peripheral1_central)
      v3 <- dcl_collect_denominators(k_peripheral2_central)

      dcl_id(v1) <- quote(v1)
      dcl_id(cl) <- quote(cl)
      dcl_id(v2) <- quote(v2)
      dcl_id(q1) <- quote(q1)
      dcl_id(v3) <- quote(v3)
      dcl_id(q2) <- quote(q2)

      vec_c(
        v1, cl, v2, q1, v3, q2
      )
    }
  )
),
advan12 = list(
  adjacency_matrix = matrix(
    data =
      c(0, 1, 0, 0, 0,
        0, 0, 1, 1, 1,
        0, 1, 0, 0, 0,
        0, 1, 0, 0, 0,
        0, 0, 0, 0, 0),
    nrow = 5,
    ncol = 5,
    byrow = TRUE,
    dimnames = list(c("depot", "central", "peripheral1", "peripheral2", "output"),
                    c("depot", "central", "peripheral1", "peripheral2", "output"))
  ),
  parameterizations = list(
    trans1 = function(k_depot_central,
                      k_central_peripheral1,
                      k_central_peripheral2,
                      k_peripheral1_central,
                      k_peripheral2_central,
                      k_central_output){
      dcl_id(k_depot_central) <- quote(ka)
      dcl_id(k_central_peripheral1) <- quote(k12)
      dcl_id(k_central_peripheral2) <- quote(k13)
      dcl_id(k_peripheral1_central) <- quote(k21)
      dcl_id(k_peripheral2_central) <- quote(k31)
      dcl_id(k_central_output) <- quote(k)
      vec_c(
        k_depot_central,
        k_central_peripheral1,
        k_central_peripheral2,
        k_peripheral1_central,
        k_peripheral2_central,
        k_central_output
      )
    },
    trans4 = function(k_depot_central,
                      k_central_peripheral1,
                      k_central_peripheral2,
                      k_peripheral1_central,
                      k_peripheral2_central,
                      k_central_output){
      ka <- k_depot_central
      v2 <- dcl_collect_denominators(k_central_output)
      cl <- dcl_discard_denominators(k_central_output)
      q1 <- dcl_discard_denominators(k_peripheral1_central)
      q2 <- dcl_discard_denominators(k_peripheral2_central)
      v3 <- dcl_collect_denominators(k_peripheral1_central)
      v4 <- dcl_collect_denominators(k_peripheral2_central)

      dcl_id(ka) <- quote(ka)
      dcl_id(v2) <- quote(v2)
      dcl_id(cl) <- quote(cl)
      dcl_id(v3) <- quote(v3)
      dcl_id(q1) <- quote(q1)
      dcl_id(v4) <- quote(v4)
      dcl_id(q2) <- quote(q2)

      vec_c(
        ka, v2, cl, v3, q1, v4, q2
      )
    }
  )
)
)


