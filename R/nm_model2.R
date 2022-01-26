# $PROBLEM ------------------------------------------------------------------
NmProblemRecord <- setClass(
  "NmProblemRecord",
  contains = "Component",
  slots = c(text = "character")
)

nm_problem <- function(text) {
  NmProblemRecord(
    text = text
  )
}

# $INPUT ------------------------------------------------------------------

NmDataInputLabel <- setClass(
  "NmDataInputLabel",
  contains = "NamedComponent",
  slots = c(
    drop = "logical",
    new_name = "character"
  ),
  prototype = prototype(drop = FALSE, new_name = character())
)

nm_input_label <- function(name, drop = FALSE, new_name = character()) {
  NmDataInputLabel(
    name = name,
    drop = drop,
    new_name = new_name
  )
}

NmInputRecord <- setClass(
  "NmInputRecord",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmDataInputLabel")
)

nm_input <- function() {
  NmInputRecord()
}


# $DATA ------------------------------------------------------------------
NmIgnoreStatement <- setClass(
  "NmIgnoreStatement",
  contains = "Component",
  slots = c(variable = "character", operator = "character", value = "character")
)

nm_ignore_statement <- function(variable, operator, value) {
  NmIgnoreStatement(
    variable = variable,
    operator = operator,
    value = value
  )
}

NmDataRecord <- setClass(
  "NmDataRecord",
  contains = "ComponentList",
  prototype = prototype(component_class = "NmIgnoreStatement"),
  slots = c(
    filename = "character",
    ignore_character = "character"
  )
)

nm_data_record <- function(filename, ignore_character = "@") {
  NmDataRecord(
    filename = filename,
    ignore_character = ignore_character
  )
}

# $SUBROUTINES -----------------------------------------------------------

NmSubroutine <- setClass(
  "NmSubroutine",
  contains = "NamedComponent"
)

nm_subroutine <- function(name) {
  NmSubroutine(
    name = name
  )
}

NmSubroutinesRecord <- setClass(
  "NmSubroutinesRecord",
  contains = "NamedComponentList",
  slots = c(
    tol = "integer",
    atol = "integer",
    sstol = "integer",
    ssatol = "integer"
  ),
  prototype = prototype(component_class = "NmSubroutine")
)

nm_subroutine_record <- function(tol = integer(), atol = integer(), sstol = integer(), ssatol = integer()) {
  NmSubroutinesRecord(
    tol = tol,
    atol = atol,
    sstol = sstol,
    ssatol = ssatol
  )
}



# $MODEL -----------------------------------------------------------------

NmCompartment2 <- setClass(
  "NmCompartment2",
  contains = "NamedComponent",
  slots = c(
    initial_off = "logical",
    no_dose = "logical",
    no_off = "logical",
    equilibrium = "logical",
    exclude = "logical",
    default_observation = "logical",
    default_dose = "logical"
  ),
  prototype = c(initial_off = FALSE)
)

nm_compartment <- function(name, initial_off = FALSE) {
  NmCompartment2(
    name = name,
    initial_off = initial_off
  )
}

NmModelRecord <- setClass(
  "NmModelRecord",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmCompartment2")
)

nm_model_record <- function() {
  NmModelRecord()
}


# abbriviated code --------------------------------------------------------
NmAbbrivatedCodeBlock <- setClass(
  "NmAbbrivatedCodeBlock",
  slots = c(
    statements = "assemblerr_statement"
  ),
  contains = "Component"
)

NmAbbrivatedCodeRecord <- setClass(
  "NmAbbrivatedCodeRecord",
  contains = "ComponentList",
  prototype = prototype(component_class = "NmAbbrivatedCodeBlock")
)


# $PK ---------------------------------------------------------------------

NmPkCode <- setClass(
  "NmPkCode",
  contains = "NmAbbrivatedCodeBlock"
)

nm_pk_code <- function(...) {
  dots <- rlang::list2(...)
  NmPkCode(
    statements = statement(!!!dots)
  )
}

NmPkRecord <- setClass(
  "NmPkRecord",
  contains = "NmAbbrivatedCodeRecord",
  prototype = prototype(component_class = "NmPkCode")
)

# $DES ---------------------------------------------------------------------

NmDesCode <- setClass(
  "NmDesCode",
  contains = "NmAbbrivatedCodeBlock"
)


nm_des_code <- function(...) {
  dots <- rlang::list2(...)
  NmDesCode(
    statements = statement(!!!dots)
  )
}

NmDesRecord <- setClass(
  "NmDesRecord",
  contains = "NmAbbrivatedCodeRecord",
  prototype = prototype(component_class = "NmDesCode")
)


# $ERROR ------------------------------------------------------------------



NmErrorCode <- setClass(
  "NmErrorCode",
  contains = "NmAbbrivatedCodeBlock"
)


nm_error_code <- function(...) {
  dots <- rlang::list2(...)
  NmErrorCode(
    statements = statement(!!!dots)
  )
}

NmErrorRecord <- setClass(
  "NmErrorRecord",
  contains = "NmAbbrivatedCodeRecord",
  prototype = prototype(component_class = "NmErrorCode")
)


# $ESTIMATION -------------------------------------------------------------


NmEstimationRecord2 <- setClass(
  "NmEstimationRecord2",
  contains = "Component",
  slots = c(
    method = "character",
    maxeval = "integer",
    interaction = "logical",
    auto = "logical",
    additional_options = "list"
  )
)

nm_estimation_record <- function(method = "cond", interaction = logical(), maxeval = integer(), auto = logical()){
  NmEstimationRecord2(
    method = method,
    interaction = interaction,
    maxeval = maxeval,
    auto = NA
  )
}

NmEstimationRecordList <- setClass(
  "NmEstimationRecordList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NmEstimationRecord2")
)


# $COVARIANCE -------------------------------------------------------------


NmCovarianceRecord <- setClass(
  "NmCovarianceRecord",
  contains = "Component",
  slots = c(
    active = "logical",
    matrix = "character",
    print = "character"
  ),
  prototype = prototype(active = FALSE)
)

nm_covariance_record <- function(matrix = character(), print = character()) {
  NmCovarianceRecord(
    active = TRUE,
    matrix = matrix,
    print = print
  )
}

# $TABLE -------------------------------------------------------------

NmTableEntry <- setClass(
  "NmTableEntry",
  contains = "NamedComponent"
)

NmTableRecord2 <- setClass(
  "NmTableRecord2",
  contains = "NamedComponentList",
  slots = c(
    filename = "character",
    print = "logical",
    append = "logical",
    oneheader = "logical",
    additional_options = "list"
  ),
  prototype = prototype(
    component_class = "NmTableEntry",
    print = FALSE,
    append = FALSE,
    oneheader = FALSE
  )
)

nm_table_record <- function(filename, entries = list()) {
  tab <- NmTableRecord2(
    filename = filename
  )
  if (length(entries) > 0) {
    for (e in entries) {
      tab <- add_component(tab, NmTableEntry(name = e))
    }
  }
  return(tab)
}

NmTableRecordList <- setClass(
  "NmTableRecordList",
  contains = "ComponentList",
  prototype = prototype(component_class = "NmTableRecord2")
)


# $THETA ------------------------------------------------------------------

NmThetaRecord <- setClass(
  "NmThetaRecord",
  contains = "NamedComponent",
  slots = c(
    c(initial = "numeric", lbound = "numeric", ubound = "numeric")
  ),
  prototype = prototype(lbound = -Inf, ubound = Inf)
)

nm_theta_record <- function(name, initial = 0.1, lbound = -Inf, ubound = Inf) {
  NmThetaRecord(
    name = name,
    initial = initial,
    lbound = lbound,
    ubound = ubound
  )
}

NmThetaRecordList <- setClass(
  "NmThetaRecordList",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmThetaRecord")
)

# $OMEGA ------------------------------------------------------------------

NmOmegaRecord <- setClass(
  "NmOmegaRecord",
  contains = "NamedComponent",
  slots = c(
    c(initial = "numeric")
  )
)

nm_omega_record <- function(name, initial) {
  NmOmegaRecord(
    name = name,
    initial = initial
  )
}

NmOmegaRecordList <- setClass(
  "NmOmegaRecordList",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmOmegaRecord")
)


# $SIGMA ------------------------------------------------------------------

NmSigmaRecord <- setClass(
  "NmSigmaRecord",
  contains = "NamedComponent",
  slots = c(
    c(initial = "numeric")
  )
)

nm_sigma_record <- function(name, initial) {
  NmSigmaRecord(
    name = name,
    initial = initial
  )
}

NmSigmaRecordList <- setClass(
  "NmSigmaRecordList",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmSigmaRecord")
)

# Model ------------------------------------------------------------------
NmModel2 <- setClass(
  "NmModel2",
  contains = "Component",
  slots = c(
    problem = "NmProblemRecord",
    input = "NmInputRecord",
    data = "NmDataRecord",
    subroutines = "NmSubroutinesRecord",
    model = "NmModelRecord",
    pk = "NmPkRecord",
    des = "NmDesRecord",
    error = "NmErrorRecord",
    estimation = "NmEstimationRecordList",
    covariance = "NmCovarianceRecord",
    tables = "NmTableRecordList",
    thetas = "NmThetaRecordList",
    omegas = "NmOmegaRecordList",
    sigmas = "NmSigmaRecordList"
  )
)

