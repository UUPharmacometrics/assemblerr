# $PROBLEM ------------------------------------------------------------------
NmProblemRecord <- setClass(
  "NmProblemRecord",
  contains = "Component",
  slots = c(text = "character")
)

# $INPUT ------------------------------------------------------------------

NmDataInputLabel <- setClass(
  "NmDataInputLabel",
  contains = "NamedComponent",
  slots = c(
    drop = "logical",
    new_new = "character"
  ),
  prototype = prototype(drop = FALSE, new_name = character())
)

NmInputRecord <- setClass(
  "NmInputRecord",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmDataInputLabel")
)

# $DATA ------------------------------------------------------------------
NmIgnoreStatement <- setClass(
  "NmIgnoreStatement",
  contains = "Component",
  slots = c(variable = "character", operator = "character", value = "character")
)

NmDataRecord <- setClass(
  "NmDataRecord",
  contains = "ComponentList",
  prototype = prototype(component_class = "NmIgnoreStatement"),
  slots = c(
    filename = "character",
    ignore_character = "character"
  )
)

# $SUBROUTINES -----------------------------------------------------------

NmSubroutine <- setClass(
  "NmSubroutine",
  contains = "NamedComponent"
)

NmSubroutinesRecord <- setClass(
  "NmSubroutinesRecord",
  contains = "NamedComponentList",
  slots = c(
    name = "character",
    tol = "integer",
    atol = "integer",
    sstol = "integer",
    ssatol = "integer"
  ),
  prototype = prototype(component_class = "NmSubroutine")
)



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

NmModelRecord <- setClass(
  "NmModelRecord",
  contains = "NamedComponentList",
  prototype = prototype(component_class = "NmCompartment2")
)



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
  contains = "ComponentList"
)


# $PK ---------------------------------------------------------------------

NmPkCode <- setClass(
  "NmPkCode",
  contains = "NmAbbrivatedCodeBlock"
)

NmPkRecord <- setClass(
  "NmPkRecord",
  contains = "NmAbbrivatedCodeRecord"
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
    pk = "NmPkRecord"
  )
)

