.templates <- list()

.templates[["NmModel2"]] <-
"{render_component2(problem)}
{render_component2(input) %>% wrap()}
{render_component2(data) %>% wrap()}
{render_component2(subroutines) %>% wrap()}
{render_component2(model) %>% wrap()}
{render_component2(pk)}
{render_component2(des)}"

.templates[["NmProblemRecord"]] <- "$PROBLEM {text}"

.templates[["NmInputRecord"]] <- "$INPUT {render_collection(elements)}"

.templates[["NmDataInputLabel"]] <- function(drop, ...) {
  if (drop) "{name}=DROP"
  else "{name}"
}

.templates[["NmDataRecord"]] <- "$DATA {filename} IGNORE={ignore_character} {render_collection(elements)}"

.templates[["NmIgnoreStatement"]] <- "IGNORE=({variable}{operator}{value})"

.templates[["NmSubroutinesRecord"]] <- "$SUBROUTINES {render_collection(elements)} {p('TOL=',tol)} {p('SSTOL=',sstol)} {p('ATOL=',atol)} {p('SSATOL=',ssatol)}"

.templates[["NmSubroutine"]] <- "{name}"

.templates[["NmModelRecord"]] <- "$MODEL {render_collection(elements)}"

.templates[["NmCompartment2"]] <- "COMPARTMENT=({name}{ie(initial_off,' INITIALOFF')}{ie(no_off,' INITIALOFF')}{ie(no_dose,' NODOSE')}{ie(equilibrium,' EQUILIBRIUM')}{ie(default_observation,' DEFOBSERVATION')}{ie(default_dose,' DEFDOSE')})"

.templates[["NmPkRecord"]] <- "$PK
{render_collection(elements, collapse = NULL)}"

.templates[["NmDesRecord"]] <- "$DES
{render_collection(elements, collapse = NULL)}"


.templates[["NmAbbrivatedCodeBlock"]] <- "{indent(render_component(statements, collapse = NULL))}"

