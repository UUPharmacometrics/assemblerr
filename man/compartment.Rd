% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compartment.R
\name{compartment}
\alias{compartment}
\alias{cmp}
\title{Compartment}
\usage{
compartment(name, volume = 1)

cmp(name, volume = 1)
}
\arguments{
\item{name}{Name of the compartment}

\item{volume}{Volume as a number, formula or parameter name}
}
\value{
A building block of type 'compartment'
}
\description{
Defines name and volume of a compartment.
}
\details{
In most applications, compartments contain kinetically homogeneous amount of drug (applications where the compartment
content represents other quantities are also possible). In assemblerr, a compartment is defined by providing a
a name and the compartment volume.
\subsection{Compartment names}{

Every compartment must have a valid name. A compartment name can contain letters, numbers as well as the underscore character, and
needs to start with a letter.  Adding a compartment with an already existing name will replace the definition of the compartment.
}

\subsection{Compartment volumes}{

The compartment volume can be provided as a number, R formula, or a parameter name. It will be used by assemblerr to replace
references to the compartment concentration (e.g., \code{~C["central"]}) with the corresponding amount divided by volume (e.g., \verb{~A["central]/vc}).
}
}
\examples{
# model with depot and central compartment
m <- model() +
 compartment("depot", volume = 1) +
 compartment("central", volume = "vc") +
 flow(~ka*A, from = "depot", to = "central") +
 flow(~cl*C, from = "central") +
 prm_log_normal("ka") +
 prm_log_normal("cl") +
 prm_log_normal("vc") +
 obs_additive(conc~C["central"])

render(
  model = m,
  options = assemblerr_options(
    ode.use_special_advans = FALSE,
    ode.use_general_linear_advans = FALSE
   )
)
}
\seealso{
\link{flow} for how to describe compartment kinetics
}
