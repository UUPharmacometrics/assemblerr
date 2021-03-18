# assemblerr (development version)
* variables that are not defined in the model are automatically added as input variables 
* added warning when a new building block replaces an existing one

# assemblerr 0.0.0.9005 (development version)
* added `pk_elimination_linear_mm` for mixed linear-mm elimination
* models are now checked for issues before rendering
* improved console printing of building blocks and models

# assemblerr 0.0.0.9004
* added `check()` function to check and list model issues  
* improved error checking and reporting when creating building blocks, e.g., `prm_normal()`
* prediction declarations for observations do not longer need a left-hand side, e.g., `obs_additive(~C["central"])` is now valid.
* `pk_elimination_mm` has been replaced with `pk_elimination_nl` to clarify that the default parameterization is not the classical MM model. Additionally, `pk_elimination_nl` allows selecting the classical parameterization using `pk_elimination_nl(prm_vmax = prm_log_normal("vmax"), prm_clmm = NULL)`
* unsupported building blocks are now ignored with a warning when added to a model (e.g., `pk_model()+compartment('central')`) 
* option to include estimation & covariance step records in generated NONMEM model through `render(m, options = assemblerr_options(default_record.covariance_step = nm_covariance()))`
* Added a `NEWS.md` file to track changes to the package.
