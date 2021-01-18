# assemblerr (development version)

* unsupported building blocks are now ignored with a warning when added to a model (e.g., `pk_model()+compartment('central')`) 
* option to include estimation & covariance step records in generated NONMEM model through `render(m, options = assemblerr_options(default_record.covariance_step = nm_covariance()))`
* Added a `NEWS.md` file to track changes to the package.
