# ensure consisten output on CI system
app <- cli::start_app(theme = list(
  ".alert-warning" = list(before = "WARNING! "),
  ".alert-success" = list(before = "SUCCESS! ")
  ), .auto_close = FALSE)


withr::defer(cli::stop_app(app), teardown_env())
