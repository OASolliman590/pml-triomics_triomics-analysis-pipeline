d_ora_cli_run <- function(config_path, stage, output_root) {
  cli <- triomics_test_resource("exec", "triomics")
  previous_wd <- setwd(dirname(cli))
  on.exit(setwd(previous_wd), add = TRUE)
  output <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(basename(cli)), "--config", shQuote(config_path),
      "--stage", shQuote(stage), "--output", shQuote(output_root)),
    stdout = TRUE, stderr = TRUE
  )
  exit_code <- attr(output, "status")
  if (is.null(exit_code)) exit_code <- 0L
  list(exit_code = as.integer(exit_code), output = output)
}

testthat::test_that("D-T04 executes declared-universe ORA after native B and C", {
  config <- triomics_test_resource("examples", "tickets", "D-T04.yaml")
  output_root <- tempfile("d-ora-native-output-", tmpdir = "/private/tmp")
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(output_root, recursive = TRUE, force = TRUE), add = TRUE)

  b_run <- d_ora_cli_run(config, "B", output_root)
  testthat::expect_identical(b_run$exit_code, 0L, info = paste(b_run$output, collapse = "\n"))
  if (b_run$exit_code != 0L) return(invisible(NULL))
  c_run <- d_ora_cli_run(config, "C", output_root)
  testthat::expect_identical(c_run$exit_code, 0L, info = paste(c_run$output, collapse = "\n"))
  if (c_run$exit_code != 0L) return(invisible(NULL))
  d_run <- d_ora_cli_run(config, "D", output_root)

  testthat::expect_identical(d_run$exit_code, 0L, info = paste(d_run$output, collapse = "\n"))
})
