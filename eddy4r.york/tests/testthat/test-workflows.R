testScenarios = create_test_scenarios()

# Run test scenarios
for(i in 1:length(testScenarios)){

  scenarioName = names(testScenarios)[[i]]

  # If the result tryCatch while running the workflow is NULL, there where no unhandled errors
  test_that(paste0("Scenario ", scenarioName, " runs without unhandled error"),{
    expect_no_error(eddy4R.york::wrap.towr(testScenarios[[i]]))
  })

  # If the workflow catches an error, it should record it in the logfile.
  # If there are no caught errors, there should be no lines with the word error in.
  test_that(paste0("Scenario ", scenarioName, " runs without caught error"),{
    logfile = readLines(file.path(tempdir(), scenarioName, "wrap_tower_logfile.txt" ))

    logfile_errors = logfile[stringr::str_detect(logfile, "error")]

    expect_equal(length(logfile_errors), 0)
  })

}

