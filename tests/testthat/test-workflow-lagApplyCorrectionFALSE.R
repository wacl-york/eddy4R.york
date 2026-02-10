scenarioName = "test_lagApplyCorrectionFALSE"

testScenario = default_scenario()

testScenario$lagApplyCorrection = F

run_workflow_tests(testScenario, scenarioName)
