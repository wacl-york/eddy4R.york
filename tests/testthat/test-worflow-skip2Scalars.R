scenarioName = "test_skip2Scalars"

testScenario = default_scenario(fileType = "no")

testScenario$lagApplyCorrection = F

run_workflow_tests(testScenario, scenarioName)
