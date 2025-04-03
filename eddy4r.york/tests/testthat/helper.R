default_scenario = function(fileType = "o3"){

  switch (fileType,

          # Good EC File with O3 data to calculate a flux from
          o3 = eddy4R.york::def.para(
            DirWrk = "/home/york/eddy4r.york/tests/fixtures/",
            DirInp = "example_data_o3",
            siteName = "siteName",
            lat = 32.2644,
            runID = "runID",
            analysis = "analysis",
            fileMask = "th_o3_%Y%m%d_%H%M%S.csv",
            fileDuration = 3600,
            species = "O3",
            aggregationDuration = 3600,
            writeFastData = T,
            AlgBase = "trnd",
            idepVar = "unixTime",
            MethRot = "double",
            missingMethod = "mean",
            lagApplyCorrection = T,
            lagDefaults = c(0,0,-6),
            lagApplyRangeLimit = TRUE,
            lagRangeLimit = list(c(0,0),
                                 c(0,0),
                                 c(0,-20))
          ),

          # One file is missing large amounts of NO and NO2 data - in this file both scalars should be rejected at sensible missing thresholds
          no = eddy4R.york::def.para(
            DirWrk = "/home/york/eddy4r.york/tests/fixtures/",
            DirInp = "example_data_no_no2",
            DirOut = file.path(tempdir(), "test_skip2Scalars"), # write to temp dir for testing
            siteName = "siteName",
            lat = 32.2644,
            runID = "runID",
            analysis = "analysis",
            fileMask = "NOx_5Hz_%y_%m_%d_%H%M%S_flux_input.csv",
            fileDuration = 3600,
            species = c("NO","NO2"),
            aggregationDuration = 3600,
            writeFastData = T,
            AlgBase = "trnd",
            idepVar = "unixTime",
            MethRot = "double",
            missingMethod = "mean",
            lagApplyCorrection = T,
            lagDefaults = c(0,0,-6,-6),
            lagApplyRangeLimit = TRUE,
            lagRangeLimit = list(c(0,0),
                                 c(0,0),
                                 c(0,-20),
                                 c(0,-20))
          )

  )

}


create_test_scenarios = function(){

  testScenarios = list()

  testScenarios$test_default = default_scenario()
  testScenarios$test_default$DirOut = file.path(tempdir(), "test_default")

  testScenarios$test_lagApplyCorrectionFALSE = default_scenario()
  testScenarios$test_lagApplyCorrectionFALSE$DirOut = file.path(tempdir(), "test_lagApplyCorrectionFALSE")
  testScenarios$test_lagApplyCorrectionFALSE$lagApplyCorrection = F

  testScenarios$test_skip2Scalars = default_scenario(fileType = "no")
  testScenarios$test_skip2Scalars$DirOut = file.path(tempdir(), "test_skip2Scalars")

  #
  testScenarios

}
