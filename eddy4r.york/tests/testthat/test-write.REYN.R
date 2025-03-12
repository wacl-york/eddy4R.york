
REYN = list(data = iris |>
              dplyr::mutate(unixTime = 1:dplyr::n())
)
REYN$mean = REYN$data
REYN$sd = REYN$data

lag_out = list(
  eddy.data = REYN$data,
  lagTimes = data.frame(
    name = c("tempAir", "rtioMoleDryH2o","rtioMoleDryO3"),
    lagTime = c(0,0, 5),
    corr = c(0,0,0.9)
  )
)

test_that("lag_out being NULL is ok",{
  expect_no_error(write.REYN(REYN = REYN,
                             lag_out = NULL,
                             DirOut = file.path(tempdir(), "test_write.REYN"),
                             analysis  ="test",
                             tz = "UTC",
                             writeFastData = T,
                             subDir = "none"))
})

