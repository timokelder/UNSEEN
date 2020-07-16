context("UNSEEN timeseries ggplot")

test_that("output of unseen timeseries ggplot() is stable", {
  p <- unseen_timeseries(ensemble = SEAS5_UK, obs = EOBS_UK)

  vdiffr::expect_doppelganger("unseen timeseries", p)
})
