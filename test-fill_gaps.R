context("fill_gaps")

# Below are temporary - delete when done with direct testing
# load_all('../../')
# library(tools)
# out_base <- NULL
# use_IDL <- TRUE
# sample_size <- 20
# size_wind <- 12
# class_num <- 4
# DN_min <- 0.0
# DN_max <- 1.0
# patch_long <- 1000
# idl <- "C:/Program Files/Exelis/IDL83/bin/bin.x86_64/idl.exe"
# verbose <- TRUE

slc_off <- brick('fill_gaps/TM20100429_toaR_gap')
fill <- brick('fill_gaps/TM20100515_toaR')
timeseries <- c(brick('fill_gaps/TM20100208_toaR'))

# Open reference images from running Xiaolin's original code
filled_ref <- brick('fill_gaps/TM20100429_toaR_gap_GNSPI_reference')
filled_ref_uncertainty <- brick('fill_gaps/TM20100429_toaR_gap_GNSPI_uncertainty_reference')

# Approx 165 seconds
out_base_no_timeseries <- 'fill_gaps/TM20100429_toaR_gap_no_timeseries'
system.time(filled_no_timeseries <- fill_gaps(slc_off, fill, timeseries=c(), 
                                              out_base=out_base_no_timeseries))

# Approx 205 seconds
out_base_with_timeseries <- 'fill_gaps/TM20100429_toaR_gap_with_timeseries'
system.time(filled_with_timeseries <- fill_gaps(slc_off, fill, 
                                                timeseries=timeseries, 
                                                out_base=out_base_with_timeseries))

# plot(slc_off[[1]])
# plot(fill[[1]])
# plot(filled_ref[[1]])
# plot(filled_ref_uncertainty[[1]])
# plot(filled_no_timeseries$filled[[1]] - filled_ref[[1]])
# plot(filled_with_timeseries$filled[[1]] - filled_ref[[1]])

test_that("input data is correct", {
    expect_less_than(abs(mean(getValues(slc_off) - getValues(filled_ref))), .05)
    expect_less_than(abs(mean(getValues(fill) - getValues(filled_ref))), .02)
})

test_that("gap fill works properly", {
    expect_less_than(abs(mean(getValues(filled_no_timeseries$filled) - getValues(filled_ref))), 1e-4)
    expect_less_than(abs(mean(getValues(filled_with_timeseries$filled) - getValues(filled_ref))), 1e-5)
})

test_that("gap fill uncertainty works properly", {
    expect_less_than(abs(mean(getValues(filled_no_timeseries$uncertainty) - getValues(filled_ref_uncertainty))), 1.07)
    expect_less_than(abs(mean(getValues(filled_with_timeseries$uncertainty) - getValues(filled_ref_uncertainty))), .8)
})
