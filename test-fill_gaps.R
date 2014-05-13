context("fill_gaps")

slc_off <- brick('fill_gaps/TM20100429_toaR_gap')
fill <- brick('fill_gaps/TM20100515_toaR')
timeseries <- c(brick('fill_gaps/TM20100208_toaR'))

# Open reference images from running Xiaolin's original code
filled_ref <- brick('fill_gaps/TM20100429_toaR_gap_GNSPI_reference')
filled_ref_uncertainty <- brick('fill_gaps/TM20100429_toaR_gap_GNSPI_uncertainty_reference')

test_that("input data is correct", {
    expect_less_than(abs(mean(getValues(slc_off) - getValues(filled_ref))), .05)
    expect_less_than(abs(mean(getValues(fill) - getValues(filled_ref))), .02)
})

# Approx 165 seconds
filled_no_timeseries <- fill_gaps(slc_off, fill, timeseries=c())
# Approx 165 seconds
filled_no_timeseries_out_base <- fill_gaps(slc_off, fill, timeseries=c(), 
                                           out_base=rasterTmpFile())

# Approx 205 seconds
filled_with_timeseries <- fill_gaps(slc_off, fill, timeseries=timeseries)
# Approx 205 seconds
filled_with_timeseries_out_base <- fill_gaps(slc_off, fill, 
                                             timeseries=timeseries, 
                                             out_base=rasterTmpFile())

test_that("gap fill works properly", {
    expect_less_than(abs(mean(getValues(filled_no_timeseries$filled) - getValues(filled_ref))), 1e-4)
    expect_less_than(abs(mean(getValues(filled_with_timeseries$filled) - getValues(filled_ref))), 1e-5)
})

test_that("gap fill uncertainty works properly", {
    expect_less_than(abs(mean(getValues(filled_no_timeseries$uncertainty) - 
                              getValues(filled_ref_uncertainty))), 1.07)
    expect_less_than(abs(mean(getValues(filled_with_timeseries$uncertainty) - 
                              getValues(filled_ref_uncertainty))), .8)
})

test_that("gap fill works with out_base specified", {
    expect_equivalent(filled_no_timeseries$filled, 
                      filled_no_timeseries_out_base$filled)
    expect_equivalent(filled_no_timeseries$uncertainty, 
                      filled_no_timeseries_out_base$uncertainty)
    expect_equivalent(filled_with_timeseries$filled, 
                      filled_with_timeseries_out_base$filled)
    expect_equivalent(filled_with_timeseries$uncertainty, 
                      filled_with_timeseries_out_base$uncertainty)
})
