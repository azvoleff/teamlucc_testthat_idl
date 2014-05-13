context("cloud_remove")

cloudy <- raster('cloud_remove/L20080724_cloudy')
clear <- raster('cloud_remove/L20080606')
cloud_mask <- raster('cloud_remove/cloud_mask')

# Load reference image filled with Xiaolin's original code
filled_ref <- raster('cloud_remove/img_cloudy_stack_filled_reference')

test_that("input data is correct", {
    expect_less_than(abs(mean(getValues(clear) - getValues(cloudy))), 1.8)
})

# Approx 97 second run time
out_idl_byblock <- cloud_remove(cloudy, clear, cloud_mask)
# Approx 97 second run time
out_idl <- cloud_remove(cloudy, clear, cloud_mask, byblock=FALSE)

# Approx 17 second run time
out_idl_fast_byblock <- cloud_remove(cloudy, clear, cloud_mask, fast=TRUE)
# Approx 17 second run time
out_idl_fast <- cloud_remove(cloudy, clear, cloud_mask, fast=TRUE, 
                             byblock=FALSE)

# Approx 22 second run time
out_r_byblock <- cloud_remove(cloudy, clear, cloud_mask, use_IDL=FALSE)
# Approx 22 second run time
out_r <- cloud_remove(cloudy, clear, cloud_mask, use_IDL=FALSE, byblock=FALSE)

test_that("byblock=FALSE and byblock=TRUE match for RasterLayers", {
    expect_equivalent(getValues(out_idl), getValues(out_idl_byblock))
    expect_equivalent(getValues(out_idl_fast), getValues(out_idl_fast_byblock))
    expect_equivalent(getValues(out_r), getValues(out_r_byblock))
})

test_that("IDL cloud fill works correctly for RasterLayers", {
    expect_less_than(abs(mean(getValues(out_idl_fast) -
                              getValues(filled_ref))), .07)
    expect_less_than(abs(mean(getValues(out_idl) -
                              getValues(filled_ref))), .02)
})

test_that("R cloud fill works correctly for RasterLayers", {
    expect_less_than(abs(mean(getValues(out_r) - getValues(filled_ref))), .11)
})

# Test code works when using out_names
out_idl_outname <- cloud_remove(cloudy, clear, cloud_mask, 
                                out_name=rasterTmpFile())
out_idl_fast_outname <- cloud_remove(cloudy, clear, cloud_mask, fast=TRUE, 
                                     out_name=rasterTmpFile())
# Code for cloud_remove_R also needs to test outnames with and without byblock 
# (not tested above as this code doesn't vary for cloud_remove_IDL).
out_r_outname_byblock <- cloud_remove(cloudy, clear, cloud_mask, use_IDL=FALSE, 
                                      out_name=rasterTmpFile())
out_r_outname <- cloud_remove(cloudy, clear, cloud_mask, use_IDL=FALSE, 
                              byblock=FALSE, out_name=rasterTmpFile())

test_that("cloud_remove works when out_name is specified", {
    expect_equivalent(getValues(out_idl), getValues(out_idl_outname))
    expect_equivalent(getValues(out_idl_fast), getValues(out_idl_fast_outname))
    expect_equivalent(getValues(out_r), getValues(out_r_outname))
    expect_equivalent(getValues(out_r), getValues(out_r_byblock))
})

###############################################################################
# Repeat above tests, but run on a layer stack

cloudy_stack <- stack(cloudy, cloudy, cloudy)
clear_stack <- stack(clear, clear, clear)
filled_ref_stack <- stack(filled_ref, filled_ref, filled_ref)

# Approx 150 second run time
out_idl_stack_byblock <- cloud_remove(cloudy_stack, clear_stack, cloud_mask)
# Approx 150 second run time
out_idl_stack <- cloud_remove(cloudy_stack, clear_stack, cloud_mask, 
                              byblock=FALSE)

# Approx 30 second run time
out_idl_stack_fast_byblock <- cloud_remove(cloudy_stack, clear_stack, 
                                           cloud_mask, fast=TRUE)
# Approx 30 second run time
out_idl_stack_fast <- cloud_remove(cloudy_stack, clear_stack, cloud_mask, 
                                   fast=TRUE, byblock=FALSE)

# Approx 30 second run time
out_r_stack_byblock <- cloud_remove(cloudy_stack, clear_stack, cloud_mask, 
                                    use_IDL=FALSE)
# Approx 30 second run time
out_r_stack <- cloud_remove(cloudy_stack, clear_stack, cloud_mask, 
                            use_IDL=FALSE, byblock=FALSE)

test_that("IDL cloud fill works correctly for RasterStack", {
    # IDL
    expect_less_than(abs(mean(getValues(out_idl_stack[[1]]) - 
                              getValues(filled_ref_stack[[1]]))), .02)
    expect_less_than(abs(mean(getValues(out_idl_stack[[2]]) - 
                              getValues(filled_ref_stack[[2]]))), .02)
    expect_less_than(abs(mean(getValues(out_idl_stack[[3]]) - 
                              getValues(filled_ref_stack[[3]]))), .02)
    # IDL fast
    expect_less_than(abs(mean(getValues(out_idl_stack_fast[[1]]) - 
                              getValues(filled_ref_stack[[1]]))), .07)
    expect_less_than(abs(mean(getValues(out_idl_stack_fast[[2]]) - 
                              getValues(filled_ref_stack[[2]]))), .07)
    expect_less_than(abs(mean(getValues(out_idl_stack_fast[[3]]) - 
                              getValues(filled_ref_stack[[3]]))), .07)
})

test_that("R cloud fill works correctly for RasterStack", {
    expect_less_than(abs(mean(getValues(out_r_stack[[1]]) - 
                              getValues(filled_ref_stack[[1]]))), .11)
    expect_less_than(abs(mean(getValues(out_r_stack[[2]]) - 
                              getValues(filled_ref_stack[[2]]))), .11)
    expect_less_than(abs(mean(getValues(out_r_stack[[3]]) - 
                              getValues(filled_ref_stack[[3]]))), .11)
})

test_that("byblock=FALSE and byblock=TRUE match for RasterStacks", {
    expect_equivalent(out_idl_stack, out_idl_stack_byblock)
    expect_equivalent(out_idl_stack_fast, out_idl_stack_fast_byblock)
    expect_equivalent(out_r_stack, out_r_stack_byblock)
})
