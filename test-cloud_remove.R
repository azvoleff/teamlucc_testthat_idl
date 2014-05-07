context("cloud_remove")

# Below is temporary - delete when done with direct testing
# load_all('../../')
# library(tools)
# out_name <- NULL
# use_IDL <- TRUE
# fast <- FALSE
# num_class <- 1
# min_pixel <- 20
# max_pixel <- 1000
# cloud_nbh <- 1
# DN_min <- 0
# DN_max <- 255
# idl <- "C:/Program Files/Exelis/IDL83/bin/bin.x86_64/idl.exe"
# patch_long <- 1000
# verbose <- TRUE

cloudy <- raster('cloud_remove/L20080724_cloudy')
clear <- raster('cloud_remove/L20080606')
cloud_mask <- raster('cloud_remove/cloud_mask')

# Load reference image filled with Xiaolin's original code
filled_ref <- raster('cloud_remove/img_cloudy_stack_filled_reference')

out_name_idl <- 'cloud_remove/img_cloudy_stack_filled_idl'
system.time(out_idl <- cloud_remove(cloudy, clear, cloud_mask, out_name_idl))

out_name_idl_fast <- 'cloud_remove/img_cloudy_stack_filled_idl_fast'
system.time(out_idl_fast <- cloud_remove(cloudy, clear, cloud_mask, 
                                         out_name_idl_fast, fast=TRUE))

out_name_r <- 'cloud_remove/img_cloudy_stack_filled_r'
system.time(out_r <- cloud_remove(cloudy, clear, cloud_mask, out_name_r, 
                                  use_IDL=FALSE, verbose=TRUE))

# plot(clear[[1]])
# plot(cloudy[[1]])
# plot(cloud_mask[[1]])
# plot(out_idl[[1]])
# plot(out_r[[1]])
# plot(out_idl_fast[[1]])
# plot(filled_ref[[1]])
#
# mean(getValues(clear) - getValues(filled_ref))
# mean(getValues(cloudy) - getValues(filled_ref))
# expect_that(mean(getValues(out_r) - getValues(filled_ref))
#
# hist(filled_ref - out_idl)
# hist(filled_ref - out_r)
# hist(filled_ref - out_idl_fast)
#
# plotRGB(out_idl_fast, stretch="lin")
# plotRGB(out_r, stretch="lin")
# plotRGB(out_idl, stretch="lin")
#
# plot(out_idl[[1]] - filled_ref[[1]])
# plot(out_idl_fast[[1]] - filled_ref[[1]])
# plot(out_r[[1]] - filled_ref[[1]])

# Approx 97 second run time
test_that("input data is correct", {
    expect_less_than(abs(mean(getValues(clear) - getValues(cloudy))), 1.8)
})

# Approx 17 second run time
test_that("IDL cloud fill works correctly for RasterLayers", {
    expect_less_than(abs(mean(getValues(out_idl_fast) -
                              getValues(filled_ref))), .07)
    expect_less_than(abs(mean(getValues(out_idl) -
                              getValues(filled_ref))), .02)
})

# Approx 22 second run time
test_that("R cloud fill works correctly for RasterLayers", {
    expect_less_than(abs(mean(getValues(out_r) -
                              getValues(filled_ref))), .05)
})


###############################################################################
# Repeat above tests, but run on a layer stack

cloudy_stack <- stack(cloudy, cloudy, cloudy)
clear_stack <- stack(clear, clear, clear)

# Approx 120 second run time
out_name_idl_stack <- 'cloud_remove/img_cloudy_stack_filled_idl'
system.time(out_idl_stack <- cloud_remove(cloudy_stack, clear_stack, 
                                          cloud_mask, out_name_idl_stack))

# Approx 20 second run time
out_name_idl_stack_fast <- 'cloud_remove/img_cloudy_stack_filled_idl_fast'
system.time(out_idl_stack_fast <- cloud_remove(cloudy_stack, clear_stack, 
                                               cloud_mask, 
                                               out_name_idl_stack_fast, 
                                               fast=TRUE))

# Approx 23 second run time
out_name_r_stack <- 'cloud_remove/img_cloudy_stack_filled_r'
system.time(out_r_stack <- cloud_remove(cloudy_stack, clear_stack, cloud_mask, 
                                        out_name_r_stack, use_IDL=FALSE, 
                                        verbose=TRUE))

plot(out_idl_stack[[1]])
plot(out_idl_stack_fast[[1]])
plot(out_r_stack[[1]])

filled_ref_stack <- stack(filled_ref, filled_ref, filled_ref)

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
                              getValues(filled_ref_stack[[1]]))), .05)
    expect_less_than(abs(mean(getValues(out_r_stack[[2]]) - 
                              getValues(filled_ref_stack[[2]]))), .05)
    expect_less_than(abs(mean(getValues(out_r_stack[[3]]) - 
                              getValues(filled_ref_stack[[3]]))), .05)
})
