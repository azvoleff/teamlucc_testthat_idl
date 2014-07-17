# teamlucc_testhhat_idl

This folder contains `testthat` unit tests for the 
[`teamlucc`](https://github.com/azvoleff/teamlucc) package. All of these tests 
depend on having an IDL installation in order to run. As many of these tests 
also require relatively large data files, they are not located in the main 
`teamlucc` repository. This folder is not required for most users of `teamlucc` 
- you only need to install these files along with `teamlucc` if you have an IDL 
license, and want to fully test the `teamlucc` package, or if you want access 
to the sample data included here.

To run these tests, download and  unzip [the latest version of the `teamlucc` 
package](https://github.com/azvoleff/teamlucc/archive/master.zip)
into a folder on your computer (on a Windows machine, for example, this might 
be `C:\Users\azvoleff\Desktop\teamlucc`). Then download [the IDL 
tests](https://github.com/azvoleff/teamlucc_testthat_idl/archive/master.zip), 
and unzip them into `tests\testthat_idl` within the `teamlucc` directory (for 
example `C:\Users\azvoleff\Desktop\teamlucc\tests\testthat_idl`).

**NOTE: If you are installing on Windows, you will need to install the
appropriate version of [Rtools](http://cran.r-project.org/bin/windows/Rtools/) 
for your version of R (as `teamlucc` contains C++ code) before you follow the 
below steps.**

Now, in R, run:

```R
teamlucc_dir <- "C:/Users/azvoleff/Desktop/teamlucc"
library(devtools)
library(testthat)
load_all(teamlucc_dir)
test_dir(file.path(teamlucc_dir, "tests", "testthat_idl"))
```

Be sure to replace `teamlucc_dir` with the appropriate path to the `teamlucc` 
folder on your machine. Depending on the speed of your machine, these tests can 
take up to several hours to run.
