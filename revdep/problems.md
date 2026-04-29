# new.dist (0.1.1)

* GitHub: <https://github.com/akmn35/new.dist>
* Email: <mailto:ramazanakman12345@gmail.com>
* GitHub mirror: <https://github.com/cran/new.dist>

Run `revdepcheck::cloud_details(, "new.dist")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > # Where should you do additional test configuration?
       > # Learn more about the roles of various files in:
       > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
       > # * https://testthat.r-lib.org/articles/special-files.html
       > 
       > library(testthat)
       > library(new.dist)
       > 
       > test_check("new.dist")
       Saving _problems/test_function-246.R
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 232 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_function.R:246:3'): functions returns a  vector with the expected size ──
       <rlib_error_package_not_found/rlang_error/error/condition>
       Error in `expect_vector(dbwd(10, alpha = 2, sigma = 2), ptype = double(), size = 1)`: The package "vctrs" is required.
       Backtrace:
           ▆
        1. └─testthat::expect_vector(...) at test_function.R:246:3
        2.   └─rlang::check_installed("vctrs")
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 232 ]
       Error:
       ! Test failures.
       Execution halted
     ```

# tnl.Test (0.1.0)

* GitHub: <https://github.com/ihababusaif/tnl.Test>
* Email: <mailto:censtat@gmail.com>
* GitHub mirror: <https://github.com/cran/tnl.Test>

Run `revdepcheck::cloud_details(, "tnl.Test")` for more info

## Newly broken

*   checking tests ... ERROR
     ```
     ...
       > # Where should you do additional test configuration?
       > # Learn more about the roles of various files in:
       > # * https://r-pkgs.org/tests.html
       > # * https://testthat.r-lib.org/reference/test_package.html#special-files
       > 
       > library(testthat)
       > library(tnl.Test)
       > 
       > test_check("tnl.Test")
       Saving _problems/test_function-65.R
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 64 ]
       
       ══ Failed tests ════════════════════════════════════════════════════════════════
       ── Error ('test_function.R:65:3'): functions returns a  vector with the expected size ──
       <rlib_error_package_not_found/rlang_error/error/condition>
       Error in `expect_vector(rtnl(10, 5, 10, 2), ptype = double(), size = 10)`: The package "vctrs" is required.
       Backtrace:
           ▆
        1. └─testthat::expect_vector(...) at test_function.R:65:3
        2.   └─rlang::check_installed("vctrs")
       
       [ FAIL 1 | WARN 0 | SKIP 0 | PASS 64 ]
       Error:
       ! Test failures.
       Execution halted
     ```

