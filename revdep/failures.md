# rstanarm (2.32.2)

* GitHub: <https://github.com/stan-dev/rstanarm>
* Email: <mailto:benjamin.goodrich@columbia.edu>
* GitHub mirror: <https://github.com/cran/rstanarm>

Run `revdepcheck::cloud_details(, "rstanarm")` for more info

## In both

*   checking whether package ‘rstanarm’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘rstanarm’ ...
** this is package ‘rstanarm’ version ‘2.32.2’
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.5.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/tmp/r-deps/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/tmp/r-deps/StanHeaders/include' -I'/tmp/r-deps/rstan/include' -I'/tmp/r-deps/BH/include' -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppEigen/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -I'/tmp/r-deps/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c init.cpp -o init.o
...
      |                                                                          ^~~~~~~~~
stanExports_jm.h: In constructor ‘model_jm_namespace::model_jm::model_jm(stan::io::var_context&, unsigned int, std::ostream*)’:
stanExports_jm.h:6883: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
 6883 |   model_jm(stan::io::var_context& context__, unsigned int random_seed__ = 0,
      | 
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.5.1/lib/R/etc/Makeconf:209: stanExports_jm.o] Error 1
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/new/rstanarm.Rcheck/rstanarm’


```
### CRAN

```
* installing *source* package ‘rstanarm’ ...
** this is package ‘rstanarm’ version ‘2.32.2’
** package ‘rstanarm’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘g++ (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
using C++17


g++ -std=gnu++17 -I"/opt/R/4.5.1/lib/R/include" -DNDEBUG -I"../inst/include" -I"/tmp/r-deps/StanHeaders/include/src" -DBOOST_DISABLE_ASSERTS -DEIGEN_NO_DEBUG -DBOOST_MATH_OVERFLOW_ERROR_POLICY=errno_on_error -DUSE_STANC3 -D_HAS_AUTO_PTR_ETC=0 -I'/tmp/r-deps/StanHeaders/include' -I'/tmp/r-deps/rstan/include' -I'/tmp/r-deps/BH/include' -I'/tmp/r-deps/Rcpp/include' -I'/tmp/r-deps/RcppEigen/include' -I'/tmp/r-deps/RcppParallel/include' -I/usr/local/include    -I'/tmp/r-deps/RcppParallel/include' -D_REENTRANT -DSTAN_THREADS   -fpic  -g -O2   -c init.cpp -o init.o
...
      |                                                                          ^~~~~~~~~
stanExports_jm.h: In constructor ‘model_jm_namespace::model_jm::model_jm(stan::io::var_context&, unsigned int, std::ostream*)’:
stanExports_jm.h:6883: note: variable tracking size limit exceeded with ‘-fvar-tracking-assignments’, retrying without
 6883 |   model_jm(stan::io::var_context& context__, unsigned int random_seed__ = 0,
      | 
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.5.1/lib/R/etc/Makeconf:209: stanExports_jm.o] Error 1
ERROR: compilation failed for package ‘rstanarm’
* removing ‘/tmp/workdir/rstanarm/old/rstanarm.Rcheck/rstanarm’


```
# SQLFormatteR (0.0.2)

* GitHub: <https://github.com/dataupsurge/SQLFormatteR>
* Email: <mailto:morgan@dataupsurge.com>
* GitHub mirror: <https://github.com/cran/SQLFormatteR>

Run `revdepcheck::cloud_details(, "SQLFormatteR")` for more info

## In both

*   checking whether package ‘SQLFormatteR’ can be installed ... ERROR
     ```
     Installation failed.
     See ‘/tmp/workdir/SQLFormatteR/new/SQLFormatteR.Rcheck/00install.out’ for details.
     ```

## Installation

### Devel

```
* installing *source* package ‘SQLFormatteR’ ...
** this is package ‘SQLFormatteR’ version ‘0.0.2’
** package ‘SQLFormatteR’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.78.0.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘SQLFormatteR’
* removing ‘/tmp/workdir/SQLFormatteR/new/SQLFormatteR.Rcheck/SQLFormatteR’


```
### CRAN

```
* installing *source* package ‘SQLFormatteR’ ...
** this is package ‘SQLFormatteR’ version ‘0.0.2’
** package ‘SQLFormatteR’ successfully unpacked and MD5 sums checked
** using staged installation
Error in eval(ei, envir) : 
------------------ [UNSUPPORTED RUST VERSION]------------------
- Minimum supported Rust version is 1.78.0.
- Installed Rust version is 1.75.0.
---------------------------------------------------------------
Calls: source -> withVisible -> eval -> eval
Execution halted
ERROR: configuration failed for package ‘SQLFormatteR’
* removing ‘/tmp/workdir/SQLFormatteR/old/SQLFormatteR.Rcheck/SQLFormatteR’


```
