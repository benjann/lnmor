# lnmor
Stata module to compute marginal odds ratios after model estimation

`lnmor` computes (adjusted) marginal odds ratios after `logit` or `probit`
using G-computation. `lnmor` works by applying fractional logit to averaged
counterfactual predictions from the original model.

---

Installation from GitHub:

    . net from https://raw.githubusercontent.com/benjann/lnmor/main/
    . net install lnmor, replace

The [`moremata`](https://github.com/benjann/moremata) package is required.
Type

    . ssc install moremata, replace

to install `moremata`.

---

Main changes:

    15jan2023 (version 1.1.4)
    - minor changes in how data is handled when enumerating predictions

    13jan2023 (version 1.1.3)
    - new subsample() option to evaluate the marginal odds ratio over a subsample
      of the data only; use this option, for example, to obtain an ATET equivalent
      of a marginal OR (by default, an ATE equivalent is computed)
    - the core of the code of lnmor has been rewritten (in part, so that the subsample
      option can be supported); the code is now better organized and also faster in
      most situations
    - option [no]tbal added
    - lnmor did not work with fweights; this is fixed
    - IFs were not updated to take account of the estimation of the mean in case of
      dx(atmean); this is fixed

    10jan2023 (version 1.1.2)
    - lnmor can now be specified repeatedly even if results are posted in e()
      (i.e. if option -post- or option -vce()- has been specified, or if lnmor has
      been applied after -mi estimate- or after -svy-)
    - option -miopts()- added; mi options will no longer be collected from
      command line of model
    - lnmor now also works after -mi estimate: svy: ...-
    - -or- added to program properties

    06jan2023 (version 1.1.1)
    - option -notable- added
    - revised implementation of support for svy

    29dec2022 (version 1.1.0)
    - fixed bug in display of progress dots in case of dx()

    29dec2022 (version 1.0.9)
    - option -constant- added
    - matrix r(levels) now returned if dx(numlist) or dx(levels) is specified
    - r(table) was only returned if option -post- was specified; this is fixed
    - revised labeling of terms affected by dx(); revised info on dx() and delta()
      displayed in output
    - revised display of progress dots

    02sep2022 (version 1.0.8)
    - delta(0) now computes log odds rather than log odds ratios
    - minor changes to output
    - minor changes to help

    02sep2022 (version 1.0.7)
    - options -rifgenerate()- and -ifscaling()- added
    - r(sum_w) added to returns
    
    01sep2022 (version 1.0.6)
    - lnmor now requires Stata version 15 or newer
    - can now type dx(levels) to report derivative at each observed level
    - new option delta() causes dx() to compute discrete-change effects rather than
      derivatives; new options -centered- and -normalize- provide settings; option
      -epsilon- is now undocumented (epsilon is equivalent to
      -delta(#) centered normalize- with # = 2*exp(log(c(epsdouble))/3))

    30aug2022 (version 1.0.5)
    - dx() now takes derivatives analytically; can specify -epsilon- for old
      behavior
    
    24aug2022 (version 1.0.4)
    - options dx(), epsilon(), and nowarn added

    23aug2022 (version 1.0.3)
    - can now specify interactions such as -c.x##c.x- to analyze nonlinear effects
    - some minor changes in how svyr is handled
    - lnmor no longer breaks if averaged predictions have no variance

    22aug2022 (version 1.0.2)
    - support for application after -svy- added
    - support for application after -mi estimate- added
    - now returning df_r (affects p-values and CIs)
    - improved formatting of progress information

    19aug2022 (version 1.0.1)
    - new at() option introduced; over() is discarded
    - imporved parsing of varlist and added various checks and error messages

    18aug2022 (version 1.0.0)
    - released on GitHub
