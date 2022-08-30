# lnmor
Stata module to compute marginal odds ratios after model estimation

`lnmor` computes (adjusted) marginal odds ratios after `logit` or `probit`
using G-computation. `lnmor` works by applying fractional logit to averaged
counterfactual predictions from the original model.

---

Installation from GitHub:

    . net install lnmor, replace from(https://raw.githubusercontent.com/benjann/lnmor/main/)

The [`moremata`](https://github.com/benjann/moremata) package is required.
Type

    . ssc install moremata, replace

to install `moremata`.

---

Main changes:

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