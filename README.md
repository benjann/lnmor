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

    18aug2022 (version 1.0.0)
    - released on GitHub
