{smcl}
{* 19aug2022}{...}
{hi:help lnmor}{...}
{right:{browse "http://github.com/benjann/lnmor/"}}
{hline}

{title:Title}

{pstd}{hi:lnmor} {hline 2} Compute marginal (log) odds ratios after model estimation


{title:Syntax}

{p 8 15 2}
    {cmd:lnmor} {varlist}
    [{cmd:,}
    {help lnmor##opt:{it:options}}
    ]

{synoptset 20 tabbed}{...}
{marker opt}{synopthdr:options}
{synoptline}
{syntab :Main}
{synopt:{cmdab:at(}{help lnmor##at:{it:spec}}{cmd:)}}estimate results at
    specified values of covariates
    {p_end}
{synopt :{opt atmax(#)}}maximum number of levels allowed in {cmd:at()}
    {p_end}
{synopt :{opt kmax(#)}}maximum number of levels before coarsening
    kicks in; default is {cmd:kmax(100)}
    {p_end}
{synopt :{opt nodot:s}}suppress progress dots
    {p_end}
{synopt :{opt post}}post results in {cmd:e()}
    {p_end}

{syntab :SE/Robust}
{synopt :{cmd:vce(}{help lnmor##vcetype:{it:vcetype}}{cmd:)}}use
    replication-based VCE; {it:vcetype} may be {cmdab:boot:strap} or {cmdab:jack:knife}
    {p_end}
{synopt :{opt nose}}do not estimate SEs
    {p_end}
{synopt :{opt ifgen:erate(spec)}}store influence functions; {it:spec} may be
    {it:namelist} or {it:stub}{cmd:*}
    {p_end}
{synopt :{opt replace}}allow replacing existing variables
    {p_end}

{syntab :Reporting}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt :{opt or}}report odds ratios
    {p_end}
{synopt :{opt nohead:er}}suppress table header
    {p_end}
{synopt :{help estimation options##display_options:{it:display_options}}}standard display option
    {p_end}
{synopt :{opt coefl:egend}}display legend instead of statistics
    {p_end}
{synoptline}
{pstd}
    {varlist} may contain factor variables (but no interactions); see {help fvvarlist}.
    {p_end}


{title:Description}

{pstd}
    {cmd:lnmor} computes (adjusted) marginal odds ratios
    after {helpb logit} or {helpb probit} using G-computation. {cmd:lnmor}
    works by applying fractional logit to averaged counterfactual predictions
    from the original model.

{pstd}
    {cmd:lnmor} requires {helpb moremata} to be installed on the system. Type
    {stata ssc install moremata}.


{title:Options}

{marker at}{...}
{phang}
    {opt at(spec)} reports results with covariates fixed at specific values. The
    syntax of {it:spec} is

            {varname} {cmd:=} {it:{help numlist}} [{varname} {cmd:=} {it:{help numlist}} ...]

{pmore}
    Computations will be repeated for all combinations of specified values. You
    can also type {opth at(varlist)} in which case 
    the levels found in the data will be used. In any case, the variables specified
    in {cmd:at()} must be different from the variables specified in the main
    {it:varlist}. Furthermore, only variables that appear as covariates
    in the original model are allowed.

{phang}
    {opt atmax(#)} sets the maximum number of (combinations of) values that is
    allowed in {cmd:at()}. The default is {cmd:atmax(50)}.

{phang}
    {opt kmax(#)} sets the maximum number of levels (distinct values) that are
    allowed for continuous predictors. If a variable has more levels,
    {cmd:lnmor} will provide approximate results based on a linearly binned
    variable.

{phang}
    {opt nodots} suppresses the progress dots.

{phang}
    {opt post} stores results in {cmd:e()} rather than in {cmd:r()}. {cmd:post}
    has no effect of {cmd:vce(bootstrap)} or {cmd:vce(jackknife)} is specified
    (results will always be stored in {cmd:e()} in these cases).

{marker vcetype}{...}
{phang}
    {cmd:vce(}{it:vcetype}{cmd:)} specifies the variance estimation method. The
    default is to compute robust standard errors based on influence functions (taking
    account of clustering if the original model includes clustering). Use option
    {cmd:vce()} to request replication-based standard errors; {it:vcetype} may be
    {cmdab:boot:strap} or {cmdab:jack:knife}; see {it:{help vce_option}}. If replication-based
    standard errors are requested, {cmd:lnmor} will reestimate the original model
    within replications.

{pmore}
    For clarification, if you want to obtain replication-based standard errors, must specify
    {cmd:vce(bootstrap)} or {cmd:vce(jackknife)} with {cmd:lnmor}, not with the
    the original command. Specifying {cmd:vce(bootstrap)} or
    {cmd:vce(jackknife)} with the original model will have no effect on the
    results by {cmd:lnmor} (apart from clustering being picked up, if
    relevant). That is, the replication-based variance matrix of the original
    model is not used in the computations by {cmd:lnmor}.

{phang}
    {opt nose} suppresses calculation of the VCE and standard errors. The
    variance matrix will be set to zero in this case. {cmd:vce(bootstrap)}
    and {cmd:vce(jackknife)} imply {cmd:nose} (to save computer time).

{phang}
    {opt ifgenerate(spec)} stores the influence functions of the estimates. Either
    specify a list of new variables names,
    or specify {it:stub}{cmd:*}, in which case the new variables will be named
    {it:stub}{cmd:1}, {it:stub}{cmd:2}, etc. Option {cmd:ifgenerate()} is not
    allowed with {cmd:vce(bootstrap)} or {cmd:vce(jackknife)}.

{phang}
    {opt replace} allows to overwrite existing variables.

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)} or as
    set by {helpb set level}.

{phang}
    {opt or} reports the estimated coefficients transformed to odds ratios,
    that is, exp(b) rather than b. Standard errors and confidence intervals are
    similarly transformed. This option affects how results are displayed, not
    how they are estimated. {cmd:or} may be specified at estimation or when
    replaying previously estimated results.

{phang}
    {opt noheader} suppresses the display of the table header.

{phang}
    {it:display_options} are usual display options as documented in
    {helpb estimation options##display_options:[R] Estimation options}.

{phang}
    {opt coeflegend} specifies that the legend of the coefficients and how to
    specify them in an expression be displayed rather
    than displaying the statistics for the coefficients.


{title:Example}

{pstd}
    The following example illustrates how the adjusted marginal odds ratio can be
    different from the conditional odds ratio:

        {it:unadjusted marginal odds ratio}
        . {stata webuse lbw}
        . {stata logit low i.smoke, or vce(robust)}
        . {stata lnmor i.smoke, or}

        {it:conditional odds ratio}
        . {stata logit low i.smoke i.race age lwt ptl ht ui, or}

        {it:adjusted marginal odds ratio}
        . {stata lnmor i.smoke, or}

{pstd}
    Results can be obtained for several variables in one call:

        . {stata logit low i.smoke i.race age lwt ptl ht ui, or}
        . {stata lnmor i.smoke i.race age lwt, or}

{pstd}
    Use option {cmd:at()} to explore interactions:

        . {stata logit low i.smoke i.race age lwt ptl ht ui i.smoke#i.race, or}
        . {stata lnmor i.smoke, at(race) or}

{pstd}
    Use use {cmd:ibn.}{it:varname} to obtain marginal odds by level rather than odds ratios:

        . {stata logit low i.smoke i.race age lwt ptl ht ui i.smoke#i.race, or}
        . {stata lnmor ibn.smoke, at(race) or}

{pstd}
    For categorical predictors, a combination of {helpb margins} and {helpb nlcom}
    can be used to replicate the results by {cmd:lnmor}:

        . {stata logit low i.smoke i.race age lwt ptl ht ui, vce(robust)}
        . {stata lnmor i.smoke, or}
        . {stata margins, at(smoke=(0 1)) post vce(unconditional)}
        . {stata "nlcom (smoke:(logit(_b[2._at]) - logit(_b[1._at]))), post"}
        . {stata ereturn display, eform(Odds Ratio)}


{title:Stored results}

{pstd}
{cmd:lnmor} stores the following in {cmd:r()}:

{synoptset 23 tabbed}{...}
{p2col 5 23 26 2: Scalars}{p_end}
{synopt:{cmd:r(N)}}number of observations{p_end}
{synopt:{cmd:r(N_clust)}}number of clusters{p_end}
{synopt:{cmd:r(k_eq)}}number of equations in {cmd:r(b)}{p_end}
{synopt:{cmd:r(nterms)}}number of terms{p_end}
{synopt:{cmd:r(k}{it:#}{cmd:)}}number levels in term {it:#}{p_end}
{synopt:{cmd:r(rank)}}rank of {cmd:r(V)}{p_end}

{p2col 5 23 26 2: Macros}{p_end}
{synopt:{cmd:r(cmd)}}{cmd:lnmor}{p_end}
{synopt:{cmd:r(cmdline)}}command as typed{p_end}
{synopt:{cmd:r(est_cmd)}}{cmd:e(cmd)} from original estimation results{p_end}
{synopt:{cmd:r(est_cmdline)}}{cmd:e(cmdline)} from original estimation results{p_end}
{synopt:{cmd:r(title)}}title in estimation output{p_end}
{synopt:{cmd:r(depvar)}}name of dependent variable{p_end}
{synopt:{cmd:r(term}{it:#}{cmd:)}}specification of term {it:#}{p_end}
{synopt:{cmd:r(name}{it:#}{cmd:)}}variable name of term {it:#}{p_end}
{synopt:{cmd:r(type}{it:#}{cmd:)}}{cmd:variable} or {cmd:factor}; type of term {it:#}{p_end}
{synopt:{cmd:r(atnames)}}variable names from {cmd:at()}{p_end}
{synopt:{cmd:r(wtype)}}weight type{p_end}
{synopt:{cmd:r(wexp)}}weight expression{p_end}
{synopt:{cmd:r(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:r(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:r(vcetype)}}title used to label Std. err.{p_end}
{synopt:{cmd:r(ifgenerate)}}names of variables containing IFs{p_end}

{p2col 5 23 26 2: Matrices}{p_end}
{synopt:{cmd:r(b)}}coefficient vector{p_end}
{synopt:{cmd:r(V)}}variance-covariance matrix of the estimators{p_end}
{synopt:{cmd:r(at)}}matrix of values from {cmd:at()}{p_end}
{synopt:{cmd:r(levels}{it:#}{cmd:)}}values and counts of levels of term {it:#}{p_end}

{pstd}
If {cmd:post} is specified, results are stored in {cmd:e()} rather than
{cmd:r()}, and function {cmd:e(sample)} that marks the estimation sample is
added. Results are also stored in {cmd:e()} in case of {cmd:vce(bootstrap)}
or {cmd:vce(jackknife)} (along with the results stored by {helpb bootstrap}
or {helpb jackknife}).


{title:Methods and Formulas}

{pstd}
    See Jann and Karlson (2022).


{title:References}

{phang}
    Jann, Ben, and Kristian Bernt Karlson. 2022. Marginal odds ratios: What they
    are, how to compute them, and why sociologists might want to use them. Working paper.
    {p_end}


{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B. (2022). lnmor: Stata module to compute marginal odds ratios after model estimation. Available from
    {browse "http://github.com/benjann/lnmor/"}.


{title:Also see}

{psee}
    Online:  help for
    {helpb logit}, {helpb probit}, {helpb ipwlogit} (if installed), {helpb riflogit} (if installed)
