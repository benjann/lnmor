{smcl}
{* 29dec2022}{...}
{hi:help lnmor}{...}
{right:{browse "http://github.com/benjann/lnmor/"}}
{hline}

{title:Title}

{pstd}{hi:lnmor} {hline 2} Compute marginal (log) odds ratios after model estimation

{pstd}
    {help lnmor##syntax:Syntax} -
    {help lnmor##description:Description} -
    {help lnmor##options:Options} -
    {help lnmor##examples:Examples} -
    {help lnmor##returns:Stored results} -
    {help lnmor##author:Author} -
    {help lnmor##alsosee:Also see}


{marker syntax}{...}
{title:Syntax}

{p 8 15 2}
    {cmd:lnmor} {it:termlist} [{cmd:,}
    {help lnmor##opt:{it:options}}
    ]

{pstd}
    where {it:termlist} is

        {it:term} [{it:term} ...]

{pstd}
    and {it:term} may be a simple {varname}, a factor variable specification
    such as {cmd:i.}{it:varname}, {cmd:ibn.}{it:varname}, or
    {bind:{cmd:i(2 3 4).}{it:varname}}, or an interaction specification of a
    continuous variable with itself, such as
    {cmd:c.}{it:varname}{cmd:##}{cmd:c.}{it:varname}
    (see {help fvvarlist}; interactions containing different variables or
    interactions involving factor variables or are not allowed). Consecutive
    elements referring to the same variable (e.g.,
    {cmd:2.cat 3.cat 4.cat} or {cmd:x1 c.x1#c.x1 c.x1#c.x1#c.x1})
    will be merged into a single term (as long as they are of the same type). Likewise,
    {varlist} specifications such as {cmd:x*} or {cmd:x1-x5}, or groups
    such as {cmd:i.(var1 var2)} will be split into separate terms, one
    for each variable. In general, each {it:term} must refer to a distinct
    variable (that is, variables may not be repeated across terms) and all
    specified variables must appear among the covariates of the model to
    which {cmd:lnmor} is applied.


{synoptset 20 tabbed}{...}
{marker opt}{synopthdr:options}
{synoptline}
{syntab :Main}
{synopt:{cmdab:dx}[{cmd:(}{help lnmor##dx:{it:spec}}{cmd:)}]}use derivative-based
    evaluation method instead of fractional logit (continuous terms only)
    {p_end}
{synopt:{cmd:delta}[{cmd:(}{it:#}{cmd:)}]}let {cmd:dx()} compute 
    discrete change effects rather than derivatives
    {p_end}
{synopt:{opt center:ed}}use symmetric definition of discrete change effects
    {p_end}
{synopt:{opt norm:alize}}normalize discrete change effects
    {p_end}
{synopt:{cmdab:at(}{help lnmor##at:{it:spec}}{cmd:)}}estimate results at
    specified values of covariates
    {p_end}
{synopt :{opt atmax(#)}}maximum number of patterns allowed in {cmd:at()}
    {p_end}
{synopt :{opt kmax(#)}}maximum number of treatment levels before coarsening
    kicks in; default is {cmd:kmax(100)}
    {p_end}
{synopt :{opt nodot:s}}suppress progress dots
    {p_end}
{synopt :{opt nowarn}}suppress variable type warning messages
    {p_end}
{synopt:{opt cons:tant}}include constant from {helpb fracreg} in results; only allowed in some situations
    {p_end}
{synopt :{opt post}}post results in {cmd:e()}
    {p_end}

{syntab :SE/Robust}
{synopt :{cmd:vce(}{help lnmor##vcetype:{it:vcetype}}{cmd:)}}use
    replication-based VCE; {it:vcetype} may be {cmdab:boot:strap} or {cmdab:jack:knife}
    {p_end}
{synopt :{opt nose}}do not estimate SEs
    {p_end}
{synopt :{opt ifgen:erate(spec)}}store influence functions; {it:spec} is
    {it:namelist} or {it:stub}{cmd:*}
    {p_end}
{synopt :{opt rif:gerate(spec)}}store recentered influence functions; {it:spec} is
    {it:namelist} or {it:stub}{cmd:*}
    {p_end}
{synopt :{opt ifs:caling(spec)}}scaling of (recentered) IFs; {it:spec} if {cmd:total} (default) or {cmd:mean}
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


{marker description}{...}
{title:Description}

{pstd}
    {cmd:lnmor} computes (adjusted) marginal odds ratios after {helpb logit} or
    {helpb probit} (possibly including the {helpb svy} or {helpb mi estimate}
    prefix) using G-computation. By default, {cmd:lnmor}
    works by applying fractional logit ({helpb fracreg}) to averaged
    counterfactual predictions from the original model. Alternatively, for
    continuous predictors, specify option {helpb lnmor##dx:dx()} to compute
    derivative-based results. For methods and formulas see 
    {help lnmor##author:Jann and Karlson (2022)}.

{pstd}
    {cmd:lnmor} requires {helpb moremata} to be installed on the system. Type
    {stata ssc install moremata}.


{marker options}{...}
{title:Options}

{marker dx}{...}
{phang}
    {cmdab:dx}[{cmd:(}{it:spec}{cmd:)}] uses a derivative-based approach
    to compute results for continuous terms. By default, {cmd:lnmor} generates
    population-averaged counterfactual predictions for each level of a variable
    and then computes the marginal OR by applying fractional logit 
    ({helpb fracreg}) to these averaged predictions. Alternatively, if
    {cmd:dx()} is specified, results will be obtained by taking derivatives of
    population-averaged counterfactual predictions. This is only relevant for
    continuous terms that do not include interactions. That is, {cmd:dx()} will
    be ignored for factor-variable terms and interaction terms. {cmd:dx()} will
    also be ignored if a term is specified as continuous in {cmd:lnmor}, but
    the relevant variable has been included as a factor variable in the
    original model. {it:spec} may be one of the following.

{p2colset 13 24 27 2}{...}
{p2col:{opt ave:rage}}report the average derivative across the distribution
    of the variable; this is the default
    {p_end}
{p2col:{opt atm:ean}}report the derivative at the mean of the variable
    {p_end}
{p2col:{opt obs:erved}}report a derivative based on a marginal shift in
    observed values
    {p_end}
{p2col:{opt .}}same as {cmd:observed}
    {p_end}
{p2col:{help numlist:{it:numlist}}}report derivative at each specified level
    {p_end}
{p2col:{opt lev:els}}report derivative at each observed level; not allowed if
    {it:termlist} contains multiple terms affected by {cmd:dx()}
    {p_end}

{phang}
    {opt delta}[{cmd:(}{it:#}{cmd:)}] requests that {cmd:dx()} computes
    discrete change effects rather than derivatives. Discrete change effects
    are computed as the difference in population-averaged log odds at
    {it:t}+{it:#} and {it:t}, where {it:t} is the treatment value at which
    the effect is to be evaluated. {cmd:delta} without argument is equivalent
    to {cmd:delta(1)} (unit change effect). {cmd:delta()} implies {cmd:dx()}.

{pmore}
    Discrete change effect are not defined, if {it:#} is 0. In this case, {cmd:lnmor}
    will compute (log) odds rather than (log) odds ratios. That is, you can 
    specify {cmd:delta(0)} to obtain levels rather than effects ({cmd:centered}
    and {cmd:normalize} will be ignored).

{phang}
    {opt centered} requests that discrete change effects are computed
    using predictions at {it:t}+{it:#}/2 and {it:t}-{it:#}/2 rather than
    {it:t}+{it:#} and {it:t}. {cmd:centered} is only relevant if {cmd:delta()}
    has been specified.

{phang}
    {opt normalize} divides discrete change effects by {it:#}. {cmd:normalize}
    is only relevant if {cmd:delta()} has been specified.

{marker at}{...}
{phang}
    {opt at(spec)} reports results with covariates fixed at specific values. The
    syntax of {it:spec} is

            {varname} {cmd:=} {it:{help numlist}} [{varname} {cmd:=} {it:{help numlist}} ...]

{pmore}
    Computations will be repeated for each pattern of combinations of the
    specified covariate values. You can also type {opth at(varlist)} to use the levels
    found in the data for each variable instead of specifying custom values. In
    any case, the variables specified in {cmd:at()} must be different from the
    variables specified in {it:termlist}. Furthermore, only variables
    that appear as covariates in the original model are allowed.

{phang}
    {opt atmax(#)} sets the maximum number of patterns (combinations of values) that is
    allowed in {cmd:at()}. The default is {cmd:atmax(50)}.

{phang}
    {opt kmax(#)} sets the maximum number of levels (distinct values) that are
    allowed for continuous treatments. If a variable has more levels,
    {cmd:lnmor} will provide approximate results based on linearly binned
    levels. The default is {cmd:kmax(100)}.

{phang}
    {opt nodots} suppresses the progress dots.

{phang}
    {opt nowarn} suppresses the warning messages that are displayed if the types
    of the specified terms do not match the types of the
    relevant variables in the original model.

{phang}
    {opt constant} includes the constant of the fractional logit in the
    results vector. The default is to remove the constant. Including
    the constant may be helpful, for example, if you want to generate
    predictions from results of nonlinear terms. {cmd:constant} cannot be
    combined with {cmd:dx()} and is not allowed if {it:termlist} contains
    multiple terms.

{phang}
    {opt post} stores results in {cmd:e()} rather than in {cmd:r()}. {cmd:post}
    has no effect if {cmd:vce(bootstrap)} or {cmd:vce(jackknife)} is specified,
    or if {cmd:lnmor} is applied after {helpb svy}; results will always be stored
    in {cmd:e()} in these cases. If {cmd:lnmor} is applied after
    {helpb mi estimate}, option {cmd:post} behaves in the same way as option
    {cmd:post} in {helpb mi estimate}.

{marker vcetype}{...}
{phang}
    {cmd:vce(}{it:vcetype}{cmd:)} specifies the variance estimation method. The
    default is to compute robust standard errors based on influence functions (taking
    account of clustering if the original model includes clustering). Use option
    {cmd:vce()} to request replication-based standard errors; {it:vcetype} may be
    {cmdab:boot:strap} or {cmdab:jack:knife}; see {it:{help vce_option}}. If replication-based
    standard errors are requested, {cmd:lnmor} will reestimate the original model
    within replications. Option {cmd:vce()} is not allowed after {helpb svy}
    or {helpb mi estimate}.

{phang}
    {opt nose} suppresses calculation of the VCE and standard errors. The
    variance matrix will be set to zero in this case. To save computer time,
    {cmd:nose} is implied by {cmd:vce(bootstrap)} and {cmd:vce(jackknife)},
    or if {cmd:lnmor} is applied after {helpb svy} with replication-based VCE. Option
    {cmd:nose} is not allowed after {helpb svy} with linearization-based VCE or after
    {helpb mi estimate}.

{phang}
    {opt ifgenerate(spec)} stores the influence functions of the estimates. Either
    specify a list of new variables names, or specify {it:stub}{cmd:*}, in which
    case the new variables will be named
    {it:stub}{cmd:1}, {it:stub}{cmd:2}, etc. Option {cmd:ifgenerate()} is not
    allowed with {cmd:vce(bootstrap)} or {cmd:vce(jackknife)}, after {helpb svy}
    with replication-based VCE, or after {helpb mi estimate}. {cmd:ifgenerate()}
    has no effect if {cmd:nose} is specified.

{phang}
    {opt rifgenerate(spec)} stores the recentered influence functions; see the description
    of {cmd:ifgenerate()}. Only one of {cmd:ifgenerate()} and {cmd:rifgenerate()}
    is allowed.

{phang}
    {opt ifscaling(spec)} determines the scaling of the stored (recentered) influence
    functions. {it:spec} can be {opt t:otal} (scaling for analysis by
    {helpb total}) or {opt m:ean} (scaling for analysis by {helpb mean}). Default is 
    {cmd:ifscaling(total)}.

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


{marker examples}{...}
{title:Examples}

    {help lnmor##exbasic:Basic usage}
    {help lnmor##exnl:Nonlinear effects}
    {help lnmor##exbs:Bootstrap standard errors}
    {help lnmor##exsvy:Survey estimation}
    {help lnmor##exmi:Multiple imputation}

{marker exbasic}{...}
{dlgtab:Basic usage}

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
    Use option {helpb lnmor##at:at()} to explore interactions:

{p 8 12 2}. {stata logit low i.smoke i.race age lwt ptl ht ui i.smoke#i.race, or}{p_end}
{p 8 12 2}. {stata lnmor i.smoke, at(race) or}{p_end}

{pstd}
    Use {cmd:ibn.}{it:varname} to obtain marginal odds by level rather than odds ratios:

{p 8 12 2}. {stata logit low i.smoke i.race age lwt ptl ht ui i.smoke#i.race, or}{p_end}
{p 8 12 2}. {stata lnmor ibn.smoke, at(race) or}{p_end}

{pstd}
    Note that, for categorical predictors, a combination of {helpb margins} and {helpb nlcom}
    can be used to replicate the results by {cmd:lnmor}:

{p 8 12 2}. {stata logit low i.smoke i.race age lwt ptl ht ui, vce(robust)}{p_end}
{p 8 12 2}. {stata lnmor i.smoke, or}{p_end}
{p 8 12 2}. {stata margins, at(smoke=(0 1)) post vce(unconditional)}{p_end}
{p 8 12 2}. {stata "nlcom (smoke:(logit(_b[2._at]) - logit(_b[1._at]))), post"}{p_end}
{p 8 12 2}. {stata ereturn display, eform(Odds Ratio)}{p_end}

{marker exnl}{...}
{dlgtab:Nonlinear effects}

{pstd}
    Nonlinear effects can, for example, be explored using the {helpb lnmor##dx:dx()}
    option. Example:

{p 8 12 2}. {stata sysuse nlsw88}{p_end}
{p 8 12 2}. {stata logit union i.south grade c.grade#c.grade c.grade#c.grade#c.grade age}{p_end}
{p 8 12 2}. {stata lnmor grade, dx(levels) post}{p_end}

{pstd}
    The output reports the marginal (log) odds ratio at different levels of
    {cmd:grade}. Graphically, the pattern looks as follows (type
    {cmd:ssc install coefplot} to make {cmd:coefplot} available on your system):

        . {stata estimates store dx}
        . {stata coefplot dx, at(levels1[,1]) yline(0) yti(ln OR)}

{pstd}
    The effect appears to be positive at the bottom and at the top, and negative
    in the middle.

{pstd}
    A similar analysis could also be performed by specifying interaction terms
    when calling {cmd:lnmor}:

{p 8 12 2}. {stata logit union i.south grade c.grade#c.grade c.grade#c.grade#c.grade age}{p_end}
{p 8 12 2}. {stata lnmor grade c.grade#c.grade c.grade#c.grade#c.grade}{p_end}

{pstd}
    Qualitatively, results from the two approaches are very similar, as can be
    illustrated as follows:

{p 8 12 2}. {stata local function `=el(r(b),1,1)' + 2*`=el(r(b),1,2)'*x + 3*`=el(r(b),1,3)'*x^2}{p_end}
{p 8 12 2}. {stata coefplot dx, at(levels1[,1]) yline(0) yti(ln OR) addplot(function `function', range(0 18))}{p_end}

{pstd}
    As is evident, the effect function from the second approach matches the
    point estimates from the first approach very well.

{marker exbs}{...}
{dlgtab:Bootstrap standard errors}

{pstd}
    By default, {cmd:lnmor} reports robust standard errors based on
    influence-functions (possibly including clustering, which will be picked up
    automatically if the original model included clustering). If you want to
    obtain replication-based standard errors, specify {cmd:vce(bootstrap)} or
    {cmd:vce(jackknife)} with {cmd:lnmor}, not with the original command
    (it does not hurt to specify these options with the original command, but
    {cmd:lnmor} will not pick them up). An example is as follows:

        . {stata webuse lbw}
        . {stata logit low i.smoke i.race age lwt ptl ht ui, or}
        . {stata lnmor i.smoke i.race age lwt, vce(bootstrap) or}

{pstd}
    To {cmd:lnmor} reestimates the original model within the single
    replications.

{marker exsvy}{...}
{dlgtab:Survey estimation}

{pstd}
    {cmd:lnmor} can be applied after survey estimation, i.e. after a model
    to which the {helpb svy} prefix has been applied.

        . {stata webuse nhanes2f}
        . {stata "svy: logit highbp i.female i.race height weight age, or"}
        . {stata lnmor i.female i.race, or}

{pstd}
    This also works with replication-based VCE.

        . {stata webuse nhanes2brr}
        . {stata "svy: logit highbp i.female i.race height weight age, or"}
        . {stata lnmor i.female i.race, or}

{marker exmi}{...}
{dlgtab:Multiple imputation}

{pstd}
    {cmd:lnmor} can be applied after a model to which multiple imputation
    has been applied.

        . {stata webuse mheart1s20}
        . {stata "mi estimate, dots: logit attack i.smokes age bmi i.hsgrad i.female, or"}
        . {stata lnmor i.smokes, or}


{marker returns}{...}
{title:Stored results}

{pstd}
{cmd:lnmor} stores the following in {cmd:r()}:

{synoptset 23 tabbed}{...}
{p2col 5 23 26 2: Scalars}{p_end}
{synopt:{cmd:r(N)}}number of observations{p_end}
{synopt:{cmd:r(N_clust)}}number of clusters{p_end}
{synopt:{cmd:r(sum_w)}}sum of weights{p_end}
{synopt:{cmd:r(k_eq)}}number of equations in {cmd:r(b)}{p_end}
{synopt:{cmd:r(df_r)}}{cmd:r(N)}-1 or {cmd:r(N_clust)}-1{p_end}
{synopt:{cmd:r(nterms)}}number of terms{p_end}
{synopt:{cmd:r(k}{it:#}{cmd:)}}number evaluation levels used for term {it:#}{p_end}
{synopt:{cmd:r(dx}{it:#}{cmd:)}}whether {cmd:dx()} has been applied to term {it:#}{p_end}
{synopt:{cmd:r(delta)}}value of {cmd:delta()}, if specified{p_end}
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
{synopt:{cmd:r(type}{it:#}{cmd:)}}{cmd:factor}, {cmd:variable}, or {cmd:interaction}; type of term {it:#}{p_end}
{synopt:{cmd:r(dxtype}{cmd:)}}{cmd:average}, {cmd:atmean}, {cmd:observed}, {cmd:levels}, or empty{p_end}
{synopt:{cmd:r(dxlevels}{cmd:)}}levels specified in {cmd:dx()}{p_end}
{synopt:{cmd:r(centered}{cmd:)}}{cmd:centered} or empty{p_end}
{synopt:{cmd:r(normalize}{cmd:)}}{cmd:normalize} or empty{p_end}
{synopt:{cmd:r(atnames)}}variable names from {cmd:at()}{p_end}
{synopt:{cmd:r(wtype)}}weight type{p_end}
{synopt:{cmd:r(wexp)}}weight expression{p_end}
{synopt:{cmd:r(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:r(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:r(vcetype)}}title used to label Std. err.{p_end}
{synopt:{cmd:r(ifgenerate)}}names of variables containing IFs{p_end}
{synopt:{cmd:r(iftype)}}{cmd:IF} or {cmd:RIF} (or empty){p_end}
{synopt:{cmd:r(ifscaling)}}{cmd:total} or {cmd:mean} (or empty){p_end}

{p2col 5 23 26 2: Matrices}{p_end}
{synopt:{cmd:r(b)}}coefficient vector{p_end}
{synopt:{cmd:r(V)}}variance-covariance matrix of the estimators{p_end}
{synopt:{cmd:r(at)}}matrix of values from {cmd:at()}{p_end}
{synopt:{cmd:r(levels}{it:#}{cmd:)}}values and counts of evaluation levels used for term {it:#}{p_end}
{synopt:{cmd:r(levels)}}vector matching {cmd:r(b)} containing levels from {cmd:dx(levels)} or {opt dx(numlist)}{p_end}
{synopt:{cmd:r(table)}}information from the coefficient table{p_end}

{pstd}
If {cmd:post} is specified, results are stored in {cmd:e()} rather than
{cmd:r()}, and function {cmd:e(sample)} that marks the estimation sample is
added. An exception is {cmd:r(table)}, which will remain in {cmd:r()}; see 
help {helpb _coef_table} or {helpb ereturn} for details in {cmd:r(table)}.


{marker author}{...}
{title:Author}

{pstd}
    Ben Jann, University of Bern, ben.jann@unibe.ch

{pstd}
    Thanks for citing this software as

{phang2}
    Jann, B., K.B. Karlson. 2022. Estimation of marginal odds ratios. University
    of Bern Social Sciences Working Papers 44. Available from
    {browse "https://ideas.repec.org/p/bss/wpaper/44.html"}.
    {p_end}
{pstd}
    or
    {p_end}
{phang2}
    Jann, B. 2022. lnmor: Stata module to compute marginal odds
    ratios after model estimation. Available from {browse "http://github.com/benjann/lnmor/"}.


{marker alsosee}{...}
{title:Also see}

{psee}
    Online:  help for
    {helpb logit}, {helpb probit}, {helpb ipwlogit} (if installed), {helpb riflogit} (if installed)
