*! version 1.1.5  16jan2023  Ben Jann

program lnmor, properties(or)
    version 15
    if replay() {
        if "`e(cmd)'"!="lnmor" {
            if `"`e(cmd)'"'=="mi estimate" & `"`e(cmd_mi)'"'=="lnmor" {
                mi estimate `0'
                exit
            }
            error 301
        }
        Replay `0'
        exit
    }
    local version : di "version " string(_caller()) ":"
    _parse_opts `0' // returns 00, diopts, post, prefix, cmdline; may update 0
    // application with prefix command
    if "`prefix'"!="" {
        tempname ecurrent
        _estimates hold `ecurrent', restore
        if "`prefix'"=="mi" {
            // mi: using display routine of mi estimate
            `version' `00'
        }
        else {
            // svy/bootstrap/jackknife: using own display routine
            if "`prefix'"=="svyr" {
                nobreak {
                    capt noisily break {
                        `version' `00'
                    }
                    if _rc {
                        local rc = _rc
                        capt mata mata drop _LNMOR_TMP_IFs
                        exit `rc'
                    }
                }
                _ereturn_svy
            }
            else if "`prefix'"=="svyb" {
                `version' `00'
                _ereturn_svy
            }
            else { // bootstrap/jackknife
                `version' `00'
            }
            Replay, `diopts'
            Describe_newvars `e(ifgenerate)'
        }
        _ereturn_cmdline `0'
        _ereturn_est_cmdline `cmdline'
        _estimates unhold `ecurrent', not
        exit
    }
    // application without prefix command
    Estimate `00'
    _return_cmdline `0'
    _return_est_cmdline `cmdline'
    if "`post'"!="" {
        _postrtoe, all
        Replay, `diopts'
        Describe_newvars `e(ifgenerate)'
        exit
    }
    Replay_from_r, `diopts'
    Describe_newvars `r(ifgenerate)'
end

program _ereturn_cmdline, eclass
    eret local cmdline `"lnmor `0'"'
end

program _ereturn_est_cmdline, eclass
    eret local est_cmdline `"`0'"'
end

program _return_cmdline, rclass
    return add
    return local cmdline `"lnmor `0'"'
end

program _return_est_cmdline, rclass
    return add
    return local est_cmdline `"`0'"'
end

program _parse_opts
    // check for _lnmor() and run estimation command if needed
    _parse comma lhs 0 : 0
    syntax [, _lnmor(str asis) _prefix(str asis) * ]
    local has_lnmor = `"`_lnmor'"'!=""
    if `has_lnmor' {
        // run estimation command
        local estcmd `lhs'
        if `"`options'"'!="" local estcmd `estcmd', `options'
        if `"`_prefix'"'!="" local estcmd `_prefix': `estcmd'
        `estcmd'
        // update syntax
        local 0 `_lnmor'
        c_local 0 `0' // update returned cmdline
        _parse comma lhs 0 : 0
    }
    // parse options
    syntax [, vce(passthru) post or noHEADer noTABle NOSE /*
        */ IFGENerate(passthru) RIFgenerate(passthru) * ]
    _get_diopts diopts options, `options'
    local diopts `or' `header' `table' `diopts'
    // multiple imputation
    if `"`e(mi)'"'=="mi" {
        _parse_miopts, `options'
        local miopts cmdok `miopts' `post' `diopts'
        if `"`e(cmd)'"'=="mi estimate" local cmd_mi cmd_mi
        else                           local cmd_mi cmd
        if `"`e(`cmd_mi')'"'=="lnmor" {
            // results in memory are from lnmor
            local cmdline `"`e(est_cmdline)'"'
        }
        else {
            _check_source_model `e(`cmd_mi')'
            local cmdline `"`e(cmdline_mi)'"'
        }
        c_local cmdline `"`cmdline'"'
        if `"`ifgenerate'`rifgenerate'"'!="" {
            di as err "{bf:ifgenerate()} not allowed after {bf:mi estimate}"
            exit 198
        }
        if `"`nose'"'!="" {
            di as err "{bf:nose} not allowed after {bf:mi estimate}"
            exit 198
        }
        _parse_opts_vce, `vce' // returns prefix if bootstrap or jackknife
        if "`prefix'"!="" {
            di as err "{bf:vce(`prefix')} not allowed after {bf:mi estimate}"
            exit 198
        }
        _on_colon_parse `cmdline' // remove -mi estimate-
        if `"`e(prefix)'"'!="" {
            // move prefix (e.g. svy) into option
            _on_colon_parse `s(after)'
            local _prefix _prefix(`s(before)')
        }
        _parse_estcmd 0 `s(after)' // returns estcmd
        c_local 00 mi estimate, `miopts':/*
            */ lnmor `estcmd' _lnmor(`lhs', post `options') `_prefix'
        c_local prefix "mi"
        exit
    }
    // confirm that e() contains valid model and collect command line
    if !`has_lnmor' & `"`e(cmd)'"'=="lnmor" {
        // results in memory are from lnmor
        local cmdline `"`e(est_cmdline)'"'
    }
    else {
        _check_source_model `e(cmd)'
        local cmdline `"`e(cmdline)'"'
    }
    c_local cmdline `"`cmdline'"'
    // svy
    if `"`e(prefix)'"'=="svy" {
        if `"`vce'"'!="" {
            di as err "{bf:vce()} not allowed after {bf:svy}"
            exit 198
        }
        local vcetype `"`e(vce)'"'
        if `"`vcetype'"'=="linearized" local svytype "svyr"
        else                           local svytype "svyb"
        _on_colon_parse `cmdline'
        _parse_svy `s(before)'     // returns svy
        _parse_estcmd 0 `s(after)' // returns estcmd
        if "`svytype'"=="svyr" {
            // linearized VCE
            if `"`nose'"'!="" {
                di as err "{bf:nose} not allowed after {bf:svy `vcetype'}"
                exit 198
            }
            c_local 00 `svy' noheader notable:/*
                */ _lnmor_`svytype' estimate `estcmd'/*
                */ _lnmor(`lhs', post saveifuninmata/*
                */ `ifgenerate' `rifgenerate' `options')
        }
        else {
            // replication-based VCE
            if `"`ifgenerate'`rifgenerate'"'!="" {
                di as err `"{bf:ifgenerate()} not allowed after {bf:svy `vcetype'}"'
                exit 198
            }
            c_local 00 `svy' noheader notable:/*
                */ _lnmor_`svytype' `estcmd'/*
                */ _lnmor(`lhs', post nose `options')
        }
        c_local diopts `diopts'
        c_local prefix "`svytype'"
        exit
    }
    // check for vce(bootstrap) or vce(jackknife)
    _parse_opts_vce, `vce' // returns prefix vcelevel
    if "`prefix'"=="" {
        if `"`e(cmd)'"'=="lnmor" {
            // results in memory are from lnmor; must refit the original model
            di as txt `"(refitting {bf:`e(est_cmd)'} model)"'
            quietly `cmdline'
        }
        c_local 00 `lhs', `vce' `nose' `ifgenerate' `rifgenerate' `options'
        c_local diopts `diopts'
        c_local post `post'
        c_local prefix ""
        exit
    }
    // bootstrap / jackknife
    if `"`ifgenerate'`rifgenerate'"'!="" {
        di as err "{bf:ifgenerate()} not allowed with replication based VCE"
        exit 198
    }
    if `"`e(wtype)'"'=="fweight" {
        di as err "{bf:vce(`prefix')} not allowed with {bf:fweight}s"
        exit 198
    }
    _parse_diopts_level, `diopts' // returns level diopts
    if `"`vcelevel'"'!="" local level `vcelevel' // vcelevel takes precedence
    if "`prefix'"=="jackknife" local ropts jkopts
    else                       local ropts bootopts
    _parse_estcmd 1 `cmdline' // returns estcmd
    c_local 00 _vce_parserun lnmor, noeqlist wtypes(pw iw)/*
        */ `ropts'(noheader notable force):/*
        */ `estcmd' `vce' `level' _lnmor(`lhs', post nose `options')
    c_local diopts `level' `diopts'
    c_local prefix "`prefix'"
end

program _parse_svy
    _parse comma lhs 0 : 0
    syntax [, * ]
    c_local svy `lhs', `options'
end

program _ereturn_svy, eclass
    eret local cmdname ""
    eret local command ""
    eret local predict ""
end

program _parse_miopts
    syntax [, MIopts(str) * ]
    c_local miopts `miopts'
    c_local options `options'
end

program _parse_estcmd // remove or; possibly remove vce, cluster, robust, level
    gettoken remove 0 : 0
    if `remove' local rmopts or vce(passthru) CLuster(passthru) Robust Level(passthru)
    else        local rmopts or
    _parse comma lhs 0 : 0
    syntax [, `rmopts' * ]
    c_local estcmd `lhs', `options'
end

program _parse_opts_vce
    syntax [, vce(str) ]
    _parse comma v 0 : vce
    syntax [, Level(passthru) * ]
    local l = strlen(`"`v'"')
    if      `"`v'"'==substr("bootstrap",1,max(4,`l')) local prefix bootstrap
    else if `"`v'"'==substr("jackknife",1,max(4,`l')) local prefix jackknife
    c_local prefix `prefix'
    c_local vcelevel `level'
end

program _parse_diopts_level
    syntax [, Level(passthru) * ]
    c_local level `level'
    c_local diopts `options'
end

program _check_source_model
    if !inlist(`"`0'"',"logit","probit") {
        di as err "last estimates not found; " /*
            */ "{bf:lnmor} only allowed after {bf:logit} or {bf:probit}"
        exit 301
    }
end

program _postrtoe, eclass
    syntax [, all ]
    // b V depvar N wtype wexp
    if "`all'"!="" {
        tempname touse
        qui gen byte `touse' = e(sample)
    }
    tempname b
    matrix `b' = r(b)
    capt confirm matrix r(V)
    if _rc==1 exit _rc
    if _rc==0 {
        tempname V
        matrix `V' = r(V)
    }
    eret post `b' `V' [`r(wtype)'`r(wexp)'], depname(`r(depvar)') /*
        */ obs(`r(N)') esample(`touse')
    // scalars
    local rscalars: r(scalars)
    local drop N
    local rscalars: list rscalars - drop
    foreach r of local rscalars {
        eret scalar `r' = r(`r')
    }
    // macros
    local rmacros: r(macros)
    local drop depvar wtype wexp
    local rmacros: list rmacros - drop
    foreach r of local rmacros {
        eret local `r' `"`r(`r')'"'
    }
    // matrices
    local rmatrices: r(matrices)
    local drop b V
    local rmatrices: list rmatrices - drop
    foreach r of local rmatrices {
        matrix `b' = r(`r')
        eret matrix `r' = `b'
    }
end

program Replay, rclass
    syntax [, or noHEADer noTABle eform(passthru) * ]
    if "`or'"!="" & `"`eform'"'=="" local eform eform(Odds Ratio)
    if "`header'"=="" {
        if `"`eform'"'!="" local title "Marginal odds ratio"
        else               local title "Marginal log odds ratio"
        local hflex 1
        if      c(stata_version)<17            local hflex 0
        else if d(`c(born_date)')<d(13jul2021) local hflex 0
        local w1 17
        local c1 49
        local c2 = `c1' + `w1' + 1
        local w2 10
        local c3 = `c2' + 2
        if `hflex' local headopts head2left(`w1') head2right(`w2')
        else       local headopts
        _coef_table_header, nomodeltest title(`title') `headopts'
        if `hflex' {
            // if _coef_table_header used more space than allocated
            local offset1 = max(0, `s(head2_left)' - `w1')
            local offset2 = max(0, `s(head2_right)' - `w2')
            local c1 = `c1' - `offset1' - `offset2'
            local c2 = `c2' - `offset2'
        }
        if `"`e(subsample)'"'!="" {
            di as txt _col(`c1') "Subsample n. obs" _col(`c2') "=" /*
                */ _col(`c3') as res %`w2'.0gc e(N_subsmp)
        }
        di as txt _col(`c1') "Command" _col(`c2') "=" /*
            */ _col(`c3') as res %`w2's e(est_cmd)
        if `"`e(dxtype)'"'!="" {
            di as txt _col(`c1') "Type of dx()" _col(`c2') "=" /*
                */ _col(`c3') as res %`w2's e(dxtype)
        }
        if `"`e(delta)'"'!="" {
            di as txt _col(`c1') "Value of delta()" _col(`c2') "=" /*
                */ _col(`c3') as res %`w2'.0g e(delta)
            di as txt _col(`c1') "Centered" _col(`c2') "=" _col(`c3') /*
                */ as res %`w2's cond(`"`e(centered)'"'!="","yes","no")
            di as txt _col(`c1') "Normalized" _col(`c2') "=" _col(`c3') /*
                */ as res %`w2's cond(`"`e(normalize)'"'!="","yes","no")
        }
        if `"`e(atvars)'"'!="" {
            di as txt "Evaluated at:"
            local atvars `"`e(atvars)'"'
            local K = e(k_eq)
            forv i = 1/`K' {
                di as res %5s "`i'" as txt ": " _c
                local j 0
                foreach v of local atvars {
                    if `j' di as txt ", " _c
                    local ++j
                    di as txt "`v' = " as res el(e(at), `i',`j') _c
                }
                di ""
            }
        }
        di ""
    }
    if "`table'"=="" {
        eret di, `eform' `options' /* note: eform does not seem to work if e(V)
                                      is missing */
        return add
        local dxtype `"`e(dxtype)'"'
        if `"`dxtype'"'=="" exit
        di as txt "Terms affected by dx():" _c
        forv i = 1/`e(nterms)' {
            if e(dx`i')==1 di as res " `e(term`i')'" _c
        }
        di ""
        if `"`dxtype'"'=="levels" {
            di as txt "Levels of dx(): " as res `"`e(dxlevels)'"'
        }
    }
end

program Replay_from_r, rclass
    tempname ecurrent
    _estimates hold `ecurrent', restore
    _postrtoe
    return add
    Replay `0'
    return add
end

program Describe_newvars
    if `"`0'"'=="" exit
    tempname rcurrent
    _return hold `rcurrent'
    describe `0'
     _return restore `rcurrent'
end

program Estimate, rclass
    syntax varlist(numeric fv) [, nowarn CONStant NOTBAL TBAL /*
        */ kmax(numlist int max=1 >1 missingokay) /*
        */ dx DX2(passthru) delta DELTA2(passthru) CENTERed NORMalize /*
        */ EPSilon /* undocumented
        */ at(passthru) atmax(int 50) /*
        */ SUBSAMPle(str) /*
        */ noDOTs NOSE saveifuninmata /*
        */ IFGENerate(str) RIFgenerate(str) IFScaling(str) replace ]
    if "`kmax'"==""   local kmax 100
    if "`warn'"!=""   local warn qui
    if "`notbal'"!="" local tbal notbal
    
    // ifgen
    if `"`rifgenerate'"'!="" {
        if `"`ifgenerate'"'!="" {
            di as err "{bf:ifgenerate()} and {bf:rifgenerate()} not both allowed"
            exit 198
        }
        local ifgenerate `"`rifgenerate'"'
        local iftype     "RIF"
    }
    else if `"`ifgenerate'"'!="" local iftype "IF"
    capt _parse_ifscaling, `ifscaling'
    if _rc==1 exit _rc
    if _rc {
        di as err "{bf:ifscaling()}: invalid specification"
        exit 198
    }
    
    // parse dx() option
    if "`epsilon'"!="" {
        local delta     delta
        local delta2    = 2*exp(log(c(epsdouble))/3)
        local centered  centered
        local normalize normalize
    }
    else if `"`delta2'"'!="" {
        local delta delta
        _parse_delta, `delta2' // returns delta2
    }
    if "`delta'"!="" {
        local dx dx
        if `"`delta2'"'=="" local delta2 1
    }
    else {
        local centered
        local normalize
    }
    if `"`dx2'"'!="" local dx dx
    if "`dx'"!="" {
        _parse_dx, `dx2' // returns dxtype dxlevels
    }
    if "`delta2'"=="0" {
        local npred 1
        local centered
        local normalize
    }
    else if "`delta2'"!="" local npred 2
    else                   local npred 1
    
    // collect some info on original model
    local est_N       = e(N)
    local est_cmd     `"`e(cmd)'"'
    local clustvar    `"`e(clustvar)'"'
    if `"`clustvar'"'!="" {
        local N_clust = e(N_clust)
        local vceopt vce(cluster `clustvar')
    }
    _collect_model_info // returns mvars mnames mfv mcons mk
    
    // preserve model and select sample
    tempname ecurrent
    _estimates hold `ecurrent', restore copy
    preserve
    qui keep if e(sample)
    if `"`subsample'"'!="" {
        tempvar subuse
        _set_subsamp `subuse' `subsample'
    }
    else local subuse
    
    // weights
    tempname sum_w0 sum_w
    local weight `"`e(wtype)'"'
    local exp    `"`e(wexp)'"'
    if "`weight'"!="" {
        local wvar = substr(`"`exp'"', 3, .) // strip "= "
        capt confirm var `wvar'
        if _rc==1 exit _rc
        if _rc {
            tempname wvar
            qui gen double `wvar' `exp'
        }
        local awgt "[aw=`wvar']"
        su `wvar', meanonly
        local N0        = cond("`weight'"=="fweight", r(sum), r(N))
        scalar `sum_w0' = r(sum)
    }
    else {
        qui count
        local N0        = r(N)
        scalar `sum_w0' = r(N)
    }
    if `N0'!=`est_N' {
        di as error "something is wrong; inconsistent estimation sample"
        exit 498
    }
    if "`subuse'"!="" {
        if "`weight'"!="" {
            su `wvar' if `subuse', meanonly
            local N        = cond("`weight'"=="fweight", r(sum), r(N))
            scalar `sum_w' = r(sum)
        }
        else {
            qui count if `subuse'
            local N        = r(N)
            scalar `sum_w' = r(N)
        }
    }
    else {
        local N          `N0'
        scalar `sum_w' = `sum_w0'
    }
    if `N'==0 error 2000
    
    // process varlist
    fvexpand `varlist' // (use full sample)
    _collect_fvinfo `r(varlist)' // returns names nterms term# type# name# lvls# bn#
    local tmp: list dups names
    if `"`tmp'"'!="" {
        di as error "inconsistent varlist; duplicate variables not allowed"
        exit 198
    }
    local tmp: list names - mnames
    if `"`tmp'"'!="" {
        gettoken tmp : tmp
        di as error "{bf:`tmp'} not found in list of covariates"
        exit 111
    }
    local ndx 0
    forv j = 1/`nterms' {
        local mfv`j': list name`j' in mfv
        if "`type`j''"=="factor" {
            if !`mfv`j'' {
                `warn' di as txt "Warning: {bf:`name`j''} is a continuous" /*
                    */ " variable in the original model"
            }
            continue
        }
        if `mfv`j'' {
            `warn' di as txt "Warning: {bf:`name`j''} is a factor" /*
                */ " variable in the original model"
            continue
        }
        if "`type`j''"!="variable" continue
        if "`dx'"=="" continue
        local dxtype`j' "`dxtype'"
        local ++ndx
    }
    if `ndx'>1 {
        if "`dxtype'"=="levels"  & `"`dxlevels'"'=="" {
            di as err "{bf:dx(levels)} not allowed with multiple continuous terms"
            exit 198
        }
    }
    
    // constant
    if "`constant'"!="" {
        if `nterms'>1 {
            di as err "{bf:constant} not allowed with multiple terms"
            exit 198
        }
        if "`dxtype1'"!="" {
            di as err "{bf:constant} cannot be combined with {bf:dx()}"
            exit 198
        }
    }
    
    // at
    tempname AT
    _parse_at `AT', `at' atmax(`atmax') mnames(`mnames') subuse(`subuse')
        /* returns AT K at */
    local hasat = `"`at'"'!=""
    if `hasat' {
        local tmp: list at & names
        if `"`tmp'"'!="" {
            di as error "{it:varlist} and {bf:at()} must be distinct"
            exit 198
        }
    }
    
    // determine treatment levels
    forv j = 1/`nterms' {
        tempname levels`j'
        mata: _get_levels("`j'", "`subuse'")
            /* fills in levels#; returns k# cname# */
    }
    
    // prepare VCE
    if "`saveifuninmata'"!="" local nose
    local vce = "`nose'"==""
    if `vce' {
        // tempvars for outcome IFs
        forv k = 1 / `K' {
            forv j = 1/`nterms' {
                if "`dxtype`j''"=="levels" {
                    mata: _mktmpnames("IF`j'_`k'", `k`j'')
                }
                else if "`dxtype`j''"!="" {
                    tempvar IF`j'_`k'
                }
                else {
                    local tmp: list sizeof term`j'
                    if "`constant'"!="" & !`bn`j'' local ++tmp
                    mata: _mktmpnames("IF`j'_`k'", `tmp')
                }
                local IFs `IFs' `IF`j'_`k''
            }
        }
        foreach v of local IFs {
            qui gen double `v' = 0
        }
        // parse ifgenerate()
        if `"`ifgenerate'"'!="" {
            // expand stub*; confirm (new) varnames
            _parse_ifgenerate `"`ifgenerate'"' `:list sizeof IFs' `replace'
        }
        // obtain model IFs
        mata: _mktmpnames("mIFs", `mk')
        _get_model_IF `mcons' "`mvars'" "`mIFs'"
    }
    else local ifgenerate
    
    // compute marginal ORs
    if "`dots'"=="" {
        di as txt _n "Enumerating predictions: " _c
        local lsize = min(78,c(linesize))
        local lpos 26
    }
    if `hasat' tempname B
    local k 0
    tempname b
    tempvar name0
    forv k = 1 / `K' {
        if `hasat' {
            if "`dots'"=="" _progress_info "`k':" `lpos' `lsize'
            local j 0
            foreach v of local at {
                local ++j
                qui replace `v' = `AT'[`k', `j']
            }
        }
        forv j = 1/`nterms' {
            if "`cname`j''"!="`name`j''" {
                rename `name`j'' `name0'
                rename `cname`j'' `name`j''
            }
            if "`dots'"=="" {
                _progress_info "`name`j''" `lpos' `lsize'
            }
            local opts subuse(`subuse') /*
                */ name(`name`j'') levels(`levels`j'') k(`k`j'') /*
                */ wvar(`wvar') sum_w(`sum_w') ifs(`IF`j'_`k'') mifs(`mIFs') /*
                */ mvars(`mvars') mcons(`mcons') estcmd(`est_cmd') /*
                */ lsize(`lsize') lpos(`lpos') `dots'
            if "`dxtype`j''"=="" {
                // fractional logit
                Estimate_fl, term(`term`j'') type(`type`j'') bn(`bn`j'') /*
                    */ mfv(`mfv`j'') lvls(`lvls`j'') tbal(`tbal') `constant' /*
                    */ `opts'
            }
            else if "`delta2'"=="" {
                // derivatives
                Estimate_dx, type(`dxtype`j'') `opts'
            }
            else {
                // discrete change effects
                Estimate_dc, type(`dxtype`j'') delta(`delta2') `centered' /*
                    */ `normalize' `opts'
            }
            matrix `b' = nullmat(`b'), r(b)
            if "`cname`j''"!="`name`j''" {
                rename `name`j'' `cname`j''
                rename `name0' `name`j''
            }
        }
        if `hasat' {
            matrix coleq `b' = "`k'"
            matrix `B' = nullmat(`B'), `b'
            matrix drop `b'
        }
    }
    if `hasat' local b `B'
    if "`dots'"=="" {
        _progress_info "done" `lpos' `lsize'
        di ""
    }
    
    // returns
    _ms_build_info `b' `awgt'
    tempname V
    matrix `V' = `b'' * `b' * 0
    if "`saveifuninmata'"!="" {
        local rank = colsof(`V')
        mata: _lnmor_restore(1)
        mata: *crexternal("_LNMOR_TMP_IFs") = st_data(., st_local("IFs"))
        mata: st_replacematrix(st_local("V"), I(`rank'))
    }
    else if `vce' {
        qui total `IFs' [`weight'`exp'], `vceopt'
        if "`weight'"=="iweight" {
            local N_tot = el(e(_N),1,1)
        }
        else local N_tot = e(N)
        if `N0'!=`N_tot' {
            di as error "something is wrong; inconsistent VCE sample"
            exit 498
        }
        mata: st_replacematrix(st_local("V"), st_matrix("e(V)"))
        local rank = e(rank)
        if `"`clustvar'"'!="" {
            local N_clust = e(N_clust)
            local evce `"`e(vce)'"'
            local vcetype `"`e(vcetype)'"'
        }
        else {
            local evce robust
            local vcetype Robust
        }
        mata: _lnmor_restore("`ifgenerate'"!="")
    }
    else {
        restore
        _estimates unhold `ecurrent'
    }
    
    // returns
    if `ndx' {
        if "`dxtype'"=="levels" {
            tempname levels
            matrix `levels' = `b' * .
        }
    }
    return matrix b           = `b'
    return matrix V           = `V'
    return scalar k_eq        = `K'
    return scalar k_eform     = `K'
    return local  cmd         "lnmor"
    return local  est_cmd     `"`est_cmd'"'
    return local  title       "Marginal (log) odds ratio"
    return scalar N           = `N0'
    return scalar sum_w       = `sum_w0'
    if "`subuse'"!="" {
        return scalar N_subsmp     = `N'
        return scalar sum_w_subsmp = `sum_w'
        return local subsample `"`subsample'"'
    }
    return local  depvar      `"`e(depvar)'"'
    return local  wtype       `"`weight'"'
    return local  wexp        `"`exp'"'
    return local  tbal        "`tbal'"
    return scalar nterms      = `nterms'
    if `ndx' {
        return local dxtype     "`dxtype'"
        return local dxlevels   "`dxlevels'"
        if "`delta2'"!="" {
            return scalar delta    = `delta2'
            return local centered  "`centered'"
            return local normalize "`normalize'"
        }
        if "`dxtype'"=="levels" {
            local i1 1
            forv k = 1 / `K' {
                forv j = 1/`nterms' {
                    if "`dxtype`j''"!="" {
                        matrix `levels'[1,`i1'] = `levels`j''[1...,1]'
                        local i1 = `i1' + rowsof(`levels`j'')
                    }
                    else {
                        local i1 = `i1' + `:list sizeof term`j''
                    }
                }
            }
            return matrix levels = `levels'
        }
    }
    forv j = 1/`nterms' {
        return local  name`j'   "`name`j''"
        return local  term`j'   "`term`j''"
        return local  type`j'   "`type`j''"
        return scalar k`j'      = `k`j''
        return matrix levels`j' = `levels`j''
        return scalar dx`j'     = "`dxtype`j''"!=""
    }
    if `hasat' {
        return local atvars     "`at'"
        return matrix at        = `AT'
    }
    if `vce' {
        return local clustvar   `"`clustvar'"'
        if `"`clustvar'"'!="" {
            return scalar N_clust = `N_clust'
            return scalar df_r    = `N_clust' - 1
        }
        else {
            return scalar df_r = `N0' - 1
        }
        return local vce     `"`evce'"'
        return local vcetype `"`vcetype'"'
        return scalar rank   = `rank'
    }
    else {
        return scalar df_r = `N0' - 1
    }
    if "`ifgenerate'"!="" {
        tempname b
        mat `b' = return(b)
        local coln: coln `b'
        local j 0
        foreach v of local ifgenerate {
            local ++j
            gettoken IF IFs : IFs
            if "`IF'"=="" continue, break
            if "`iftype'"=="RIF" {
                qui replace `IF' = `IF' + `b'[1,`j']/`sum_w0'
            }
            if "`ifscaling'"=="mean" {
                qui replace `IF'  = `IF' * `sum_w0'
            }
            gettoken nm coln : coln
            lab var `IF' "`iftype' of `nm'"
            capt confirm variable `v', exact
            if _rc==1 exit _rc
            if _rc==0 drop `v'
            rename `IF' `v'
        }
        return local ifgenerate "`ifgenerate'"
        return local iftype "`iftype'"
        return local ifscaling "`ifscaling'"
    }
end

program _parse_ifscaling
    syntax [, Mean Total ]
    local ifscaling `mean' `total'
    if `: list sizeof ifscaling'>1 exit 198
    if `"`ifscaling'"'=="" local ifscaling total
    c_local ifscaling `ifscaling'
end

program _parse_delta
    syntax [, delta2(str) ]
    if `"`delta2'"'!="" {
        capt confirm number `delta2'
        if _rc==1 exit _rc
        if _rc {
            di as err "delta() invalid -- invalid number"
            exit 198
        }
    }
    c_local delta2 `delta2'
end

program _parse_dx
    syntax [, dx2(str) ]
    if `"`dx2'"'=="" local dx2 "average"
    if `:list sizeof dx2'==1 {
        if `"`dx2'"'=="." local dx2 "observed"
        _parse_dx_type, `dx2' // returns dxtype
    }
    if "`dxtype'"=="" {
        capt numlist `"`dx2'"', sort
        if _rc==1 exit _rc
        if _rc {
            di as err "{bf:dx()}: invalid specification"
            exit 198
        }
        local dxtype "levels"
        local dxlevels "`r(numlist)'"
    }
    c_local dxtype `dxtype'
    c_local dxlevels: list uniq dxlevels
end

program _parse_dx_type
    capt syntax [, ATMean AVErage OBServed LEVels ]
    if _rc==1 exit _rc
    if _rc    exit
    c_local dxtype `atmean' `average' `observed' `levels'
end

program _set_subsamp
    gettoken subuse 0 : 0
    syntax [varname(numeric default=none)] [if]
    mark `subuse' `if'
    if "`varlist'"!="" {
        markout `subuse' `varlist'
        qui replace `subuse' = 0 if `varlist'==0
    }
end

program _progress_info
    args s lpos lsize
    local l = strlen("`s'")
    if (`lpos'+`l'-1)>`lsize' {
        local l0 = `lsize' - `lpos' + 1
        local s0 = substr("`s'", 1, `l0')
        di as txt "`s0'"
        local lpos 1
        local s = substr("`s'", `l0'+1, .)
        local l = `l' - `l0'
        
    }
    di as txt "`s'" _c
    local lpos = `lpos' + `l'
    c_local lpos `lpos'
end

program _collect_model_info
    _ms_lf_info
    c_local mk    `r(k1)'
    c_local mcons `r(cons1)'
    c_local mvars `r(varlist1)'
    foreach t in `r(varlist1)' {
        _ms_parse_parts `t'
        if inlist(r(type), "interaction", "product") {
            forv j = 1/`r(k_names)' {
                local names `names' `r(name`j')'
                if !inlist(r(op`j'),"c","co") {
                    local fv `fv' `r(name`j')'
                }
            }
        }
        else {
            if r(type)=="factor" {
                local fv `fv' `r(name)'
            }
            local names `names' `r(name)'
        }
    }
    c_local mnames: list uniq names
    c_local mfv: list uniq fv
end

program _collect_fvinfo
    local J 0
    local bn 1
    local next 1
    foreach t of local 0 {
         _ms_parse_parts `t'
         local Type `r(type)'
         if "`Type'"=="factor" {
             local Name `r(name)'
         }
         else if "`Type'"=="variable" {
             local Name `r(name)'
             if "`type'"=="interaction" local Type "interaction"
         }
         else if "`Type'"=="interaction" {
             _collect_fvinfo_interaction `t' // returns Name or error
             if "`type'"=="variable" local type "interaction"
         }
         else {
             di as err "`Type' terms not allowed in {it:varlist}"
             exit 198
         }
         if      "`Name'"!="`name'"   local next 1
         else if "`Type'"!="`type'"   local next 1
         else                         local next 0
         if `next' {
             if `J' {
                 _collect_fvinfo_bn "`type'" "`term'" `bn'
                 c_local term`J' "`term'"
                 c_local type`J' "`type'"
                 c_local name`J' "`name'"
                 c_local lvls`J' "`lvls'"
                 c_local bn`J'   `bn'
                 local names `names' `name'
             }
             local term
             local type
             local name
             local lvls
             local bn 1
             local ++J
         }
         local term `term' `t'
         local lvls `lvls' `r(level)'
         local type `Type'
         local name `Name'
         if r(base)==1 local bn 0
    }
    _collect_fvinfo_bn "`type'" "`term'" `bn'
    c_local term`J' "`term'"
    c_local type`J' "`type'"
    c_local name`J' "`name'"
    c_local lvls`J' "`lvls'"
    c_local bn`J'   `bn'
    local names `names' `name'
    c_local names   `names'
    c_local nterms  `J'
end

program _collect_fvinfo_interaction
    local k = r(k_names)
    local names
    forv i=1/`k' {
        local names `names' `r(name`i')'
        if substr(`"`r(op`i')'"',1,1)!="c" {
            di as err "interactions involving factor variables "/*
                */ "not allowed in {it:varlist}"
            exit 198
        }
    }
    local names: list uniq names
    if `: list sizeof names'>1 {
        di as err "interactions involving different variables "/*
                */ "not allowed in {it:varlist}"
        exit 198
    }
    c_local Name `names'
end

program _collect_fvinfo_bn
    args type term bn
    if "`type'"!="factor" {
        // non-factor variable cannot have bn
        c_local bn 0
        exit
    }
    if !`bn' exit
    // check bn status in case of single
    if `: list sizeof term'>1 exit
    if substr("`term'", strpos("`term'",".")-2,2)=="bn" exit
    c_local bn 0
end

program _parse_at
    syntax anything(name=AT), atmax(str) [ at(str) mnames(str) subuse(str) ]
    // no at() option
    if `"`at'"'=="" {
        c_local AT ""
        c_local at ""
        c_local K 1
        exit
    }
    // case 1: varnam = numlist [varname = numlist ...]
    local haseq = strpos(`"`at'"', "=")
    if `haseq' {
        local j 0
        while (`"`at'"'!="") {
            gettoken t at : at, parse(" =")
            if `"`t'"'=="=" {
                if `j'==0 {
                    if `"`lag'"'=="" | `"`lag2'"'!="" {
                        di as err "invalid specification of {bf:at()}"
                        exit 198
                    }
                }
                local levels`j' `"`lag2'"'
                local ++j
                local at`j' `"`lag'"'
                local lag2
                local lag
                local space
                continue
            }
            if `"`lag'"'!="" {
                local lag2 `"`lag2'`space'`lag'"'
                local space " "
            }
            local lag `"`t'"'
        }
        if `"`lag'"'!="" {
            local lag2 `"`lag2'`space'`lag'"'
        }
        local levels`j' `"`lag2'"'
        loca J `j'
        local at0
        local K 1
        forv j=1/`J' {
            local 0 `"`at`j'', at(`levels`j'')"'
            capt n syntax varname, at(numlist sort)
            if _rc==1 exit _rc
            if _rc {
                di as err "invalid specification of {bf:at()}"
                exit 198
            }
            local at0 `at0' `varlist'
            local at`j' `varlist'
            local levels`j' `at'
            local k`j': list sizeof at
            local K = `K' * `k`j''
        }
        local at `at0'
    }
    // case 2: varlist
    else {
        local 0 `"`at'"'
        syntax varlist
        local at `varlist'
        local J: list sizeof at
        local K .
    }
    // checks
    local tmp: list dups at
    if `"`tmp'"'!="" {
        di as error "duplicate variables not allowed in {bf:at()}"
        exit 198
    }
    local tmp: list at - mnames
    if `"`tmp'"'!="" {
        gettoken tmp : tmp
        di as error "{bf:`tmp'} not found in list of covariates"
        exit 111
    }
    // create matrix of patterns
    mata: _at_expand(`K', `J', `atmax', "`subuse'")
    mat coln `AT' = `at'
    c_local at `at'
    c_local K `K'
end

program _parse_ifgenerate
    args generate k replace
    if strpos(`"`generate'"',"*") {
        gettoken stub rest : generate, parse("* ")
        if `"`rest'"'!="*" {
            di as err "ifgenerate() invalid; " /*
                */ "must specify {it:stub}{bf:*} or {it:namelist}"
            exit 198
        }
        confirm name `stub'
        local generate
        forv i = 1/`k' {
            local generate `generate' `stub'`i'
        }
    }
    confirm names `generate'
    if "`replace'"=="" {
        confirm new variable `generate'
    }
    c_local ifgenerate `generate'
end

program _get_model_IF
    args cons xvars IFs
    tempname sc
    qui predict double `sc', score
    capt confirm matrix e(V_modelbased)
    if _rc==1 exit _rc
    if _rc local V V
    else   local V V_modelbased
    mata: _get_model_IF("e(`V')", "`sc'", `cons', st_local("xvars"), /*
        */ st_local("IFs"))
end

program Estimate_fl, rclass
    syntax, term(str) type(str) name(str) bn(str) levels(str) sum_w(str) /*
        */ k(str) estcmd(str) mcons(str) mfv(str) [ constant wvar(str) /*
        */ ifs(str) mifs(str) mvars(str) subuse(str) lsize(str) lpos(str) /*
        */ lvls(str) tbal(str) nodots ]
    if "`subuse'"!="" local iff "if `subuse'"
    
    // check whether outcome model has constant (bn=1 only possible if
    // type=factor) and create additional tempvar for IF of constant, if
    // needed
    if `bn' local nocons noconstant
    local vce = `"`ifs'"'!=""
    if `vce' {
        if "`nocons'`constant'"=="" {
            tempname IFcns
            qui gen double `IFcns' = 0
            local ifs `ifs' `IFcns'
        }
    }
    
    // whether to use balanced design or not
    if "`tbal'"=="" & "`type'"=="factor" {
        // apply tbal if fractional logit is saturated, i.e. if -term- 
        // covers all existing treatment levels
        mata: st_local("tmp", invtokens(strofreal(st_matrix("`levels'")[,1]')))
        if `:list tmp in lvls' local tbal tbal
    }
    
    // compute predictions and prepare IFs
    tempvar name0 vtmp
    rename `name' `name0'
    qui gen `:type `name0'' `name' = `name0' /* copy values so that st_view()
        will not get confused if the treatment is a factor variable */
    tempvar P W T
    qui gen double `P' = .
    qui gen double `W' = .
    qui gen double `T' = .
    mata: Estimate_fl(`k', `vce', "`subuse'")
    drop `name'
    
    // apply fractional logit and finalize IFs
    rename `T' `name'
    tempname ecurrent
    _estimates hold `ecurrent', restore
    su `P' in 1/`k', meanonly
    local hasvar = r(min)!=r(max)
    if `hasvar' {
        capt _nobs `P' [iw=`W'] in 1/`k', min(2)
        if _rc {
            di _n as err "cannot apply fractional logit; predictions for at"/*
                */ " least two (positively weighted) levels required"
            exit _rc
        }
        qui fracreg logit `P' `term' [iw=`W'] in 1/`k', `nocons'
        if `vce' {
            qui predict double `vtmp' in 1/`k'
            mata: _IF_fl_finalize(`k', "`nocons'"=="", "`subuse'")
        }
    }
    else if `vce' {
        foreach IF of local ifs {
            qui replace `IF' = 0
        }
    }
    drop `name'
    rename `name0' `name'
    
    // returns
    tempname b
    if `hasvar' {
        matrix `b' = e(b)
        if "`nocons'`constant'"=="" {
            matrix `b' = `b'[1,1..colsof(`b')-1]
        }
        mat coleq `b' = ""
    }
    else {
        local tmp: list sizeof term
        if "`constant'"!="" & !`bn' local ++tmp
        mat `b' = J(1,`tmp', 0)
        local tmp `term'
        if "`constant'"!="" & !`bn' local tmp `tmp' _cons
        mat coln `b' = `tmp'
    }
    return matrix b = `b'
    c_local lpos `lpos'
end

program Estimate_dx, rclass
    syntax, type(str) name(str) levels(str) sum_w(str) k(str) /*
        */ estcmd(str) mcons(str) [ wvar(str) ifs(str) /*
        */ mifs(str) mvars(str) subuse(str) lsize(str) lpos(str) nodots ]
    if "`subuse'"!="" local iff "if `subuse'"
    
    // helper vector for derivative of the linear predictor
    tempname dzb dz
    matrix `dzb' = e(b)
    _ms_dzb_dx `name', matrix(`dzb') // undocumented utility of -margins-
    matrix `dzb' = r(b)
    
    // helper vector for double derivative of the linear predictor  IF of mean
    local vce = `"`ifs'"'!=""
    if `vce' & "`type'"=="atmean" {
        tempname ddzb ddz
        _ms_dzb_dx `name', matrix(`dzb') // undocumented utility of -margins-
        matrix `ddzb' = r(b)
        tempvar muIF 
        qui gen double `muIF' = (`name' - `levels'[1,1]) / `sum_w' `iff'
    }
    
    // compute estimates and IFs
    tempname b
    tempvar name0 vtmp
    rename `name' `name0'
    qui gen double `name' = `name0'
    mata: Estimate_dx(`k', `vce', "`subuse'")
    drop `name'
    rename `name0' `name'
    
    // returns
    return matrix b = `b'
    c_local lpos `lpos'
end

program Estimate_dc, rclass
    syntax, delta(str) type(str) name(str) levels(str) sum_w(str) k(str) /*
        */ estcmd(str) mcons(str) [ centered NORMALIZE wvar(str) ifs(str) /*
        */ mifs(str) mvars(str) subuse(str) lsize(str) lpos(str) nodots ]
    if "`subuse'"!="" local iff "if `subuse'"
    
    // helper vector for derivative of the linear predictor and IF of mean
    local vce = `"`ifs'"'!=""
    if `vce' & "`type'"=="atmean" {
        tempname dzb dz
        matrix `dzb' = e(b)
        _ms_dzb_dx `name', matrix(`dzb') // undocumented utility of -margins-
        matrix `dzb' = r(b)
        tempvar muIF // IF of mean
        qui gen double `muIF' = (`name' - `levels'[1,1]) / `sum_w' `iff'
    }
    
    // compute estimates and IFs
    tempname b
    tempvar name0 vtmp
    rename `name' `name0'
    qui gen double `name' = `name0'
    mata: Estimate_dc(`k', `vce', "`subuse'")
    drop `name'
    rename `name0' `name'
    
    // returns
    return matrix b = `b'
    c_local lpos `lpos'
end

version 15
// string
local SS string scalar
local SR string rowvector
local SC string colvector
local SM string matrix
// real
local RS real scalar
local RC real colvector
local RR real rowvector
local RM real matrix
mata:
mata set matastrict on

void _at_expand(`RS' K0, `RS' J, `RS' atmax, `SS' subuse)
{
    `RS' j, k, K, K1, R
    `SR' at
    `RC' l
    `RM' AT
    pointer rowvector L
    
    // collect levels of no levels specified
    if (K0>=.) {
        at = tokens(st_local("at"))
        L = J(1,J,NULL)
        K1 = 1
        for (j=1; j<=J; j++) {
            L[j] = &(mm_unique(st_data(., at[j], subuse)))
            K1 = K1 * rows(*L[j])
            if (hasmissing(*L[j])) {
                stata(`"di as err "something is wrong; {bf:"' + at[j] +
                    `"} has missing values within estimation sample""')
                exit(error(498))
            }
        }
        st_local("K", strofreal(K1))
    }
    else K1 = K0
    if (K1>atmax) {
        stata(`"di as err "{bf:at()}: too many patterns (\`K'); allowed maximum is \`atmax'""')
        stata(`"di as err "use option {bf:atmax()} to change allowed maximum""')
        exit(error(498))
    }
    // construct matrix of combinations
    AT = J(K1, J, 1)
    R = 1; K = K1
    for (j=1; j<=J; j++) {
        if (K0>=.) l = *L[j]
        else       l = strtoreal(tokens(st_local("levels"+strofreal(j))))'
        k = rows(l)
        K = K / k
        AT[,j] = J(R, 1, mm_expand(l, K, 1, 1))
        R = R * k
    }
    st_matrix(st_local("AT"), AT)
    st_matrixrowstripe(st_local("AT"), (J(K1,1,""), strofreal(1::K1)))
}

void _get_levels(`SS' j, `SS' subuse)
{
    `SS' Name, vtype, dxtype, dxlevels
    `RS' k, kmax
    `RC' X, w, p, a, b
    `RM' L
    
    // setup
    Name     = st_local("name"+j)
    vtype    = st_local("type"+j)
    dxtype   = st_local("dxtype"+j)
    dxlevels = st_local("dxlevels")
    kmax     = strtoreal(st_local("kmax"))
    // determine levels
    if (dxtype=="levels" & dxlevels!="") { // dx(numlist)
        L = strtoreal(tokens(dxlevels)')
        k = rows(L)
        L = L, J(k, 1, .)
        st_matrix(st_local("levels"+j), L)
    }
    else if (dxtype=="observed") {
        L = (.,.)
        k = 1
    }
    else {
        // in case of subsamp(): use all data, but set weights of obs outside
        // subsamp to zero
        X = st_data(., Name)
        if (st_local("wvar")!="") w = st_data(., st_local("wvar"))
        else                      w = 1
        if (subuse!="")           w = w :* st_data(., subuse)
        if (dxtype=="atmean") {
            L = (quadsum(w==1 ? X : w:*X)/st_numscalar(st_local("sum_w")), .)
            k = 1
        }
        else {
            p = order(X, 1)
            _collate(X, p)
            if (w!=1) _collate(w, p)
            if (X[rows(X)]>=.) {
                stata(`"di as err "something is wrong; {bf:"' + Name +
                    `"} has missing values within estimation sample""')
                exit(error(498))
            }
            a = selectindex(_mm_unique_tag(X))
            k = rows(a)
            if (vtype!="factor" & k>kmax) { // apply binning if necessary
                printf("{txt}(%s has %g levels", Name, k)
                _get_levels_bin(X, w, kmax)
                a = selectindex(_mm_unique_tag(X))
                k = rows(a)
                printf("; using %g binned levels)\n", k)
                Name = st_tempname(1)
                st_store(., st_addvar("double", Name), X[invorder(p)])
            }
            L = X[a]
            b = selectindex(_mm_unique_tag(X, 1))
            if (w==1) L = L, ((b-a):+1)
            else      L = L, mm_diff(0 \ quadrunningsum(w)[b])
            if (dxtype=="levels") {
                st_local("dxlevels", invtokens(strofreal(L[,1])'))
            }
        }
    }
    // returns
    st_matrix(st_local("levels"+j), L)
    st_matrixrowstripe(st_local("levels"+j), (J(k, 1, ""), strofreal(1::k)))
    st_matrixcolstripe(st_local("levels"+j), (J(2, 1, ""), ("value","count")'))
    st_local("k"+j, strofreal(k))
    st_local("cname"+j, Name) // name of (possibly binned) variable
}

void _get_levels_bin(`RC' X, `RC' w, `RS' k)
{
    `RS' i, j, qj, a, b, e
    `RC' q, ww
    
    i = rows(X)
    a = X[1]; b = X[i]
    e = (b - a) / (2*(k-1))
    q = rangen(a-e, b+e, k+1)
    j = rows(q)-1
    for (;j;j--) {
        qj = q[j]
        b = i
        while (X[i]>qj) {
            a = i
            i--
            if (i==0) break
        }
        if (a>b) continue // no observations in bin
        if (w==1) X[|a\b|] = J(b-a+1, 1, mean(X[|a\b|]))
        else {
            ww = w[|a\b|]
            if (sum(ww)==0) X[|a\b|] = J(b-a+1, 1, mean(X[|a\b|]))
            else            X[|a\b|] = J(b-a+1, 1, mean(X[|a\b|], ww))
        }
    }
}

void _mktmpnames(`SS' nm, `RS' k)
{
    st_local(nm, invtokens(st_tempname(k)))
}

void _get_model_IF(`SS' V, `SS' score, `RS' cons,
    `SS' xvars, `SS' IFs)
{
    `RC' sc
    `RM' X, IF
    
    st_view(sc=., ., score)
    st_view(X=., ., xvars)
    st_view(IF=., ., st_addvar("double", tokens(IFs)))
    IF[.,.] = (sc:*X, J(1, cons, sc)) * st_matrix(V)'
}

void _lnmor_restore(`RS' ifgen)
{
    `SS' touse
    `SR' IFs
    `RM' IF
    
    if (ifgen) {
        IFs = tokens(st_local("IFs"))
        IF = st_data(., IFs)
    }
    stata("restore")
    stata("_estimates unhold " + st_local("ecurrent"))
    if (ifgen) {
        touse = st_tempname()
        stata("qui gen byte "+touse+" = e(sample)")
        st_store(., st_addvar("double", IFs), touse, IF)
    }
}

void Estimate_fl(`RS' k, `RS' vce, `SS' subuse)
{
    `RS' i, pbar, dots, probit
    `SS' levls
    `RC' b, w, p
    `RM' L
    // for vce
    `SS' mvars
    `RS' W, cons, mcons, mfv
    `RC' dp, S
    `RM' X, mIF, IF
    
    // setup
    dots   = st_local("dots")==""
    levls  = st_local("levels")
    L      = st_matrix(levls)
    probit = st_local("estcmd")=="probit" // default: logit
    // balanced treatment levels
    W = st_numscalar(st_local("sum_w"))
    if (st_local("tbal")=="tbal") L[,2] = J(k, 1, W/k)
    // weights
    w = st_local("wvar")!="" ? st_data(., st_local("wvar"), subuse) : 1
    // prepare VCE
    if (vce) {
        cons  = st_local("nocons")==""
        mcons = st_local("mcons")!="0"
        mvars = st_local("mvars")
        mfv   = st_local("mfv")=="1" // tvar is factor variable in model
        st_view(mIF=., ., st_local("mifs"))
        if (subuse!="") S = selectindex(st_data(.,subuse)) // subsample index
        else            S = .
        if (!mfv) st_view(X=., ., mvars, subuse)
        else {
            // in case of factor-variable treatment, first create view based
            // on full sample to prevent columns from being flagged as omitted
            st_view(X=., ., mvars)
            if (subuse!="") st_subview(X, X, S, .)
        }
        st_view(IF=., ., st_local("ifs"))
    }
    // generate predictions
    b = J(k, 1, .)
    for (i=1; i<=k; i++) {
        _set_tvar(sprintf("%s[%g,1]", levls, i), "")
        p = _get_ps(subuse)
        b[i] = pbar = mean(p, w)
        if (vce) {
            if (probit) dp = normalden(invnormal(p))
            else        dp = p :* (1 :- p)
            if (w!=1)   dp = w :* dp
            IF[.,.] = IF +
                _IF_p(S, pbar, p, dp, X, mIF, mcons, ., .z)  *
                (L[i,2]/W * (st_data(1, st_local("term")), J(1,cons,1)))
        }
        _progress_dot(dots)
    }
    // save results for fractional logit
    st_store((1,k), (st_local("P"), st_local("T"), st_local("W")), (b, L))
}

void Estimate_dx(`RS' k, `RS' vce, `SS' subuse)
{
    `RS' i, pbar, qbar, dots, probit
    `SS' dtype, tvar, levls, at
    `RC' b, w, p, q, dz
    // for vce
    `SR' dzbx
    `RS' W, mcons
    `RC' dp, ddz, muIF, S
    `RR' dzb, dzbp 
    `RM' X, dzbX, mIF, IF
    
    // setup
    dots   = st_local("dots")==""
    dtype  = st_local("type") // average, atmean, observed, levels
    tvar   = st_local("name0")
    levls  = st_local("levels")
    probit = st_local("estcmd")=="probit" // default: logit
    // weights
    w = st_local("wvar")!="" ? st_data(., st_local("wvar"), subuse) : 1
    // prepare VCE
    if (vce) {
        W = st_numscalar(st_local("sum_w"))
        st_view(mIF=., ., st_local("mifs"))
        mcons = st_local("mcons")!="0"
        st_view(X=., ., st_local("mvars"), subuse) /* since Estimate_dx() is
            not applied to factor variables, building the view only once should
            be ok */
        dzb  = editmissing(st_matrix(st_local("dzb")) :/ st_matrix("e(b)"), 0)
        dzbx = st_matrixcolstripe(st_local("dzb"))[,2]'
        dzbp = _get_dzbp(dzb, dzbx) // modifies dzbx
        if (any(dzbp)) st_view(dzbX=., ., select(dzbx, dzbp), subuse)
        if (dtype=="atmean") st_view(muIF=., ., st_local("muIF"), subuse)
        else                 muIF = .z
        if (subuse!="") S = selectindex(st_data(.,subuse)) // subsample index
        else            S = .
        if (dtype=="average") IF = J(st_nobs(), k, 0)
        else                  st_view(IF=., ., st_local("ifs"))
    }
    // generate predictions
    b = J(k, 1, .)
    for (i=1; i<=k; i++) {
        if (dtype=="observed") at = tvar
        else                   at = sprintf("%s[%g,1]", levls, i)
        _set_tvar(at, "")
        p = _get_ps(subuse)
        pbar = mean(p, w)
        dz = _get_dz(subuse)
        if (probit) q = normalden(invnormal(p)) :* dz
        else        q = p :* (1 :- p) :* dz
        qbar = mean(q, w)
        if (vce) {
            if (dtype=="atmean") ddz = _get_ddz(subuse)
            if (probit) dp = normalden(invnormal(p))
            else        dp = p :* (1 :- p)
            if (w!=1)   dp = w :* dp
            IF[,i] =
                (_IF_q(S, p, dp, X, mIF, mcons, dz, muIF, probit, qbar, q, ddz,
                    dzb, dzbp, dzbX)
                + (qbar*(2*pbar-1)/(pbar*(1-pbar))) *
                    _IF_p(S, pbar, p, dp, X, mIF, mcons, dz, muIF)) / 
                (W * pbar * (1 - pbar))
        }
        b[i] = qbar / (pbar * (1 - pbar))
        _progress_dot(dots)
    }
    // take average
    if (dtype=="average") b = _dx_avg(b, levls, vce, W, IF, tvar, subuse, S)
    // return
    st_matrix(st_local("b"), b')
    _set_colstripe(st_local("b"), dtype, k)
}

void Estimate_dc(`RS' k, `RS' vce, `SS' subuse)
{
    `RS' i, j, J, h, pbar, dots, centr, norm, d0, probit
    `SS' dtype, tvar, levls, delta, at, eps
    `RC' b, w, p
    // for vce
    `RS' W, D, mcons
    `RC' dp, dz, muIF, S
    `RM' X, mIF, IF
    
    // setup
    dots   = st_local("dots")==""
    dtype  = st_local("type") // average, atmean, observed, levels
    tvar   = st_local("name0")
    levls  = st_local("levels")
    delta  = st_local("delta")
    centr  = st_local("centered")!=""
    norm   = st_local("normalize")!=""
    d0     = delta=="0" // estimate levels, not effects
    probit = st_local("estcmd")=="probit" // default: logit
    // weights
    w = st_local("wvar")!="" ? st_data(., st_local("wvar"), subuse) : 1
    // prepare VCE
    if (vce) {
        W = st_numscalar(st_local("sum_w"))
        st_view(mIF=., ., st_local("mifs"))
        mcons = st_local("mcons")!="0"
        st_view(X=., ., st_local("mvars"), subuse) /* since Estimate_dc() is
            not applied to factor variables, building the view only once should
            be ok */
        if (dtype=="atmean") st_view(muIF=., ., st_local("muIF"), subuse)
        else                 muIF = .z
        if (subuse!="") S = selectindex(st_data(.,subuse)) // subsample index
        else            S = .
        if (dtype=="average") IF = J(st_nobs(), k, 0)
        else                  st_view(IF=., ., st_local("ifs"))
    }
    // normalization
    h = (d0 | !norm ? 1 : strtoreal(delta))
    // generate predictions
    b = J(k, 1, .)
    J = 2 - d0 
    for (i=1; i<=k; i++) {
        if (dtype=="observed") at = tvar
        else                   at = sprintf("%s[%g,1]", levls, i)
        for (j = J; j; j--) {
            if (d0) eps = "" // 
            else {
                if (centr) {
                    if (j==1) eps = " - " + delta + "/2"
                    else      eps = " + " + delta + "/2"
                }
                else {
                    if (j==1) eps = ""
                    else      eps = " + " + delta
                }
            }
            _set_tvar(at, eps)
            p = _get_ps(subuse)
            pbar = mean(p, w)
            if (vce) {
                if (dtype=="atmean") dz = _get_dz(subuse)
                if (probit) dp = normalden(invnormal(p))
                else        dp = p :* (1 :- p)
                if (w!=1)   dp = w :* dp
                D = W * h * pbar * (1 - pbar)
                if (d0 | j!=1) 
                    IF[,i] = _IF_p(S, pbar, p, dp, X, mIF, mcons, dz, muIF) / D
                else IF[,i] = 
                    IF[,i] - _IF_p(S, pbar, p, dp, X, mIF, mcons, dz, muIF) / D
            }
            if (j==J) b[i] = logit(pbar) // level only or upper estimate
            else      b[i] = quadsum((b[i], -logit(pbar))) / h // lower estimate
            _progress_dot(dots)
        }
    }
    // take average
    if (dtype=="average") b = _dx_avg(b, levls, vce, W, IF, tvar, subuse, S)
    // return
    st_matrix(st_local("b"), b')
    _set_colstripe(st_local("b"), dtype, k)
}

void _set_tvar(`SS' at, `SS' eps)
{
    stata("replace \`name' = " + at + eps /*+ " \`iff'"*/, 1)
}

`RC' _get_ps(`SS' subuse)
{
    return(_get_vtmp("predict double \`vtmp' \`iff', pr", subuse))
}

`RC' _get_dz(`SS' subuse)
{
    return(_get_vtmp("matrix score double \`vtmp' = \`dzb' \`iff'", subuse))
}

`RC' _get_ddz(`SS' subuse)
{
    return(_get_vtmp("matrix score double \`vtmp' = \`ddzb' \`iff'", subuse))
}

`RC' _get_vtmp(`SS' cmd, `SS' subuse)
{
    `RC' v
    
    stata(cmd, 1)
    v = st_data(., st_local("vtmp"), subuse)
    stata("drop \`vtmp'")
    return(v)
}

void _progress_dot(`RS' dots)
{
    if (!dots) return
    stata("_progress_info . \`lpos' \`lsize'")
}

void _set_colstripe(`SS' b, `SS' dtype, `RS' k)
{
    `SM' cstripe
    
    if (dtype!="levels") cstripe = "", st_local("name")
    else cstripe = J(k,1,""), (st_local("name") + "@l") :+ strofreal(1::k)
    st_matrixcolstripe(b, cstripe)
}

`RC' _IF_p(`RC' S, `RS' pbar, `RC' p, `RC' dp, `RM' X, `RM' mIF, `RS' mcons,
    `RC' dz, `RC' muIF)
{
    `RR' delta
    `RC' IF
    
    delta = _colsum(dp, X, mcons)
    if (S==.) {
        // full sample; no mean estimate
        if (muIF==.z) return((p :- pbar) + mIF * delta')
        // full sample; with mean estimate
        return((p :- pbar) + mIF * delta' + muIF * quadcolsum(dp :* dz))
    }
    // subsample
    IF = mIF * delta'
    IF[S,] = IF[S,] + (p :- pbar)
    if (muIF!=.z) IF[S,] = IF[S,] + muIF * quadcolsum(dp :* dz)
    return(IF)
}

`RR' _colsum(`RC' w, `RM' X, `RS' cons)
{
    `RS' j, k
    `RR' sum
    
    k = cols(X)
    j = k + cons
    sum = J(1, j, .)
    if (j>k) {
        if (rows(w)==1) sum[j] = w * rows(X)
        else            sum[j] = quadsum(w)
        j--
    }
    for (;j;j--) sum[j] = quadsum(w :* X[,j])
    return(sum)
}

`RC' _IF_q(`RC' S, `RC' p, `RC' dp, `RM' X, `RM' mIF, `RS' mcons, `RC' dz,
    `RC' muIF, `RC' probit, `RS' qbar, `RC' q, `RC' ddz, `RR' dzb, `RR' dzbp,
    `RM' dzbX)
{
    `RR' delta
    `RC' dq, IF
    
    if (probit) dq = -invnormal(p) :* dz
    else        dq = (1 :- 2*p)    :* dz
    delta = _colsum_q(dp, X, mcons, dq, dzb, dzbp, dzbX)
    if (S==.) {
        // full sample; no mean estimate
        if (muIF==.z) return((q :- qbar) + mIF * delta')
        // full sample; with mean estimate
        return((q :- qbar) + mIF * delta'
            + muIF * quadcolsum(dp :* (dq :* dz :+ ddz)))
    }
    // subsample
    IF = mIF * delta'
    IF[S,] = IF[S,] + (q :- qbar)
    if (muIF!=.z) IF[S,] = IF[S,] + muIF * quadcolsum(dp :* (dq :* dz :+ ddz))
    return(IF)
}

`RR' _colsum_q(`RC' w, `RM' X, `RS' cons, `RC' dq, `RR' dzb, `RR' dzbp,
    `RM' dzbX)
{
    `RS' j, k
    `RC' wdq
    `RR' sum
    
    k = cols(X)
    j = k + cons
    sum = J(1, j, .)
    wdq = w :* dq
    if (j>k) sum[j--] = quadsum(wdq)
    k = cols(dzbX)
    for (;j;j--) {
        if (dzbp[j])     sum[j] = quadsum(w:*(dq:*X[,j] :+ dzb[j]:*dzbX[,k--]))
        else if (dzb[j]) sum[j] = quadsum(w:*(dq:*X[,j] :+ dzb[j]))
        else             sum[j] = quadsum(wdq :* X[,j])
    }
    return(sum)
}

`RR' _get_dzbp(`RR' b, `SR' x)
{
    `RS' j
    `SS' el
    `RR' p
    
    p = J(1, j = cols(b), 0)
    for (;j;j--) {
        if (!b[j]) continue
        (void) _msparse(el=x[j], -2, 0) // undocumented Mata function
        if (el=="_cons") continue
        x[j] = el
        p[j] = 1
    }
    return(p)
}

`RS' _dx_avg(`RC' b, `SS' levls, `RS' vce, `RS' W, `RM' IF,
    `SS' tvar, `SS' subuse, `RC' S)
{
    `RS' j
    `RC' T, p
    `RM' L
    
    L = st_matrix(levls)
    if (vce) {
        st_view(T=., ., tvar, subuse)
        p = L[,2]' / W
        IF = IF :* p
        for (j=rows(L); j; j--) {
            IF[S,j] = IF[S,j] + (((T:==L[j,1]) :- p[j]) :* b[j]) / W
        }
        st_store(., st_local("ifs"), rowsum(IF))
    }
    return(mean(b, L[,2]))
}

void _IF_fl_finalize(`RS' k, `RS' cons, `SS' subuse)
{
    `RS' i, W
    `RC' pi, w, T
    `RM' t, h, G, L, IF
    
    // obtain G from fraclogit
    t  = st_data((1,k), st_local("term"))
    pi = st_data((1,k), st_local("vtmp"))
    w  = st_data((1,k), st_local("W"))
    G = quadcross(t,cons, w:*pi:*(1:-pi), t, cons)
    if (cons) G = invsym(G, cols(G)) // swap _cons last
    else      G = invsym(G)
    // balanced treatment levels (group sizes are fixed)
    if (0) {
        st_view(IF=., ., st_local("ifs"))
        IF[.,.] = IF * G'
        return
    }
    // unbalanced: add IFs of group sizes
    h = st_data((1,k), st_local("P")) - pi
    h = t:*h, J(1, cons, h)
    W = st_numscalar(st_local("sum_w"))
    L = st_matrix(st_local("levels"))
    T = st_data(., st_local("name0"), subuse)
    st_view(IF=., ., st_local("ifs"), subuse)
    for (i=k;i;i--) {
        IF[.,.] = IF + ((T:==L[i,1]) :- L[i,2]/W) * h[i,]
    }
    if (subuse!="") {
        st_view(IF=., ., st_local("ifs"))
    }
    IF[.,.] = IF * G'
}

end
