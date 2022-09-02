*! version 1.0.7  02sep2022  Ben Jann

program lnmor
    version 15
    if replay() {
        if "`e(cmd)'" != "lnmor" {
            error 301
        }
        Display `0'
        exit
    }
    local version : di "version " string(_caller()) ":"
    _parse_opts `0' // returns diopts, header, post, 00, estcmd, prefix
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
        _ereturn_svyr
        _ereturn_cmdline `0'
        Display, `header' `diopts'
        if `"`e(ifgenerate)'"'!="" {
            describe `e(ifgenerate)'
        }
        exit
    }
    if "`prefix'"=="svyb" {
        `version' `00'
        _ereturn_svy
        _ereturn_cmdline `0'
        Display, `header' `diopts'
        exit
    }
    if "`prefix'"=="mi" {
        `version' `00'
        _ereturn_cmdline `0'
        exit
    }
    if "`prefix'"!="" {
        `version' _vce_parserun lnmor, noeqlist mark(CLuster): `00'
        _ereturn_cmdline `0'
        exit
    }
    `estcmd' // rerun estimation command in case of replication VCE
    _check_source_model `e(cmd)'
    _lnmor `00'
    _return_cmdline `0'
    if "`post'"!="" {
        _postrtoe, all
        Display, `header' `diopts'
        if `"`e(ifgenerate)'"'!="" {
            describe `e(ifgenerate)'
        }
        exit
    }
    if !c(noisily) exit
    tempname ecurrent rcurrent
    _estimates hold `ecurrent', restore
    _postrtoe
    nobreak {
        _return hold `rcurrent'
        capture noisily break {
            Display, `header' `diopts'
            if `"`e(ifgenerate)'"'!="" {
                describe `e(ifgenerate)'
            }
        }
        local rc = _rc
        _return restore `rcurrent'
        if `rc' exit `rc'
    }
end

program _ereturn_cmdline, eclass
    eret local cmdline `"lnmor `0'"'
end

program _return_cmdline, rclass
    return add
    return local cmdline `"lnmor `0'"'
end

program _parse_opts
    _parse comma lhs 0 : 0
    syntax [, vce(passthru) post or noHEADer NOSE /*
        */ IFGENerate(passthru) RIFgenerate(passthru) lnmorspec(str asis) * ]
    if `"`lnmorspec'"'!="" {
        // lnmor cmd varlist [if] [in], options lnmorspec()
        local options `vce' `post' `or' `header '`nose' /*
            */ `ifgenerate' `rifgenerate' `options'
        if `"`options'"'!="" {
            local options , `options'
        }
        c_local estcmd `lhs'`options'
        c_local prefix ""
        local 0 `lnmorspec'
        c_local 0 `0'
        _parse comma lhs 0 : 0
        syntax [, post or noHEADer * ]
        _get_diopts diopts options, `options'
        c_local diopts `or' `header' `diopts'
        c_local post `post'
        c_local 00 `lhs', `options'
        exit
    }
    // display options etc.
    _get_diopts diopts options, `options'
    c_local diopts `or' `header' `diopts'
    c_local post `post'
    // svy
    if `"`e(prefix)'"'=="svy" {
        local options `ifgenerate' `rifgenerate' `options'
        _check_source_model `e(cmd)'
        if `"`vce'"'!="" {
            di as err "{bf:vce()} not allowed after {bf:svy}"
            exit 198
        }
        local vcetype `"`e(vce)'"'
        if `"`vcetype'"'=="linearized" local svytype "svyr"
        else                           local svytype "svyb"
        _on_colon_parse `e(cmdline)'
        _parse_svy `s(before)'     // returns svy
        _parse_estcmd 0 `s(after)' // returns estcmd
        if "`svytype'"=="svyr" {
            if `"`nose'"'!="" {
                di as err "{bf:nose} not allowed after {bf:svy `vcetype'}"
                exit 198
            }
            c_local 00 `svy' noheader notable: /*
                */ _lnmor_`svytype' `estcmd' /*
                */ lnmorspec(`lhs', post saveifuninmata `options')
        }
        else {
            if `"`ifgenerate'`rifgenerate'"'!="" {
                di as err `"{bf:ifgenerate()} not allowed after {bf:svy `vcetype'}"'
                exit 198
            }
            c_local 00 `svy' noheader notable: /*
                */ _lnmor_`svytype' `estcmd' /*
                */ lnmorspec(`lhs', post nose `options')
        }
        c_local prefix "`svytype'"
        exit
    }
    // or -> eform()
    if "`or'"!="" local eform eform(Odds Ratio)
    // mi
    if `"`e(mi)'"'=="mi" {
        if `"`e(cmd)'"'=="mi estimate" {
            _check_source_model `e(cmd_mi)'
        }
        else {
            _check_source_model `e(cmd)'
        }
        if `"`ifgenerate'`rifgenerate'"'!="" {
            di as err "{bf:ifgenerate()} not allowed after {bf:mi estimate}"
            exit 198
        }
        if `"`nose'"'!="" {
            di as err "{bf:nose} not allowed after {bf:mi estimate}"
            exit 198
        }
        _parse_opts_vce, `vce' // returns prefix
        if `"`vce'"'!="" {
            di as err "{bf:vce(`prefix')} not allowed after {bf:mi estimate}"
            exit 198
        }
        _on_colon_parse `e(cmdline_mi)'
        _parse_mi `s(before)' // returns mi, mieform
        if `"`mieform'"'=="" local eform `mieform'
        _parse_estcmd 0 `s(after)' // returns estcmd
        c_local 00 `mi' `eform' `header' `diopts' `post': lnmor `estcmd' /*
            */ lnmorspec(`lhs', post `options')
        c_local prefix "mi"
        exit
    }
    // bootstrap/jackknife
    _parse_opts_vce, `vce' // returns prefix
    if "`prefix'"=="" {
        c_local 00 `lhs', `vce' `nose' `ifgenerate' `rifgenerate' `options'
        exit
    }
    _check_source_model `e(cmd)' // returns estcmd
    if `"`ifgenerate'`rifgenerate'"'!="" {
        di as err "{bf:ifgenerate()} not allowed with replication based VCE"
        exit 198
    }
    _parse_estcmd 1 `e(cmdline)'
    if `"`e(clustvar)'"'!="" local cluster cluster(`e(clustvar)')
    c_local 00 `estcmd' `vce' `cluster' `eform' `diopts' /*
        */ lnmorspec(`lhs', post nose `options')
    c_local prefix "`prefix'"
end

program _parse_svy
    _parse comma lhs 0 : 0
    syntax [, * ]
    c_local svy `lhs', `options'
end

program _ereturn_svyr, eclass
    local k_eq `"`e(k_eq_lnmor)'"'
    tempname b V
    mat `b' = e(b_lnmor)
    if `"`e(wtype)'"'!="" local awgt [aw`e(wexp)'] // really needed?
    _ms_build_info `b' if e(sample) `awgt'
    ereturn repost b=`b', rename
    eret local V_modelbased "" // remove matrix e(V_modelbased)
    foreach vmat in srs srssub srswr srssubwr msp {
        capt confirm matrix e(V_`vmat')
        if _rc==1 exit _rc
        if _rc continue
        matrix `V' = e(V)
        mata: st_replacematrix(st_local("V"), st_matrix("e(V_`vmat')"))
        eret matrix V_`vmat' `V'
    }
    eret scalar k_eq = `k_eq'
    eret local k_eq_lnmor ""
    eret local predict ""
    _ereturn_svy
end

program _ereturn_svy, eclass
    eret local cmdname ""
    eret local command ""
end

program _parse_mi // remove -post-, exctract eform()
    _parse comma lhs 0 : 0
    syntax [, post or eform(passthru) * ]
    c_local mi `lhs', cmdok `options'
    c_local mieform `or' `eform'
end

program _parse_estcmd // remove -or-; possibly remove -vce-, -cluster-, -robust-
    gettoken removevce 0 : 0
    _parse comma lhs 0 : 0
    syntax [, vce(passthru) CLuster(passthru) Robust or  * ]
    if !`removevce' local options `robust' `cluster' `vce' `options'
    c_local estcmd `lhs', `options'
end

program _parse_opts_vce
    syntax [, vce(str) ]
    gettoken v : vce, parse(", ")
    local l = strlen(`"`v'"')
    if      `"`v'"'==substr("bootstrap",1,max(4,`l')) local prefix bootstrap
    else if `"`v'"'==substr("jackknife",1,max(4,`l')) local prefix jackknife
    c_local prefix `prefix'
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

program Display
    syntax [, or noHEADer * ]
    if "`or'"!="" local eform eform(Odds Ratio)
    if "`header'"=="" {
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
        _coef_table_header, nomodeltest `headopts'
        if `hflex' {
            // if _coef_table_header used more space than allocated
            local offset1 = max(0, `s(head2_left)' - `w1')
            local offset2 = max(0, `s(head2_right)' - `w2')
            local c1 = `c1' - `offset1' - `offset2'
            local c2 = `c2' - `offset2'
        }
        di as txt _col(`c1') "Command" _col(`c2') "=" /*
            */ _col(`c3') as res %`w2's e(est_cmd)
        if `"`e(delta)'"'!="" {
            di as txt _col(`c1') "Delta" _col(`c2') "=" /*
                */ _col(`c3') as res %`w2'.0g e(delta)
        }
        if `"`e(atvars)'"'!="" {
            di ""
            local atvars `"`e(atvars)'"'
            local K = e(k_eq)
            forv i = 1/`K' {
                di as res "`i'" as txt ": " _c
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
    eret di, `eform' `options'  // eform does not seem to work if e(V) is missing
    local dxtype `"`e(dxtype)'"'
    if      `"`dxtype'"'=="average"  di as txt "(*) average effect"
    else if `"`dxtype'"'=="atmean"   di as txt "(*) effect at mean"
    else if `"`dxtype'"'=="observed" {
        di as txt "(*) effect of marginal shift in observed values"
    }
    else if `"`dxtype'"'=="levels" {
        local lsize = c(linesize)
        local dxlevels `"`e(dxlevels)'"'
        local j 1
        gettoken l dxlevels : dxlevels
        local l `:di %7.4g `l''
        local dxnote "(`j') effect at `l'"
        local l0 = strlen("`dxnote'")
        foreach l of local dxlevels {
            local ++j
            local l `:di %7.4g `l''
            local l ", (`j') at `l'"
            local l1 = strlen("`l'")
            if ((`l0'+`l1')>(`lsize'-4)) {
                if ((`l0'+`l1')<=`lsize') {
                    local dxnote "`dxnote'`l'"
                }
                else {
                    local dxnote "`dxnote' ..."
                }
                continue, break
            }
            local dxnote "`dxnote'`l'"
            local l0 = `l0' + `l1'
        }
        di as txt "`dxnote'"
    }
end

program _lnmor, rclass
    syntax varlist(numeric fv) [, nowarn /*
        */ kmax(numlist int max=1 >1 missingokay) /*
        */ dx DX2(passthru) delta DELTA2(passthru) CENtered NORMalize /*
        */ EPSilon /* undocumented
        */ at(passthru) atmax(int 50) /* 
        */ noDOTs NOSE saveifuninmata /*
        */ IFGENerate(str) RIFgenerate(str) IFScaling(str) replace ]
    if "`kmax'"=="" local kmax 100
    if "`warn'"!="" local warn qui
    
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
    
    // weights
    tempname sum_w
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
        local N        = r(N)
        scalar `sum_w' = r(sum)
    }
    else {
        qui count
        local N        = r(N)
        scalar `sum_w' = r(N)
    }
    if `N'!=`est_N' {
        di as error "something is wrong; inconsistent estimation sample"
        exit 498
    }
    
    // process varlist
    fvexpand `varlist'
    _collect_fvinfo `r(varlist)' // returns names nterms term# type# name# bn#
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
        local isinmfv: list name`j' in mfv
        if "`type`j''"=="factor" {
            if !`isinmfv' {
                `warn' di as txt "Warning: {bf:`name`j''} is a continuous" /*
                    */ " variable in the original model"
            }
            continue
        }
        if `isinmfv' {
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
    
    // at
    tempname AT
    _parse_at `AT', `at' atmax(`atmax') mnames(`mnames') // returns AT K at
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
        mata: _get_levels("`j'") // fills in levels#; returns k# cname#
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
                    mata: _mktmpnames("IF`j'_`k'", `: list sizeof term`j'')
                }
                local IFs `IFs' `IF`j'_`k''
            }
        }
        // parse ifgenerate()
        if `"`ifgenerate'"'!="" {
            _parse_ifgenerate `"`ifgenerate'"' `:list sizeof IFs' `replace'
        }
        // optain model IFs
        mata: _mktmpnames("mIFs", `mk')
        _get_model_IF `mcons' "`mvars'" "`mIFs'"
    }
    else local ifgenerate
    
    // compute marginal ORs
    if "`dots'"=="" {
        di as txt _n "Enumerating predictions:" _n "  " _c
        local lsize = c(linesize)
        local lpos 3
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
                if "`dxtype`j''"!="" & "`delta2'"!="" local kj = `k`j'' * 2
                else                                  local kj   `k`j''
                _progress_info "`name`j''[`kj']" `lpos' `lsize'
            }
            local opts /*
                */ name(`name`j'') levels(`levels`j'') k(`k`j'') /*
                */ wvar(`wvar') sum_w(`sum_w') ifs(`IF`j'_`k'') mifs(`mIFs') /*
                */ mvars(`mvars') mcons(`mcons') estcmd(`est_cmd') /*
                */ lsize(`lsize') lpos(`lpos') `dots'
            if "`dxtype`j''"!="" {
                _lnmor_dx, type(`dxtype`j'') delta(`delta2') `centered' /*
                    */ `normalize' `opts'
            }
            else {
                __lnmor, term(`term`j'') type(`type`j'') bn(`bn`j'') `opts'
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
            local tot_N = el(e(_N),1,1)
        }
        else local tot_N = e(N)
        if `N'!=`tot_N' {
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
    return matrix b           = `b'
    return matrix V           = `V'
    return scalar k_eq        = `K'
    return scalar k_eform     = `K'
    return local  cmd         "lnmor"
    return local  est_cmd     `"`est_cmd'"'
    return local  est_cmdline `"`e(cmdline)'"'
    return local  title       "Marginal (log) odds ratios"
    return scalar N           = `N'
    return scalar sum_w       = `sum_w'
    return local  depvar      `"`e(depvar)'"'
    return local  wtype       `"`weight'"'
    return local  wexp        `"`exp'"'
    return scalar nterms      = `nterms'
    forv j = 1/`nterms' {
        return local  name`j'   "`name`j''"
        return local  term`j'   "`term`j''"
        return local  type`j'   "`type`j''"
        return scalar dx`j'     = "`dxtype`j''"!=""
        return scalar k`j'      = `k`j''
        return matrix levels`j' = `levels`j''
    }
    if `ndx' {
        return local dxtype     "`dxtype'"
        return local dxlevels   "`dxlevels'"
        if "`delta2'"!="" {
            return scalar delta    = `delta2'
            return local centered  "`centered'"
            return local normalize "`normalize'"
        }
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
            return scalar df_r = `N' - 1
        }
        return local vce     `"`evce'"'
        return local vcetype `"`vcetype'"'
        return scalar rank   = `rank'
    }
    else {
        return scalar df_r = `N' - 1
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
            gettoken nm coln : coln
            capt confirm variable `v', exact
            if _rc==1 exit _rc
            if _rc==0 drop `v'
            if "`iftype'"=="RIF" {
                qui replace `IF' = `IF' + `b'[1,`j']/`sum_w'
            }
            if "`ifscaling'"=="mean" {
                qui replace `IF'  = `IF' * `sum_w'
            }
            lab var `IF' "`iftype' of _b[`nm']"
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

program _progress_info
    args s lpos lsize
    local l = strlen("`s'")
    if (`lpos'+`l'-1)>`lsize' {
        local l0 = `lsize' - `lpos' + 1
        local s0 = substr("`s'", 1, `l0')
        di as txt "`s0'" _n "  " _c
        local lpos 3
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
         else local next 0
         if `next' {
             if `J' {
                 _collect_fvinfo_bn "`type'" "`term'" `bn'
                 c_local term`J' "`term'"
                 c_local type`J' "`type'"
                 c_local name`J' "`name'"
                 c_local bn`J'   `bn'
                 local names `names' `name'
             }
             local term
             local type
             local name
             local bn 1
             local ++J
         }
         local term `term' `t'
         local type `Type'
         local name `Name'
         if r(base)==1 local bn 0
    }
    _collect_fvinfo_bn "`type'" "`term'" `bn'
    c_local term`J' "`term'"
    c_local type`J' "`type'"
    c_local name`J' "`name'"
    c_local bn`J'   `bn'
    local names `names' `name'
    c_local names   `names'
    c_local nterms   `J'
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
    syntax anything(name=AT), atmax(str) [ at(str) mnames(str) ]
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
    mata: _at_expand(`K', `J', `atmax')
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

program __lnmor, rclass
    syntax, term(str) type(str) name(str) bn(str) levels(str) sum_w(str) /*
         */ k(str) estcmd(str) mcons(str) [ wvar(str) /*
         */ ifs(str) mifs(str) mvars(str) lsize(str) lpos(str) nodots ]
    
    // weights
    if "`wvar'"!="" local wgt "[aw=`wvar']"
    
    // whether outcome model has constant (bv=1 only possible if type=factor)
    if `bn' local nocons noconstant
    
    // create tempvars for IFs
    local vce = `"`ifs'"'!=""
    if `vce' {
        if "`nocons'"=="" {
            tempname IFcns
            local ifs `ifs' `IFcns'
        }
        foreach v of local ifs {
            qui gen double `v' = 0
        }
    }
    
    // run
    tempname l pbar
    tempvar name0 name1 name2 p w PS
    rename `name' `name0'
    if `vce' {
        tempname Y touse
        qui gen double `Y' = .
        qui gen byte `touse' = .
    }
    qui gen double `p' = .
    qui gen double `w' = .
    qui gen double `name' = .
    qui gen double `name1' = .
    forv i = 1/`k' {
        scalar `l' = `levels'[`i',1]
        qui replace `name' = `l'
        qui predict double `PS', pr
        su `PS' `wgt', meanonly
        scalar `pbar' = r(mean)
        qui replace `p'     = `pbar' in `i'
        qui replace `w'     = `levels'[`i',2] in `i'
        qui replace `name1' = `l' in `i'
        if `vce' {
            qui replace `touse' = `name0'==`l'
            qui replace `Y' = `pbar' if `touse'
            mata: __lnmor_update_IF()
        }
        drop `PS'
        if "`dots'"=="" _progress_info "." `lpos' `lsize'
    }
    drop `name'
    c_local lpos `lpos'
    
    // result
    rename `name1' `name'
    tempname ecurrent
    _estimates hold `ecurrent', restore
    su `p' in 1/`k', meanonly
    local hasvar = r(min)!=r(max)
    if `hasvar' {
        qui fracreg logit `p' `term' [iw=`w'] in 1/`k', `nocons'
    }
    drop `name'
    rename `name0' `name'

    // generate IFs
    if `vce' {
        if `hasvar' {
            qui predict double `PS'
            mata: __lnmor_finalize_IF()
        }
        else {
            foreach IF of local ifs {
                qui replace `IF' = 0
            }
        }
    }
    
    // returns
    tempname b
    if `hasvar' {
        matrix `b' = e(b)
        if "`nocons'"=="" {
            matrix `b' = `b'[1,1..colsof(`b')-1]
        }
        mat coleq `b' = ""
    }
    else {
        mat `b' = J(1,`: list sizeof term', 0)
        mat coln `b' = `term'
    }
    return matrix b = `b'
end

program _lnmor_dx, rclass
    syntax, type(str) name(str) levels(str) sum_w(str) k(str) /*
        */ estcmd(passthru) mcons(passthru) [ delta(passthru) centered /*
        */ NORMALIZE wvar(passthru) ifs(str) mifs(passthru) mvars(passthru) /*
        */ lsize(passthru) lpos(str) nodots ]
    
    // helper vector for deriviatives of the linear predictor
    local vce = `"`ifs'"'!=""
    if "`delta'"=="" | (`vce' & "`type'"=="atmean") {
        tempname dzb
        matrix `dzb' = e(b)
        _ms_dzb_dx `name', matrix(`dzb') // undocumented utility of -margins-
        matrix `dzb' = r(b)
    }
    
    // common options
    local options name(`name') levels(`levels') sum_w(`sum_w') dzb(`dzb') /*
        */  `wvar' `delta' `centered' `normalize' `estcmd' `mcons' `mifs' /*
        */  `mvars' `lsize' `dots'
    
    // average of level-specific dx
    if "`type'"=="average" {
        if `vce' {
            qui gen double `ifs' = 0
            tempname IF
        }
        tempname b p
        mat `b' = J(1, 1, 0)
        mat `p' = `levels'[1...,2] / `sum_w'
        forv i = 1/`k' {
            __lnmor_dx, i(`i') lpos(`lpos') ifvar(`IF') `options'
            mat `b' = `b' + r(b) * `p'[`i',1]
            if `vce' {
                qui replace `ifs' = `ifs' + (`p'[`i',1] * `IF' ///
                    + r(b) * ((`name'==`levels'[`i',1]) - `p'[`i',1]) / `sum_w')
                drop `IF'
            }
        }
        mat coln `b' = `name'(*)
        return matrix b = `b'
        exit
    }
    // dx at mean
    if "`type'"=="atmean" {
        if `vce' {
            tempvar muIF // IF of mean
            qui gen double `muIF' = (`name' - `levels'[1,1]) / `sum_w'
        }
        tempname b
        __lnmor_dx, i(1) lpos(`lpos') ifvar(`ifs') `options'
        mat `b' = r(b)
        mat coln `b' = `name'(*)
        return matrix b = `b'
        exit
    }
    // dx at observed values
    if "`type'"=="observed" {
        tempname b
        __lnmor_dx, i(1) lpos(`lpos') ifvar(`ifs') `options'
        mat `b' = r(b)
        mat coln `b' = `name'(*)
        return matrix b = `b'
        exit
    }
    // dx at levels
    tempname b
    mat `b' = J(1, `k', .)
    local coln
    forv i = 1/`k' {
        local coln `coln' `name'(`i')
        gettoken IF ifs : ifs
        __lnmor_dx, i(`i') ifvar(`IF') lpos(`lpos') `options'
        mat `b'[1,`i'] = r(b)
    }
    mat coln `b' = `coln'
    return matrix b = `b'
end

program __lnmor_dx, rclass
    syntax, name(str) levels(str) sum_w(str) i(str) estcmd(str) mcons(str) /*
         */ [ delta(str) centered NORMALIZE wvar(str) ifvar(str) mifs(str) /*
         */ mvars(str) muif(str) dzb(str) lsize(str) lpos(str) nodots ]
    
    // weights
    if "`wvar'"!="" local wgt "[aw=`wvar']"
    
    // create tempvar for IF
    local vce = `"`ifvar'"'!=""
    if `vce' {
        qui gen double `ifvar' = 0
    }
    
    // run
    tempname b l
    scalar `l' = `levels'[`i',1]
    if "`delta'"=="" {
        if "`muif'"!="" {
            tempname ddzb
            _ms_dzb_dx `name', matrix(`dzb')
            matrix `ddzb' = r(b)
        }
        if `l'<. {
            tempvar name0
            rename `name' `name0'
            qui gen double `name' = `l'
        }
        tempvar Q PS dz
        tempname qbar pbar
        qui matrix score double `dz' = `dzb'
        qui predict double `PS', pr
        if "`estcmd'"=="probit" qui gen double `Q' = normalden(invnormal(`PS')) * `dz'
        else                    qui gen double `Q' = `PS' * (1-`PS') * `dz'
        su `Q' `wgt', meanonly
        scalar `qbar' = r(mean)
        su `PS' `wgt', meanonly
        scalar `pbar' = r(mean)
        if `vce' {
            if "`muif'"!="" {
                tempvar  ddz
                qui matrix score double `ddz' = `ddzb'
            }
            mata: __lnmor_dx_IF()
        }
        scalar `b' = `qbar' / (`pbar' * (1 - `pbar'))
        if "`dots'"=="" _progress_info "." `lpos' `lsize'
        if `l'<. {
            drop `name'
            rename `name0' `name'
        }
    }
    else {
        if "`normalize'"=="" local h 1
        else                 local h `delta'
        if "`centered'"!="" {
            local eps1 - `delta'/2
            local eps2 + `delta'/2
        }
        else {
            local eps1
            local eps2 + `delta'
        }
        if "`muif'"!="" tempvar dz
        tempname p1 p2
        tempvar name0 PS
        rename `name' `name0'
        qui gen double `name' = .
        if `l'>=. local lev `name0'
        else      local lev `l'
        forv j = 1/2 {
            qui replace `name' = `lev' `eps`j''
            qui predict double `PS', pr
            su `PS' `wgt', meanonly
            scalar `p`j'' = r(mean)
            local pbar `p`j''
            if `vce' {
                if "`muif'"!="" qui matrix score double `dz' = `dzb'
                mata: __lnmor_dc_IF(`h')
                if "`muif'"!="" drop `dz'
            }
            drop `PS'
            if "`dots'"=="" _progress_info "." `lpos' `lsize'
        }
        scalar `b' = (logit(`p2') - logit(`p1')) / `h'
        drop `name'
        rename `name0' `name'
    }
    
    // returns
    return scalar b = `b'
    c_local lpos `lpos'
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

version 15
mata:
mata set matastrict on

void _at_expand(real scalar K0, real scalar J, real scalar atmax)
{
    real scalar       j, k, K, K1, R
    string rowvector  at
    real colvector    l
    real matrix       AT
    pointer rowvector L
    
    // collect levels of no levels specified
    if (K0>=.) {
        at = tokens(st_local("at"))
        L = J(1,J,NULL)
        K1 = 1
        for (j=1; j<=J; j++) {
            L[j] = &(mm_unique(st_data(., at[j])))
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

void _get_levels(string scalar j)
{
    string scalar    Name, vtype, dxtype, dxlevels
    real scalar      k, kmax, dots
    real colvector   X, w, p, a, b
    real matrix      L
    
    // setup
    Name     = st_local("name"+j)
    dots     = st_local("dots")==""
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
        X = st_data(., Name)
        if (st_local("wvar")!="") w = st_data(., st_local("wvar"))
        else                      w = 1
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
            if (vtype!="factor" & k>kmax) { // appply binning if necessary
                if (dots) printf("{txt}(%s has %g levels", Name, k)
                _get_levels_bin(X, w, kmax)
                a = selectindex(_mm_unique_tag(X))
                k = rows(a)
                if (dots) printf("; using %g binned levels)\n", k)
                Name = st_tempname(1)
                st_store(p, st_addvar("double", Name), X)
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

void _get_levels_bin(real colvector X, real colvector w, real scalar k)
{
    real scalar    i, j, qj, a, b, e
    real colvector q
    
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
        X[|a\b|] = J(b-a+1, 1, w==1 ? mean(X[|a\b|]) : mean(X[|a\b|], w[|a\b|]))
    }
}

void _mktmpnames(string scalar nm, real scalar k)
{
    st_local(nm, invtokens(st_tempname(k)))
}

void _get_model_IF(string scalar V, string scalar score, real scalar cons,
    string scalar xvars, string scalar IFs)
{
    real colvector   sc
    real matrix      X, IF
    
    st_view(sc=., ., score)
    st_view(X=., ., xvars)
    st_view(IF=., ., st_addvar("double", tokens(IFs)))
    IF[.,.] = (sc:*X, J(1, cons, sc)) * st_matrix(V)'
}

void _lnmor_restore(real scalar ifgen)
{
    string scalar    touse
    string colvector IFs
    real matrix      IF
    
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

void __lnmor_update_IF()
{
    real matrix IF
    
    st_view(IF=., ., st_local("ifs"))
    IF[.,.] = IF + __lnmor_IF_p(0) * __lnmor_IF_delta()
}

real rowvector __lnmor_IF_delta()
{
    real scalar    touse, cons
    real rowvector delta
    real colvector w
    real matrix    X
    
    st_varrename(st_local("name"), st_local("name2"))
    st_varrename(st_local("name0"), st_local("name"))
    cons = st_local("nocons")==""
    touse = st_varindex(st_local("touse"))
    w = st_local("wvar")!="" ? st_data(., st_local("wvar"), touse) : 1
    st_view(X=., ., st_local("term"), touse)
    delta = __lnmor_colsum(w, X, cons)
    st_varrename(st_local("name"), st_local("name0"))
    st_varrename(st_local("name2"), st_local("name"))
    return(delta)
}

real rowvector __lnmor_colsum(real colvector w, real matrix X, real scalar cons)
{
    real scalar    j, k
    real rowvector sum
    
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

real matrix __lnmor_IF_p(real scalar hasq)
{
    real scalar    cons, pbar
    real colvector p, dp, dz, muIF
    real matrix    IF, X, mIF
    
    cons = strtoreal(st_local("mcons"))
    pbar = st_numscalar(st_local("pbar"))
    p    = st_data(., st_local("PS"))
    if (st_local("estcmd")=="probit") dp = normalden(invnormal(p))
    else                              dp = p :* (1 :- p)
    if (st_local("wvar")!="") dp = st_data(., st_local("wvar")) :* dp
    st_view(X=., ., st_local("mvars"))
    st_view(mIF=., ., st_local("mifs"))
    if (st_local("dz")!="") dz = st_data(., st_local("dz"))
    IF = (p :- pbar) + mIF * __lnmor_colsum(dp, X, cons)'
    if (st_local("muif")!="") {
        // dx(atmean) correction
        muIF = st_data(., st_local("muif"))
        IF = IF + muIF * quadcolsum(dp :* dz)
    }
    if (hasq) IF = IF, __lnmor_IF_q(p, dp, X, mIF, cons, dz, muIF)
    return(IF / st_numscalar(st_local("sum_w")))
}

real matrix __lnmor_IF_q(real colvector p, real colvector dp, real matrix X,
    real matrix mIF, real scalar cons, real colvector dz, real colvector muIF)
{
    real scalar    qbar
    real colvector q, dq, IF

    qbar = st_numscalar(st_local("qbar"))
    q    = st_data(., st_local("Q"))
    if (st_local("estcmd")=="probit") dq = -invnormal(p) :* dz
    else                              dq = (1 :- 2*p)    :* dz
    IF = (q :- qbar) + mIF * __lnmor_IF_q_colsum(dp, dq, X, cons)'
    if (st_local("muif")!="") {
        IF = IF + muIF * 
            quadcolsum(dp :* (dq :* dz :+ st_data(., st_local("ddz"))))
    }
    return(IF)
}

real rowvector __lnmor_IF_q_colsum(real colvector dp, real colvector dq,
    real matrix X, real scalar cons)
{
    real scalar      j, k
    real colvector   dpdq
    real rowvector   sum, dzb, dzbp
    real matrix      dzbX
    
    // prepare info from dzb
    dzb  = editmissing(st_matrix(st_local("dzb")) :/ st_matrix("e(b)"), 0)
    dzbp = __lnmor_IF_q_dzbX(dzbX=., dzb, st_local("dzb"))
    // compute components
    k = cols(X)
    j = k + cons
    sum = J(1, j, .)
    dpdq = dp :* dq
    if (j>k) sum[j--] = quadsum(dpdq)
    k = cols(dzbX)
    for (;j;j--) {
        if (dzbp[j])     sum[j] = quadsum(dp:*(dq:*X[,j] :+ dzb[j]:*dzbX[,k--]))
        else if (dzb[j]) sum[j] = quadsum(dp:*(dq :* X[,j] :+ dzb[j]))
        else             sum[j] = quadsum(dpdq :* X[,j])
    }
    return(sum)
}

real rowvector __lnmor_IF_q_dzbX(real matrix X, real rowvector b,
    string scalar m)
{
    real scalar      j
    string scalar    el
    string rowvector x
    real rowvector   p
    
    x = st_matrixcolstripe(m)[,2]'
    p = J(1, j = cols(b), 0)
    for (;j;j--) {
        if (!b[j]) continue
        (void) _msparse(el=x[j], -2, 0) // undocumented Mata function
        if (el=="_cons") continue
        x[j] = el
        p[j] = 1
    }
    if (any(p)) st_view(X, ., select(x, p))
    else        X = J(0,0,.)
    return(p)
}

void __lnmor_finalize_IF()
{
    real scalar    cons
    real colvector p, w
    real matrix    X, G, IF
    
    cons = st_local("nocons")==""
    w = st_local("wvar")!="" ? st_data(., st_local("wvar")) : 1
    st_view(X=., ., st_local("term"))
    p = st_data(., st_local("PS"))
    G = quadcross(X,cons, w:*p:*(1:-p), X, cons)
    if (cons) G = invsym(G, cols(X)+1) // swap _cons last
    else      G = invsym(G)
    p = st_data(., st_local("Y")) - p  // Y - p
    st_view(IF=., ., st_local("ifs"))
    IF[.,.] = ((X:*p, J(1,cons,p)) + IF) * G'
}

void __lnmor_dx_IF()
{
    real scalar p, q
    real matrix IF
    
    q  = st_numscalar(st_local("qbar"))
    p  = st_numscalar(st_local("pbar"))
    IF = __lnmor_IF_p(1)
    st_store(., st_local("ifvar"), (1 / (p*(1-p))) * (IF[,2] +
        (q*(2*p-1)/(p*(1-p))) * IF[,1]))
}

void __lnmor_dc_IF(real scalar h)
{
    real scalar    p, sign
    real colvector IF
    
    sign = st_local("j")=="1" ? -1 : 1
    p    = st_numscalar(st_local("pbar"))
    st_view(IF=., ., st_local("ifvar"))
    IF[.,.] = IF + __lnmor_IF_p(0) / (sign * h * p * (1 - p))
}

end

