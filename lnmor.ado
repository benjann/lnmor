*! version 1.0.3  23aug2022  Ben Jann

program lnmor
    version 14
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
    syntax [, vce(passthru) post or noHEADer NOSE IFGENerate(passthru) /*
        */ lnmorspec(str asis) * ]
    if `"`lnmorspec'"'!="" {
        // lnmor cmd varlist [if] [in], options lnmorspec()
        local options `vce' `post' `or' `header '`nose' `ifgenerate' `options'
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
        local options `ifgenerate' `options'
        _check_source_model `e(cmd)'
        if `"`vce'"'!="" {
            di as err "{bf:vce()} not allowed after {bf:svy}"
            exit 198
        }
        local vcetype `"`e(vce)'"'
        if `"`vcetype'"'=="linearized" local svytype "svyr"
        else                           local svytype "svyb"
        _on_colon_parse `e(cmdline)'
        _parse_svy `s(before)' // returns svy
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
            if `"`ifgenerate'"'!="" {
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
        if `"`ifgenerate'"'!="" {
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
        c_local 00 `lhs', `vce' `nose' `ifgenerate' `options'
        exit
    }
    _check_source_model `e(cmd)' // returns estcmd
    if `"`ifgenerate'"'!="" {
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
end

program _lnmor, rclass
    syntax varlist(numeric fv) [, /*
        */ kmax(numlist int max=1 >1 missingokay) /*
        */ at(passthru) atmax(int 50) noDOTs NOSE /*
        */ IFGENerate(str) replace saveifuninmata ]
    
    // default kmax
    if "`kmax'"=="" local kmax 100
    
    // collect some info on original model
    local est_N       = e(N)
    local est_cmd     `"`e(cmd)'"'
    local clustvar    `"`e(clustvar)'"'
    if `"`clustvar'"'!="" {
        local N_clust = e(N_clust)
        local vceopt vce(cluster `clustvar')
    }
    _collect_model_info // returns mvars, mnames, mcons, mk
    
    // preserve model and select sample
    tempname ecurrent
    _estimates hold `ecurrent', restore copy
    preserve
    qui keep if e(sample)
    
    // weights
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
    }
    
    // process varlist
    fvexpand `varlist'
    _collect_fvinfo `r(varlist)' // returns names, nterms, term#, type#, name#, bn#
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
    
    // at
    tempname AT
    _parse_at `AT', `at' atmax(`atmax') mnames(`mnames') // returns AT, K, at
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
        mata: _get_levels("`j'") // fills in levels#, returns k#, cname#
        mat coln `levels`j'' = "value" "count"
    }
    
    // prepare VCE
    if "`saveifuninmata'"!="" local nose
    local vce = "`nose'"==""
    if `vce' {
        // tempvars for outcome IFs
        forv k = 1 / `K' {
            forv j = 1/`nterms' {
                mata: _mktmpnames("IF`j'_`k'", `: list sizeof term`j'')
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
            if "`dots'"=="" _progress_info "`name`j''[`k`j'']" `lpos' `lsize'
            __lnmor [`weight'`exp'], term(`term`j'') type(`type`j'') /*
                */ name(`name`j'') bn(`bn`j'') levels(`levels`j'') /*
                */ k(`k`j'') wvar(`wvar') ifs(`IF`j'_`k'') mifs(`mIFs') /*
                */ mvars(`mvars') mcons(`mcons') estcmd(`est_cmd') /*
                */ lsize(`lsize') lpos(`lpos') `dots'
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
        capt assert (`tot_N'==`est_N')
        if _rc==1 exit _rc
        if _rc {
            di as error "something is wrong; inconsistent estimation sample"
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
    return scalar N           = `est_N'
    return local  depvar      `"`e(depvar)'"'
    return local  wtype       `"`weight'"'
    return local  wexp        `"`exp'"'
    return scalar nterms      = `nterms'
    forv j = 1/`nterms' {
        return local  term`j' "`term`j''"
        return local  type`j' "`type`j''"
        return local  name`j' "`name`j''"
        return scalar k`j'      = `k`j''
        return matrix levels`j' = `levels`j''
    }
    if `hasat' {
        return local atvars        "`at'"
        return matrix at           = `AT'
    }
    if `vce' {
        return local clustvar `"`clustvar'"'
        if `"`clustvar'"'!="" {
            return scalar N_clust = `N_clust'
            return scalar df_r = `N_clust' - 1
        }
        else {
            return scalar df_r = `est_N' - 1
        }
        return local vce `"`evce'"'
        return local vcetype `"`vcetype'"'
        return scalar rank = `rank'
    }
    else {
        return scalar df_r = `est_N' - 1
    }
    if "`ifgenerate'"!="" {
        local coln: coln e(b)
        foreach v of local ifgenerate {
            gettoken IF IFs : IFs
            if "`IF'"=="" continue, break
            gettoken nm coln : coln
            capt confirm variable `v', exact
            if _rc==1 exit _rc
            if _rc==0 drop `v'
            lab var `IF' "IF of _b[`nm']"
            rename `IF' `v'
        }
        return local ifgenerate "`ifgenerate'"
    }
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
    local names
    foreach t in `r(varlist1)' {
        _ms_parse_parts `t'
        local names `names' `r(name)'
    }
    c_local mnames: list uniq names
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
    syntax [fw iw pw], /*
        */ term(str) type(str) name(str) bn(str) levels(str) /*
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
    tempvar name0 name1 name2 p w PS l
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
        qui replace `p'     = r(mean) in `i'
        qui replace `w'     = `levels'[`i',2] in `i'
        qui replace `name1' = `l' in `i'
        if `vce' {
            qui replace `touse' = `name0'==`l'
            qui replace `Y' = r(mean) if `touse'
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

version 11
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
    string scalar  Name
    real scalar    k, dots
    real colvector X, w, p, a, b
    real matrix    L
    
    dots = st_local("dots")==""
    // data
    Name = st_local("name"+j)
    X = st_data(., Name)
    p = order(X, 1)
    _collate(X, p)
    if (st_local("wvar")!="") {
        w = st_data(., st_local("wvar"))
        _collate(w, p)
    }
    else w = 1
    if (X[rows(X)]>=.) {
        stata(`"di as err "something is wrong; {bf:"' + Name +
            `"} has missing values within estimation sample""')
        exit(error(498))
    }
    // obtain levels
    a = selectindex(_mm_unique_tag(X))
    if (st_local("type"+j)!="factor") {
        // appply binning if necessary
        k = strtoreal(st_local("kmax"))
        if (rows(a)>k) {
            if (dots) printf("{txt}(%s has %g levels",st_local("name"+j),rows(a))
            _get_levels_bin(X, w, k)
            a = selectindex(_mm_unique_tag(X))
            if (dots) printf("; using binned copy with %g levels)\n",rows(a))
            Name = st_tempname(1)
            st_store(p, st_addvar("double", Name), X)
        }
    }
    b = selectindex(_mm_unique_tag(X, 1))
    L = X[a]
    if (w==1) L = L, ((b-a):+1)
    else      L = L, mm_diff(0 \ quadrunningsum(w)[b])
    st_matrix(st_local("levels"+j), L)
    k = rows(L)
    st_matrixrowstripe(st_local("levels"+j), (J(k, 1, ""), strofreal(1::k)))
    st_local("k"+j, strofreal(k))
    st_local("cname"+j, Name) // name of (possibly coarened) variable
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
    real scalar    touse, cons, pbar
    real rowvector delta
    real colvector p, dp, w
    real matrix    IF, mIF, X
    
    // delta
    st_varrename(st_local("name"), st_local("name2"))
    st_varrename(st_local("name0"), st_local("name"))
    cons = st_local("nocons")==""
    touse = st_varindex(st_local("touse"))
    w = st_local("wvar")!="" ? st_data(., st_local("wvar"), touse) : 1
    st_view(X=., ., st_local("term"), touse)
    delta = quadcolsum(w :* (X, J(rows(X), cons, 1))) / st_numscalar("r(sum_w)")
    st_varrename(st_local("name"), st_local("name0"))
    st_varrename(st_local("name2"), st_local("name"))
    // delta * (IF of p)
    cons = strtoreal(st_local("mcons"))
    w = st_local("wvar")!="" ? st_data(., st_local("wvar")) : 1
    pbar = st_numscalar("r(mean)")
    p    = st_data(., st_local("PS"))
    if (st_local("estcmd")=="probit") dp = normalden(invnormal(p))
    else                              dp = (p:*(1:-p))
    st_view(X=., ., st_local("mvars"))
    st_view(mIF=., ., st_local("mifs"))
    st_view(IF=., ., st_local("ifs"))
    IF[.,.] = IF + ((p :- pbar) 
        + mIF * quadcolsum(w:*(dp:*X, J(1, cons, dp)))') * delta
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

end

