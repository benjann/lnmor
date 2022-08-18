*! version 1.0.0  18aug2022  Ben Jann

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
    _parse_opts `0' // returns 00, estcmd
    `version' _vce_parserun lnmor, noeqlist mark(CLuster) : `00'
    if "`s(exit)'" != "" {
        _ereturn_cmdline `0'
        exit
    }
    `estcmd' // rerun estimation command in case of replication VCE
    _check_source_model
    syntax anything [, post or * ]
    _lnmor `anything', `options' // returns diopts
    _return_cmdline `0'
    if "`post'"!="" {
        _postrtoe, all
        Display, `or' `diopts'
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
            Display, `or' `diopts'
            if `"`e(ifgenerate)'"'!="" {
                describe `e(ifgenerate)'
            }
        }
        local rc = _rc
        _return restore `rcurrent'
        if `rc' exit `rc'
    }
end

program _check_source_model
    if !inlist(`"`e(cmd)'"',"logit","probit") error 301
    if `"`e(prefix)'"'=="svy" {
        di as err "model has {bf:svy} prefix; this is not supported"
        exit 498
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
    syntax [, vce(passthru) post or NOSE lnmorspec(str asis) /*
        */ IFGENerate(passthru) * ]
    if `"`lnmorspec'"'!="" {
        // lnmor cmd varlist [if] [in], options lnmorspec()
        local options `post' `options'
        local options `or' `options'
        local options `nose' `options'
        local options `vce' `options'
        local options `ifgenerate' `options'
        if `"`options'"'!="" {
            local options , `options'
        }
        c_local 0 `lnmorspec'
        c_local estcmd `lhs'`options'
        exit
    }
    // bootstrap/jackknife
    _parse_opts_vce, `vce' // returns repl
    if !`repl' {
        c_local 00 `lhs'`0'
        exit
    }
    _check_source_model
    if `"`ifgenerate'"'!="" {
        di as err "{bf:ifgenerate()} not allowed with replication based VCE"
        exit 198
    }
    _parse_opts_estcmd `e(cmdline)'
    local post post
    local nose nose
    if "`or'"!="" local eform eform(Odds Ratio)
    if `"`e(clustvar)'"'!="" local cluster cluster(`e(clustvar)')
    c_local 00 `estcmd' `vce' `cluster' `eform' /*
        */ lnmorspec(`lhs', `post' `nose' `options')
end

program _parse_opts_vce
    syntax [, vce(str) ]
    gettoken v : vce, parse(", ")
    local repl 0
    if      `"`v'"'==substr("bootstrap",1,max(4,strlen(`"`v'"'))) local repl 1
    else if `"`v'"'==substr("jackknife",1,max(4,strlen(`"`v'"'))) local repl 1
    c_local repl `repl'
end

program _parse_opts_estcmd // remove -vce()- and -or-
    _parse comma lhs 0 : 0
    syntax [, vce(str) or  * ] 
    c_local estcmd `lhs', `options'
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
    if "`all'"=="" exit
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
        local w1 17
        local c1 49
        local c2 = `c1' + `w1' + 1
        local w2 10
        local headopts head2left(`w1') head2right(`w2')
        if      c(stata_version)<17            local headopts
        else if d(`c(born_date)')<d(13jul2021) local headopts
        _coef_table_header, `headopts'
        di as txt _col(`c1') "Command" _col(`c2') "= " as res %`w2's e(est_cmd)
        if `"`e(over)'"'!="" _svy_summarize_legend
        else di ""
    }
    eret di, `eform' `options'  // eform does not seem to work if e(V) is missing
end

program _lnmor, rclass
    syntax varlist(numeric fv) [, /*
        */ kmax(numlist int max=1 >1 missingokay) /*
        */ over(varname numeric) noDOTs NOSE noHEADer /*
        */ IFGENerate(str) replace * ]
    
    // collect diopts
    _get_diopts diopts, `options'
    c_local diopts `header' `diopts'
    
    // default kmax
    if "`kmax'"=="" local kmax 100
    
    // preserve model and select sample
    local est_N       = e(N)
    local est_cmd     `"`e(cmd)'"'
    local clustvar    `"`e(clustvar)'"'
    if `"`clustvar'"'!="" {
        local vceopt vce(cluster `clustvar')
    }
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
    
    // process varlist / determine levels
    fvexpand `varlist'
    _collect_fvinfo `r(varlist)' // returns nterms, term#, type#, name#, bn#
    forv j = 1/`nterms' {
        tempname levels`j'
        mata: _get_levels("`j'") // fills in levels#, returns k#, cname#
        mat coln `levels`j'' = "value" "count"
    }
    
    // over
    if "`over'"!="" {
        local hasover 1
        qui levelsof `over'
        local olevels "`r(levels)'"
        foreach o of local olevels {
            local olab: label (`over') `o'
            local olabels `"`olabels' `"`olab'"'"'
        }
        local olabels: list clean olabels
    }
    else {
        local hasover 0
        local olevels .
    }
    
    // prepare VCE
    local vce = "`nose'"==""
    if `vce' {
        // tempvars for outcome IFs
        local k 0
        foreach o of local olevels {
            local ++k
            forv j = 1/`nterms' {
                mata: _mktmpnames("IF`j'_`k'", "`type`j''"=="factor" ? `k`j'' : 1)
                local IFs `IFs' `IF`j'_`k''
            }
        }
        // parse ifgenerate()
        if `"`ifgenerate'"'!="" {
            _parse_ifgenerate `"`ifgenerate'"' `:list sizeof IFs' `replace'
        }
        // optain model IFs
        mata: _mktmpnames("mIFs", cols(st_matrix("e(b)")))
        _get_model_IF `mIFs' // returns mvars, mcons
    }
    else local ifgenerate
    
    // compute marginal ORs
    if "`dots'"=="" di as txt "(" _c
    if `hasover' tempname B
    local k 0
    tempname b
    tempvar name0
    foreach o of local olevels {
        local ++k
        if `hasover' {
            if "`dots'"=="" {
                if `k'>1 di ";" _c
                di as txt "`over'=`o':" _c
            }
        }
        forv j = 1/`nterms' {
            if "`cname`j''"!="`name`j''" {
                rename `name`j'' `name0'
                rename `cname`j'' `name`j''
            }
            if "`dots'"=="" di as txt "`name`j''" _c
            __lnmor [`weight'`exp'], term(`term`j'') type(`type`j'') /*
                */ name(`name`j'') bn(`bn`j'') levels(`levels`j'') /*
                */ k(`k`j'') over(`over') olevel(`o') /*
                */ wvar(`wvar') ifs(`IF`j'_`k'') mifs(`mIFs') /*
                */  mvars(`mvars') mcons(`mcons') estcmd(`est_cmd') `dots'
            matrix `b' = nullmat(`b'), r(b)
            if "`cname`j''"!="`name`j''" {
                rename `name`j'' `cname`j''
                rename `name0' `name`j''
            }
        }
        if `hasover' {
            matrix coleq `b' = "`o'"
            matrix `B' = nullmat(`B'), `b'
            matrix drop `b'
        }
    }
    if `hasover' local b `B'
    local K `k' // number of equations in b
    if "`dots'"=="" di as txt "done)"
    
    // returns
    _ms_build_info `b' `awgt'
    tempname V
    matrix `V' = `b'' * `b' * 0 
    if `vce' {
        qui total `IFs' [`weight'`exp'], `vceopt'
        capt assert (e(N)==`est_N')
        if _rc==1 exit _rc
        if _rc {
            di as error "something is wrong; inconsistent estimation sample"
            exit 498
        }
        mata: st_replacematrix(st_local("V"), st_matrix("e(V)"))
        local rank = e(rank)
        if `"`clustvar'"'!="" {
            local evce `"`e(vce)'"'
            local vcetype `"`e(vcetype)'"'
        }
        else {
            local evce robust
            local vcetype Robust
        }
        mata: _lnmor_restore()
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
    if `hasover' {
        return local over          "`over'"
        return local over_namelist "`olevels'"
        return local over_labels   `"`olabels'"'
    }
    if `vce' {
        return local clustvar `"`clustvar'"'
        if `"`clustvar'"'!="" {
            return scalar N_clust = e(N_clust)
        }
        return local vce `"`evce'"'
        return local vcetype `"`vcetype'"'
        return scalar rank = `rank'
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

program _collect_fvinfo
    local J 0
    local name
    local bn 1
    foreach t of local 0 {
         _ms_parse_parts `t'
         if !inlist("`r(type)'", "variable", "factor") {
             di as err "`r(type)' terms not allowed in {it:varlist}"
             exit 198
         }
         if "`r(name)'"!="`name'" {
             if `J' {
                 c_local term`J' "`term'"
                 c_local type`J' "`type'"
                 c_local name`J' "`name'"
                 c_local bn`J'   "`bn'"
             }
             local term
             local type
             local name
             local bn 1
             local ++J
         }
         local term `term' `t'
         local type `r(type)'
         local name `r(name)'
         if r(base)==1 local bn 0
    }
    c_local term`J' "`term'"
    c_local type`J' "`type'"
    c_local name`J' "`name'"
    c_local bn`J'   "`bn'"
    c_local nterms   `J'
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
        */ k(str) estcmd(str) [ over(str) olevel(str) wvar(str) ifs(str) /*
        */ mifs(str) mvars(str) mcons(str) nodots ]
    
    // weights
    if "`wvar'"!="" local wgt "[aw=`wvar']"
    
    // whether outcome model has constant
    if "`type'"=="factor" & `bn' local nocons noconstant
    
    // over
    if "`over'"!="" {
        qui replace `over' = `olevel'
    }
    
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
        if "`dots'"=="" di "." _c
    }
    drop `name'
    
    // result
    rename `name1' `name'
    tempname ecurrent
    _estimates hold `ecurrent', restore
    qui fracreg logit `p' `term' [iw=`w'] in 1/`k', `nocons'
    drop `name'
    rename `name0' `name'

    // generate IFs
    if `vce' {
        qui predict double `PS', cm
        mata: __lnmor_finalize_IF()
    }
    
    // return
    tempname b
    matrix `b' = e(b)
    if "`nocons'"=="" {
        matrix `b' = `b'[1,1..colsof(`b')-1]
    }
    mat coleq `b' = ""
    return matrix b = `b'
end

program _get_model_IF
    tempname sc
    qui predict double `sc', score
    capt confirm matrix e(V_modelbased)
    if _rc==1 exit _rc
    if _rc local V V
    else   local V V_modelbased
    mata: _get_model_IF(st_local("0"), "e(`V')", "`sc'")
    c_local mvars `xvars'
    c_local mcons  `cons'
end

version 11
mata:
mata set matastrict on

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
    // obtain levels
    a = selectindex(_mm_unique_tag(X))
    if (st_local("type"+j)!="factor") {
        // appply binning if necessary
        k = strtoreal(st_local("kmax"))
        if (rows(a)>k) {
            if (dots) printf("{txt}(%s has %g levels",st_local("name"+j),rows(a))
            _get_levels_bin(X, w, k)
            a = selectindex(_mm_unique_tag(X))
            if (dots) printf("; reduced to %g)\n",rows(a))
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

// void _get_levels_bin(real colvector X, real colvector w, real scalar k)
// {
//     real scalar    i, j, qj, cj
//     real colvector q
//     
//     q = _mm_unique(_mm_quantile(X, w, rangen(0,1,k)*(1-1/k):+1/(2*k)))
//     j = rows(q); i = rows(X)
//     if (j==1) { // single quantile only => binned X will be constant
//         X = J(i, 1, q)
//         return
//     }
//     qj = q[j]
//     cj = (qj+q[j-1])/2
//     for (; i; i--) {
//         while (X[i]<cj) {
//             j--
//             qj = q[j]
//             if (j==1) cj = X[1]
//             else      cj = (qj+q[j-1])/2
//         }
//         X[i] = qj
//     }
// }

void _mktmpnames(string scalar nm, real scalar k)
{
    st_local(nm, invtokens(st_tempname(k)))
}

void _get_model_IF(string scalar IFs, string scalar V, string scalar score)
{
    real scalar      k, cons
    string rowvector xvars
    real colvector   sc
    real matrix      X, IF
    
    // obtain X
    xvars = st_matrixcolstripe(V)[,2]'
    k = length(xvars)
    if (strpos(xvars[k], "."))
         cons = (substr(xvars[k], strpos(xvars[k],".")+1, .)=="_cons") 
    else cons = (xvars[k]=="_cons")
    if (cons) {
        k = k - 1
        st_local("cons", "cons")
    }
    if (k) {
        xvars = xvars[|1\k|]
        st_view(X=., ., xvars)
        st_local("xvars", invtokens(xvars))
    }
    // obtains score
    st_view(sc=., ., score)
    // compute IFs
    st_view(IF=., ., st_addvar("double", tokens(IFs)))
    if (cons) IF[.,k+1]      = sc
    if (k)    IF[|1,1 \.,k|] = sc:*X
    IF[.,.] = IF * st_matrix(V)'
}

void _lnmor_restore()
{
    string scalar    touse
    real scalar      ifgen
    string colvector IFs
    real matrix      IF
    
    ifgen = st_local("ifgenerate")!=""
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
    cons = st_local("mcons")!=""
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
    G = invsym(quadcross(X,cons, w:*p:*(1:-p), X, cons))
    p = st_data(., st_local("Y")) - p  // Y - p
    st_view(IF=., ., st_local("ifs"))
    IF[.,.] = ((X:*p, J(1,cons,p)) + IF) * G'
}

end

