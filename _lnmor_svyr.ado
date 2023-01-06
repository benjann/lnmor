*! version 1.0.3  30dec2022  Ben Jann
*! helper program for -lnmor- after -svy-; do not use manually

program _lnmor_svyr, eclass properties(svylb)
    version 15
    local version : di "version " string(_caller()) ":"
    gettoken subcmd 0 : 0
    if `"`subcmd'"'=="predict" {
        _lnmor_svyr_p `0'
        exit
    }
    if `"`subcmd'"'!="estimate" exit 198
    `version' lnmor `0'
    eret local predict "_lnmor_svyr predict"
end

program _lnmor_svyr_p
    version 15
    syntax [anything] [if] [in], [ SCores ]
    _score_spec `anything', ignoreeq
    local vlist `s(varlist)'
    local tlist `s(typlist)'
    mata: st_store(., /*
        */ st_addvar(tokens(st_local("tlist")), tokens(st_local("vlist"))), /*
        */ *findexternal("_LNMOR_TMP_IFs"))
    mata mata drop _LNMOR_TMP_IFs
end
