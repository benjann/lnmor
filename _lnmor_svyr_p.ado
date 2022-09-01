*! version 1.0.1  31aug2022  Ben Jann
*! helper program for -lnmor- after -svy-; do not use manually

program _lnmor_svyr_p
    version 15
    syntax [anything] [if] [in], [ SCores ]
    _score_spec `anything', scores
    local vlist `s(varlist)'
    local tlist `s(typlist)'
    mata: st_store(., /*
        */ st_addvar(tokens(st_local("tlist")), tokens(st_local("vlist"))), /*
        */ *findexternal("_LNMOR_TMP_IFs"))
    mata mata drop _LNMOR_TMP_IFs
end


