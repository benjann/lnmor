*! version 1.0.2  31aug2022  Ben Jann
*! helper program for -lnmor- after -svy-; do not use manually

program _lnmor_svyr, eclass properties(svyr)
    version 15
    local version : di "version " string(_caller()) ":"
    `version' lnmor `0'
    local k_eq = e(k_eq)
    tempname b
    mat `b' = e(b)
    ereturn matrix b_lnmor = `b', copy
    mata: _ereturn_svy_rename()
    ereturn repost b=`b', rename
    eret local k_eq_lnmor "`k_eq'"
    eret local predict "_lnmor_svyr_p"
end

version 15
mata:
mata set matastrict on

void _ereturn_svy_rename()
{
    string matrix cstripe
    
    cstripe = st_matrixcolstripe(st_local("b"))
    cstripe[,1] = cstripe[,1] :+ "@" :+ cstripe[,2]
    cstripe[,2] = J(rows(cstripe), 1, "_cons")
    st_matrixcolstripe(st_local("b"), cstripe)
}

end
exit
