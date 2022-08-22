*! version 1.0.0  21aug2022  Ben Jann
*! helper program for -lnmor- after -svy-; do not use manually

program _lnmor_svyb, properties(svyb svyj)
    version 14
    local version : di "version " string(_caller()) ":"
    `version' lnmor `0'
end

