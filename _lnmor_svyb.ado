*! version 1.0.1  31aug2022  Ben Jann
*! helper program for -lnmor- after -svy-; do not use manually

program _lnmor_svyb, properties(svyb svyj)
    version 15
    local version : di "version " string(_caller()) ":"
    `version' lnmor `0'
end

