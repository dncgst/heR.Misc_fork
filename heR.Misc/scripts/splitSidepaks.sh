#used the following command to split the file into many SP files
csplit -f "Sidepak_$1_" -b %02d.csv $1 "/SidePak Aerosol Monitor/" {*}
