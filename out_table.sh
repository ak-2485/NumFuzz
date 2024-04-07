#!/bin/bash

in_file=tests.txt   # Input file

res_file=table.txt  # Temporary file
sep=' '             # Separator character

# Print header
row1=("Benchmark" " " "Bound" " " " " "Timing (s)");
row2=(" " "NumFuzz" "FPTaylor" "Gappa" "NumFuzz" "FPTaylor" "Gappa");
printf  '%-20s ' "${row1[@]}" >> $res_file;
printf  '%s\n' "" >> $res_file; 
printf  '%-20s ' "${row2[@]}" >> $res_file;
printf  '%s\n' "" >> $res_file; 

# Parse and print values
bstr="START BENCHMARK:";
nstr="I  [General] : Relative error:";
ntstr="Execution time:";
fstr="Relative error (exact):";
ftstr="Elapsed time:";
gstr="{";
gtstr="real";
while read line; do 
  if [[ $line =~ "$bstr" ]] ; then 
    tmpb=${line#*$bstr}; 
    bnch=${tmpb% *}; fi
  if [[ $line =~ "$nstr" ]] ; then 
    nbnd=${line#*error:}; fi
  if [[ $line =~ "$ntstr" ]] ; then 
    tmpnt=${line#*$ntstr}; 
    nt=${tmpnt%s*};  fi
  if [[ $line =~ "$fstr" ]] ; then
    tmp=${line#*$fstr}; 
    fbnd=${tmp% *}; 
    else if [[ $line =~ "Cannot compute" ]]; then
            fbnd="${sp2}-${sp}"; fi ; fi
  if [[ $line =~ "$ftstr" ]] ; then
    ft=${line#*$ftstr}; fi
  if [[ $line =~ "$gstr" ]] ; then  
    tmpg=${line#*$gstr}; 
    gbnd=${tmpg%,*}; fi
  if [[ $line =~ "$gtstr" ]] ; then 
    tmpgt1=${line#*$gtstr}; 
    tmpgt=${tmpgt1#*0m}; 
    gt=${tmpgt%s*}; 
    row=($bnch $nbnd $fbnd $gbnd $nt $ft $gt);
    printf  '%-20s ' "${row[@]}" >> $res_file;
    printf  '%s\n' "" >> $res_file; fi
done < $in_file
