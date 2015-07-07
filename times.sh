for i in  972 2016 4250 8905 16871
  do
  head -$i xmldata > xx
  time ./adt2min < xx > cc 
done
