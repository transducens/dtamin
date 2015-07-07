head -`expr $1 + $2` xxx > xx
head -$1 xxx > x
time ./adt2min < xx > cc
./adt2min < x > c
tail -$2 xx | ./adt2min c > ccc