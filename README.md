# dtamin
DTA minimization

A frontier-to-root deterministic finite-state tree automaton
(DTA) can be used as a compact data structure to store collections of
unranked ordered trees. DTAs are usually sparser than string automata,
as most transitions are undefined and therefore, special care must be
taken in order to minimize them efficiently. However, it is difficult to
find simple and detailed descriptions of the minimization procedure in
the published literature. Here, we fully describe a simple implementation
of the standard minimization algorithm that needs a time in O(|A|^2),
with |A| being the size of the DTA.

Reference: 	Rafael C. Carrasco, Jan Daciuk, Mikel L. Forcada:
An Implementation of Deterministic Tree Automata Minimization. CIAA 2007: 122-129


To compile just type Make

To learn about usage type adt2min -h

To test the code type ./debug.sh N M

* N= # of trees to build initital DTA, e.g. 80; 
* M = # of trees to add, e.g., 10 



Sample script to test the incremental construction:
```
head -`expr $1 + $2` xmldata.xml > large_sample.xml
head -$1 xmldata.xml > small_sample.xml
time ./adt2min < large_sample.xml > large_dta
./adt2min < small_sample.xml > small_dta
tail -$2 large_sample.xml | ./adt2min small_dta > incremental_dta
```
