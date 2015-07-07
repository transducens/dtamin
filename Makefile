OBJS = adt2min Partition.o pre.C xml.C xml

adt2min: Partition.o adt2min.C
	c++  -O3 -o adt2min adt2min.C Partition.o
Partition.o: Partition.h Partition.C
	c++ -O3 -c Partition.C
pre.C: xml.l
	flex -opre.C xml.l
xml.C: pre.C xml.y
	bison -oxml.C xml.y 
xml: xml.C
	c++ -O3 -oxml xml.C 
tar:
	tar cvf progs_dta.tar ../dta
clean:
	rm $(OBJS)