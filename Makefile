OBJS = adt2min Partition.o

adt2min: Partition.o adt2min.C
	c++  -O3 -o adt2min adt2min.C Partition.o

Partition.o: Partition.h Partition.C
	c++ -O3 -c Partition.C

clean:
	rm $(OBJS)
