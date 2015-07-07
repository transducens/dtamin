OBJS = pre.C xml.C xml

all: xml

pre.C: xml.l
	flex -opre.C xml.l

xml.C: pre.C xml.y
	bison -oxml.C xml.y 

xml: xml.C
	c++ -O3 -oxml xml.C 

clean:
	rm $(OBJS)
