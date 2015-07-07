#include "Partition.h"
/* Name:	Partition class constructor
 * Class:	Partition.
 * Space:	None.
 * Purpose:	Creates a trivial partition with a single class.
 * Parameters:	Q 		- (i) input set.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	Q cannot be empty.
 */
Partition::Partition ( const std::set<uint>& Q ) { 
  if ( Q.empty() ) {
    Error::fatal( "Partition", "set canot be empty");
  } else {
    P.push_back(Q);
    for ( std::set<uint>::iterator i = Q.begin(); i != Q.end(); ++i ) {
      B[*i] = 0;
    }
  }
}
 
/**
 * Return the representative of the given element.
 * @param k an element in the partitioned set.
 * @return the smallest integer in same class as k
 */
uint 
Partition::first (uint k) const {
    return *P[B.find(k)->second].begin(); 
  } 

/**
 * Return next element in class (cyclic).
 * @param k an element in the partitioned set.
 * @return the smallest integer larger than k if there is one in the class
 * and the smallest integer in class otherwise.
 */
uint  
Partition::next (uint k) const { 
    uint n = B.find(k)->second;
    std::set<uint>::const_iterator i = ++P[n].find(k);
    if ( i == P[n].end() )
	return *P[n].begin();
    else
	return *i;
}  

/* Name:	equiv
 * Class:	Partition.
 * Space:	None.
 * Purpose:	Returns true if i and j are equivalent.
 * Parameters:	i 		- (i) state number.
                j               - (i) state number
 * Returns:	True if i and j are equivalent false otherwise.
 * Globals:	None.
 * Remarks:	Does not check limits.
 */
bool
Partition::equiv(uint i, uint j) const {
  return B.find(i)->second == B.find(j)->second;
}

/* Name:	refine
 * Class:	Partition.
 * Space:	None.
 * Purpose:	Splits container class into two subclasses: the input subset and the remainder.
 * Parameters:	subset 		- (i) subset of states.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	Input subset must be properly contained in a class. 
                Input subset cannot contain a class representive.
 */
void
Partition::refine( const std::set<uint>& subset ) {
  std::set<uint>::const_iterator i = subset.begin();  
  for ( i = subset.begin(); i != subset.end(); ++i ) {
    P[ B[*i] ].erase(*i);
    B[*i] = P.size();
  }
  P.push_back(subset);
}

/**
 * Return equivalence class.
 * 
 */
 std::set<uint> 
 Partition::block (uint k) const {
   return P[ B.find(k)->second ];
  }

/**
 * Output operator
 */
std::ostream& 
operator<< (std::ostream& os, const Partition& P) {
  os << '[' << P.P << ']';
  return os;
};
