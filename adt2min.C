//---------------------------------------------------------------------
// RCC 20070720 Program to deal with DTA (deterministic tree automata )
// TODO: strip WS in tags;
//---------------------------------------------------------------------
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <queue>
#include <ext/hash_map>
#include "Error.h"
#include "Partition.h"

#ifndef __ROOT    // ROOT state cannot appear at lhs
#define __ROOT 0  // Then, it can be used as absorption state
#endif

/*
 * Template to generate a string representation.
 */
namespace rcc {
  template<class T>
  std::string 
  toString (const T& t) {
    std::stringstream buff;
    buff << t;
    return buff.str();
  }
}

//-----------------------------------------------------------------------
// Collection is a vector with two-way access
//-----------------------------------------------------------------------
template <class T> 
class Collection {
  std::vector <const T*> element;
  std::map <const T, uint> index;

public:
  size_t  size (void)  const { return element.size(); }
  const T& operator[] (uint n) const { return  *element[n]; }
  uint operator[] (const T& t) {
    std::pair<typename std::map<const T, uint>::iterator, bool> r; 

    r = index.insert( std::pair<const T, uint>(t, index.size()) );
    if ( r.second ) 
      element.push_back( &r.first->first );
    
    return (r.first->second);
  }
};

//----------------------------------------------------------------------------
// rhs of rule (label stored as uint)
typedef             
std::vector<uint> Right;

// <rhs,lhs> pairs
typedef            
std::map<const Right, uint> Rules;
  
// Subset of states
typedef
std::set<uint> States;

// Subset of transitions
typedef
std::set<const Right*> Rights; // set could be a vector
  
// Store reverse transitions
typedef
std::map<uint, Rights> BackTrack; 

// Triples of ints
typedef
std::pair<uint, std::pair<uint, uint> > Triple; 

// main call option
typedef
enum { HELP, BUILD, ADD, DEL, PARSE, MINIM } Option; 

//----------------------------------------------------------------------------
// DTA
class DTA {

  Collection<std::string> L;       // Labels
  std::set<uint> Q;                // States
  Rules  R;                        // Transitions
  std::set <uint> F;               // Accepting states
  uint qabs;                       // Absorption state 
  std::map<uint, uint> C;          // # of rules with rhs 
  Option opt;                      // Stores option

  friend std::istream& operator>> (std::istream& is, DTA& );
  friend std::ostream& operator<< (std::ostream& os, const DTA&);

private:
  void           reset (void);
  bool         accepts (uint q) const { return F.find(q) != F.end(); }  
  uint        nextFree (void) { return 1 + *Q.rbegin(); }
  bool        tokenize (const std::string&, uint&, Right&);
  std::string toString (const Right&) const;
  void        addState (uint);
  void         rmState (uint);
  void         addRule (uint, const Right&);
  uint           delta (const Right&) const;
  bool             neq (uint, uint, std::map<uint, uint>&) const;
  void           merge (std::map<uint,uint>&);
  void           merge (const Partition&);
  void         initial (Partition&,States&);
  int         readTree (std::istream&);
  void           clone (uint, uint);
  int          addTree (std::istream&, States&);
  Triple        triple (uint, uint, uint);
  std::map<uint,States>
  split (const Partition&, const Right*, uint);
    
public:
  DTA (void) : qabs(__ROOT), opt(ADD) { Q.insert(qabs); }
  void   setOpt (Option _opt) { opt = _opt; }
  Option getOpt (void) const { return opt; }
  size_t  Qsize (void) const { return Q.size(); }  
  size_t   size (void) const;
  DTA&    minimize (void);
  DTA&    build (std::istream&);
  DTA&    parse (std::istream&);
  DTA&      add (std::istream&);  
  
};

std::istream&  
operator>> (std::istream& is, DTA& A) {
  Right r;
  uint q;
  std::string line;

  A.reset();

  while ( getline(is, line) ) {
    if ( A.tokenize(line, q, r) ) {
      if( q == __ROOT ) { 
	A.addState(r[0]);
	A.F.insert(r[0]);
      } else {
	A.addRule (q, r);
      }
    } else {
      Error::fatal("istream>>DTA", line);
    }
  }

  return is;
}

std::ostream&  
operator<< (std::ostream& os, const DTA& A) {
  Rules::const_iterator rule;
  States::const_iterator state;
  
 
  for ( rule = A.R.begin(); rule != A.R.end(); ++rule )
    os << rule->second << ' ' << A.toString(rule->first) << std::endl;


  if ( A.F.empty() )
    Error::fatal("ostream<<DTA", "DTA has no accepting states");
  else
    os << __ROOT << ' ' << *A.F.begin();

  for ( state = ++A.F.begin(); state != A.F.end(); ++state )
    os << '\n' << __ROOT << ' ' << *state;

  return os;
}

void
DTA::reset (void) {
  Q.clear();
  Q.insert(qabs);
  R.clear();
  F.clear();
  C.clear();
}

bool
DTA::tokenize (const std::string& _string, uint& q, Right& r) {
  std::stringstream buffer;
  std::string label;
  uint n;

  r.clear();
  buffer << _string;

  if ( buffer >> q ) {

    if ( q == __ROOT) {
      if ( buffer >> n )
	r.push_back(n);
      else
	return false;
    } else {
      if ( buffer >> label )
	r.push_back( L[label] );
      else
	return false; 
      while ( buffer >> n ) 
	r.push_back(n);
    }
  } else {
    return false;
  }
  return true;
}
  
std::string
DTA::toString ( const Right& r ) const {
  std::stringstream buffer;
  
  if ( r.size() > 0 )
    buffer << L[ r[0] ];
  for ( uint k = 1; k < r.size(); ++k ) 
    buffer  << ' ' << r[k];

  return buffer.str();
}

void
DTA::addState (uint q) {
  if ( q == qabs ) 
    Error::fatal("DTA::addState ", rcc::toString(qabs) +
		 " is a reserved state label");
  else
    Q.insert(q);

}
 
void
DTA::addRule (uint q, const Right& r ) {
  addState(q);
  for ( uint k = 1; k < r.size(); ++k ) 
    addState( r[k] );
  
  if ( R.find(r) == R.end() ) {
    R[r] = q;
    ++C[q];
  } else  if ( R[r] != q ) {
    Error::fatal("DTA::addRule", "non-deterministic automaton");
  }
} 

uint
DTA::delta (const Right& r) const {
  if ( R.find(r) == R.end() )
    return qabs;  
  else
    return R.find(r)->second; 
  
}

bool
DTA::neq (uint i, uint j, std::map<uint,uint>& E) const {  
  uint q;
  Right r;
  Rules::const_iterator rule;

  if ( accepts(i) != accepts(j) )
    return true;

  for ( rule = R.begin(); rule != R.end(); ++rule ) {
    r = rule -> first;
    q = rule -> second;
    for ( uint k = 1; k < r.size(); ++k ) {
      if ( r[k] == i ) {
	r[k] = j;
	if ( E[ delta(r) ] != E[q] )
	  return true;
	r[k] = i;        // keep r = rule->first
      } 
    }
   
    for ( uint k = 1; k < r.size(); ++k ) {
      if ( r[k] == j ) 
	r[k] = i;
    }
    if ( r != rule->first && E[ delta(r) ] != E[q] )
      return true;
    
  }

  return false;
}

void
DTA::rmState( uint q ) {
  Q.erase(q);
  F.erase(q); 
  C.erase(q);
}

void
DTA::merge ( std::map<uint,uint>& E ) {
  States::const_iterator state;
  Right r;

  state = Q.begin(); 
  while ( state != Q.end() ) {
    if ( E.find(*state) == E.end() )
      Error::fatal("DTA::merge", rcc::toString(*state) +
		   " has no equivalence class");
   
    if ( *state != E[*state] ) {
      C[ E[*state] ] += C[*state];
      rmState( * (state++) ); // beware iterator
    } else {
      ++state;
    }
  }

  Rules::iterator rule = R.begin(); 
  while ( rule != R.end() ) { 
    r = rule -> first;
    for ( uint k = 1; k < r.size(); ++k )
      r[k] = E [ r[k] ];   
    if ( r != rule->first ) {
      R[r] = E[ rule->second ];  
      R.erase(rule++);
    } else {
      rule -> second = E [ rule->second ];
      ++rule;
    }
  }
}

/**
 * Return size of transiton table
 */
  size_t
    DTA::size() const {
    uint size = 0;
    Rules::const_iterator rule;
    for ( rule = R.begin(); rule != R.end(); ++rule ) {
      size += rule->first.size();
    }
    return size;
  }


  Triple
    DTA::triple( uint i, uint j, uint k ) {
    return Triple( i, std::pair<uint,uint>(j ,k) );
  }

  typedef 
    std::set<Triple> Signature;

  void
    DTA::initial (Partition& P, States& K) {
    std::map<uint, Signature> signatures;
    Rules::iterator rule;
    States::iterator state;
    uint q;
    for ( rule = R.begin(); rule != R.end(); ++rule ) {
      Right right = rule->first;
      uint left = rule->second;
      for ( int k = 1; k < right.size(); ++k ) {
	Triple t = triple (right[0], right.size() - 1, k);
	q = right[k];
	signatures[q].insert(t);
      }
    }
    for ( state = F.begin(); state != F.end(); ++state ) {
      signatures[*state].insert( triple(0, 0, 0) );
    }

    std::map<Signature, States> blocks;
    std::map<Signature, States>::iterator block;
    for ( state = Q.begin(); state != Q.end(); ++state ) {
      q = *state;
      if ( q != qabs ) 
	blocks[ signatures[q] ].insert(q);
    }

    for ( block = blocks.begin(); block != blocks.end(); ++block ) {
      q = *block->second.begin();
      P.refine(block->second);
      K.insert(q);
    }
    std::cerr << " -> " << K.size();
  }

/* Name:	minim
 * Class:	DTA
 * Space:	None.
 * Purpose:     Minimizing the automaton
 * Parameters:	None
 * Returns:	The minimized automaton
 * Globals:	None.
 * Remarks:	It refines a partition P till all states in the same class
 are equivalent; when no further refinement is possible, 
 then merge is called.
*/
  DTA&
    DTA::minimize (void) {
    BackTrack B;                  // Stores transitions with a given output
    Partition P(Q);               // State set partition
    States K;                     // Partition frontier
    States::iterator state;       // Iterator over set of states
    Rules::iterator rule;         // Iterator over rules.
    uint numIter = 1;             // Number of iterations

    if ( F.empty() )
      Error::fatal("DTA::minimize", "DTA has no accepting state");

    // Build B 
    for (rule = R.begin(); rule != R.end(); ++rule ) {
      B[ rule->second ].insert( &rule->first );
    }

    // Create initial P and K with two classes.
    initial(P, K);
   
    // Minimization loop
    while ( K.size() > 0 && P.getSize() < Q.size() ) {  
      std::map<uint,States> subparts;             // subparts
      std::map<uint,States>::iterator part, largest;   
      States states = P.block( *K.begin() );
      Rights::const_iterator iter;
      ++numIter; 
      //    std::cerr << "Checking " << *K.begin() << ' ' << states << std::endl;
      K.erase( K.begin() );
      for ( state = states.begin(); state != states.end(); ++state ) {
	//   std::cerr << "Checking " << B[*state].size() << " trans" << std::endl;
	    for ( iter = B[*state].begin(); iter != B[*state].end(); ++iter ) {
	      Right right = **iter;
		uint left = delta(right);  // store output
		for ( uint k = 1; k < right.size(); ++k ) {   // loop argument positions
		  uint q = right[k];                          // original argument
		  right[k] = P.next(q);                       // replace by next
		  if ( ! P.equiv( delta(right), left ) ) {    // inconsistent
		    subparts.clear();
		    uint i = P.first(q); 
		    uint size = 0;
		    do {                                     // Refine q-class 
		      right[k] = i;
		      subparts[ P.first( delta(right) ) ].insert(i);  
		      i = P.next(i);
		      ++size;
		    } while ( i != P.first(q) );
			largest = subparts.begin();
			for ( part = subparts.begin(); part != subparts.end(); ++part ) { 
			  if ( part->second.size() > largest->second.size() ) 
			    largest = part;
			}
			for ( part = subparts.begin(); part != subparts.end(); ++part ) {
			  uint j = *( part->second.begin() );
			  if ( part != largest ) { // update P
			    P.refine( part->second );
				K.insert(j);                  // update K
			  }
			}
		  }
		    right[k] = q; // keep right (restore argument)
		}
	    }
      }
    }
    std::cerr << " -> (" << numIter << "iter)";
    merge(P);
    return *this;
  }

  void
    DTA::merge(const Partition& P) {
    States::const_iterator state = Q.begin(); 
    while ( state != Q.end() ) {
      uint rep = P.first( *state );  // representative
      if ( rep != *state ) {
	C[ rep ] += C[*state];
	rmState( * (state++) ); // beware iterator
      } else {
	++state;
      }
    }

    Rules::iterator rule = R.begin(); 
    while ( rule != R.end() ) { 
      Right r = rule -> first;
      for ( uint k = 1; k < r.size(); ++k )
	r[k] = P.first( r[k] );   
      if ( r != rule->first ) {
	R [r] = P.first( rule->second );  
	R.erase(rule++);
      } else {
	rule -> second = P.first( rule->second  );
	++rule;
      }
    }
  }

  int   // does not check well-formedness
    DTA::readTree (std::istream& is) { 
    char c;  
    std::string s;
    Right r;
    int n;

    is >> c;

    if ( is.eof() ) 
      return EOF;
    else if ( c != '<'  || ! getline(is, s, '>' ) )
      Error::fatal("DTA::readTree", "wrong tree format");

    if ( s.size() == 0 )
      Error::fatal("DTA::readTree", "missing tag");
    else if ( s[0] == '/' )                    // end tag
      return -L[ s.substr( 1, s.size() - 1 )]; // may be 0
    else if ( s[s.size() - 1] == '/' )         // leaf
      r.push_back( L[ s.substr( 0, s.size() - 1 ) ] );  
    else {
      r.push_back( L[s] );
      while ( (n = readTree(is)) > 0 ) 
	r.push_back(n);
    }

    if ( opt != PARSE && R.find(r) == R.end() )
      addRule(nextFree(), r);

    return delta(r); 
  }     


  void  // works because R is ordered upwards   
    DTA::clone ( uint i, uint j ) { 
    Rules::iterator rule;
    Right r;

    if ( i >= j )
      Error::fatal("DTA::clone", "cloned state is newer" ); 
    if ( accepts(i) )
      F.insert(j);
    for ( rule = R.begin(); rule != R.end(); ++rule ) {
      r = rule->first;
      for ( uint k = 1; k < r.size(); ++k )
	if ( r[k] == i ) { 
	  r[k] = j;
	  if ( R.find(r) == R.end() ) {
	    R[r] = rule->second; 
	    ++C[rule->second];
	  }
	  r[k] = i;
	}
    }
  }

  int
    DTA::addTree (std::istream& is, States& Theta) { 
    char c;  
    std::string s;
    Right r;
    int n, q;

    is >> c;
    if ( is.eof() ) 
      return EOF;
    else if ( c != '<' || ! getline(is, s, '>') )
      Error::fatal("DTA::addTree", "wrong tree format");

    if ( s.size() == 0 )
      Error::fatal("DTA::addTree", "missing tag");
    else if ( s[0] == '/' )             // end tag 
      return -L[ s.substr( 1, s.size() - 1 )];
    else if ( s[s.size() - 1] == '/' )  // leaf
      r.push_back( L[ s.substr( 0, s.size() - 1 ) ] );  
    else {
      r.push_back( L[s] );
      while ( (n = addTree(is,Theta)) > 0 ) 
	r.push_back(n);
    }

    q = delta(r);
    if ( C[q] != 1 ) {
      n = nextFree();
      addState(n); 
      C[n] = 1;
      if ( C[q] > 0 )
	clone(q,n);
      R[r] = n;
      for ( int k = 1; k < r.size(); ++k )
	Theta.insert( r[k] );
    } 
    return R[r];
  }

  DTA&
    DTA::build (std::istream& is) {
    int n;
    while ( (n = readTree(is)) > 0 ) 
      F.insert(n);

    return *this;
  }

  DTA&
    DTA::parse (std::istream& is) {
    int n;
    while ( (n = readTree(is)) > 0 )  
      std::cout << accepts(n); 

    return *this;
  }

  DTA&
    DTA::add (std::istream& is) {
    int n;
    States Theta;
    Right r;
    std::set<uint> P;
    std::set<uint>::iterator i;
    std::set<uint>::reverse_iterator j;
    std::map<uint,uint> E;
    Rules::iterator rule;

    if ( C.empty() )
      for ( rule = R.begin(); rule != R.end(); ++rule )
	++C[rule->second];

    while ( (n = addTree(is, Theta)) > 0 ) {
      F.insert(n); 
      Theta.insert(n);

      // very inefficient minimization follows
      for ( i = Q.begin(); i != Q.end(); ++i )
	E[*i] = *i;
      for ( j = Theta.rbegin(); j != Theta.rend(); ++j ) {
	for ( i = Q.begin(); i != Q.end() && E[*j] == *j; ++i ) {
	  if ( ! ( *i == *j || neq( *i, *j, E) ) ) 
	    E[*j] = *i;	       
	}	
      }
      merge(E);
      E.clear();
      Theta.clear();
    }

    return *this;
  }

//==========================================================
  int 
    main (int argc, char** argv) {
    DTA A;
    std::ifstream f;

    if ( argc == 1 ) {
      A.setOpt( BUILD ); 
    } else {
      for ( int k = 1; k < argc; ++k ) {
	if ( std::string(argv[k]) == "-h" ) {
	  A.setOpt( HELP );
	} else if ( std::string(argv[k]) == "-a"  ) {
	  A.setOpt( ADD );
	} else if ( std::string(argv[k]) == "-d" ) {
	  A.setOpt( DEL );
	} else if ( std::string(argv[k]) == "-p" ) {
	  A.setOpt( PARSE );
	} else if ( std::string(argv[k]) == "-m" ) {
	  A.setOpt( MINIM );
	} else { 
	  f.open( argv[k] ); 
	  if ( !f ) {
	    A.setOpt( HELP );
	    break;
	  } else
	    f >> A;
	}
      }
    }

    switch ( A.getOpt() ) { 
	case HELP:
	  std::cerr << "usage:\n"
		    << "adt2min < tree_file \t build minimal DTA\n" 
		    << "adt2min [-a] dta_file < tree_file \t"
		    << "add trees to minimal DTA\n"
		    << "adt2min -d dta_file < tree_file \t"
		    << "remove trees from minimal DTA\n" 
		    << "adt2min -p dta_file < tree_file \t"
		    << "parse trees from DTA\n"
		    << "adt2min -m dta_file\tminimize DTA"
		    << std::endl;
	  break;
	case BUILD:
	  A.build( std::cin ); 
	  std::cerr << A.size() << ':' << A.Qsize();
	  A.minimize();
	  std::cerr << " -> " << A.Qsize() << std::endl;
	  break;
	case ADD:
	  std::cerr << A.Qsize() << " -> ";
	  A.add( std::cin );
	  std::cerr << A.Qsize() << std::endl;
	  break;
	case DEL:
	  //  A.del( cin );
	  break;
	case PARSE:
	  A.parse( std::cin );
	  break;
	case MINIM:
	  A.minimize();
	  break;
    }

    if ( A.getOpt() != PARSE )
      std::cout << A << std::endl;
    return 0;
  }




  
