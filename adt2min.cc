//---------------------------------------------------------------------
// RCC 20040330 Program to deal with DTA (deterministic tree automata )
// TODO: strip WS in tags; accelerate minim by separating qabs ab-initio
//---------------------------------------------------------------------
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <queue>
#include <list>

#include "Error.h"

// Number 0 is the absorption state, but it also marks final states
#ifndef __ROOT    // ROOT state cannot appear at rhs
#define __ROOT 0  // Then, it can be used as absorption state
#endif

// Convert a value of type T to its string representation
template<class T>
std::string 
toString (const T& t) {
  std::stringstream buff;
  buff << t;
  return buff.str();
}

#ifdef JD2
enum equiv_results { s_equiv, s_nequiv, s_notfound };
#endif

//-----------------------------------------------------------------------
// Collection is a vector with two-way access
// Converts objects (usually strings) to the corresponding integers
// and vice versa (use [] operator).
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
typedef             // rhs of rule (label stored as uint)
std::vector<uint> Right;       
typedef             // <rhs,lhs> pairs
std::map<const Right, uint> Rules;
typedef             // main call option
enum { HELP, BUILD, ADD, DEL, PARSE, MINIM } Option; 
#ifdef JD
typedef		    // a set of right-hand sides of rules
std::set<Right> DeltaArgs;
#ifdef JD2
typedef		    // how many times a state is target of a transition
                    // originating at a given state
std::map<uint,uint> TargetCount;
#endif
#endif
//----------------------------------------------------------------------------
// DTA - Deterministic Tree Automaton (frontier-to-root)
class DTA {

  Collection<std::string> L;       // Labels
  std::set<uint> Q;                // States
  Rules  R;                        // Transitions
  std::set <uint> F;               // Accepting states
  uint qabs;                       // Absorption state 
  std::map<uint, uint> C;          // # of rules with lhs 
  Option opt;                      // Stores option (see enum above)
#ifdef JD
  std::vector<DeltaArgs> BackTr;   // State -> a set of RHSs
#ifdef JD2
  std::vector<TargetCount> followers;// {p->(q,|r|)|r=(...,p,...)->q}
  bool states_equiv2(const uint q1, const uint q2);
  equiv_results equiv_in_rule(const uint q1, const uint q2,
			      Right *rp, DeltaArgs *all, uint argindex);
  void repl_in_rule(const uint q1, const uint q2, Right rp,
		    DeltaArgs *all, const uint argindex);
  //  void print_rules(Rules &r);
#endif //JD2
  bool states_equivalent(const uint q1, const uint q2, const uint m, Right r1,
			 Right r2);
  bool contained_below(const uint q1, const uint q2);
#endif

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
  int         readTree (std::istream&);
  void         replace (uint, uint);
  void           clone (uint, uint);
  int         newRules (std::istream&, Rules&);
#ifdef JD
  DTA&		 jdmin (const uint n, const uint f, const uint m);
#endif
#ifdef JD2
  DTA&		jdmin2 (const uint n, const uint f, const uint m);
#endif

public:
            DTA (void) : qabs(__ROOT), opt(ADD) { Q.insert(qabs);
#ifdef JD
	    BackTr.push_back(DeltaArgs());
#ifdef JD2
	    followers.push_back(TargetCount());
#endif
#endif
	    }
  void   setOpt (Option _opt) { opt = _opt; }
  Option getOpt (void) { return opt; }
  size_t   size (void) { return Q.size(); }  
  DTA&    minim (void);
  DTA&    build (std::istream&);
  DTA&    parse (std::istream&);
  DTA&      add (std::istream&);  

};

/* Name:	operator>>
 * Class:	None (friend of class DTA).
 * Space:	None.
 * Purpose:	Reads the contents of the DTA from a stream.
 * Parameters:	is		- (i/o) input stream;
 *		A		- (o) the DTA that is read.
 * Returns:	The stream.
 * Globals:	None.
 * Remarks:	One line of the input contains one record describing
 *		either a transition or a final state.
 *
 *		The format of the line containing a transition is:
 *		state_number label state_number*
 *		where state_number* means any number of states
 *		(including zero),
 *		labels is a string, and state_number is an integer.
 *
 *		The format of a final state is:
 *		0 state_number
 *		0 (__ROOT) means that the following number is a final state.
 *		
 */
std::istream&  
operator>> (std::istream& is, DTA& A) {
  Right r;
  uint q;
  std::string line;

  A.reset();

  while ( getline(is, line) ) {
    if ( A.tokenize(line, q, r) ) {
      if( q == __ROOT ) { // we have just read a final state
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


/* Name:	operator<<
 * Class:	None (friend of class DTA).
 * Space:	None.
 * Purpose:	Writes the contents of the DTA to a stream.
 * Parameters:	is		- (i/o) output stream;
 *		A		- (o) the DTA that is to be written.
 * Returns:	The stream.
 * Globals:	None.
 * Remarks:	0 means that the following number is a final state.
 *		Unused states are not printed, i.e. states are printed
 *		only if they appear in at least one rule.
 */
std::ostream&  
operator<< (std::ostream& os, const DTA& A) {
  Rules::const_iterator m;
  std::set<uint>::iterator i;
  
  for ( m = A.R.begin(); m != A.R.end(); ++m )
    os << m->second << ' ' << A.toString(m->first) << std::endl;

  if ( A.F.empty() )
    Error::fatal("ostream<<DTA", "DTA has no accepting states");
  else
    os << __ROOT << ' ' << *A.F.begin();

  for ( i = ++A.F.begin(); i != A.F.end(); ++i )
    os << '\n' << __ROOT << ' ' << *i;

  return os;
}

/* Name:	reset.
 * Class:	DTA
 * Space:	None.
 * Purpose:	Clears the automaton.
 * Parameters:	None.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	qabs is the absorption state. An empty automaton has
 *		the absorption state.
 */
void
DTA::reset (void) {
  Q.clear();
  Q.insert(qabs);
  R.clear();
  F.clear();
  C.clear();
}

/* Name:	tokenize.
 * Class:	DTA.
 * Space:	None.
 * Purpose:	Splits strings representing transitions into a state number
 *		(destination state), label number and
 *		a sequence of state numbers.
 * Parameters:	_string		- (i) input string;
 *		q		- (o) the leading number (destination state);
 *		r		- (o) label number followed by a sequence
 *					of state numbers.
 * Returns:	true if reading OK, false otherwise.
 * Globals:	None.
 * Remarks:	r contains at least the label number.
 */
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
  
/* Name:	toString
 * Class:	DTA
 * Purpose:	Returns a string representation of a transition argument.
 * Parameters:	r		- (i) the transition argument.
 * Returns:	The string representation.
 * Globals:	None.
 * Remarks:	None.
 */
std::string
DTA::toString ( const Right& r ) const {
  std::stringstream buffer;
  
  if ( r.size() > 0 )
    buffer << L[ r[0] ];
  for ( uint k = 1; k < r.size(); ++k ) 
    buffer  << ' ' << r[k];

  return buffer.str();
}

/* Name:	addState
 * Class:	DTA
 * Space:	None.
 * Purpose:	Adds a state to the automaton.
 * Parameter:	q		- (i) state number.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	It check that we are not reusing the number reserved for
 *		the absorption state.
 */
void
DTA::addState (uint q) {
  if ( q == qabs ) 
    Error::fatal("DTA::addState ", ::toString(qabs) +
		 " is a reserved state label");
  else
    Q.insert(q);
#ifdef JD2
  while (followers.size() <= q) {
    followers.push_back(TargetCount());
  }
#endif
}
 
/* Name:	addRule
 * Class:	DTA.
 * Space:	None.
 * Purpose:	Adds a transition to the automaton.
 * Parameters:	q		- (i) destination state;
 *		r		- (i) argument of the transition
 *					(label and sequence of states).
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	C is a counter for the number of times the state q was used
 *		as the destination state.
 */
void
DTA::addRule (uint q, const Right& r ) {
  addState(q);
  /* JD: the following was redundant:
  for ( uint k = 1; k < r.size(); ++k ) 
    addState( r[k] );
  */
  
  if ( R.find(r) == R.end() ) {
    R[r] = q;
    ++C[q];
#ifdef JD
    BackTr.push_back(DeltaArgs());
    BackTr[q].insert(r);
#ifdef JD2
    // Update followers
    std::set<uint> already_processed;
    for (uint i = 1; i < r.size(); i++) {
      if (already_processed.find(r[i]) == already_processed.end()) {
	uint p = r[i];		// source state
	if (followers[p].find(q) == followers[p].end()) {
	  // q follows p for the first time
	  followers[p][q] = 1;
	}
	else {
	  followers[p][q]++;
	}
	already_processed.insert(p);
      }
    }
#endif
#endif
  } else  if ( R[r] != q ) {
    Error::fatal("DTA::addRule", "non-deterministic automaton");
  }
} 

/* Name:	delta
 * Class:	DTA
 * Space:	None.
 * Purpose:	Returns the destination state for the transition argument.
 * Parameters:	r		- (i) the argument of the transition.
 * Returns:	The destination state.
 * Globals:	None.
 * Remarks:	None.
 */
uint
DTA::delta (const Right& r) const {
  if ( R.find(r) == R.end() )
    return qabs;  
  else
    return R.find(r)->second; // R[r] warns constant qualifier
  
}

/* Name:	neq
 * Class:	DTA
 * Space:	None.
 * Purpose:	Checks if two states can be equivalent.
 * Parameters:	i		- (i) first state;
 *		j		- (i) second state;
 *		E		- (i) partition.
 * Returns:	True if i and j are not equivalent.
 * Globals:	None.
 * Remarks:	Two states are nmot equivalent when either:
 *		1. One is accepting state, and the other is not.
 *		2. There are transitions, in which replacing i with j leads
 *			to states in different classes.*
 *
 *		The partition E assigns numbers to states. Those numbers
 *		can be interpreted as class numbers. Two states that are
 *		equivalent have the same class number assigned with E.
 *		The reverse is not true due to the way minimization process
 *		is performed.
 *
 *		Highly inefficient!
 */
bool
DTA::neq (uint i, uint j, std::map<uint,uint>& E) const {  
  uint q;
  Right r;
  Rules::const_iterator m;

  if ( accepts(i) != accepts(j) )
    return true;

  for ( m = R.begin(); m != R.end(); ++m ) {
    r = m -> first;
    q = m -> second;
    // Replace one i by a j
    for ( uint k = 1; k < r.size(); ++k ) {
      if ( r[k] == i ) {
	r[k] = j;
	if ( E[ delta(r) ] != E[q] )
	  return true;
	r[k] = i;        // keep r = m->first
      } 
    }

    // Replace all j´s by i´s
    for ( uint k = 1; k < r.size(); ++k ) {
      if ( r[k] == j ) 
	r[k] = i;
    }
    if ( r != m->first && E[ delta(r) ] != E[q] )
      return true;
    
  }

  return false;
}

/* Name:	rmState
 * Class:	DTA
 * Space:	None.
 * Purpose:	Removes a state from the automaton.
 * Parameters:	q		- (i) the state to be removed.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	It does not remove the transitions. They have to be removed
 *		elsewhere.
 */
void
DTA::rmState( const uint q ) {
    F.erase(q); 
    C.erase(q);
    Q.erase(q);
}

/* Name:	merge
 * Class:	DTA.
 * Space:	None.
 * Purpose:	Merges all the states in one partition.
 * Parameters:	E		- (i) partition.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	For each state, it replaces i with E[i].
 */
void
DTA::merge ( std::map<uint,uint>& E ) {
  std::set<uint>::iterator i;
  Rules::iterator m;
  Right r;

  // Remove (replace) states
  i = Q.begin(); 
  while ( i != Q.end() ) {
    if ( E.find(*i) == E.end() )
      Error::fatal("DTA::merge", ::toString(*i) + " has no equivalence class");
   
    if ( *i != E[*i] ) {
      C[ E[*i] ] += C[*i];
      rmState( * (i++) ); // beware iterator
    } else {
      ++i;
    }
  }

  // Remove rules
  m = R.begin(); 
  while ( m != R.end() ) { 
    r = m -> first;
    for ( uint k = 1; k < r.size(); ++k )
	r[k] = E [ r[k] ];   
    if ( r != m->first ) {
      R [r] = E[ m->second ];  
      R.erase(m++);
    } else {
      m -> second = E [ m->second ];
      ++m;
    }
  }
}

/* Name:	minim
 * Class:	DTA
 * Space:	None.
 * Purpose:     Minimizing the automaton
 * Parameters:	None
 * Returns:	The minimized automaton
 * Globals:	None.
 * Remarks:	It refines a partition E till all states in the same class
                are equivalent according to function neq; 
		when no further refinement is possible, then merge is called.
 */
DTA&
DTA::minim (void) {
  uint preSize = 1, size = 2, depth = 0, q, p;
  std::map<uint, uint> E, G;    // representative
  std::map<uint, std::vector<uint> > spinOff; // split 
  std::set<uint>::iterator i;
  std::vector<uint>::iterator j;
  std::map<uint, uint>::iterator k;

  if ( F.empty() )
    Error::fatal("DTA::minim", "DTA has no accepting state");

  for ( i = Q.begin(); i != Q.end(); ++i ) {
    if ( accepts(*i) )  
      E[*i] = *F.begin();
    else 
      E[*i] = qabs;
  }

  while ( size > preSize ) {
    preSize = size;
    //   std::cerr << "\n depth=" << depth << " size=" << size;
    ++depth;
    for ( i = Q.begin(); i != Q.end(); ++i ) {
      q = *i;
      p = E[q];  // representative
      if ( q != p && neq(q, p, E) ) {
	j = spinOff[p].begin(); 
	while ( j != spinOff[p].end() && neq(q, *j, E) ) 
	  ++j;
	if ( j != spinOff[p].end() )
	  G[q] = *j;
	else {
	  G[q] = q;
	  spinOff[p].push_back(q);
	  ++size;
	}
      }
    }
    for ( k = G.begin(); k != G.end(); ++k ) 
      E[k->first] = k->second;
    G.clear();
    spinOff.clear();
  } 
  merge(E);

  return *this;
}

/* Name:	readTree
 * Class:	DTA
 * Space:	None.
 * Purpose:	Recursively reads a tree in XML format from stream.
 * Parameters:	is		- (i/o) the stream.
 * Returns:	The destination state after procesing the subtree it has
 *		just read, or EOF (a negative number) if there is nothing
 *		more to be read.
 * Globals:	None.
 * Remarks:	The result of applying the automaton on the subtrees
 *		that are read are stored in r.
 *
 *		The automaton is build on-the-fly by calling addRule,
 *		unless the option is parsing (only).
 */
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
    // The main part - processing subtrees
    r.push_back( L[s] );
    while ( (n = readTree(is)) > 0 ) 
      r.push_back(n);
  }

  if ( opt != PARSE && R.find(r) == R.end() )
    addRule(nextFree(), r);	// nextFree is just |Q|+1

#ifdef JD
  uint old_target = delta(r);
  if (C[old_target] > 1) {
    // Confluence state: create a copy
    std::cerr << "Counter for state " << old_target << " is " << C[old_target]
	      << std::endl;
    BackTr[old_target].erase(r);
    R.erase(r);
    addRule(nextFree(), r);
    uint new_target = delta(r);
    BackTr[new_target].insert(r);
#ifdef JD2
    // update followers
    std::set<uint> already_processed;
    for (uint i = 1; i < r.size(); i++) {
      uint p = r[i];
      if (already_processed.find(p) == already_processed.end()) {
	// erase old target
	if (--followers[p][old_target] == 0) {
	  followers[p].erase(old_target);
	}
	// add new target
	if (followers[p].find(new_target) == followers[p].end()) {
	  followers[p][new_target] = 1;
	}
	else {
	  followers[p][new_target]++;
	}
      }
    }
#endif
    if (F.find(old_target) != F.end()) {
      std::cerr << "Cloned a final state " << old_target << " -> "
		  << new_target << std::endl;
      F.insert(new_target);
    }
    std::map<const Right, uint> nR;
    for (Rules::iterator ri = R.begin(); ri != R.end(); ri++) {
      // Also copy outgoing transitions
      uint rs = ri->first.size();
      Right rc = ri->first;
      uint parent = ri->second;
      for (uint i = 1; i < rs; i++) {
	if (rc[i] == old_target) {
	  rc[i] = new_target;
#ifdef RAFA
	  nR[rc] = parent;
	  BackTr[parent].insert(rc);
#endif
	}
      }
#ifndef RAFA
      nR[rc] = parent;
      BackTr[parent].insert(rc);
#endif
    }
    R.insert(nR.begin(), nR.end());
    nR.clear();
  }
#endif
  return delta(r); 
}     

/* Name:	replace
 * Class:	DTA
 * Space:	None.
 * Purpose:	Replaces a state with another as transition arguments
 * Parameters:	i		- (i) the replaced state;
                j               - (i) the replacing state.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:     This function is used instead of clone in the case
                no destination state needs to be replaced 
		(indeed, it does nothing with destination states, 
                only with transition arguments).
 */
void
DTA::replace ( const uint i, const uint j ) { 
  Rules::iterator m;
  Right r;

 if ( i >= j )
    Error::fatal("DTA::replace", "replacing recent state" );

/* The two lines below were missing in older version */
 if ( accepts(i) )
   F.insert(j);

  m = R.begin(); 
  while ( m != R.end() ) {
    r = m->first; 
    for ( uint k = 1; k < r.size(); ++k )
      if ( r[k] == i ) 
	r[k] = j; 
    if ( r != m->first ) { // something changed
      R[r] = m->second;
#ifdef JD
      BackTr[m->second].erase(m->first);
      BackTr[m->second].insert(r);
#ifdef JD2
      // update followers
      if (--followers[i][m->second] == 0) {
	followers[i].erase(m->second);
      }
      if (followers[j].find(m->second) == followers[j].end()) {
	followers[j][m->second] = 1;
      }
      else {
	followers[j][m->second]++;
      }
#endif
#endif
      R.erase(m++);        // erase the older transition
    } else {
      ++m;
    }
  }
  rmState(i);
}
/* Name:	clone
 * Class:	DTA
 * Space:	None.
 * Purpose:	Clones a state with another 
 * Parameters:	i		- (i) the cloned state.
                j               - (i) the clone state
 * Returns:	nothing
 * Globals:	None.
 * Remarks:     For every transition, it creates one or several 
                new transitions 
                where i's are (partially or totally) replaced by j's. 
                For this purpose, the added transitions are also examined 
                and modified if they still contain i's. 
                Note that the iterator eventually reaches all new transitions
                because they are stored later in the map 
                as j is grater than i.
                Because of possible duplications, 
                care has to be taken before inserting the new transition 
                Example: (a i i) initially creates (a j i) and (a i j);
                when theiterator finds (a j i) the function adds (a j j);
                later the iterator finds (a j i) and creates (a j j) 
                but this is a duplication. This is especially important 
                to keep the counter C.
 */
void  
DTA::clone ( uint i, uint j ) { 
  Rules::iterator m;
  Right r;
  
  if ( i >= j )
    Error::fatal("DTA::clone", "cloned state is newer" );

/* The two lines below were missing in older version */
 if ( accepts(i) )
   F.insert(j);

  for ( m = R.begin(); m != R.end(); ++m ) {
    r = m->first;
    for ( uint k = 1; k < r.size(); ++k )
      if ( r[k] == i ) { 
	r[k] = j;
	if ( R.find(r) == R.end() ) {
	  R[r] = m->second; 
	  ++C[m->second];
	}
	r[k] = i;
      }
  }
}


/* Name:	newRules
 * Class:	DTA
 * Space:	None.
 * Purpose:	Create a set of new rules needed to accept a new tree 
 * Parameters:	is		- (i) the input stream (one tree).
                S               - (o) the added transitions
 * Returns:	Destination state obtained as the result of 
                processin the subtree or EOF (negative number) 
                if no tree was found at input.
 * Globals:	None.
 * Remarks:     It modifies the automaton, by adding the transitions in S;
                S is only kept for tracking purposes 
                (a list could be used instead).
		The first time (for this tree) a transion is needed,
                the old destination state q is cloned with a new one n. 
                Two cases are considered to add a transition:
                1. The cloned state q is not a destination state; 
                   then a simple replacement of q is enough.
                2. The cloned state q is a destination state; then 
                  standard cloning is called for.
*/            
int
DTA::newRules (std::istream& is, Rules& S) { 
  char c;  
  std::string s;
  Right r;
  int n;

  is >> c;
  if ( is.eof() ) 
    return EOF;
  else if ( c != '<' || ! getline(is, s, '>') )
    Error::fatal("DTA::newRules", "wrong tree format");

  if ( s.size() == 0 )
    Error::fatal("DTA::newRules", "missing tag");
  else if ( s[0] == '/' )             // end tag 
    return -L[ s.substr( 1, s.size() - 1 )];
  else if ( s[s.size() - 1] == '/' )  // leaf
    r.push_back( L[ s.substr( 0, s.size() - 1 ) ] );  
  else {
    r.push_back( L[s] );
    while ( (n = newRules(is,S)) > 0 ) 
      r.push_back(n);
  }
  // At this point, r is a sequence of child states
  if ( S.find(r) == S.end() ) {
    n = nextFree();
    addState(n);
    C[n] = 1;    
    if ( R.find(r) != R.end() ) {  
      uint q = R[r];
      --C[q];
      if ( C[q] == 0 ) 
	replace(q, n);
      else 
	clone(q, n);
    }

    S[r] = R[r] = n;
  }

  return R[r];
}

/* Name:	build
 * Class:	DTA
 * Space:	None.
 * Purpose:	Buids a DTA that accepts a collection of trees
 * Parameters:	is		- (i) the input stream (trees)
 * Returns:	the automaton
 * Globals:	None.
 * Remarks:     While there are trees in the input stream 
                add the rules needed to accept them.
 */           
DTA&
DTA::build (std::istream& is) {
  int n;
#ifdef JD
  int prev_last = Q.size() + 1;
  uint super_root = 0;
#endif
  while ( (n = readTree(is)) > 0 ) {
#ifdef JD
    if (F.size() > 0) {
      F.insert(n);
      // Start minimization
#ifdef JD2
      jdmin2((unsigned int)n, super_root, prev_last);
#else
      jdmin((unsigned int)n, super_root, prev_last);
#endif
      if (Q.find(n) != Q.end()) {
	// n is a subtree of DTA or vic versa
	if (contained_below(super_root, n)) {
	  super_root = n;
	}
      }
    }
    else {
      // First tree in the automaton
      F.insert(n);
      super_root = n;
    }
    prev_last = nextFree();
#else
    F.insert(n);
#endif
  }

  return *this;
}

#ifdef JD
  struct agenda_item {
    uint	tree_item;	// state number from the tree
    uint	autom_item;	// state number from the automaton
    uint	parent;		// state number of the parent of tree_item

    agenda_item(const uint t, const uint a, const uint p) : tree_item(t),
							    autom_item(a),
							    parent(p) {}
    agenda_item(void) : tree_item(0), autom_item(0), parent(0) {}
  };

/* Name:	jdmin
 * Class:	DTA
 * Space:	None.
 * Purpose:	Performs a local minimization of an automaton
 *		after states recognizing a new tree have been added to it.
 * Parameters:	n		- (i) root of the tree;
 *		f		- (i) finite state of already existing DTA;
 *		m		- (i) last state of the automaton before
 *					addition.
 * Returns:	The minimized automaton.
 * Globals:	None.
 * Remarks:	Since the final states - corresponding to roots of trees -
 *		do not have any outgoing transitions, they are all equivalent.
 *		Then we can check the states that appear as arguments
 *		of transition function when the result is a final state.
 *		In that way, we proceed recursively towards leaves.
 */
DTA&
DTA::jdmin(const uint n, const uint f, const uint m) {
  if (n == f) {
    return *this;		// nothing to minimize
  }
  if (contained_below(f,n)) {
    return *this;		// no loops please
  }
  if (contained_below(n,f)) {
    return *this;		// no loops please
  }
  std::list<agenda_item> agenda; // list of pairs of states that must be
				      // minimized
  std::set<std::pair<uint,uint> > on_agenda;	// set of pairs of states on that list
					// (faster lookup)
  std::map<uint,uint> replaced; // replacements already done
  //  std::set<uint> unique;	// states already found to be unique
  // Start agenda
  on_agenda.insert(std::pair<uint,uint>(n,f));
  for (agenda.push_back(agenda_item(n,f,0)); agenda.begin() != agenda.end();
       agenda.pop_front()) {
    agenda_item a = agenda.front();
    uint ct = a.tree_item;	// state from the current tree
    uint ca = a.autom_item;	// state from the automaton
    //uint parent = a.parent;
    DeltaArgs ts = BackTr[ct];	// set of rules giving ct
    DeltaArgs as = BackTr[ca];	// set of rules giving ca
    // There should be only one such rule...
    for (DeltaArgs::iterator tr = ts.begin(); tr != ts.end(); tr++) {
      // *tr is a right-hand side of a rule giving ct
      Right new_rule = *tr;
      for (DeltaArgs::iterator ar = as.begin(); ar != as.end(); ar++) {
	// *ar is a right-hand side of a rule giving ca
#ifdef JD2
	if (states_equiv2(ct, ca)) {
	  // Check individual pairs
	  for (unsigned int i = 1; i < tr->size(); i++) {
	    uint q1 = (*tr)[i]; uint q2 = (*ar)[i];
	    bool q1_final = (F.find(q1) != F.end());
	    bool q2_final = (F.find(q2) != F.end());
	    if (q1 != q2 && q1 >= m && (q1_final == q2_final) &&
		!contained_below(q2, q1)) {
	      new_rule[i] = q2;
	      agenda.push_back(agenda_item(q1,q2,ct));
	      on_agenda.insert(std::pair<uint,uint>(q1,q2));
	    }
	  }
	}
#else //!JD2
	if (states_equivalent(ct, ca, m, *tr, *ar)) {
	  // Check individual pairs
	  for (unsigned int i = 1; i < tr->size(); i++) {
	    uint q1 = (*tr)[i]; uint q2 = (*ar)[i];
	    bool q1_final = (F.find(q1) != F.end());
	    bool q2_final = (F.find(q2) != F.end());
	    if (q1 != q2 && q1 >= m && (q1_final == q2_final) &&
		!contained_below(q2, q1)) {
	      new_rule[i] = q2;
	      agenda.push_back(agenda_item(q1,q2,ct));
	      on_agenda.insert(std::pair<uint,uint>(q1,q2));
	    }
	  }
	}
#endif
      }
      if (new_rule != *tr) {
	// Rule arguments have changed
	R.erase(*tr);
      }
      R[new_rule] = ca;
      C[ca]++;
      C[ct]--;
      BackTr[ct].erase(*tr);
      BackTr[ca].insert(new_rule);
    }
    rmState(ct);
    replaced[ct] = ca;
    on_agenda.erase(std::pair<uint,uint>(a.tree_item,a.autom_item));
  }// for all agenda items
  return *this;
}//jdmin

#ifdef JD2
///* Name:	print_rules
// * Class:	DTA.
// * Space:	None.
// * Purpose:	Prints all transition functions in R.
// * Parameters:	A		- (i) tree automaton;
// *		r		- (i) rule set.
// * Returns:	Nothing.
// * Globals:	cerr.
// * Remarks:	Only for debugging. Prints on standard error.
// */
//void
//DTA::print_rules(Rules &r) {
//  for (Rules::iterator ri = r.begin(); ri != r.end(); ri++) {
//    std::cerr << ri->second << " <-";
//    if (ri->first.size()) {
//      std::cerr << " " << L[ri->first[0]] << "(" << ri->first[0] << ")";
//    }
//    for (uint i = 1; i < ri->first.size(); i++) {
//      std::cerr << " " << ri->first[i];
//    }
//    std::cerr << "\n";
//  }
//}//print_rules

/* Name:	jdmin2
 * Class:	DTA
 * Space:	None.
 * Purpose:	Performs a local minimization of an automaton
 *		after states recognizing a new tree have been added to it.
 * Parameters:	n		- (i) root of the tree;
 *		f		- (i) finite state of already existing DTA;
 *		m		- (i) last state of the automaton before
 *					addition.
 * Returns:	The minimized automaton.
 * Globals:	None.
 * Remarks:	Since the final states - corresponding to roots of trees -
 *		do not have any outgoing transitions, they are all equivalent.
 *		Then we can check the states that appear as arguments
 *		of transition function when the result is a final state.
 *		In that way, we proceed recursively towards leaves.
 */
DTA&
DTA::jdmin2(const uint n, const uint f, const uint m) {
  if (n == f) {
    return *this;		// nothing to minimize
  }
  if (contained_below(f,n)) {
    return *this;		// no loops please
  }
  if (contained_below(n,f)) {
    return *this;		// no loops please
  }
  std::list<agenda_item> agenda; // list of pairs of states that must be
				      // minimized
  //  std::cerr << "Beginning of minimization (" << n << "," << f << ")\n";
  //  print_rules(R);

  //std::set<std::pair<uint,uint> > on_agenda;	// set of pairs of states on that list
					// (faster lookup)
  //std::map<uint,uint> replaced; // replacements already done
  //  std::set<uint> unique;	// states already found to be unique
  // Start agenda
  //on_agenda.insert(std::pair<uint,uint>(n,f));
  for (agenda.push_back(agenda_item(n,f,0)); agenda.begin() != agenda.end();
       agenda.pop_front()) {
    agenda_item a = agenda.front();
    uint ct = a.tree_item;	// state from the current tree
    uint ca = a.autom_item;	// state from the automaton
    if (states_equiv2(ct, ca)) {

      //      std::cerr << ct << " and " << ca << " found equivalent\n";
      // Process children
      DeltaArgs ts1 = BackTr[ct];	// set of rules giving ct
      DeltaArgs as = BackTr[ca];	// set of rules giving ca
      // There should be only one such rule...
      for (DeltaArgs::iterator tr = ts1.begin(); tr != ts1.end(); tr++) {
	// *tr is a right-hand side of a rule giving ct
	Right new_rule = *tr;
	for (DeltaArgs::iterator ar = as.begin(); ar != as.end(); ar++) {
	  // *ar is a right-hand side of a rule giving ca

	  // Now *tr and *ar are rules giving the same state
	  // (actually for the moment a pair of equivalent states)
	  // States in *tr and *ar can be equivalent only if arity
	  // of *tr and *ar is the same
	  if (tr->size() == ar->size()) {
	    // Check individual pairs
	    for (unsigned int i = 1; i < tr->size(); i++) {
	      uint q1 = (*tr)[i]; uint q2 = (*ar)[i];
	      bool q1_final = (F.find(q1) != F.end());
	      bool q2_final = (F.find(q2) != F.end());
	      if (q1 != q2 && (q1_final == q2_final) &&
		  !contained_below(q2, q1)) {
		new_rule[i] = q2;
		agenda.push_back(agenda_item(q1,q2,ca));
		//on_agenda.insert(std::pair<uint,uint>(q1,q2));
	      }
	    }
	  }
	}
      }
      // Replace ct with ca in all rules.
      // We don't need to process all rules, only those that contain ct

      // Process arguments of rules giving ct
      DeltaArgs ts = BackTr[ct];	// set of rules giving ct
      for (DeltaArgs::iterator tr = ts.begin(); tr != ts.end(); tr++) {
	// *tr is a right-hand side of a rule giving ct
	R.erase(*tr);
	R[*tr] = ca;
	BackTr[ca].insert(*tr);
	std::set<uint> already_handled;
	for (uint tri = 1; tri < tr->size(); tri++) {
	  uint qi = (*tr)[tri];
	  if (already_handled.find(qi) == already_handled.end()) {
	    already_handled.insert(qi);
	    if (--followers[qi][ct] == 0) {
	      followers[qi].erase(ct);
	    }
	    if (followers[qi].find(ca) == followers[qi].end()) {
	      followers[qi][ca] = 1;
	    }
	    else {
	      followers[qi][ca]++;
	    }
	  }
	}
      }
      BackTr[ct].clear();
      //      std::cerr << "Replaced arguments of " << ct << "\n";
      //print_rules(R);

      // Process rules having ct as one or more arguments
      for (TargetCount::iterator ctf = followers[ct].begin();
	   ctf != followers[ct].end(); ctf++) {
	// *ctf is a state that is a target of a transition from ct
	for (DeltaArgs::iterator bctf = BackTr[ctf->first].begin();
	     bctf != BackTr[ctf->first].end(); ) {
	  if (find(bctf->begin(), bctf->end(), ct) != bctf->end()) {
	    // Since ct is equivalent to ca, there is another rule with ca
	    R.erase(*bctf);
	    BackTr[ctf->first].erase(bctf++);
	  }
	  else {
	    bctf++;
	  }
	}
      }
      //std::cerr << "Replaced targets of " << ct << "\n";
      //print_rules(R);

      // Modify agenda to replace ct with ca and to remove (ct, ca) pairs
      std::list<agenda_item>::iterator ai = agenda.begin();
      for (ai++; ai != agenda.end(); ai++) {
	if (ai->tree_item == ct) {
	  if (ai->autom_item == ca) {
	    agenda.erase(ai--);
	  }
	  else {
	    ai->tree_item = ca;
	  }
	}
      }
      rmState(ct);
      //replaced[ct] = ca;
    }
    //on_agenda.erase(std::pair<uint,uint>(a.tree_item,a.autom_item));
  }// for all agenda items
  return *this;
}//jdmin2

/* Name:	states_equiv2
 * Class:	DTA
 * Space:	None.
 * Purpose:	Checks equivalence of two states.
 * Parameters:	q1		- (i) first state number;
 *		q2		- (i) second state number.
 * Returns:	True if states equivalent, false otherwise.
 * Globals:	None.
 * Remarks:	It is assumed that all states reachable from q1 and q2 are
 *		unique in the automaton.
 *		q1 comes from a new tree, q2 - from already existing automaton.
 *
 *		q1 is equivalent to q2 if in all rules that use q1 as an
 *		argument, q1 can be replaced with q2, and vice versa.
 *		So if there is (q1, q1), there should also be (q1, q2),
 *		(q2, q1), and (q2, q2).
 */
bool
DTA::states_equiv2(const uint q1, const uint q2) {
  // Check if sets of followers are the same
  // This is so complicated because followers[.] is a map
  if (followers[q1].size() != followers[q2].size()) {
    return false;
  }
  TargetCount::iterator fi2 = followers[q2].begin();
  for (TargetCount::iterator fi1 = followers[q1].begin();
       fi1 != followers[q1].end(); fi1++) {
    if (fi1->first != (*fi2++).first) {
      return false;
    }
  }
  for (TargetCount::iterator si = followers[q1].begin();
       si != followers[q1].end(); si++) {
    // Now si->first is a target of a transition containing q1 and a target
    // of a transition containing q2. It may also be a target of many other
    // transitions
    for (DeltaArgs rs = BackTr[si->first]; rs.size() != 0; ) {
      Right rp = *(rs.begin());
      switch (equiv_in_rule(q1, q2, &rp, &rs, 1)) {

      case s_nequiv:
	return false;

      case s_notfound:
	rs.erase(rs.begin());
	break;

      case s_equiv:
	// Remove matching rules, so that they will not be reexamined later
	repl_in_rule(q1, q2, *(rs.begin()), &rs, 1);
	break;
      }
    }
  }
  return true;
}//DTA::states_equiv2

/* Name:	equiv_in_rule
 * Class:	DTA
 * Space:	None.
 * Purpose:	Checks whether two states are equivalent in a set
 *		of transitions.
 * Parameters:	q1		- (i) first state;
 *		q2		- (i) second state;
 *		rp		- (i) rule to examine;
 *		all		- (i) set of all rules;
 *		argindex	- (i) current state position in rp.
 * Returns:	s_nequiv if states are not equivalent,
 *		s_notfound if the rule in question contains neither q1 nor q2,
 *		s_equiv if for each q1 and q2 (at least one) found in the rule
 *		there is a rule with the other state in the same place.
 * Globals:	None.
 * Remarks:	We skip all non-q1 and non-q2 positions in the for loop.
 *		If we find q1 or q2, we see if we can find a rule that has
 *		the other state at the same position. If not, the obviously
 *		states are not equivalent. If yes, then we recursively check
 *		whether we can repeat the procedure for q1 and q2 in both
 *		the original and the modified rule.
 */
equiv_results
DTA::equiv_in_rule(const uint q1, const uint q2, Right *rp,
		    DeltaArgs *all, uint argindex) {
  for (uint i = argindex; i < rp->size(); i++) {
    if ((*rp)[i] == q1) {
      Right rp1 = *rp;
      rp1[i] = q2;
      if (R.find(rp1) == R.end()) {
	return s_nequiv;
      }
      if (equiv_in_rule(q1, q2, rp, all, i + 1) == s_nequiv) {
	return s_nequiv;
      }
      if (equiv_in_rule(q1, q2, &rp1, all, i + 1) == s_nequiv) {
	return s_nequiv;
      }
      return s_equiv;
    }
    else if ((*rp)[i] == q2) {
      Right rp2 = *rp;
      rp2[i] = q1;
      if (R.find(rp2) == R.end()) {
	return s_nequiv;
      }
      if (equiv_in_rule(q1, q2, rp, all, i + 1) == s_nequiv) {
	return s_nequiv;
      }
      if (equiv_in_rule(q1, q2, &rp2, all, i + 1) == s_nequiv) {
	return s_nequiv;
      }
      return s_equiv;
    }
  }
  return s_notfound;
}//DTA::equiv_in_rule

/* Name:	repl_in_rule
 * Class:	DTA
 * Space:	None.
 * Purpose:	Removes all rules like the given one, in which q1 or q2 appear.
 * Parameters:	q1		- (i) first state number;
 *		q2		- (i) second state number;
 *		rp		- (i) original rule;
 *		all		- (i/o) set of all rules;
 *		argindex	- current state position in rp.
 * Returns:	Nothing.
 * Globals:	None.
 * Remarks:	Deletion is done when the index goes beyond the rule.
 *		If q1 or q2 is found, there are two recursive calls for both
 *		versions.
 */
void
DTA::repl_in_rule(const uint q1, const uint q2, Right rp, DeltaArgs *all,
		  const uint argindex) {
  for (uint i = argindex; i < rp.size(); i++) {
    if (rp[i] == q1) {
      repl_in_rule(q1, q2, rp, all, i + 1);
      rp[i] = q2;
      repl_in_rule(q1, q2, rp, all, i + 1);
      return;
    }
    else if (rp[i] == q2) {
      repl_in_rule(q1, q2, rp, all, i + 1);
      rp[i] = q1;
      repl_in_rule(q1, q2, rp, all, i + 1);
      return;
    }
  }
  all->erase(rp);
}//DTA::repl_in_rule
#endif //JD2

/* Name:	states_equivalent
 * Class:	DTA
 * Space:	None.
 * Purpose:	Checks equivalence of two states.
 * Parameters:	q1		- (i) first state number;
 *		q2		- (i) second state number;
 *		m		- (i) state number of the first state of
 *					the new tree;
 *		r1		- (i) rule producing q1;
 *		r2		- (i) rule producing q2.
 * Returns:	True if states equivalent, false otherwise.
 * Globals:	None.
 * Remarks:	It is assumed that all states reachable from q1 and q2 are
 *		unique in the automaton.
 *		q1 comes from a new tree, q2 - from already existing automaton.
 *		The tree that has just been added may contain states
 *		that either already exist in the automaton created so far,
 *		or new states. New states have numbers not less than m.
 *		r1 and r2 are one of several possible rules giving q1 and q2.
 */
bool
DTA::states_equivalent(const uint q1, const uint q2, const uint m, Right r1,
		       Right r2) {
  if (r1.size() != r2.size() || r1[0] != r2[0]) {
    return false;
  }
  int counter = 0;
  for (unsigned int i = 1; i < r1.size(); i++) {
    if (r1[i] != r2[i]) {
      if (r1[i] < m) {
	return false;
      }
      else {
	counter++;
      }
    }
  }
  if (counter > 1) {
    return false;
  }
  return true;
}

/* Name:	contained_below
 * Class:	DTA
 * Space:	None.
 * Purpose:	Checks whether a state in the old part of the automaton
 *		is present in a subtree beginning with a state in the new
 *		part.
 * Parameters:	q1		- (i) state from the old part of the automaton;
 *		q2		- (i) new state.
 * Returns:	True if q1 is present in the subtree of q2, false otherwise.
 * Globals:	None.
 * Remarks:	This check is necessary to avoid creating loops.
 *		If we make q1 and q2 equivalent, and q1 is in the subtree
 *		of q2, a loop may be created.
 */
bool
DTA::contained_below(const uint q1, const uint q2) {
  DeltaArgs da = BackTr[q2];
  for (DeltaArgs::iterator di = da.begin(); di != da.end(); di++) {
    unsigned int n = di->size();
    for (unsigned int i = 1; i < n; i++) {
      if ((*di)[i] == q1) {
	return true;
      }
      else if (contained_below(q1, (*di)[i])) {
	return true;
      }
    }
  }
  return false;
}
#endif

/* Name:	parse
 * Class:	DTA
 * Space:	None.
 * Purpose:	process trees and do not modify the automaton
 * Parameters:	is		- (i) the input stream (trees)
 * Returns:	the automaton
 * Globals:	None.
 * Remarks:     While there are trees in the input stream 
                process them and check if it accept them.
                Does not modify the automaton.
 */     
DTA&
DTA::parse (std::istream& is) {
  int n;
  while ( (n = readTree(is)) > 0 )  
    std::cout << accepts(n); 

  return *this;
}

/* Name:        add
 * Class:	DTA
 * Space:	None.
 * Purpose:	Modify a minimal automaton so that it
                accept an additional  collection of trees and keep it minimal.
 * Parameters:	is		- (i) the input stream (trees)
 * Returns:	the automaton
 * Globals:	None.
 * Remarks:     While there are trees in the input stream 
                add the new rules needed. then minimize the result.
                Highly inefficient backwards minimization
 */    
DTA&
DTA::add (std::istream& is) {
  int n;
  Rules S;
  Right r;
  std::set<uint> P;
  std::set<uint>::iterator i;
  std::set<uint>::reverse_iterator j;
  std::map<uint,uint> E;
  Rules::iterator m;

  if ( C.empty() )  // if C is not initialized, do it
    for ( m = R.begin(); m != R.end(); ++m )
      ++C[m->second];

  while ( (n = newRules(is, S)) > 0 ) {  // read the trees and add rules
    F.insert(n); 
   
    // very inefficient minimization follows
    for ( m = S.begin(); m != S.end(); ++m ) 
      P.insert(m->second);
    for ( i = Q.begin(); i != Q.end(); ++i )
      E[*i] = *i;
     for ( j = P.rbegin(); j != P.rend(); ++j ) {
       for ( i = Q.begin(); *i < *P.begin() && E[*j] == *j; ++i ) {
	 if ( ! ( *i == *j || neq( *i, *j, E) ) ) 
	   E[*j] = *i;	       
       }	
     }
     merge(E);
     E.clear();
     P.clear();
     S.clear();
  }

  return *this;
}
/* Name: main
 see option -h
*/
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
      } else if ( std::string(argv[k]) == "-e" ) {
	DTA B; // temporal option for debugging
	f.open(argv[++k]); 
	f >> A; 
	A.minim();
	std::ifstream g;
	g.open(argv[++k]); 
	g >> B; 
	B.minim(); 
	if ( A.size() != B.size() ) 
	  std::cerr << "\nDIFFER " << A.size() 
		    << ' ' << B.size() << std::endl;
	else
	  std::cerr << "\nSIMILAR " << A.size() << std::endl;
	exit(0);
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
//#ifdef JD
	std::cerr << A << std::endl; // to know the original
//#endif
	std::cerr << A.size() << " -> ";
	A.minim();
	std::cerr << A.size() << std::endl;
	break;
      case ADD:
	std::cerr << A.size() << " -> ";
	A.add( std::cin );
	std::cerr << A.size() << std::endl;
	break;
      case DEL:
	//  A.del( cin );
	break;
      case PARSE:
	A.parse( std::cin );
	break;
      case MINIM:
	A.minim();
	break;
  }

  if ( A.getOpt() != PARSE )
    std::cout << A << std::endl;
  return 0;
}




