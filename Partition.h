#ifndef _PARTITION_H
#define _PARTITION_H

#include <iostream>
#include <map>
#include <vector>
#include <set>
#include <ext/hash_map>
#include "Error.h"
#include "extio.h"

//----------------------------------------------------------------
// Integer set partition.
// Each class is represented by its smallest integer. 
// Elements are cyclicly returned by increasing value.
//-----------------------------------------------------------------
class Partition {
  __gnu_cxx::hash_map<uint,uint> B;
  std::vector< std::set<uint> > P;

public:
  void init  (const std::set<uint>&);        // Create trivial partition.
  Partition  (const std::set<uint>&);        // Constructor
  uint first (uint) const;
  uint  next (uint) const;
  uint getSize () const { return P.size(); }
  bool equiv (uint, uint) const;              // check equivalence
  void refine (const std::set<uint>&);        // split container class
  std::set<uint> block (uint) const;          // return equivalence class

  friend std::ostream& operator<< (std::ostream&, const Partition&);

};
#endif
