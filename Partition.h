/*
 * Copyright (C) 2013 Universidad de Alicante
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
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
