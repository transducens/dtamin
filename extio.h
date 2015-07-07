/**
 * @file extio.h Extends iostream operators to some STL containers.
 */
#ifndef _EXTIO_H
#define _EXTIO_H

#include <iostream>
#include <sstream>
#include <vector>
#include <set>
#include "Error.h"

///
/// Generic operator << for vectors
///
template <class T> 
std::ostream& 
operator<< ( std::ostream& os, const std::vector<T>& v ) {

  os << '(';
  if ( v.size() > 0 ) {
    os << v[0];
    for ( uint k = 1; k < v.size(); ++k ) 
      os << ", " << v[k];
  }
  os << ')';
  return os;
}

///
/// Generic operator << for sets
///
template <class T> 
std::ostream& 
operator<< ( std::ostream& os, const std::set<T>& s ) {
  typename std::set<T>::const_iterator it = s.begin();

  os << '{';
  if ( it != s.end() ) {
    os << *it;
    while ( ++it != s.end() ) os << ", " << *it;
  }
  os << '}';
  return os;
}

///
/// Generic operator >> for sets
///
template <class T> 
std::istream& operator>> ( std::istream& is, std::set<T>& s ) {  
  char c;
  T element;
  std::string text;
  std::stringstream buff;

  s.clear();
  is >> c;
  if ( c != '{' ) 
    Error::fatal(__FUNCTION__,  "set must be in brackets" );
  
  getline( is, text, '}' ); 
  buff <<  text;

  while ( getline( buff, text, ',' ) ) {
    if ( text.size() >  0 ) {
      std::stringstream _is ( text.c_str() );  // string to T
      _is >> element;
      s.insert( element );
    }
  }

  return is;
}

///
/// Generic operator << for pairs 
///
template <class T1, class T2>
std::ostream& 
operator<< ( std::ostream& os, const std::pair<T1,T2>& p ) {
  os << '(' << p.first << ',' << p.second << ')';
  return os;
}

#endif
