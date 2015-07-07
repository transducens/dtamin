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
//
// Random tree  generator (for testing)
//
#include <iostream>
#include <cmath>
int
main (int argc, char** args) {
  int N, S, left, right;

  N = atoi(args[2]);
  S = atoi(args[1]);

  srand48( atoi(args[3]) );

  /*
  for ( int k = 0; k < N; ++k ) {
    if ( drand48() < 0.001 )
      std::cout << "0 " << k + 1 << std::endl;    
  }
  */
  std::cout << "0 1" << std::endl;
  for ( char s = 0; s < S; ++ s )
    for ( left = 0; left < N; ++left )
      for ( right = 0; right < N; ++right ) {
	int x = drand48()*(N + 1);
	if ( x > 0 )
	  std::cout << x << ' ' << char(65 + s) 
		    << ' ' << left + 1 << ' ' << right + 1 << std::endl;
      }
}
