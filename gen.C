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
