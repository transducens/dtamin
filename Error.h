#ifndef _Error_H
#define _Error_H

#include <string>
#include <iostream>

/**
 * Error messages and actions (replacement for err.h).
 */
class Error {

/**
 * Report fatal error and exit.
 * @param err_ctxt: the context.
 * @param err_msg: the error message.
 */
 public:
  static void 
    fatal ( const std::string& err_ctxt, const std::string& err_msg ) {
    std::cerr << err_ctxt << ": " << err_msg << std::endl;
    exit(1);
  }
  
/**
 * Report warning.
 * @param err_ctxt: the context.
 * @param err_msg: the warning message.
 */
  static void 
    warning ( const std::string& err_ctxt, const std::string& err_msg ) {
    std::cerr << "Warning " << err_ctxt << ": " 
	      << err_msg << std::endl;
  }

/**
 * Report file not open error and exit.
 * @param err_ctxt: the context.
 * @param file_name: the name of the wrong file.
 */
  static void 
    file_not_open ( const std::string& err_ctxt, 
		    const std::string& file_name ) {
    fatal( err_ctxt, "cannot open " + file_name );
  }
  
};

#endif
