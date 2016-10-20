/**
@cond IGNORE

======================================================
 SFSEXP: Small, Fast S-Expression Library version 1.2
 Written by Matthew Sottile (mjsottile@gmail.com)
======================================================

Copyright (2003-2006). The Regents of the University of California. This
material was produced under U.S. Government contract W-7405-ENG-36 for Los
Alamos National Laboratory, which is operated by the University of
California for the U.S. Department of Energy. The U.S. Government has rights
to use, reproduce, and distribute this software. NEITHER THE GOVERNMENT NOR
THE UNIVERSITY MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
LIABILITY FOR THE USE OF THIS SOFTWARE. If software is modified to produce
derivative works, such modified software should be clearly marked, so as not
to confuse it with the version available from LANL.

Additionally, this library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2.1 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
for more details.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, U SA

LA-CC-04-094

@endcond
**/
#ifndef __SEXP_H__
#define __SEXP_H__

#include <stddef.h>
#include <stdio.h> /* for BUFSIZ only */
#include "faststack.h"
#include "cstring.h"
#include "sexp_memory.h"
#include "sexp_errors.h"

/* doxygen documentation groups defined here */

/**
 * \defgroup IO I/O routines
 */

/**
 * \defgroup parser Parser routines
 */

/**
 * \mainpage A small and quick S-expression parsing library.
 *
 * \section intro Introduction
 * 
 * This library was created to provide s-expression parsing and manipulation
 * facilities to C and C++ programs.  The primary goals were speed and
 * efficiency - low memory impact, and the highest speed we could achieve in
 * parsing.  Suprisingly, no other libraries on the net were found that were
 * not bloated with features or involved embedding a full-fledged LISP or
 * Scheme interpreter into our programs.  So, this library evolved to fill
 * this gap.  As such, it does not guarantee that every valid LISP
 * expression is parseable, and many features that are not required aren't
 * implemented.  See Rivest's S-expression library for an example of a much
 * more featureful library.
 *
 * What features does this library include?  At the heart of the code is a
 * continuation-based parser implementing a basic parser state machine.
 * Continuations allow users to accumulate multiple streams of characters,
 * and parse each stream simultaneously.  A continuation allows the parser
 * to stop midstream, start working on a new expression, and return to the
 * first without corruption of complex state management in the users code.
 * No threads, no forking, nothing more than a data structure that must be
 * passed in and captured as data becomes available to parse.  Once an
 * expression has been parsed, a simple structure is returned that
 * represents the "abstract syntax tree" of the parsed expression.  For the
 * majority of users, the parser and this data structure will be all that
 * they will ever need to see.  For convenience reasons, other functions
 * such as I/O wrappers and AST traversal routines have been included, but
 * they are not required if users don't wish to use them.
 *
 * \section credits Credits
 *
 * SFSEXP: Small, Fast S-Expression Library version 1.2, October 2007 \n
 * Written by Matthew Sottile (mjsottile@gmail.com)
 * 
 * \section license License Information
 *
 * Copyright (2003-2006). The Regents of the University of California. This
 * material was produced under U.S. Government contract W-7405-ENG-36 for Los
 * Alamos National Laboratory, which is operated by the University of
 * California for the U.S. Department of Energy. The U.S. Government has rights
 * to use, reproduce, and distribute this software. NEITHER THE GOVERNMENT NOR
 * THE UNIVERSITY MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. If software is modified to produce
 * derivative works, such modified software should be clearly marked, so as not
 * to confuse it with the version available from LANL.
 * 
 * Additionally, this library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, U SA
 * 
 * LA-CC-04-094
 *
 */

/**
 * \file sexp.h
 *
 * \brief API for a small, fast and portable s-expression parser library.
 */

/*==============*/
/* ENUMERATIONS */
/*==============*/

/**
 * An element in an s-expression can be one of three types: a <i>value</i>
 * represents an atom with an associated text value.  A <i>list</i>
 * represents an s-expression, and the element contains a pointer to
 * the head element of the associated s-expression.
 */
typedef enum { 
  /**
   * An atom of some type.  See atom type (aty) field of element structure
   * for details as to which atom type this is.
   */
  SEXP_VALUE,

  /**
   * A list.  This means the element points to an element representing the
   * head of a list.
   */
  SEXP_LIST
} elt_t; 

/**
 * For an element that represents a value, the value can be interpreted
 * as a more specific type.  A <i>basic</i> value is a simple string with
 * no whitespace (and therefore no quotes required).  A <i>double quote</i>
 * value, or <i>dquote</i>, is one that contains characters (such as
 * whitespace) that requires quotation marks to contain the string.  A
 * <i>single quote</i> value, or <i>squote</i>, represents an element that is
 * prefaced with a single tick-mark.  This can be either an atom or
 * s-expression, and the result is that the parser does not attempt to parse
 * the element following the tick mark.  It is simply stored as text.  This
 * is similar to the meaning of a tick mark in the Scheme or LISP family
 * of programming languages.  Finally, <i>binary</I> allows raw binary to
 * be stored within an atom.  Note that if the binary type is used, the data
 * is stored in bindata with the length in binlength.  Otherwise, the data
 * us stored in the val field with val_used and val_allocated tracking the
 * size of the value string and the total memory allocated for it.
 */
typedef enum { 
  /**
   * Basic, unquoted value.
   */
  SEXP_BASIC, 

  /**
   * Single quote (tick-mark) value - contains a string representing
   * a non-parsed portion of the s-expression.
   */
  SEXP_SQUOTE, 

  /**
   * Double-quoted string.  Similar to a basic value, but potentially
   * containing white-space.
   */
  SEXP_DQUOTE,

  /**
   * Binary data.  This is used when the specialized parser is active
   * and supports inlining of binary blobs of data inside an expression.
   */
  SEXP_BINARY
} atom_t;

/*============*/
/* STRUCTURES */
/*============*/

/**
 * An s-expression is represented as a linked structure of elements,
 * where each element is either an <I>atom</I> or <I>list</I>.  An
 * atom corresponds to a string, while a list corresponds to an
 * s-expression.  The following grammar represents our definition of
 * an s-expression:<P>
 *
 * <pre>
 * sexpr  ::= ( sx )
 * sx     ::= atom sxtail | sexpr sxtail | 'sexpr sxtail | 'atom sxtail | NULL
 * sxtail ::= sx | NULL
 * atom   ::= quoted | value
 * quoted ::= "ws_string"
 * value  ::= nws_string
 * </pre>
 * <P>
 *
 * An atom can either be a quoted string, which is a string containing
 * whitespace (possibly) surrounded by double quotes, or a non-whitespace
 * string that does not require surrounding quotes.  An element representing
 * an atom will have a type of <i>value</i> and data stored in the <i>val</i>
 * field.  An element of type <i>list</i> represents an s-expression
 * corresponding to <i>sexpr</i> in the grammar, and will have a pointer to
 * the head of the appropriate s-expression.  Details regarding these fields
 * and their values given with the fields themselves.  Notice that a single
 * quote can appear directly before an s-expression or atom, similar to the
 * use in LISP.  
 */
typedef struct elt {
  /**
   * The element has a type that determines how the structure is used.
   * If the type is <B>SEXP_VALUE</B>, then a programmer knows that
   * either the val field or bindata field is meaningful dependin on
   * the value of the aty field, and contains the data associated with
   * this element of the s-expression.  If the type is
   * <B>SEXP_LIST</B>, then the list field contains a pointer to the
   * s-expression element representing the head of the list.  For each
   * case, the field for the opposite case contains no meaningful data
   * and using them in any way is likely to cause an error.
   */
  elt_t ty;

  /**
   * If the type of the element is <B>SEXP_VALUE</B> and the aty field
   * is not <B>SEXP_BINARY</B>, this field will contain the actual data
   * represented by this element.
   */
  char  *val;

  /**
   * Number of bytes allocated for val.
   */
  size_t   val_allocated;
  
  /**
   * Number of bytes used in val (<= val_allocated).
   */
  size_t   val_used;
  
  /**
   * If the type of the element is <B>SEXP_LIST</B>, this field will contain
   * a pointer to the head element of the list.
   */
  struct elt *list;

  /**
   * The <I>next</I> field is a pointer to the next element in the current
   * expression.  If this element is the last element in the s-expression,
   * this field will be <I>NULL</I>.
   */
  struct elt *next;

  /**
   * For elements that represent <I>values</I>, this field will specify the
   * specific type of value that it represents.  This can be used by
   * functions to determine how this value should be printed (ie: how it
   * should be quoted) or interpreted (ie: interpreting s-expressions that
   * are prefixed with a tick-mark.).  This value also indicates whether or
   * not the programmer should look in the val field or bindata field for
   * the atom data.
   */
  atom_t aty;

  /**
   * For elements that represent <i>binary</I> blobs, this field will
   * point to a memory location where the data resides.  The length
   * of this memory blob is the next field.  char* implies byte sized
   * elements.  This is only used in INLINE_BINARY parser mode.
   * <B>IMPORTANT NOTE</B>: The data in this field is freed on a
   * destroy_sexp() call, so users should copy it to memory they allocate
   * if they wish it to persist after the sexp_t has been freed. 
   */
  char *bindata;

  /** 
   * The length of the data pointed at by bindata in bytes.
   */
  size_t binlength;
} sexp_t;

/**
 * parser mode flag used by continuation to toggle special parser
 * behaviour.
 */
typedef enum {
  /**
   * normal (LISP-style) s-expression parser behaviour.
   */
  PARSER_NORMAL,

  /**
   * treat atoms beginning with \#b\# as inlined binary data.  everything
   * else is treated the same as in PARSER_NORMAL mode.
   */
  PARSER_INLINE_BINARY,

  /**
   * if the event_handlers field in the continuation contains a non-null
   * value, the handlers specified in the parser_event_handlers_t struct
   * will be called as appropriate, but the parser will not allocate a
   * structure composed of sexp_t structs.  Note that if the event_handlers
   * is set to null and this mode is selected, the user would be better off
   * not calling anything in the first place, as they are telling the parser
   * to walk the string, but do nothing productive in the process.
   */
  PARSER_EVENTS_ONLY
} parsermode_t;

/**
 * Some users would prefer to, instead of parsing a full string and walking
 * a potentially huge sexp_t structure, use an XML SAX-style parser where
 * events are triggered as certain parts of the s-expression are encountered.
 * This structure contains a set of function pointers that are called by
 * the parser as it hits expression start and end, and completes reading
 * atoms and binary data.  NOTE: The parser_event_handler struct that is
 * a field in the continuation data structure is NOT freed by
 * destroy_continuation since structs for callbacks are ALWAYS malloc'd
 * by the user, not the library.
 */
typedef struct parser_event_handlers {
  /**
   * The start_sexpr function pointer is called when an open parenthesis
   * is encountered starting an expression.
   */
  void (* start_sexpr)();
 
  /**
   * The end_sexpr function pointer is called when an close parenthesis
   * is encountered ending an expression.
   */
  void (* end_sexpr)();

  /**
   * The characters function pointer is called when an atom is completely
   * parsed.  The function must take three arguments: a pointer to the
   * atom data, the number of elements the atom contains, and the
   * specific type of atom that the data represents.
   */
  void (* characters)(const char *data, size_t len, atom_t aty);

  /**
   * The binary function pointer is called when the parser is functioning
   * in INLINE_BINARY mode and binary data is encountered.  The function
   * must take two arguments: a pointer to the beginning of the binary data
   * and the number of bytes of data present.
   */
  void (* binary)(const char *data, size_t len);
} parser_event_handlers_t;

/**
 * A continuation is used by the parser to save and restore state between
 * invocations to support partial parsing of strings.  For example, if we
 * pass the string "(foo bar)(goo car)" to the parser, we want to be able
 * to retrieve each s-expression one at a time - it would be difficult to
 * return all s-expressions at once without knowing how many there are in
 * advance (this would require more memory management than we want...).
 * So, by using a continuation-based parser, we can call it with this string
 * and have it return a continuation when it has parsed the first 
 * s-expression.  Once we have processed the s-expression (accessible
 * through the <i>last_sexpr</i> field of the continuation), we can call
 * the parser again with the same string and continuation, and it will be
 * able to pick up where it left off.<P>
 *
 * We use continuations instead of a state-ful parser to allow multiple
 * concurrent strings to be parsed by simply maintaining a set of
 * continuations.  Manipulating continuations by hand is required if the
 * continuation-based parser is called directly.  This is <b>not
 * recommended</b> unless you are willing to deal with potential errors and
 * are willing to learn exactly how the continuation relates to the
 * internals of the parser.  A simpler approach is to use either the
 * <i>parse_sexp</i> function that simply returns an s-expression without
 * exposing the continuations, or the <i>iparse_sexp</i> function that
 * allows iteratively popping one s-expression at a time from a string
 * containing one or more s-expressions.  Refer to the documentation for
 * each parsing function for further details on behavior and usage.
 */
typedef struct pcont {
  /**
   * The parser stack used for iterative parsing.
   */
  faststack_t *stack;

  /**
   * The last full s-expression encountered by the parser.  If this is
   * NULL, the parser has not encountered a full s-expression and more
   * data is required for the current s-expression being parsed.  If this
   * is non-NULL, then the parser has encountered one s-expression and may
   * be partially through parsing the next s-expression.
   */
  sexp_t     *last_sexp;

  /**
   * Pointer to a temporary buffer used to store atom values during parsing.
   */
  char        *val;

  /**
   * Current number of bytes allocated for val.
   */
  size_t          val_allocated;

  /**
   * Current number of used bytes in val.
   */
  size_t          val_used;

  /**
   * Pointer to the character following the last character in the current
   * atom value being parsed.
   */
  char        *vcur;

  /**
   * Pointer to the last character to examine in the string being parsed.
   * When the parser is called with the continuation, this is the first
   * character that will be processed.  If this is NULL, the parser will
   * start parsing at the beginning of the string passed into the parser.
   */
  char        *lastPos;

  /**
   * This is a pointer to the beginning of the current string being
   * processed.  lastPos is a pointer to some value inside the string
   * that this points to.
   */
  char        *sbuffer;

  /**
   * This is the depth of parenthesis (the number of left parens encountered)
   * that the parser is currently working with.
   */
  unsigned int depth;

  /**
   * This is the depth of parenthesis encountered after a single quote (tick)
   * if the character immediately following the tick was a left paren.
   */
  unsigned int qdepth;

  /**
   * This is the state ID of the current state of the parser in the
   * DFA representing the parser.  The current parser is a DFA based parser
   * to simplify restoring the proper state from a continuation.
   */
  unsigned int state;

  /**
   * This is a flag indicating whether the next character to be processed
   * should be assumed to have been prefaced with a '\' character to escape
   * it.
   */
  unsigned int esc;

  /**
   * Flag whether or not we are processing an atom that was preceeded by
   * a single quote.
   */
  unsigned int squoted;

  /**
   * Error code.  Used to indicate that the continuation being returned does
   * not represent a successful parsing and thus the contents aren't of much
   * value.  
   */
  sexp_errcode_t error;

  /**
   * Mode.  The parsers' specialized behaviours can be activated by
   * tweaking the mode setting.  There are currently two available:
   * normal and inline_binary.  Inline_binary treats atoms that start
   * with \#b\# specially, assuming that they have the structure:
   *
   *    \#b\#s\#data
   *
   * Where s is a positive (greater than 0) integer representing the length
   * of the data, and data is s bytes of binary data following the \#
   * sign.  After the s bytes, it is assumed normal s-expression data
   * continues.
   */
  parsermode_t mode;

  /* -----------------------------------------------------------------
   * These fields below are related to dealing with INLINE_BINARY mode
   * ----------------------------------------------------------------- */

  /**
   * Length to expect of the current binary data being read in.
   * this also corresponds to the size of the memory allocated for
   * reading this binary data into.
   */
  size_t binexpected;

  /**
   * Number of bytes of the binary blob that have already been read in.
   */
  size_t binread;
  
  /**
   * Pointer to the memory containing the binary data being read in.
   */
  char *bindata;

  /**
   * Pointer to a structure holding handlers for sexpr events.  NULL for
   * normal parser operation.  This field is NOT freed by
   * destroy_continuation and must be free'd by the user.  This is because
   * these are malloc'd outside the library ALWAYS, so they are the user's
   * responsibility.
   */
  parser_event_handlers_t *event_handlers;
} pcont_t;

/**
 * \ingroup IO
 * This structure is a wrapper around a standard I/O file descriptor and
 * the parsing infrastructure (continuation and a buffer) required to
 * parse off of it.  This is used so that routines can hide the loops and
 * details required to accumulate up data read off of the file descriptor
 * and parse expressions individually out of it.
 */
typedef struct sexp_iowrap {
  /**
   * Continuation used to parse off of the file descriptor.
   */
  pcont_t *cc;

  /**
   * The file descriptor.  Currently CANNOT be a socket since implementation
   * uses read(), not recv().
   */
  int fd;

  /**
   * Buffer to read data into before parsing.
   */
  char buf[BUFSIZ];
  
  /**
   * Byte count for last read.  If it is -1, there was an error.  Otherwise,
   * it will be a value from 0 to BUFSIZ.
   */
  size_t cnt;
} sexp_iowrap_t;

/*========*/
/* GLOBAL */
/*========*/

/**
 * Global value indicating the most recent error condition encountered.
 * This value can be reset to SEXP_ERR_OK by calling sexp_errno_reset().
 */
extern sexp_errcode_t sexp_errno;

/*===========*/
/* FUNCTIONS */
/*===========*/

/* this is for C++ users */
#ifdef __cplusplus
extern "C" {
#endif
  /**
   * \ingroup parser
   * Set the parameters on atom value buffer allocation and growth sizes.
   * This is an important point for performance tuning, as many factors in
   * the expected expression structure must be taken into account such as:
   *
   *   - Average size of atom values
   *   - Variance in sizes of atom values
   *   - Amount of memory that is tolerably ''wasted'' (allocated but not
   *     used)
   *
   * The \a ss parameter specifies the initial size of all atom buffers.
   * Ideally, this should be sufficiently large to capture MOST atom values,
   * or at least close enough such that one growth is required.  The
   * \a gs parameter specifies the number of bytes to increase the buffer size
   * by when space is exhausted.  A safe choice for parameter sizes would
   * be on the order of the average size for \a ss, and one standard
   * deviation for \a gs.  This ensures that 50% of all expressions are
   * guaranteed to fit in the initial buffer, and rougly 80-90% will fit in
   * one growth.  If memory is not an issue, choosing ss to be the mean plus
   * one standard deviation will capture 80-90% of expressions in the initial
   * buffer, and a gs of one standard deviation will capture nearly all
   * expressions.
   *
   * Note: These parameters can be tuned at runtime as needs change, and they
   * will be applied to all expressions and expression elements parsed after
   * they are modified.  They will not be applied retroactively to expressions
   * that have already been parsed.
   */
  sexp_errcode_t set_parser_buffer_params(size_t ss, size_t gs);

  /**
   * return an allocated sexp_t.  This structure may be an already allocated
   * one from the stack or a new one if none are available.  Use this instead
   * of manually mallocing if you want to avoid excessive mallocs.  <I>Note:
   * Mallocing your own expressions is fine - you can even use 
   * sexp_t_deallocate to deallocate them and put them in the pool.</I>
   * Also, if the stack has not been initialized yet, this does so.
   */
  sexp_t *sexp_t_allocate(void);

  /**
   * given a malloc'd sexp_t element, put it back into the already-allocated
   * element stack.  This method will allocate a stack if one has not been
   * allocated already.
   */
  void sexp_t_deallocate(sexp_t *s);
  
  /**
   * In the event that someone wants us to release ALL of the memory used
   * between calls by the library, they can free it.  If you don't call
   * this, the caches will be persistent for the lifetime of the library
   * user.  Note that in the event of an error condition resulting in
   * sexp_errno being set, the user might consider calling this to clean up
   * any memory that may be lingering around that should be cleaned up.
   */
  void sexp_cleanup(void);

  /**
   * print a sexp_t struct as a string in the LISP style.  If the buffer
   * is large enough and the conversion is successful, the return value
   * represents the length of the string contained in the buffer.  If the
   * buffer was too small, or some other error occurred, the return
   * value is -1 and the contents of the buffer should not be assumed to
   * contain any useful information.  When the return value is -1, the
   * caller should check the contents of sexp_errno for details on what
   * error may have occurred.
   */
  int print_sexp(char *loc, size_t size, const sexp_t *e);

  /**
   * print a sexp_t structure to a buffer, growing it as necessary instead
   * of relying on fixed size buffers like print_sexp.  Important argument
   * to tune for performance reasons is <tt>ss</tt> - the
   * buffer start size.  The growsize used by the CSTRING routines also should
   * be considered for tuning via the sgrowsize() function.  This routine no
   * longer requires the user to specify the growsize, and uses the current
   * setting without changing it.
   */
  int print_sexp_cstr(CSTRING **s, const sexp_t *e, size_t ss);

  /**
   * Allocate a new sexp_t element representing a list.
   */
  sexp_t *new_sexp_list(sexp_t *l);

  /**
   * Allocate a new sexp_t element representing a raw binary atom.
   * This element will contain a pointer to the raw binary data
   * provided, as well as the binary data length.  The character
   * atom fields will be NULL and the corresponding val
   * length and allocation size will be set to zero since this
   * element is carrying a binary pointer only.
   */
  sexp_t *new_sexp_binary_atom(char *data, size_t binlength);
  
  /**
   * Allocate a new sexp_t element representing a value.  The user must
   * specify the precise type of the atom.  This used to default to
   * SEXP_BASIC, but this can lead to errors if the user did not expect this
   * assumption.  By explicitly passing in the atom type, the caller should
   * ensure that the data in the buffer is valid given the requested atom
   * type.  For performance reasons, such checks are left to the caller if
   * they are desired, and not performed in the library if they are not
   * wanted.
   */
  sexp_t *new_sexp_atom(const char *buf, size_t bs, atom_t aty);
    
  /** 
   * create an initial continuation for parsing the given string 
   */
  pcont_t *init_continuation(char *str);

  /**
   * destroy a continuation.  This involves cleaning up what it contains,
   * and cleaning up the continuation itself.
   */
  void destroy_continuation (pcont_t * pc);

  /**
   * \ingroup IO
   * create an IO wrapper structure around a file descriptor.  A NULL return
   * value indicates some problem occurred allocating the wrapper, so the
   * user should check the value of sexp_errno for further information.
   */
  sexp_iowrap_t *init_iowrap(int fd);

  /**
   * \ingroup IO
   * destroy an IO wrapper structure.  The file descriptor wrapped in the
   * wrapper will <B>not</B> be closed, so the caller is responsible
   * for manually calling close on the file descriptor.
   */
  void destroy_iowrap(sexp_iowrap_t *iow);

  /**
   * \ingroup IO 
   * given and IO wrapper handle, read one s-expression
   * off of it.  this expression may be contained in a continuation,
   * so there is no guarantee that under the covers an IO read
   * actually is occuring.  A return value of NULL can either indicate
   * a parser error or no more data on the input IO handle.  In the
   * event that NULL is returned, the user should check to see if
   * sexp_errno contains SEXP_ERR_IO_EMPTY (no more data) or a more
   * problematic error.
   */
  sexp_t *read_one_sexp(sexp_iowrap_t *iow);
  
  /** 
   * \ingroup parser
   * wrapper around parser for compatibility. 
   */
  sexp_t *parse_sexp(char *s, size_t len);

  /** 
   * \ingroup parser
   * wrapper around parser for friendlier continuation use 
   * pre-condition : continuation (cc) is NON-NULL! 
   */
  sexp_t *iparse_sexp(char *s, size_t len, pcont_t *cc);
  
  /**
   * \ingroup parser
   * given a LISP style s-expression string, parse it into a set of
   * connected sexp_t structures. 
   */
  pcont_t *cparse_sexp(char *s, size_t len, pcont_t *pc);
  
  /**
   * given a sexp_t structure, free the memory it uses (and recursively free
   * the memory used by all sexp_t structures that it references).  Note
   * that this will call the deallocation routine for sexp_t elements.
   * This means that memory isn't freed, but stored away in a cache of
   * pre-allocated elements.  This is an optimization to speed up the
   * parser to eliminate wasteful free and re-malloc calls.  Note: If using
   * inlined binary mode, this will free the data pointed to by the bindata
   * field.  So, if you care about the data after the lifetime of the
   * s-expression, make sure to make a copy before cleaning up the sexpr.
   */
  void destroy_sexp(sexp_t *s);

  /**
   * reset the value of sexp_errno to SEXP_ERR_OK.
   */
  void reset_sexp_errno();

  /**
   * print the contents of the parser continuation stack to a buffer.
   * this is useful if an expression is partially parsed and the caller
   * realizes that something is wrong with it.  with this routine,
   * the caller can reconstruct the expression parsed so far and use it
   * for error reporting.  this works with fixed size buffers allocated
   * by the caller.  there is not a CSTRING-based version currently.
   */
  void print_pcont(pcont_t * pc, char * buf, size_t buflen);

/* this is for C++ users */
#ifdef __cplusplus
}
#endif

#include "sexp_ops.h"

#endif /* __SEXP_H__ */

