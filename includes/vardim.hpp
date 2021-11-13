//-*- mode: c++; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

// Vardimme fa le cose a gazimme.

//
// file vardim.h
//

#ifndef _VARDIM_H
#define _VARDIM_H

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cstdarg>
#include <cstdbool>
#include <string>

#define NAME_LENGTH 32
#define OPTIONS_COUNT 4

// Enumerations

enum tagImplicitMode {
  IMPLICIT_NONE_MODE = 0,
  IMPLICIT_8_4_MODE,
  IMPLICIT_16_8_MODE,
  IMPLICIT_M_N_MODE
};

typedef enum tagImplicitMode tImplicitMode;

// Fortran 90 Variable type definition.

struct tagVariable {
  std::string m_sName;
  std::string m_pOptions[ OPTIONS_COUNT ];
  int m_nKind;
  int m_nDimension;
};

typedef struct tagVariable tVariable;

struct tagVariablesList {
  tVariable m_sVariable;
  struct tagVariablesList *m_pNext;
};

typedef struct tagVariablesList tVariablesList;

// Functions.

void make_subroutine_comment (std::fstream *, const std::string &);
void make_implicit_statement (std::fstream *, int, ...);
void make_include_statement (std::fstream *, const std::string &);
void make_use_statement (std::fstream *, const std::string &);
std::string make_type_declaration (const std::string &, int);
void make_variable_declaration (std::fstream *, const tVariable &);
void make_equivalence_declaration (std::fstream *, std::string *, int *);
void make_equivalence_declaration (std::fstream *, std::string *, int *);

// Main function.

int main(int, char *argv[]);

#endif // _VARDIM_H
