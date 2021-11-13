//-*- mode: c++; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-

//
// file vardim.c
//

//
// subroutine make_comment.
//

#include "vardim.hpp"

// Common variables.

// Default values for EMTP list sizes follow.

std::string pMessages[ 3 ] = {
  " 1st card (lists 1-10).",
  " 2nd card (lists 11-20).",
  " 3rd card (lists 21-30)."
};

char pTypes[ 4 ][ 16 ] = {
  "real(8)",
  "complex(8)",
  "character(8)",
  "integer(4)"
};

int lstdef[] = {
  250,
  300,
  500,
  100,
  2500,
  40,
  550,
  1750,
  75,
  160,
  50,
  50,
  5,
  460,
  50,
  40,
  4,
  5,
  1600,
  650,
  100,
  150,
  4000,
  3,
  400,
  50,
  50,
  1080
};

tVariablesList pModVars1[] = {
  { { "x", {}, 1, 3 }, &pModVars1[ 1 ] },
  { { "ykm", {}, 1, 5 }, &pModVars1[ 2 ] },
  { { "km", {}, 4, 5 }, &pModVars1[ 3 ] },
  { { "xk", {}, 1, 72 }, &pModVars1[ 4 ] },
  { { "xm", {}, 1, 72 }, &pModVars1[ 5 ] },
  { { "weight", {}, 1, 14 }, &pModVars1[ 6 ] },
  { { "iwtent", {}, 4, 52 }, &pModVars1[ 7 ] },
  { { "con1", {}, 1, 51 }, &pModVars1[ 8 ] },
  { { "iskip", {}, 4, 13 }, &pModVars1[ 9 ] },
  { { "zinf", {}, 1, 13 }, &pModVars1[ 10 ] },
  { { "eta", {}, 1, 13 }, &pModVars1[ 11 ] },
  { { "nhist", {}, 4, 13 }, &pModVars1[ 12 ] },
  { { "stailm", {}, 1, 15 }, &pModVars1[ 13 ] },
  { { "stailk", {}, 1, 15 }, &pModVars1[ 14 ] },
  { { "xmax", {}, 1, 58 }, &pModVars1[ 15 ] },
  { { "koutvp", {}, 4, 62 }, &pModVars1[ 16 ] },
  { { "bnrg", {}, 4, 62 }, &pModVars1[ 17 ] },
  { { "sconst", {}, 1, 20 }, &pModVars1[ 18 ] },
  { { "cnvhst", { "target" }, 1, 73 }, &pModVars1[ 19 ] },
  { { "sfd", {}, 1, 71 }, &pModVars1[ 20 ] },
  { { "qfd", {}, 1, 71 }, &pModVars1[ 21 ] },
  { { "semaux", { "target" }, 1, 22 }, &pModVars1[ 22 ] },
  { { "ibsout", {}, 4, 12 }, &pModVars1[ 23 ] },
  { { "bvalue", {}, 1, 12 }, &pModVars1[ 24 ] },
  { { "sptacs", {}, 1, 19 }, &pModVars1[ 25 ] },
  { { "kswtyp", {}, 4, 6 }, &pModVars1[ 26 ] },
  { { "modswt", {}, 4, 6 }, &pModVars1[ 27 ] },
  { { "kbegsw", {}, 4, 6 }, &pModVars1[ 28 ] },
  { { "lastsw", {}, 4, 6 }, &pModVars1[ 29 ] },
  { { "kentnb", {}, 4, 6 }, &pModVars1[ 30 ] },
  { { "nbhdsw", {}, 4, 63 }, &pModVars1[ 31 ] },
  { { "topen", {}, 1, 60 }, &pModVars1[ 32 ] },
  { { "crit", {}, 1, 6 }, &pModVars1[ 33 ] },
  { { "kdepsw", {}, 4, 60 }, &pModVars1[ 34 ] },
  { { "tdns", {}, 1, 6 }, &pModVars1[ 35 ] },
  { { "isourc", {}, 4, 6 }, &pModVars1[ 36 ] },
  { { "energy", {}, 1, 6 }, &pModVars1[ 37 ] },
  { { "iardub", {}, 4, 63 }, &pModVars1[ 38 ] },
  { { "ardube", {}, 1, 64 }, &pModVars1[ 39 ] },
  { { "nonlad", {}, 4, 9 }, &pModVars1[ 40 ] },
  { { "nonle", {}, 4, 9 }, &pModVars1[ 41 ] },
  { { "vnonl", {}, 1, 9 }, &pModVars1[ 42 ] },
  { { "curr", {}, 1, 9 }, &pModVars1[ 43 ] },
  { { "anonl", {}, 1, 9 }, &pModVars1[ 44 ] },
  { { "vecnl1", {}, 1, 9 }, &pModVars1[ 45 ] },
  { { "vecnl2", {}, 1, 9 }, &pModVars1[ 46 ] },
  { { "namenl", {}, 4, 9 }, &pModVars1[ 47 ] },
  { { "vzero", {}, 1, 9 }, &pModVars1[ 48 ] },
  { { "ilast", {}, 4, 9 }, &pModVars1[ 49 ] },
  { { "nltype", {}, 4, 9 }, &pModVars1[ 50 ] },
  { { "kupl", {}, 4, 9 }, &pModVars1[ 51 ] },
  { { "nlsub", {}, 4, 9 }, &pModVars1[ 52 ] },
  { { "xoptbr", {}, 1, 2 }, &pModVars1[ 53 ] },
  { { "coptbr", {}, 1, 2 }, &pModVars1[ 54 ] },
  { { "cursub", {}, 1, 53 }, &pModVars1[ 55 ] },
  { { "cchar", {}, 1, 10 }, &pModVars1[ 56 ] },
  { { "vchar", {}, 1, 10 }, &pModVars1[ 57 ] },
  { { "gslope", {}, 1, 10 }, &pModVars1[ 58 ] },
  { { "ktrans", {}, 4, 1 }, &pModVars1[ 59 ] },
  { { "kk", {}, 4, 1 }, &pModVars1[ 60 ] },
  { { "c", {}, 1, 3 }, &pModVars1[ 61 ] },
  { { "tr", {}, 1, 5 }, &pModVars1[ 61 ] },
  { { "tx", {}, 1, 5 }, &pModVars1[ 62 ] },
  { { "r", {}, 1, 3 }, &pModVars1[ 63 ] },
  { { "nr", {}, 4, 2 }, &pModVars1[ 64 ] },
  { { "length", {}, 4, 2 }, &pModVars1[ 65 ] },
  { { "cik", {}, 1, 2 }, &pModVars1[ 66 ] },
  { { "ci", {}, 1, 2 }, &pModVars1[ 67 ] },
  { { "ck", {}, 1, 2 }, &pModVars1[ 68 ] },
  { { "ismout", {}, 4, 70 }, &pModVars1[ 69 ] },
  { { "elp", {}, 1, 65 }, &pModVars1[ 70 ] },
  { { "cu", {}, 1, 66 }, &pModVars1[ 71 ] },
  { { "shp", {}, 1, 67 }, &pModVars1[ 72 ] },
  { { "histq", {}, 1, 68 }, &pModVars1[ 73 ] },
  { { "ismdat", {}, 4, 69 }, &pModVars1[ 74 ] },
  { { "texvec", { "target" }, 3, 7 }, &pModVars1[ 75 ] },
  { { "ibrnch", {}, 4, 12 }, &pModVars1[ 76 ] },
  { { "jbrnch", {}, 4, 12 }, &pModVars1[ 77 ] },
  { { "tstop", {}, 1, 4 }, &pModVars1[ 78 ] },
  { { "nonlk", {}, 4, 9 }, &pModVars1[ 79 ] },
  { { "nonlm", {}, 4, 9 }, &pModVars1[ 80 ] },
  { { "spum", {}, 1, 25 }, &pModVars1[ 81 ] },
  { { "kks", {}, 4, 1 }, &pModVars1[ 82 ] },
  { { "kknonl", {}, 4, 57 }, &pModVars1[ 83 ] },
  { { "znonl", {}, 1, 57 }, &pModVars1[ 84 ] },
  { { "znonlb", {}, 1, 1 }, &pModVars1[ 85 ] },
  { { "znonlc", {}, 1, 1 }, &pModVars1[ 86 ] },
  { { "finit", {}, 1, 1 }, &pModVars1[ 87 ] },
  { { "ksub", {}, 4, 53 }, &pModVars1[ 88 ] },
  { { "msub", {}, 4, 53 }, &pModVars1[ 89 ] },
  { { "isubeg", {}, 4, 55 }, &pModVars1[ 90 ] },
  { { "litype", {}, 4, 2 }, &pModVars1[ 91 ] },
  { { "imodel", {}, 4, 2 }, &pModVars1[ 92 ] },
  { { "kbus", {}, 4, 2 }, &pModVars1[ 93 ] },
  { { "mbus", {}, 4, 2 }, &pModVars1[ 94 ] },
  { { "kodebr", {}, 4, 2 }, &pModVars1[ 95 ] },
  { { "cki", {}, 1, 2 }, &pModVars1[ 96 ] },
  { { "ckkjm", {}, 1, 2 }, &pModVars1[ 97 ] },
  { { "indhst", {}, 4, 2 }, &pModVars1[ 98 ] },
  { { "kodsem", {}, 4, 2 }, &pModVars1[ 99 ] },
  { { "namebr", { "target" }, 4, 54 }, &pModVars1[ 100 ] },
  { { "iform", {}, 4, 4 }, &pModVars1[ 101 ] },
  { { "node", {}, 4, 4 }, &pModVars1[ 102 ] },
  { { "crest", {}, 1, 4 }, &pModVars1[ 103 ] },
  { { "time1", {}, 1, 4 }, &pModVars1[ 104 ] },
  { { "time2", {}, 1, 4 }, &pModVars1[ 105 ] },
  { { "tstart", {}, 1, 4 }, &pModVars1[ 106 ] },
  { { "sfreq", {}, 1, 4 }, &pModVars1[ 107 ] },
  { { "kmswit", {}, 4, 60 }, &pModVars1[ 108 ] },
  { { "nextsw", {}, 4, 6 }, &pModVars1[ 109 ] },
  { { "rmfd", {}, 1, 61 }, &pModVars1[ 110 ] },
  { { "cikfd", {}, 1, 61 }, &pModVars1[ 111 ] },
  { { "imfd", {}, 4, 27 }, &pModVars1[ 112 ] },
  { { "tclose", {}, 1, 6 }, &pModVars1[ 113 ] },
  { { "adelay", {}, 1, 60 }, &pModVars1[ 114 ] },
  { { "kpos", {}, 4, 6 }, &pModVars1[ 115 ] },
  { { "namesw", {}, 4, 6 }, &pModVars1[ 116 ] },
  { { "e", { "target" }, 1, 1 }, &pModVars1[ 117 ] },
  { { "f", { "target" }, 1, 1 }, &pModVars1[ 118 ] },
  { { "kssfrq", {}, 4, 1 }, &pModVars1[ 119 ] },
  { { "kode", {}, 4, 1 }, &pModVars1[ 120 ] },
  { { "kpsour", {}, 4, 1 }, &pModVars1[ 121 ] },
  { { "volti", { "target" }, 1, 59 }, &pModVars1[ 122 ] },
  { { "voltk", { "target" }, 1, 26 }, &pModVars1[ 123 ] },
  { { "volt", { "target" }, 1, 59 }, &pModVars1[ 124 ] },
  { { "bus", {}, 3, 1 }, &pModVars1[ 125 ] },
  { { "karray", {}, 4, 0 }, NULL },
};

tVariablesList pModVars2[] = {
  { { "tp", {}, 1, 23 }, &pModVars2[ 1 ] },
  { { "norder", {}, 4, 1 }, &pModVars2[ 2 ] },
  { { "index", {}, 4, 1 }, &pModVars2[ 3 ] },
  { { "diag", {}, 1, 1 }, &pModVars2[ 4 ] },
  { { "diab", {}, 1, 1 }, &pModVars2[ 5 ] },
  { { "solr", {}, 1, 1 }, &pModVars2[ 6 ] },
  { { "soli", {}, 1, 1 }, &pModVars2[ 7 ] },
  { { "ich1", {}, 4, 1 }, &pModVars2[ 8 ] },
  { { "bnd", {}, 1, 9 }, &pModVars2[ 9 ] },
  { { "iloc", {}, 4, 23 }, &pModVars2[ 10 ] },
  { { "gnd", {}, 1, 23 }, &pModVars2[ 11 ] },
  { { "karray", {}, 4, 9 }, &pModVars2[ 12 ] },
  { { "xdat", {}, 1, 71 }, &pModVars2[ 13 ] },
  { { "ydat", {}, 1, 71 }, &pModVars2[ 14 ] },
  { { "aphdat", {}, 1, 71 }, &pModVars2[ 15 ] },
  { { "jndex", {}, 4, 1 }, &pModVars2[ 16 ] },
  { { "diagg", {}, 1, 1 }, &pModVars2[ 17 ] },
  { { "diabb", {}, 1, 1 }, &pModVars2[ 18 ] },
  { { "solrsv", {}, 1, 1 }, &pModVars2[ 19 ] },
  { { "solisv", {}, 1, 1 }, &pModVars2[ 20 ] },
  { { "gndd", {}, 1, 23 }, &pModVars2[ 21 ] },
  { { "bndd", {}, 1, 23 }, &pModVars2[ 22 ] },
  { { "nekfix", {}, 4, 4 }, &pModVars2[ 23 ] },
  { { "fxtem1", {}, 1, 4 }, &pModVars2[ 24 ] },
  { { "fxtem2", {}, 1, 4 }, &pModVars2[ 25 ] },
  { { "fxtem3", {}, 1, 4 }, &pModVars2[ 26 ] },
  { { "fxtem4", {}, 1, 4 }, &pModVars2[ 27 ] },
  { { "fxtem5", {}, 1, 4 }, &pModVars2[ 28 ] },
  { { "fxtem6", {}, 1, 4 }, &pModVars2[ 29 ] },
  { { "fixbu1", {}, 1, 4 }, &pModVars2[ 30 ] },
  { { "fixbu2", {}, 1, 4 }, &pModVars2[ 31 ] },
  { { "fixbu3", {}, 1, 4 }, &pModVars2[ 32 ] },
  { { "fixbu4", {}, 1, 4 }, &pModVars2[ 33 ] },
  { { "fixbu5", {}, 1, 4 }, &pModVars2[ 34 ] },
  { { "fixbu6", {}, 1, 4 }, &pModVars2[ 35 ] },
  { { "fixbu7", {}, 1, 4 }, &pModVars2[ 36 ] },
  { { "fixbu8", {}, 1, 4 }, &pModVars2[ 37 ] },
  { { "fixbu9", {}, 1, 4 }, &pModVars2[ 38 ] },
  { { "fixbu10", {}, 1, 4 }, &pModVars2[ 39 ] },
  { { "fixbu11", {}, 1, 4 }, &pModVars2[ 40 ] },
  { { "kndex", {}, 4, 4 }, &pModVars2[ 41 ] },
  { { "karray", {}, 4, 9 }, &pModVars2[ 42 ] },
  { { "p", {}, 1, 75 }, &pModVars2[ 43 ] },
  { { "z", {}, 1, 75 }, &pModVars2[ 44 ] },
  { { "ic", {}, 4, 71 }, &pModVars2[ 45 ] },
  { { "r", {}, 1, 71 }, &pModVars2[ 46 ] },
  { { "d", {}, 1, 71 }, &pModVars2[ 47 ] },
  { { "gmd", {}, 1, 71 }, &pModVars2[ 48 ] },
  { { "x", {}, 1, 71 }, &pModVars2[ 49 ] },
  { { "y", {}, 1, 71 }, &pModVars2[ 50 ] },
  { { "tb2", {}, 1, 71 }, &pModVars2[ 51 ] },
  { { "itb3", {}, 4, 71 }, &pModVars2[ 52 ] },
  { { "workr1", {}, 1, 71 }, &pModVars2[ 53 ] },
  { { "workr2", {}, 1, 71 }, &pModVars2[ 54 ] },
  { { "text", {}, 3, 76 }, &pModVars2[ 55 ] },
  { { "gd", {}, 1, 74 }, &pModVars2[ 56 ] },
  { { "bd", {}, 1, 74 }, &pModVars2[ 57 ] },
  { { "yd", {}, 1, 74 }, &pModVars2[ 58 ] },
  { { "itbic", {}, 4, 73 }, &pModVars2[ 59 ] },
  { { "tbr", {}, 1, 73 }, &pModVars2[ 60 ] },
  { { "tbd", {}, 1, 73 }, &pModVars2[ 61 ] },
  { { "tbg", {}, 1, 73 }, &pModVars2[ 62 ] },
  { { "tbx", {}, 1, 73 }, &pModVars2[ 63 ] },
  { { "tby", {}, 1, 73 }, &pModVars2[ 64 ] },
  { { "tbtb2", {}, 4, 73 }, &pModVars2[ 65 ] },
  { { "itbtb3", {}, 4, 73 }, &pModVars2[ 66 ] },
  { { "tbtext", {}, 3, 73 }, &pModVars2[ 67 ] },
  { { "karray", {}, 4, 9 }, &pModVars2[ 68 ] },
  { { "karray", {}, 4, 9 }, NULL }
};

// functions bodies.

void make_subroutine_comment (std::fstream *pStream, const std::string &sSubName)
{
  if (pStream) {
    if (pStream -> is_open()) {
      *pStream << "!" << std::endl;
      *pStream << "! subroutine " << sSubName << std::endl;
      *pStream << "!" << std::endl;
    }
  }
}

void make_implicit_statement (std::fstream *pStream, int nMode, ...)
{
  int i, nRealDim, nIntegerDim;
  va_list aplist;

  if (pStream) {
    if (pStream -> is_open()) {
      switch  (nMode) {
      case IMPLICIT_NONE_MODE:
        *pStream << "  implicit none" << std::endl;
        break;

      case IMPLICIT_8_4_MODE:
        make_implicit_statement (pStream, IMPLICIT_M_N_MODE, 8, 4);
        break;

      case IMPLICIT_16_8_MODE:
        make_implicit_statement (pStream, IMPLICIT_M_N_MODE, 16, 8);
        break;

      case IMPLICIT_M_N_MODE:
        va_start (aplist, nMode);
        for (i = 0; i < 2; i++) {
          switch (i) {
          case 0:
            nRealDim = (int) va_arg(aplist, int);
            break;

          case 1:
            nIntegerDim = (int) va_arg(aplist, int);
            break;
          }
          va_end (aplist);
        }
        *pStream << "  implicit real(" << nRealDim << ") (a-h, o-z), integer(" << nIntegerDim << ") (i-n)" << std::endl;
        break;
      }
    }
  }
}

void make_include_statement (std::fstream *pStream, const std::string &sIncludeFilename)
{
  if (pStream) {
    if (pStream -> is_open()) {
      *pStream << "  include '" << sIncludeFilename << "'" << std::endl;
    }
  }
}

void make_use_statement (std::fstream *pStream, const std::string &sModuleName)
{
  if (pStream) {
    if (pStream -> is_open()) {
      *pStream << "  use " << sModuleName << std::endl;
    }
  }
}

std::string make_type_declaration (const std::string &sTypeName, int nDimension)
{
  std::string strOut;
  std::stringstream s(strOut, std::ios_base::in);

  if (nDimension > 0) {
    s << sTypeName << "(" << nDimension << ")";
  } else {
    s << sTypeName;
  }
  return strOut;
}

void make_variable_declaration (std::fstream *pStream, tVariable sVariable, std::string *pTypes)
{
  int i;

  if (pStream) {
    if (pStream -> is_open()) {
      if (pTypes) {
        if (!sVariable.m_sName.empty()) {
          *pStream << "  " << pTypes[ sVariable.m_nKind ];
          if (sVariable.m_pOptions) {
            for (i = 0; !sVariable.m_pOptions[ i ].empty(); i++) {
              *pStream << ", " << sVariable.m_pOptions[ i ];
            }
          }
          *pStream << " :: " << sVariable.m_sName;
          if (sVariable.m_nDimension < 0) {
            *pStream << "(:)" << std::endl;
          } else if (sVariable.m_nDimension == 0) {
            *pStream << std::endl;
          } else {
            *pStream << "(" << sVariable.m_nDimension << ")" << std::endl;
          }
        }
      }
    }
  }
}

void make_equivalence_declaration (std::fstream *pStream, std::string *pVarNames, int *pDimensions)
{
  int i;

  if (pStream) {
    if (pStream -> is_open()) {
      if (pVarNames) {
        if (pDimensions) {
          *pStream << "  equivalence (";
          for (i = 0; i < 2; i++) {
            if(pDimensions[ i ] > 0) {
              *pStream << pVarNames[ i ] << "(" << pDimensions[ i ] << ")";
            } else {
              *pStream << pVarNames[ i ];
            }
            if (i <= 1)
              *pStream << ", ";
            else
              *pStream << std::endl;
          }
        }
      }
    }
  }
}

int main (int argc, char *arv[])
{
  int i, ii, indexm, integerdim, ios;
  int j;
  int kextra[ 29 ], kk;
  int lm, lstdef[ 49 ], ltlabl, lstnew[ 99 ];
  int m, mextra, modvarc, mtot, mul34, mulvar[ 7 ];
  int n1, n3, n4, n7, n9, n18, n28, n37, n86, n87, nd, ndate, nh, nm;
  int nonneg, nrec3, ns, ntime, numkex, numlst, ny;
  int realdim;
  float d1, d2, d3;
  float lphd2, lphase;
  void *pLunit[ 16 ] = {
    (void *) &std::cerr,
    NULL,
    NULL,
    NULL,
    (void *) &std::cin,
    (void *) &std::cout,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  };
  std::string sBus1, sBus2, sChars = std::string("ijklmn");
  tVariable sTempVar;
  tVariablesList *pTemp;
  // ---
  pLunit[ 1 ] = (void *) new std::fstream ("fort.8", std::fstream::in | std::fstream::out);
  if (((std::fstream *) pLunit[ 1 ]) -> is_open()) {
    pLunit[ 2 ] = (void *) new std::fstream ("fort.9", std::fstream::in | std::fstream::out);
    if (((std::fstream *) pLunit[ 2 ]) -> is_open()) {
      indexm = 0;
      mextra = 0;
      // One less than the number of "vardim" modules.
      numkex = 7;
      // Default values for extra offsets of "vardim"
      // modules belonging to non-solution overlays.
      for (i = 0; i < numkex; i++)
        kextra[ i ] = 0;
      for (i = 0; i < numlst; i++)
        *((std::istream *) pLunit[ 4 ]) >> std::setw(8) >> lstnew[ i ];
      *((std::ostream *) pLunit[ 5 ]) << "\n Pseudo-listing of data cards which have been read by the variable-dimensioning program 'vardim' .   Only\n";
      *((std::ostream *) pLunit[ 5 ]) << "if all data fields are punched with 'clean' i8 integer information will this be a true listing.   Data cards\n";
      *((std::ostream *) pLunit[ 5 ]) << "are in fact read in and then printed out using integer variables and 10i8 format.\n";
      *((std::ostream *) pLunit[ 5 ]) << " " << std::string (111, '-');
      *((std::ostream *) pLunit[ 5 ]) << std::string (31, ' ') << "0" << std::string (9, ' ');
      for (i = 0; i < 8; i++)
        *((std::ostream *) pLunit[ 5 ]) << std::string (9, ' ') << std::setw (1) << i;
      *((std::ostream *) pLunit[ 5 ]) << std::endl;
      *((std::ostream *) pLunit[ 5 ]) << std::string (31, ' ') << "0";
      for (i = 0; i < 8; i++)
        *((std::ostream *) pLunit[ 5 ]) << std::string (9, ' ') << "0";
      *((std::ostream *) pLunit[ 5 ]) << std::endl;
      *((std::ostream *) pLunit[ 5 ]) << std::string (80, '-') << std::endl;
      for (j = 0; j < 3; j++) {
        *((std::ostream *) pLunit[ 5 ]) << " 1st card (lists 1-10)." << std::string (7, ' ') << "1";
        for (i = 0; i < 10; i++)
          *((std::ostream *) pLunit[ 5 ]) << std::setw(8) << lstnew[ 10 * j + i ];
        *((std::ostream *) pLunit[ 5 ]) << std::endl;
      }
      if (lstnew[ 0 ] / 10000000 != 9)
        goto a5294;
      for (i = 0; i < numkex; i++)
        *((std::istream *) pLunit[ 4 ]) >> std::setw(8) >> kextra[ i ];
      *((std::ostream *) pLunit[ 5 ]) << " Supplemental offsets.          1" << std::endl;
      for (i = 0; i < numkex; i++)
        *((std::ostream *) pLunit[ 5 ]) << std::setw(8) << kextra[ i ];
      *((std::ostream *) pLunit[ 5 ]) << std::endl;
      lstnew[ 0 ] -= 90000000;

    a5294:
      if (lstnew[ 10 ] / 10000000 != 9)
        goto a5297;
      n4 = lstnew[ 10 ] - 90000000;
      for (j = 0; j < numlst; j++)
        lstnew[ j ] = lstdef[ j ] * n4;

    a5297:
      if (lstnew[ 0 ] > 0)
        lstnew[ 0 ] += 2;
      *((std::fstream *) pLunit[ 5 ]) << std::string(111, '-') << std::endl;
      for (i = 0; i < numlst; i++) {
        n1 = i;
        if (lstnew[ i ] >= 10000000)
          goto a9000;
        if (lstnew[ i ] <= 0)
          lstnew[ i ] = lstdef[ i ];
      }
      if (lstnew[ 18 ] <= 23)
        lstnew[ 18 ] = 23;
      if (lstnew[ 25 ] <= 10)
        lstnew[ 25 ] = 10;
      n1 = lstnew[ 15 ] / 2;
      if (2 * n1 != lstnew[ 26 ])
        ++lstnew[ 26 ];
      // List number 51 is a dependent list, always twice
      // the size of list number 13.
      // This is for frequency-dependence arrays
      // 'con1', 'con2', and 'con3' .
      lstnew[ 50 ] = 6 * lstnew[ 12 ];
      // List number 52 is a dependent list, always twice
      // the size of list number 1, plus one.
      // This is for frequency-dependence array   'iwtent' .
      lstnew[ 51 ] = lstnew[ 50 ] + 1;
      // List number 7 also serves for storage as part of
      // list number 3.   Hence list 7 must not be shorter.
      // If (  lstnew(3)  .gt.  lstnew(7) )
      //    1 lstnew(7) = lstnew(3)
      // List number 53 is for terminal pairs
      // associated with compensation elements.
      lstnew[ 52 ] = lstnew[ 8 ] + 3 * lstnew[ 16 ];
      // Cable adds 5 list-2 extensions to  'namebr'
      lstnew[ 53 ] = 6 * lstnew[ 1 ];
      // List 55 has one entry for each subnetwork.
      lstnew[ 54 ] = lstnew[ 8 ] + lstnew[ 16 ];
      // List 56 is for vladimir's "cikfd" and "rmfd"
      // Arrays for freq-depend. generator equivalent
      lstnew[ 57 ] = lstnew[ 1 ] * lstnew[ 26 ] / 2;
      // List 57 maps #phases into znonl size.
      lstnew[ 56 ] = lstnew[ 0 ] * lstnew[ 23 ];
      // List 58 is for extrema vector  "xmax" .
      lstnew[ 58 ] = 4 * lstnew[ 11 ];
      // List 59 is extended working vector "volti"
      lstnew[ 59 ] = 2 * lstnew[ 25 ];
      // List 60 provides double-length "kmswit" as
      // Replacement for earlier "klow" and "khigh"
      lstnew[ 59 ] = 3 * lstnew[ 5 ];
      // List 61 is for ontario hydro freq-dep source
      lstnew[ 60 ] = 1;
      // List 62 is for power/energy ("koutvp" which
      // Now includes former, separate "koutie")
      lstnew[ 61 ] = 2 * lstnew[ 17 ];
      lstnew[ 62 ] = 3 * lstnew[ 5 ];
      lstnew[ 63 ] = 4 * lstnew[ 5 ];
      // List 65 is for type-59 s.m. electrical data:
      lstnew[ 64 ] = 101 * lstnew[ 16 ];
      // List 66 is for type-59 s.m. electrical variables:
      lstnew[ 65 ] = 24 * lstnew[ 16 ];
      // List 67 is for type-59 s.m. mechanical data:
      lstnew[ 66 ] = 12 * lstnew[ 15 ];
      // List 68 is for type-59 s.m. mechanical variables:
      lstnew[ 67 ] = 6 * lstnew[ 15 ];
      // List 69 is for type-59 s.m. pointers:
      lstnew[ 68 ] = 30 * lstnew[ 16 ];
      // List 70 is for type-59 s.m. output pointers:
      lstnew[ 69 ] = 5 * lstnew[ 10 ] + 2;
      // List 71 is to extend list 21 for ljg (18 aug 1987):
      lstnew[ 70 ] = 2 * lstnew[ 20 ];
      lstnew[ 71 ] = lstnew[ 7 ] + lstnew[ 27 ];
      // List 73 is to extend list 22 for ljg (10 mar 1988):
      lstnew[ 72 ] = lstnew[ 21 ] + lstnew[ 20 ];
      mtot = 0;
      for (pTemp = &pModVars1[ 0 ]; pTemp; pTemp = pTemp -> m_pNext) {
        n9 = pTemp -> m_sVariable.m_nDimension;
        if ((n9 != 0) && (n9 != 98))
          goto a4301;
        n37 = 3;
        sBus1 = pTemp -> m_sVariable.m_sName;
        sBus2 = std::string (1, sBus1[ 1 ]);
        for (unsigned long j = 0; j < sChars.length(); j++) {
          if (sBus2[ 0 ] == sChars[ j ]) {
            n37 = 4;
            break;
          }
        }
        if (pTemp -> m_sVariable.m_nKind != 0)
          n37 = pTemp -> m_sVariable.m_nKind;
        mtot += mulvar[ n37 - 1 ] * lstnew[ n9 - 1 ];
      }

    a4301:
      *((std::fstream *) pLunit[ 1 ]) << "!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-\n\n";
      *((std::fstream *) pLunit[ 1 ]) << "!\n! file newmods.f90\n!\n";
      *((std::fstream *) pLunit[ 3 ]) << "!-*- mode: f90; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-\n\n";
      *((std::fstream *) pLunit[ 3 ]) << "!\n! file labcom.f90\n!\n";
      *((std::fstream *) pLunit[ 3 ]) << "module labcom\n";
      *((std::fstream *) pLunit[ 3 ]) << "  use blkcom\n";
      make_subroutine_comment ((std::fstream *) pLunit[ 1 ], "main10");
      *((std::fstream *) pLunit[ 1 ]) << "#ifdef WITH_MAIN10\n";
      *((std::fstream *) pLunit[ 1 ]) << "subroutine main10\n";
      make_use_statement ((std::fstream *) pLunit[ 1 ], "labcom");
      make_implicit_statement ((std::fstream *) pLunit[ 1 ], IMPLICIT_NONE_MODE);
      make_implicit_statement ((std::fstream *) pLunit[ 3 ], IMPLICIT_NONE_MODE);
      for (pTemp = &pModVars1[ 0 ]; pTemp; pTemp = pTemp -> m_pNext) {
        n3 = pTemp -> m_sVariable.m_nDimension;
        nonneg = lstnew[ n3 - 1 ];
        if (nonneg <= 0) {
          nonneg = 1;
          n4 = pTemp -> m_sVariable.m_nKind;
          if (n4 != 0) {
            sTempVar = pTemp -> m_sVariable;
            sTempVar.m_nDimension = nonneg;
            make_variable_declaration ((std::fstream *) pLunit[ 3 ], sTempVar);
          }
        }
      }
      *((std::fstream *) pLunit[ 1 ]) << "  !\n";
      *((std::fstream *) pLunit[ 3 ]) << "  !\n";
      for (pTemp = &pModVars1[ 0 ]; pTemp; pTemp = pTemp -> m_pNext) {
        n3 = pTemp -> m_sVariable.m_nDimension;
        nonneg = lstnew[ n3 - 1 ];
        if (nonneg <= 0)
          nonneg = 1;
        n4 = pTemp -> m_sVariable.m_nKind;
        if (n4 != 0) {
          sTempVar = pTemp -> m_sVariable;
          sTempVar.m_nDimension = nonneg;
          make_variable_declaration ((std::fstream *) pLunit[ 4 ], sTempVar);
        }

      a9000:
        *((std::ostream *) pLunit[ 5 ]) << "Exiting vardim!" << std::endl;
      }
      ((std::fstream *) pLunit[ 2 ]) -> close();
      delete (std::fstream *) pLunit[ 2 ];
    }
    ((std::fstream *) pLunit[ 1 ]) -> close();
    delete (std::fstream *) pLunit[ 1 ];
  }
}

// end of file vardim.c
