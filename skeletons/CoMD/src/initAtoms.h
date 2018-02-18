/// \file
/// Initialize the atom configuration.

#ifndef __INIT_ATOMS_H
#define __INIT_ATOMS_H

#include "mytype.h"

struct SimFlatSt;
struct LinkCellSt;

/// Atom data
typedef struct AtomsSt
{
   // atom-specific data
   int nLocal;    //!< total number of atoms on this processor
   long long nGlobal;   //!< total number of atoms in simulation
#pragma sst null_variable delete_all
   int* gid;      //!< A globally unique id for each atom
#pragma sst null_variable delete_all
   int* iSpecies; //!< the species index of the atom
#pragma sst null_variable delete_all
   real3*  r;     //!< positions
#pragma sst null_variable delete_all
   real3*  p;     //!< momenta of atoms
#pragma sst null_variable delete_all
   real3*  f;     //!< forces
#pragma sst null_variable delete_all
   real_t* U;     //!< potential energy per atom
} Atoms;


/// Allocates memory to store atom data.
Atoms* initAtoms(struct LinkCellSt* boxes);
void destroyAtoms(struct AtomsSt* atoms);

void createFccLattice(int nx, int ny, int nz, real_t lat, struct SimFlatSt* s);

void setVcm(struct SimFlatSt* s, real_t vcm[3]);
void setTemperature(struct SimFlatSt* s, real_t temperature);
void randomDisplacements(struct SimFlatSt* s, real_t delta);
#endif
