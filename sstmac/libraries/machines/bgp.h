#ifndef sstmac_libraries_machines_BGP_H
#define sstmac_libraries_machines_BGP_H

extern "C" {

struct _BGP_Personality_t
{
  struct
  {
    int Xnodes;
    int Ynodes;
    int Znodes;
    int Xcoord;
    int Ycoord;
    int Zcoord;
  } Network_Config;
};

void
Kernel_GetPersonality(_BGP_Personality_t* p, int size);

}

#endif // BGP_H
