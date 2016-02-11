/*
 *  This file is part of SST/macroscale: 
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top 
 *  SST/macroscale directory.
 */

#include <minimd.h>
#include <minimd-in.h>
#include <minimd-atom.h>
#include <minimd-force.h>
#include <minimd-neighbor.h>
#include <minimd-integrate.h>
#include <minimd-thermo.h>
#include <minimd-comm.h>
#include <minimd-timer.h>
#include <lib_compute_minimd.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sprockit/basic_string_tokenizer.h>
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <math.h>

using namespace sstmac;
using namespace sstmac::sw;

namespace mini {
    SpktRegisterApp("minimd", minimd, "miniapp for molecular dynamics");

    // Pointer types.

    // Utility function.
    inline void
    gettok(std::ifstream &infile, std::string &line,
        std::deque<std::string> &tok, size_t min_tokens, const char *message)
    {
      getline(infile, line);
      if (!infile.good())
        throw sprockit::value_error(std::string(message) + ":  Insufficient input lines.");
      tok.clear();
      pst::BasicStringTokenizer::tokenize(line, tok);
      if (tok.size() < min_tokens)
        throw sprockit::value_error(std::string(message) + "  Insufficient input fields.");
    }

    // It takes really long to type lexical_cast<> -- blc is boost::lexical_cast
    template<typename T>
      T
      blc(const std::string &value)
      {
        return lexical_cast<T>(value);
      }

    //
    // Simulate the parameter broadcasting 
    //
    void
    minimd::input()
    {
        mpi_comm* world = mpi()->comm_world();
        mpi()->comm_rank(world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);

        mpi()->barrier(world);

        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_double->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
        mpi()->bcast(1, mpi_type::mpi_int->id, mpi_id(0), world);
    }

    //
    // Set up the box.
    //
    void
    minimd::create_box()
    {
      double latt = pow((4 / in_->rho), (1.0 / 3.0));
      atom_->box.set_prd(in_->nx * latt, in_->ny * latt, in_->nz * latt);
    }

    //
    // Pretend to compute all atom positions etc.
    //
    void
    minimd::create_atoms()
    {
      mpi_comm* world = mpi()->comm_world();
      int atoms = 4 * in_->nx * in_->ny * in_->nz;
      assert(atoms>0 || 0 == "atoms computation overflow");
      int size = world->size();
      int rank = world->rank();
      atom_->natoms = atoms;
      atom_->nghost = 0;
      // We're faking the nlocal to be evenly divided (best-case scenario).
      int eachatoms = atoms / size;
      int sum = eachatoms * size;
      atom_->neighatoms.clear();
      atom_->neighatoms.resize(size, eachatoms);
      for (int i = 0; sum + i < atoms; ++i)
        ++atom_->neighatoms.at(i);
      atom_->nlocal = atom_->neighatoms.at(rank);
      // Now pretend to create the actual atoms like miniMD does.
      execution_->compute("create_atoms", atom_, in_);

      // Simulated mpi call
      mpi()->comm_rank(world);
      mpi()->allreduce(1, mpi_type::mpi_int->id, mpi_op::max, world);
      mpi()->allreduce(1, mpi_type::mpi_int->id, mpi_op::sum, world);
    }

    //
    // Pretend to assign velocities to all atoms.
    //
    void
    minimd::create_velocity()
    {
      mpi_comm* world = mpi()->comm_world();
      execution_->compute("create_velocity", atom_, NULL, 0);
      thermo_->temperature(atom_);
      mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, world);
      mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, world);
      mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, world);
      execution_->compute("create_velocity", atom_, NULL, 1);
    }

    //
    // We're sort of ignoring output for now.
    //
    void
    minimd::output()
    {
      // get the sum of all profiled timings for this method.
      execution_->compute("output", NULL, NULL);
    }

    //
    // Hi.
    //
    minimd::minimd() :
        in_(new in),
        atom_(new atom),
        force_(new force),
        neighbor_(new neighbor),
        integrate_(new integrate),
        thermo_(new thermo),
        comm_(new comm),
        timer_(new timer),
        execution_(0)
    {
    }

    //
    // Goodbye.
    //
    minimd::~minimd() throw ()
    {
    }

    //
    // This object gets access to the mpi api.
    //
    void
    minimd::consume_params(sprockit::sim_parameters* params)
    {
      execution_ = new lib_compute_minimd(id_);
      register_lib(execution_);

      in_->init(execution_, mpi());
      atom_->init(execution_, mpi());
      force_->init(execution_, mpi());
      neighbor_->init(execution_, mpi());
      integrate_->init(execution_, mpi());
      thermo_->init(execution_, mpi());
      comm_->init(execution_, mpi());
      timer_->init(execution_, mpi());

      in_->nx = params->get_int_param("minimd_nx");
      in_->ny = params->get_int_param("minimd_ny");
      in_->nz = params->get_int_param("minimd_nz");

      integrate_->ntimes = params->get_int_param("minimd_ntimes");

      neighbor_->set_bins(params->get_int_param("minimd_bins_x"),
          params->get_int_param("minimd_bins_y"),
          params->get_int_param("minimd_bins_z"));

      integrate_->dt = params->get_double_param("minimd_timestep");

      in_->t_request = params->get_double_param("minimd_temp");

      in_->rho = params->get_double_param("minimd_density");

      neighbor_->every = params->get_int_param("minimd_reneighbor");

      force_->cutforce = params->get_double_param("minimd_cutoff_inner");
      neighbor_->cutneigh = params->get_double_param("minimd_cutoff_outer");

      thermo_->nstat = params->get_int_param("minimd_thermo");
    }

    //
    // Go.
    //
    void
    minimd::skeleton_main()
    {
      //#define SSTMAC_DB(command) std::cerr << rank << " " << #command << "\n"; command
#define SSTMAC_DB(command) command
      timestamp startat = mpi()->init();
      mpi_id rank = mpi()->comm_rank(mpi()->comm_world());
      SSTMAC_DB( this->input());
      SSTMAC_DB( this->create_box());

      // This is, conveniently, pretty much a fixed overhead.
      // unsigned int seedp = rank.id * 12 + 123;
      // double randnoise = (double(rand_r(&seedp)) / RAND_MAX - 0.5) * 0.05;
      timestamp time((0.0003481));
      app::compute(time);
      //SSTMAC_DB( mpi()->compute(execution_->get("main", 0)) );
      //SSTMAC_DEBUG << "minimd[" << int(rank) << "]: setting up \n";
      SSTMAC_DB( comm_->setup(neighbor_->cutneigh, atom_));
      SSTMAC_DB( this->create_atoms());
      SSTMAC_DB( this->create_velocity());

      //SSTMAC_DEBUG << "minimd[" << int(rank) << "]: exchanging atoms \n";
      SSTMAC_DB( comm_->exchange(atom_));
      SSTMAC_DB( comm_->borders(atom_));
      SSTMAC_DB( neighbor_->build(atom_));
      SSTMAC_DB( thermo_->compute(0, atom_, neighbor_, force_));

      // Starting dynamics.
      //SSTMAC_DEBUG << "minimd[" << int(rank) << "]: starting dynamics \n";
      SSTMAC_DB( timer_->barrier_start(timer::TIME_TOTAL));
      SSTMAC_DB(
          integrate_->run(atom_, force_, neighbor_, comm_, thermo_, timer_));
      SSTMAC_DB( timer_->barrier_stop(timer::TIME_TOTAL));

      SSTMAC_DB( thermo_->compute(-1, atom_, neighbor_, force_));
      SSTMAC_DB( this->output());

      // This compute time is also not really dependent on problem size,
      // although it is a little noisier than the earlier value.
      // randnoise = (double(rand_r(&seedp)) / RAND_MAX - 0.5) * 0.10;
      time = timestamp(0.001495);
      app::compute(time);
      //SSTMAC_DB( mpi()->compute(execution_->get("main", 1)) );
      timestamp endat = SSTMAC_DB( mpi()->finalize() );

#undef SSTMAC_DB
    }

} //end namespace
