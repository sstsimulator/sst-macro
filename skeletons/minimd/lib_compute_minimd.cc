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

#include "lib_compute_minimd.h"
#include <sstmac/sstmacro.h>

#include "minimd-atom.h"
#include "minimd-neighbor.h"
#include "minimd-in.h"

using namespace sstmac;
using namespace std;

namespace mini
{

  lib_compute_minimd::lib_compute_minimd(software_id id) :
    lib_compute_time(id)

  {
    id_ = id;
    using_loops_ = sstmac::env::params->get_bool_param(
        "lib_compute_loops_enable");
    if (!using_loops_)
      {

        funcs_["create_atoms"] = timestamp(1e-7);
        funcs_["create_velocity"] = timestamp(1e-7);
        funcs_["output"] = timestamp(1e-7);
        funcs_["create_atoms"] = timestamp(1e-7);
        funcs_["create_atoms"] = timestamp(1e-7);

        funcs_["Neighbor::build"] = timestamp(1e-7);
        funcs_["Thermo::temperature"] = timestamp(1e-7);
        funcs_["Thermo::energy"] = timestamp(1e-7);
        funcs_["Force::compute"] = timestamp(1e-7);
        funcs_["Thermo::pressure"] = timestamp(1e-7);
        funcs_["Thermo::compute"] = timestamp(1e-7);
        funcs_["Integrate::run"] = timestamp(1e-7);

      }
    libname_ = "minimdcomputelib" + id.to_string();

  }

  lib_compute_minimd::~lib_compute_minimd()
  {

  }

  void
  lib_compute_minimd::compute(std::string func,
      const sprockit::refcount_ptr<sprockit::ptr_type> &a1,
      const sprockit::refcount_ptr<sprockit::ptr_type> &a2, int index)
  {
    if (using_loops_)
      {
        if (!libloop_)
          {
            libloop_ = new sstmac::sw::lib_compute_loops(id_);
            register_lib(libloop_);
          }

        if (func == "create_atoms")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            minimd::in::ptr in = ptr_safe_cast(minimd::in, a2,
                 "lib_compute_minimd::Force::compute: problem casting input");

            double alat = pow((4.0 / in->rho), (1.0 / 3.0));
            int ilo = static_cast<int> (at->box.xlo / (0.5 * alat) - 1);
            int ihi = static_cast<int> (at->box.xhi / (0.5 * alat) + 1);
            int jlo = static_cast<int> (at->box.ylo / (0.5 * alat) - 1);
            int jhi = static_cast<int> (at->box.yhi / (0.5 * alat) + 1);
            int klo = static_cast<int> (at->box.zlo / (0.5 * alat) - 1);
            int khi = static_cast<int> (at->box.zhi / (0.5 * alat) + 1);

            ilo = std::max(ilo, 0);
            ihi = std::min(ihi, 2 * in->nx - 1);
            jlo = std::max(jlo, 0);
            jhi = std::min(jhi, 2 * in->ny - 1);
            klo = std::max(klo, 0);
            khi = std::min(khi, 2 * in->nz - 1);

            libloop_->compute_loop3(klo, khi, jlo, jhi, ilo, ihi, 12);
          }
        else if (func == "create_velocity")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            if (index == 0)
              {
                libloop_->compute_loop(0, at->nlocal, 3);
              }
            else
              {
                libloop_->compute_loop(0, at->nlocal, 12);
              }
          }
        else if (func == "Force::compute")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            minimd::neighbor::ptr ne = ptr_safe_cast(minimd::neighbor, a2,
                "lib_compute_minimd::Force::compute: problem casting neighbor");

            libloop_->compute_loop(0, at->nlocal + at->nghost, 3);
            libloop_->compute_loop(0, at->nlocal, 5);
            libloop_->compute_loop2(0, at->nlocal, 0, 10, 8);
          }
        else if (func == "Neighbor::build")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            libloop_->compute_loop(0, at->nlocal, 25);
          }
        else if (func == "Neighbor::setup")
          {
            libloop_->compute_loop(0, 1, 54);
          }
        else if (func == "Thermo::temperature")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            libloop_->compute_loop(0, at->nlocal, 4);
          }
        else if (func == "Thermo::energy")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            libloop_->compute_loop(0, at->nlocal, 16);
          }
        else if (func == "Thermo::pressure")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            libloop_->compute_loop(0, at->nlocal, 2);
          }
        else if (func == "Thermo::compute")
          {

            libloop_->compute_loop(0, 1, 14);
          }
        else if (func == "Integrate::run")
          {
            minimd::atom::ptr at = ptr_safe_cast(minimd::atom, a1,
                "lib_compute_minimd::Force::compute: problem casting atom");

            if (index == 1)
              {
                libloop_->compute_loop(0, at->nlocal, 3);
              }
            else
              {
                libloop_->compute_loop(0, at->nlocal, 6);
              }

          }
      }
    else
      {
        if (funcs_.find(func) == funcs_.end())
          {
            spkt_throw_printf(sprockit::library_error,
                "lib_compute_minimd: cannot find function %s",
                func.c_str());
          }
        lib_compute_time::compute(funcs_[func]);
      }
  }

  int
  lib_compute_minimd::get_int(const std::string &func, int index)
  {
    return 1;
  }

}
