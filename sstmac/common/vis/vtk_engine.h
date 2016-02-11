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

#ifndef SSTMAC_COMMON_VIS_VIS_VTK_H_INCLUDED
#define SSTMAC_COMMON_VIS_VIS_VTK_H_INCLUDED

#include <sstmac/common/vis/vis.h>

//#include <vtkAutoInit.h>
// VTK_MODULE_INIT(vtkRenderingOpenGL);

#define vtkRenderingCore_AUTOINIT 4(vtkInteractionStyle,vtkRenderingFreeType,vtkRenderingFreeTypeOpenGL,vtkRenderingOpenGL)
#define vtkRenderingVolume_AUTOINIT 1(vtkRenderingVolumeOpenGL)

#include <vtkRenderWindow.h>
#include <vtkCamera.h>
#include <vtkActor.h>
#include <vtkRenderer.h>
#include <vtkMapper.h>
#include <vtkPolyDataAlgorithm.h>
#include <vtkTextActor.h>

namespace sstmac {
namespace vis {

class vtk_obj : public vis_obj
{
 public:

  static ptr
  construct(vtkPolyDataAlgorithm* al, vtkActor* ac, vtkMapper* ma) {
    return ptr(new vtk_obj(al, ac, ma));
  }

  virtual void
  set_activity(double val);

  vtkPolyDataAlgorithm*
  get_alg() {
    return alg_;
  }

  vtkActor*
  get_act() {
    return act_;
  }

  virtual void
  rotate(VIS_DIR dim, int degrees);

 protected:

  vtkPolyDataAlgorithm* alg_;
  vtkActor* act_;
  vtkMapper* map_;


  vtk_obj(vtkPolyDataAlgorithm* al, vtkActor* ac, vtkMapper* ma);

  virtual
  ~vtk_obj() {
    alg_->Delete();
    act_->Delete();
    map_->Delete();
  }

  virtual std::string
  to_string() const {
    return "vtk_obj";
  }
};

class vtk_engine : public vis_engine
{
 public:

  static ptr
  construct(sprockit::sim_parameters* params) {
    return ptr(new vtk_engine());
  }

  virtual void
  init();

  virtual void
  draw();

  virtual void
  draw(timestamp now);

  virtual void
  update(timestamp now);

  virtual void
  update_label(timestamp now);

  virtual vis_obj*
  create_cube(double x, double y, double z, double len, double degrees = 0,
              int dir = 0);

  virtual vis_obj*
  create_cylinder(double x, double y, double z, double rad, double len,
                  int dir, int degrees = 90);

  virtual vis_obj*
  create_line(double p1[3], double p2[3]);

  void
  write_to_file(const std::list<vis_obj*> &objs, std::string fname);

  virtual void
  display_label(std::string text);

  void
  init_factory_params(sprockit::sim_parameters* params);

 protected:

  vtk_engine();

  virtual
  ~vtk_engine() {
    rend_->Delete();
    renWin_->Delete();
  }

  virtual std::string
  to_string() const {
    return "vtk_engine";
  }

  vtkRenderer *rend_;
  vtkRenderWindow *renWin_;

  std::list<vtk_obj*> objs_;

  bool hasTurned_;

  timestamp update_;
  timestamp lastupdate_;
  timestamp start_;

  vtkTextActor* label_;
  std::string labeltext_;
};

}
}

#endif

