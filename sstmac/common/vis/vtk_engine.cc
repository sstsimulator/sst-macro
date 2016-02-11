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

#include <sstream>

#include <sstmac/common/vis/vtk_engine.h>

#include <vtkAppendPolyData.h>
#include <vtkCellData.h>
#include <vtkCubeSource.h>
#include <vtkCylinderSource.h>
#include <vtkLineSource.h>
#include <vtkMatrixToLinearTransform.h>
#include <vtkPNGWriter.h>
#include <vtkPointData.h>
#include <vtkPolyDataMapper.h>
#include <vtkPolyDataWriter.h>
#include <vtkProperty.h>
#include <vtkProperty2D.h>
#include <vtkSmartPointer.h>
#include <vtkTransform.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkWindowToImageFilter.h>

namespace sstmac {
  namespace vis {
    SpktRegister("vtk", vis_engine, vtk_engine,
		"Implements a network visualization tool for linking with VTK");

    vtk_obj::vtk_obj(vtkPolyDataAlgorithm* al, vtkActor* ac, vtkMapper* ma) :
      alg_(al), act_(ac), map_(ma)
    {

    }

    void
    vtk_obj::set_activity(double val)
    {
      double r = 0.1;
      double g = 0.1;
      double b = 0.1;

      if (val < 0.2)
	{
	  b = std::max(0.1, (val) / 0.2);
	}
      else if (val < 0.4)
	{
	  g = std::max(0.1, (val - 0.2) / 0.2);
	  b = 1.0;
	}
      else if (val < 0.6)
	{
	  g = 1.0;
	  b = std::max(0.1, 1.0 - (val - 0.4) / 0.2);
	}
      else if (val < 0.8)
	{
	  r = std::max(0.1, (val - 0.6) / 0.2);
	  g = 1.0;
	}
      else
	{
	  r = 1.0;
	  g = std::max(0.1, 1.0 - (val - 0.8) / 0.2);
	}

      act_->GetProperty()->SetColor(r, g, b);
      act_->GetProperty()->SetOpacity(0.5);
    }

    void
    vtk_obj::rotate(VIS_DIR dim, int degrees)
    {
      switch (dim)
        {
        case VIS_X:
          act_->RotateX(degrees);
          break;
        case VIS_Y:
          act_->RotateY(degrees);
          break;
        case VIS_Z:
          act_->RotateZ(degrees);
          break;
        default:
          return;
        } // switch (dim)
    }

    vtk_engine::vtk_engine() :
      hasTurned_(false), lastupdate_(timestamp(0))
    {
    }

    void
    vtk_engine::init_factory_params(sprockit::sim_parameters* params)
    {
      update_ = params->get_optional_time_param("vis_update", timestamp(0.001));
      start_ = params->get_optional_time_param("vis_start", timestamp(0.000));
    }

    void
    vtk_engine::init()
    {
      // Finally we create the render window which will show up on the screen.
      rend_ = vtkRenderer::New();
      if (!rend_)
	{
	  spkt_throw_printf(sprockit::spkt_error, "vtk_engine - could not create vtkRenderer");
	}
      rend_->SetBackground(1.0, 1.0, 1.0);

      renWin_ = vtkRenderWindow::New();
      renWin_->AddRenderer(rend_);
      renWin_->SetSize(900, 900);
    }

    void
    vtk_engine::draw()
    {
      renWin_->Render();

      if (!hasTurned_) 
        {
          rend_->GetActiveCamera()->Azimuth(30.);
          rend_->ResetCameraClippingRange();
          hasTurned_ = true;
          renWin_->Render();
        }
    }

    void
    vtk_engine::draw(timestamp now)
    {
      renWin_->Render();

      // Create image of window
      vtkSmartPointer<vtkWindowToImageFilter> win_image = vtkSmartPointer<vtkWindowToImageFilter>::New();
      win_image->SetInput(renWin_);

      // Create image name
      std::ostringstream filename;
      filename << "SST-"
	       << std::fixed << std::setprecision(15) 
	       << now.sec()
	       << ".png";

      // Create writer
      vtkSmartPointer<vtkPNGWriter> writer = vtkSmartPointer<vtkPNGWriter>::New();
      writer->SetInputConnection(win_image->GetOutputPort());
      writer->SetFileName(filename.str().c_str());
      writer->Write();

      if (!hasTurned_) 
	{
	  rend_->GetActiveCamera()->Azimuth(30.);
	  rend_->ResetCameraClippingRange();
	  hasTurned_ = true;
	  renWin_->Render();
	}
    }

    vis_obj*
    vtk_engine::create_cube(double x, double y, double z, double len, double degrees, int dir)
    {
      vtkSmartPointer<vtkCubeSource> cube = vtkSmartPointer<vtkCubeSource>::New();
      cube->SetBounds(x, x + len, y, y + len, z, z + len);

      // Create mapper
      vtkSmartPointer<vtkPolyDataMapper> cubeMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
      cubeMapper->SetInputConnection(cube->GetOutputPort());

      // Create actor
      vtkSmartPointer<vtkActor> cubeActor = vtkSmartPointer<vtkActor>::New();
      cubeActor->SetMapper(cubeMapper);
      cubeActor->GetProperty()->SetOpacity(0.5);
      cubeActor->SetOrigin(cube->GetCenter());
      switch (dir)
	{
	case VIS_X:
	  cubeActor->RotateWXYZ(degrees, 1, 0, 0);
	  break;
	case VIS_Y:
	  cubeActor->RotateWXYZ(degrees, 0, 1, 0);
	  break;
	case VIS_Z:
	  cubeActor->RotateWXYZ(degrees, 0, 0, 1);
	  break;
	} // switch (dir)

      rend_->AddActor(cubeActor);

      vtk_obj* o = vtk_obj::construct(cube, cubeActor, cubeMapper);
      //objs_.push_back(o);

      if (!vis_obj::clear_set)
	{
	  cubeActor->GetProperty()->GetColor(vis_obj::clear_r, vis_obj::clear_g, vis_obj::clear_b);
	  vis_obj::clear_set = true;
	}

      return o;
    }

    vis_obj*
    vtk_engine::create_cylinder(double x, double y, double z, double rad,
				double len, int dir, int degrees)
    {
      vtkSmartPointer<vtkCylinderSource> cube =	vtkSmartPointer<vtkCylinderSource>::New();
      cube->SetCenter(x, y, z);
      cube->SetRadius(rad);
      cube->SetHeight(len);
      cube->CappingOn();

      //create mapper
      vtkSmartPointer<vtkPolyDataMapper> cubeMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
      cubeMapper->SetInputConnection(cube->GetOutputPort());

      //create actor
      vtkSmartPointer<vtkActor> cubeActor = vtkSmartPointer<vtkActor>::New();
      cubeActor->SetMapper(cubeMapper);
      cubeActor->GetProperty()->SetOpacity(0.5);
      cubeActor->SetOrigin(cube->GetCenter());
      switch (dir)
	{
	case VIS_X:
	  cubeActor->RotateWXYZ(degrees, 1, 0, 0);
	  break;
	case VIS_Y:
	  cubeActor->RotateWXYZ(degrees, 0, 1, 0);
	  break;
	case VIS_Z:
	  cubeActor->RotateWXYZ(degrees, 0, 0, 1);
	  break;
	} // switch (dir)
      
      rend_->AddActor(cubeActor);

      vtk_obj* o = vtk_obj::construct(cube, cubeActor, cubeMapper);
      //objs_.push_back(o);

      if (!vis_obj::clear_set)
	{
	  cubeActor->GetProperty()->GetColor(vis_obj::clear_r, vis_obj::clear_g, vis_obj::clear_b);
	  vis_obj::clear_set = true;
	}

      return o;
    }

    vis_obj*
    vtk_engine::create_line(double p1[3], double p2[3])
    {
      vtkSmartPointer<vtkLineSource> line = vtkSmartPointer<vtkLineSource>::New();
      line->SetPoint1(p1);
      line->SetPoint2(p2);

      //create mapper
      vtkSmartPointer<vtkPolyDataMapper> cubeMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
      cubeMapper->SetInputConnection(line->GetOutputPort());

      //create actor
      vtkSmartPointer<vtkActor> cubeActor = vtkSmartPointer<vtkActor>::New();
      cubeActor->SetMapper(cubeMapper);
      cubeActor->GetProperty()->SetOpacity(0.5);

      vtk_obj* o = vtk_obj::construct(line, cubeActor, cubeMapper);

      if (!vis_obj::clear_set)
	{
	  cubeActor->GetProperty()->GetColor(vis_obj::clear_r, vis_obj::clear_g, vis_obj::clear_b);
	  vis_obj::clear_set = true;
	}

      return o;
    }

    void
    vtk_engine::write_to_file(const std::list<vis_obj*> &objs, std::string fname)
    {
      vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
      writer->SetFileName(fname.c_str());

      vtkSmartPointer<vtkAppendPolyData> app = vtkSmartPointer<vtkAppendPolyData>::New();
      vtkSmartPointer<vtkMatrixToLinearTransform> mtl =	vtkSmartPointer<vtkMatrixToLinearTransform>::New();

      std::list<vis_obj*>::const_iterator it, end = objs.end();
      for (it = objs.begin(); it != end; it++)
	{
	  vtk_obj* vob = ptr_safe_cast<vtk_obj>(*it);
	  vtkPolyDataAlgorithm* a = vob->get_alg();
	  a->Update();
	  vtkActor* act = vob->get_act();
	  vtkPolyData* pdata = a->GetOutput();
	  vtkPoints* p = pdata->GetPoints();
	  vtkMatrix4x4* mat = act->GetMatrix();
	  mtl->SetInput(mat);
	  vtkSmartPointer<vtkPoints> pout = vtkSmartPointer<vtkPoints>::New();
	  
	  mtl->TransformPoints(p, pout);
	  vtkSmartPointer<vtkPolyData> pd = vtkSmartPointer<vtkPolyData>::New();
	  pd->DeepCopy(pdata);
	  pd->SetPoints(pout);
	  
	  vtkSmartPointer<vtkUnsignedCharArray> colors = vtkSmartPointer<vtkUnsignedCharArray>::New();
	  colors->SetNumberOfComponents(4);
	  colors->SetName("Colors");
	  
	  double r, g, b;
	  act->GetProperty()->GetColor(r, g, b);
	  
	  // The color vector
	  unsigned char col[4] = { r * 255, g * 255, b * 255, 255 };
	  
	  // Add 6 of them for the 6 sizes of the polygon
	  colors->InsertNextTupleValue(col);
	  colors->InsertNextTupleValue(col);
	  colors->InsertNextTupleValue(col);
	  colors->InsertNextTupleValue(col);
	  colors->InsertNextTupleValue(col);
	  colors->InsertNextTupleValue(col);
	  
	  pd->GetCellData()->SetScalars(colors);
	  
	  app->AddInputData(pd);
	}

      writer->SetInputConnection(app->GetOutputPort());
      writer->Write();
    }

    void
    vtk_engine::display_label(std::string text)
    {
      labeltext_ = text;
      label_ = vtkTextActor::New();
      label_->SetDisplayPosition(1, 1);
      label_->SetHeight(1);
      label_->GetProperty()->SetColor(0., 0., 1.);
      update_label(timestamp(-1));
      rend_->AddActor(label_);
    }

    void
    vtk_engine::update_label(timestamp now)
    {
      std::stringstream ss;
      ss << labeltext_ << " -- (t=" << now.sec() << ")\n";
      label_->SetInput(ss.str().c_str());
    }

    void
    vtk_engine::update(timestamp now)
    {
      update_label(now);
      if (now >= start_ && now >= lastupdate_ + update_)
	{
	draw(now);

	lastupdate_ = now;
      }
    }
  }
}

