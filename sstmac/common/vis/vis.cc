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
#include <sstmac/common/vis/vis.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::vis::vis_engine);

namespace sstmac {
  namespace vis {

    double vis_obj::clear_r;
    double vis_obj::clear_g;
    double vis_obj::clear_b;
    bool vis_obj::clear_set = false;

    void
    vis_comp::vis_set_activity(double val, timestamp now)
    {
      if (vobj_ && veng_) {
	vobj_->set_activity(val);
	// veng_->update(now);
      }
    }

    void
    vis_display::init_factory_params(sprockit::sim_parameters* params)
    {
      /**
	 sstkeyword {
	 docstring=Visualization engine to be used in generating the display.
	 This is generally used for visualizing network activity.;
	 }
      */
      eng_ = vis_engine_factory::get_extra_param("vis_engine", params);
      /**
	 sstkeyword {
	 docstring=Only used in fancy VTK visualizations. Ignore for now;
	 }
      */
      file_interval_ = params->get_optional_time_param("vis_file_interval", 0);
    }

    void
    vis_display::vis_start(bool save_viz)
    {
      if (eng_)
	{
	  eng_->init();
	  
	  vis_topology::vis_switch_map vismap = vis_nodes();
      vis_topology* vistop = vis_topol();
	  
	  vistop->display_nodes(vismap, eng_, objs_);
	  
	  eng_->display_label(name_);
	  eng_->draw();
	  
	  if (save_viz)
	    {
	      // Write beginning out to file
	      eng_->write_to_file(objs_, "interconnect-start.vtp");
	    }
	}
    }

    void
    vis_display::vis_update(timestamp now)
    {
      if (eng_)
	{
	  //eng_->update_label(now);
	  eng_->update(now);
	  if (file_interval_.sec() > 0 && now >= last_file_ + file_interval_)
	    {
	      std::stringstream ss;
	      ss << name_ << "-" << filenum_ << ".vtp";
	      eng_->write_to_file(objs_, ss.str());
	      last_file_ = now;
	      filenum_++;
	    }
	}
    }
  }
}
