/**
Copyright 2009-2022 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2022, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/
#include <sst_config.h>

#include <sst/core/simulation.h>

#include <sstmac/hardware/vtk/statoutputexodus.h>
#include <sst/core/statapi/statgroup.h>
#include <sstmac/hardware/vtk/stattraffic.h>
#include <sstmac/hardware/vtk/vtk_stats.h>
#include <sstmac/hardware/topology/topology.h>

namespace sstmac {
namespace hw {


StatisticOutputEXODUS::StatisticOutputEXODUS(SST::Params& outputParameters)
    : StatisticOutput (outputParameters)
{
    // Announce this output object's name
    Output &out = Simulation::getSimulationOutput();
    out.verbose(CALL_INFO, 1, 0, " : StatisticOutputEXODUS enabled...\n");
    setStatisticOutputName("StatisticOutputEXODUS");

    m_currentComponentName = "";
    m_firstEntry = false;
    m_processedAnyStats = false;
}

void StatisticOutputEXODUS::addOptionnalCallBack(std::function<void (const std::multimap<uint64_t, traffic_event> &, int, int)> callBack){
  std::cout << "StatisticOutputEXODUS::addOptionnalCallBack" << std::endl;
  callBackFct = callBack;
}

bool StatisticOutputEXODUS::checkOutputParameters()
{
    bool foundKey;
    std::string topHeaderFlag;
    std::string simTimeFlag;
    std::string rankFlag;

    // Review the output parameters and make sure they are correct, and
    // also setup internal variables

    // Look for Help Param
    getOutputParameters().find<std::string>("help", "1", foundKey);
    if (true == foundKey) {
        return false;
    }

    // Get the parameters
    m_FilePath = getOutputParameters().find<std::string>("filepath", "./vtkStatisticOutput.e");
    simTimeFlag = getOutputParameters().find<std::string>("outputsimtime", "1");
    rankFlag = getOutputParameters().find<std::string>("outputrank", "1");

    m_outputSimTime = ("1" == simTimeFlag);
    m_outputRank = ("1" == rankFlag);

    if (0 == m_FilePath.length()) {
        // Filepath is zero length
        return false;
    }

    return true;
}

void StatisticOutputEXODUS::printUsage()
{
    // Display how to use this output object
    Output out("", 0, 0, Output::STDOUT);
    out.output(" : Usage - Sends all statistic output to a Exodus File.\n");
    out.output(" : Parameters:\n");
    out.output(" : help = Force Statistic Output to display usage\n");
    out.output(" : filepath = <Path to .e file> - Default is ./StatisticOutput.e\n");
    out.output(" : outputsimtime = 0 | 1 - Output Simulation Time - Default is 1\n");
    out.output(" : outputrank = 0 | 1 - Output Rank - Default is 1\n");
}

void StatisticOutputEXODUS::startOfSimulation()
{
  std::cout << "StatisticOutputEXODUS::startOfSimulation"<< std::endl;

    // Open the finalized filename
//   if ( ! openFile() )
//        return;

}

void StatisticOutputEXODUS::endOfSimulation()
{
  std::cout << "StatisticOutputEXODUS::endOfSimulation"<< std::endl;
    // Close the file
    closeFile();
}

void StatisticOutputEXODUS::implStartOutputEntries(SST::Statistics::StatisticBase* statistic)
{
}

void StatisticOutputEXODUS::implStopOutputEntries()
{
}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), int32_t data)
{
}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), uint32_t data)
{

}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), int64_t data)
{

}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), uint64_t data)
{

}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), float data)
{
}

void StatisticOutputEXODUS::outputField(fieldHandle_t UNUSED(fieldHandle), double data)
{
}

void StatisticOutputEXODUS::outputField(fieldHandle_t fieldHandle, traffic_event data){
  traffic_progress_map_.insert({data.time_, data});
}

bool StatisticOutputEXODUS::openFile(void)
{
    return true;
}

void StatisticOutputEXODUS::closeFile(void)
{
}

void StatisticOutputEXODUS::printIndent() {

}

void StatisticOutputEXODUS::implStartRegisterGroup(SST::Statistics::StatisticGroup* group)
{
  m_statGroups.push_back(group);
}

void StatisticOutputEXODUS::implStopRegisterGroup()
{
}

void StatisticOutputEXODUS::implStartOutputGroup(SST::Statistics::StatisticGroup* UNUSED(group))
{
}

void StatisticOutputEXODUS::implStopOutputGroup()
{
  std::cout << "StatisticOutputEXODUS::implStopOutputGroup"<< std::endl;

  //DUMP FOR TRAFFIC
  std::cout << "StatisticOutputEXODUS::traffic_progress_map_ size  "<< traffic_progress_map_.size()<< std::endl;
  std::multimap<std::string, std::multimap<int, int>> tf_nodes_map;
  for (auto it = traffic_progress_map_.cbegin(); it != traffic_progress_map_.cend(); ++it){
    auto nodeId = it->second.id_;
    auto portId = it->second.p_;
    auto nodIdPortIdKey = std::to_string(nodeId) +":"+ std::to_string(portId);
    auto resIt = tf_nodes_map.find(nodIdPortIdKey);
    if(resIt == tf_nodes_map.cend()){
      auto map = std::multimap<int, int>{};
      map.insert({it->first, it->second.intensity_});

      tf_nodes_map.insert({nodIdPortIdKey, map});
    } else {
      auto &map = resIt->second;
      map.insert({it->first, it->second.intensity_});
    }
  }

  //  TORM: display the map in the console
     for (auto it = tf_nodes_map.cbegin(); it != tf_nodes_map.cend(); ++it){
       auto NodeId = it->first;
       const auto &map = it->second;
       std::cout<<NodeId<<":::";
       for(auto it = map.cbegin(); it != map.cend(); ++it){
         std::cout<< it->second << " ";
       }
       std::cout<<std::endl;
     }

  StatVTK::outputExodus(m_FilePath, traffic_progress_map_, Topology::global());
}

} //namespace Statistics
} //namespace SST
