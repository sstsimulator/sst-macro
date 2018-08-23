// Copyright 2009-2018 NTESS. Under the terms
// of Contract DE-NA0003525 with NTESS, the U.S.
// Government retains certain rights in this software.
//
// Copyright (c) 2009-2018, NTESS
// All rights reserved.
//
// This file is part of the SST software package. For license
// information, see the LICENSE file in the top level directory of the
// distribution.

#include <sst_config.h>

#include <sst/core/simulation.h>

#include <sstmac/hardware/vtk/statoutputexodus.h>
#include <sst/core/statapi/statgroup.h>
#include <sstmac/hardware/vtk/stattraffic.h>
#include <sstmac/hardware/vtk/vtk_stats.h>

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
    m_FilePath = getOutputParameters().find<std::string>("filepath", "./StatisticOutput.e");
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
  std::cout << "StatisticOutputEXODUS::implStartOutputEntries"<< std::endl;
}

void StatisticOutputEXODUS::implStopOutputEntries()
{
  std::cout << "StatisticOutputEXODUS::implStopOutputEntries"<< std::endl;
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
  std::cout << "StatisticOutputEXODUS::implStartRegisterGroup"<< std::endl;
  m_statGroups.push_back(group);
}

void StatisticOutputEXODUS::implStopRegisterGroup()
{
  std::cout << "StatisticOutputEXODUS::implStopRegisterGroup"<< std::endl;
}

void StatisticOutputEXODUS::implStartOutputGroup(SST::Statistics::StatisticGroup* UNUSED(group))
{
  std::cout << "StatisticOutputEXODUS::implStartOutputGroup"<< std::endl;
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
       auto node_id = it->first;
       const auto &map = it->second;
       std::cout<<node_id<<":::";
       for(auto it = map.cbegin(); it != map.cend(); ++it){
         std::cout<< it->second << " ";
       }
       std::cout<<std::endl;
     }

  int count_x = std::atoi(getOutputParameters().find<std::string>("count_x", "1").c_str());
  int count_y = std::atoi(getOutputParameters().find<std::string>("count_y", "1").c_str());

  stat_vtk::outputExodus(traffic_progress_map_, count_x, count_y);
}

} //namespace Statistics
} //namespace SST
