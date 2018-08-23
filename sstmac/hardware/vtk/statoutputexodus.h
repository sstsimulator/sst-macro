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

#ifndef SSTMAC_HW_VTK_STATISTICS_OUTPUT_EXODUS
#define SSTMAC_HW_VTK_STATISTICS_OUTPUT_EXODUS

#include "sst/core/sst_types.h"

#include <sst/core/statapi/statoutput.h>
#include <sstmac/hardware/vtk/vtk_stats.h>

namespace sstmac {
namespace hw {

class StatisticGroup;
/**
    \class StatisticOutputEXODUS

  The class for statistics output to a EXODUS formatted file
*/
class StatisticOutputEXODUS : public SST::Statistics::StatisticOutput
{
public:
    SST_ELI_REGISTER_STATISTIC_OUTPUT(StatisticOutputEXODUS, "statOutputEXODUS");
    /** Construct a StatOutputJSON
     * @param outputParameters - Parameters used for this Statistic Output
     */
    StatisticOutputEXODUS(SST::Params& outputParameters);

    void addOptionnalCallBack(std::function<void (const std::multimap<uint64_t, traffic_event> &, int, int)> callBack);

    bool acceptsGroups() const override { return true; }

    // Methods for Outputting Fields  (Called by Statistic Objects)
    // Output fields (will call virtual functions of Derived Output classes)
    // These aren't really part of a generic interface - optimization purposes only
    /** Output field data.
     * @param fieldHandle - The handle of the registered field.
     * @param data - The data to be output.
     */
    void outputField(fieldHandle_t fieldHandle, int32_t data) override;
    void outputField(fieldHandle_t fieldHandle, uint32_t data) override;
    void outputField(fieldHandle_t fieldHandle, int64_t data) override;
    void outputField(fieldHandle_t fieldHandle, uint64_t data) override;
    void outputField(fieldHandle_t fieldHandle, float data) override;
    void outputField(fieldHandle_t fieldHandle, double data) override;
    // specific method for this kind of output
    void outputField(fieldHandle_t fieldHandle, traffic_event data);

protected:
    /** Perform a check of provided parameters
     * @return True if all required parameters and options are acceptable
     */
    bool checkOutputParameters() override;

    /** Print out usage for this Statistic Output */
    void printUsage() override;

    /** Indicate to Statistic Output that simulation started.
     *  Statistic output may perform any startup code here as necessary.
     */
    void startOfSimulation() override;

    /** Indicate to Statistic Output that simulation ended.
     *  Statistic output may perform any shutdown code here as necessary.
     */
    void endOfSimulation() override;

    /** Implementation function for the start of output.
     * This will be called by the Statistic Processing Engine to indicate that
     * a Statistic is about to send data to the Statistic Output for processing.
     * @param statistic - Pointer to the statistic object than the output can
     * retrieve data from.
     */
    void implStartOutputEntries(SST::Statistics::StatisticBase* statistic) override;

    /** Implementation function for the end of output.
     * This will be called by the Statistic Processing Engine to indicate that
     * a Statistic is finished sending data to the Statistic Output for processing.
     * The Statistic Output can perform any output related functions here.
     */
    void implStopOutputEntries() override;


    void implStartRegisterGroup(SST::Statistics::StatisticGroup* group) override;
    void implStopRegisterGroup() override;
    void implStartOutputGroup(SST::Statistics::StatisticGroup* UNUSED(group)) override;
    void implStopOutputGroup() override;

    void printIndent();

protected:
    StatisticOutputEXODUS() {;} // For serialization

private:
    bool openFile();
    void closeFile();

private:
    FILE*                    m_hFile;
    std::string              m_FilePath;
    std::string              m_currentComponentName;
    std::string              m_currentStatisticName;
    std::string              m_currentStatisticSubId;
    std::string              m_currentStatisticType;
    bool                     m_outputSimTime;
    bool                     m_outputRank;
    bool					 m_firstEntry;
    bool					 m_firstField;
    bool 					 m_processedAnyStats;
    int						 m_curIndentLevel;

    std::vector<SST::Statistics::StatisticGroup *> m_statGroups;
    std::multimap<uint64_t, traffic_event> traffic_progress_map_;
    std::function<void (std::multimap<uint64_t, traffic_event>, int, int)> callBackFct;

};

} //namespace hw
} //namespace sstmac

#endif
