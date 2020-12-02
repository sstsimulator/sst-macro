# -*- coding: utf-8 -*-

from sst_unittest import *
from sst_unittest_support import *

################################################################################
# Code to support a single instance module initialize, must be called setUp method

module_init = 0
module_sema = threading.Semaphore()

def initializeTestModule_SingleInstance(class_inst):
    global module_init
    global module_sema

    module_sema.acquire()
    if module_init != 1:
        # Put your single instance Init Code Here
        module_init = 1
    module_sema.release()

################################################################################

class testcase_sst_macro(SSTTestCase):

    def initializeClass(self, testName):
        super(type(self), self).initializeClass(testName)
        # Put test based setup code here. it is called before testing starts
        # NOTE: This method is called once for every test

    def setUp(self):
        super(type(self), self).setUp()
        initializeTestModule_SingleInstance(self)
        # Put test based setup code here. it is called once before every test

    def tearDown(self):
        # Put test based teardown code here. it is called once after every test
        super(type(self), self).tearDown()

#####

    def test_sst_macro_make_check(self):
        self.sst_macro_test_template("check")

    def test_sst_macro_make_installcheck(self):
        self.sst_macro_test_template("installcheck")


#####

    def sst_macro_test_template(self, testcase, testtimeout = 60):
        # Get the path to the test files
        test_path = self.get_testsuite_dir()
        outdir = self.get_test_output_run_dir()
        tmpdir = self.get_test_output_tmp_dir()

        MacroElementDir = os.path.abspath("{0}/../".format(test_path))

        # Set the various file paths
        testDataFileName="test_sst_macro_{0}".format(testcase)
        outfile = "{0}/{1}.out".format(outdir, testDataFileName)
        errfile = "{0}/{1}.err".format(outdir, testDataFileName)

        # Launch SST-Macro Test
        oscmd = "make {0}".format(testcase)
        rtn = OSCommand(oscmd, output_file_path = outfile,
                        error_file_path = errfile,
                        set_cwd = MacroElementDir).run(timeout_sec=testtimeout)

        # Look for runtime error conditions
        err_str = "SST-Macro Timed-Out ({0} secs) while running {1}".format(testtimeout, oscmd)
        self.assertFalse(rtn.timeout(), err_str)
        err_str = "SST-Macro returned {0}; while running {1}".format(rtn.result(), oscmd)
        self.assertEqual(rtn.result(), 0, err_str)

