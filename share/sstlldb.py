import lldb
import commands
import optparse
import shlex

def start(debugger, command, result, internal_dict):
  debugger.HandleCommand("pro handle SIGUSR1 -p true -s false -n false")
  debugger.HandleCommand("br set -b sst_gdb_swap")
  debugger.HandleCommand("br com add -o bt")
  newCmd = "expr  --ignore-breakpoints false -- sst_gdb_set_active(1)"
  debugger.HandleCommand(newCmd)

def select(debugger, command, result, internal_dict):
  newCmd = "expr  --ignore-breakpoints false -- sst_gdb_select_rank(%s)" % command
  debugger.HandleCommand(newCmd)

# And the initialization code to add your commands 
def __lldb_init_module(debugger, internal_dict):
  debugger.HandleCommand('command script add -f sst.start sst_start')
  debugger.HandleCommand('command script add -f sst.select sst_select')
  debugger.HandleCommand('br set -b main')
  print('The sst commands have been installed and are ready for use')

