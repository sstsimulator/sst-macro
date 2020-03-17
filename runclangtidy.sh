#! /bin/bash

$HOME/Software/llvm/llvm-project/clang-tools-extra/clang-tidy/tool/run-clang-tidy.py \
  -clang-tidy-binary $HOME/Software/llvm/bootstrap_installs/llvmHead-default/bin/clang-tidy \
  -clang-apply-replacements-binary /home/canlewi/Software/llvm/bootstrap_installs/llvmHead-default/bin/clang-apply-replacements \
  -checks="-*\
  , modernize-use-override \
  " \
  -j48 \
  -fix \
  -header-filter=.*macro.* sstmac/* bin/clang/* \
  2>&1 > clang-tidy-output.txt
