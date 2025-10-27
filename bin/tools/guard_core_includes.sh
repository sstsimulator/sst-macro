#!/bin/bash

# Define the preamble and postscript text
preamble="\\
#ifdef main\\
#define SSTMAC_SAVE_MAIN main\\
#undef main\\
#endif\\
"

postscript="#ifdef SSTMAC_SAVE_MAIN\\
#undef main\\
#define main SSTMAC_SAVE_MAIN\\
#undef SSTMAC_SAVE_MAIN\\
#endif\\
\\
"

# Use find to locate files and sed to modify them
find . -type f -exec sed -i '' "/#include.*sst\/core/i\\
$preamble" {} \;
find . -type f -exec sed -i '' "/#include.*sst\/core/a\\
$postscript"  {} \;
