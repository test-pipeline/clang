set( LLVM_LINK_COMPONENTS support mc)

add_library(SMTConstraintManagerPlugin MODULE
 SMTConstraintManager.cpp)

set_target_properties(SMTConstraintManagerPlugin
  PROPERTIES
  LINKER_LANGUAGE CXX
  PREFIX "")

set (CMAKE_CXX_FLAGS "-std=c++11 -fPIC -pedantic -fno-common -Wcast-qual -fno-strict-aliasing -Wno-long-long -Wall -Wno-unused-parameter -Wwrite-strings -fno-rtti")

set (CMAKE_MODULE_LINKER_FLAGS "-L/usr/local/lib -Wl,--whole-archive -lstp -Wl,--whole-archive -lminisat")
