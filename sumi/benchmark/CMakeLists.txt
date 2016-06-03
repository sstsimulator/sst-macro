
add_executable(collectives collectives.cc)
add_executable(pingpong pingpong.cc)

include_directories( "${CMAKE_SOURCE_DIR}" )

target_link_libraries(collectives sumi_api)
target_link_libraries(pingpong sumi_api)

