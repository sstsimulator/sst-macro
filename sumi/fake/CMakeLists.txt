set (fake_HEADERS
fake_transport.h
)

set (fake_SOURCES 
fake_transport.cc
)

include_directories( "${CMAKE_SOURCE_DIR}" )

if (CRAPPY_OLD_CMAKE)
add_library( sumi_fake SHARED ${fake_SOURCES} ${fake_HEADERS} )
else()
add_library( fake OBJECT ${fake_SOURCES} ${fake_HEADERS} )
endif()

install (FILES fake_transport.h DESTINATION include/fake)

