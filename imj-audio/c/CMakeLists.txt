cmake_minimum_required(VERSION 3.18)

project(test_wrapper)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)


find_package(PkgConfig REQUIRED)
# on windows the name of the package might be 'portaudio' instead
pkg_check_modules(PA REQUIRED portaudio-2.0)


add_executable(tw
  library.cpp
  memory.cpp
  extras.cpp
  wrapper.cpp
  test_wrapper.cpp)

target_link_libraries(tw
    ${PA_LINK_LIBRARIES}
    "-framework Accelerate"
)

#target_link_directories(/usr/local/Cellar/portaudio/19.6.0/lib)
target_include_directories(tw PUBLIC ${PA_INCLUDE_DIRS})
target_compile_options(tw PUBLIC ${PA_CFLAGS_OTHER})
