cmake_minimum_required(VERSION 3.17)
project(interprocess LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(interprocess
  STATIC
    src/interprocess/shared_object_name.cpp
)
if (WIN32)
target_sources(interprocess
  PRIVATE
)
else()
target_sources(interprocess
  PRIVATE
    src/interprocess/posix/blob.cpp
    src/interprocess/posix/growing_blob.cpp
    src/interprocess/posix/memory_map.cpp
    src/interprocess/posix/storage_descriptor.cpp
)
endif()

# Need rt library for shm_open/shm_unlink on linux
if (UNIX AND NOT APPLE)
  target_link_libraries(interprocess PRIVATE rt)
endif ()

target_include_directories(interprocess PUBLIC include/)
target_compile_options(interprocess
  PUBLIC
    -fno-exceptions
    -fno-rtti
    -Wall
    -Wextra
    -Wpedantic
    -Werror
    # -DINTERPROCESS_DEBUG
)


add_subdirectory(scratch)
add_subdirectory(test)
