add_executable(scratch scratch.cpp)
target_link_libraries(scratch
  PRIVATE
    interprocess
)