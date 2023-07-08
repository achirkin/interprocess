# # scratchpad
# add_executable(scratch scratch.cpp)
# target_link_libraries(scratch
#   PRIVATE
#     interprocess
# )

# gtest
include(FetchContent)

FetchContent_Declare(googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.13.0)

# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(test
  resource.cpp
)

target_link_libraries(test
  PRIVATE
    interprocess
    GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(test)