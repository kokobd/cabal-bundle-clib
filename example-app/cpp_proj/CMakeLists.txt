cmake_minimum_required(VERSION 3.13)
project(cpp_proj)

set(CMAKE_CXX_STANDARD 17)

add_library(mycpplib src/library.cpp src/library.h)

set_property(TARGET mycpplib PROPERTY POSITION_INDEPENDENT_CODE ON)
