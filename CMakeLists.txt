cmake_minimum_required(VERSION 3.10)

project(stringEval VERSION 0.1
		DESCRIPTION "A string evaluator"
		LANGUAGES Fortran)
enable_language(Fortran)

add_executable(eval src/main.f90 src/f90getopt.F90)
