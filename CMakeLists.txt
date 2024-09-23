cmake_minimum_required(VERSION 3.10)

project(stringEval VERSION 0.1
		DESCRIPTION "A string evaluator"
		LANGUAGES Fortran)
enable_language(Fortran)

<<<<<<< HEAD
add_executable(eval src/main.f90 src/f90getopt.F90)
=======
add_executable(eval src/main.f90 src/eval.f90 src/f90getopt.F90)
>>>>>>> a66507e90e45a4e86aaf8ac6402333061c2aa42e
