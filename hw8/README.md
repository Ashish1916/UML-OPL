# Assignment 8

* Download and uncompress hw8.zip.
* The directory hw8 has  the following files:
   * ``hw8.pdf`` : Describes the homework problems.
   * ``main.tex`` : Use this template to get started with your homework.
   * ``*.sty`` : Helper files necessary to compile main.tex
   * ``lam`` : Helper .ml files discussed below for the typechecker.
* Upload your (single) pdf for question (1) in Gradescope under assignment "HW8 Problems"
* Upload your ``check.ml`` file (do not change the file name) in gradescope under assignment "HW8 Programming"
* Due Date: December 3rd, 2024 at 11:59pm
* Late submission with penalty:

    * You can use 3 additional days with penalty to submit your homework.
    * Each late day will incur a penalty of 5 points from your score.
    * After 3 days, i.e., after Dec 6th, 2024, you can no longer submit. The submission window closes.


Description of Files:
---------------------
  - ast.ml    
      Defines the datatypes for the abstract syntax trees (ASTs) and values.

  - eval.ml
      The interpreter for the ASTs. 

  - check.ml
      A typechecker for the ASTs. You will edit this and only this
      file for Question 2.

  - poly.ml
      A typechecker for the ASTs that supports let-polymorphism. This is
      not implemented. We may revisit this at some point in the future.

  - helper.ml
      Functions that should be helpful for implementing your solutions.

  - lexer.mll
  - parser.mly
      A lexer and parser for the source language.

  - pprint.ml
      A pretty printer for the ASTs.

  - main.ml
      The top level code that parses in an input file and runs the
      interpreter/typechecker.

  - examples
      Some test programs in the source language. You should really try
      your solution out on all of these examples, and also come up
      with some of your own. There's even a little script (test.sh) to 
      help you try your solution against some test cases. The script will
      say that your test case fails if it doesn't exactly match the
      "gold standard" output. This may be due to different variable
      names and different order of constraints, and your implementation
      may still be correct.

How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. Successful compilation will produce an executable file called "lam".

How to run:
-----------

./lam <filename>
  - attempts to type check and then evaluate the program in <file>

Note: Ignore other options from the usage. They are meant for grading.   
