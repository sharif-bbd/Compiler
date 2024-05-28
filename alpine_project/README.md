# Alpine Project

Bouabid Sharif and Lieberherr Lorin (group 081).

## New files
- `alpine_project/src/main/scala/alpine/codegen/CPrinter.scala` which is used for the C transpilation.
- Inside the `Cgen` folder, you will find a MakeFile project to be able to compile the generated C code
- `alpine_project/src/test/res/transpiler/unique_test.al` which you can use to write the alpine syntax do transpile

## Modified files
The following files have been modified for the project :
- `alpine_project/src/main/scala/alpine/driver/Driver.scala`
- `alpine_project/src/main/scala/alpine/ast/Trees.scala`
- `alpine_project/src/main/scala/alpine/parsing/Parser.scala`
- `alpine_project/src/main/scala/alpine/typing/Typer.scala`
- `alpine_project/src/test/scala/alpine/parsing/ParserTests.scala`
- `alpine_project/src/test/scala/alpine/typing/TyperTests.scala`<br>

You can see more details on our github repository.

## How to transpile :
- Write the alpine syntax in `alpine_project/src/test/res/transpiler/test_cases.al` (Only the alpine syntax).
- Inside a sbt shell, write `run ./src/test/res/transpiler/unique_test.al`. 
This will modify the `output.c` file from the`Cgen` folder. 
- Then, you can go inside the `Cgen` folder and run in a terminal : `make` to compile the C code. Of course,
you will need `gcc` to be able to compile the C code.

Note : The goal of our project was to support the syntax from `./src/test/res/transpiler/test_cases.al`. There is some tests that are not handled by our transpiler,
we have marked them with `-- not implemented`. You will see that there is additional syntax for the methods at the end of the `test_cases.al` file.