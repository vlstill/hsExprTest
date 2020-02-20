# Adding Checkers

A checker is a script which takes an assignment and student's solution, and produces a response consisting of

* an indication whether the solution passed the tests;
* a feedback for students;
* and optionally points for sub-assignments, if there are any (using the *extended interface*).

Currently, the sub-assignments are not available in the IS MU mode (due to limitations on the IS side).

The checker to be executed for given course is set by the [main configuration][mainconf].
The, when the evaluator receives the assignment solution, it will execute the checker with the command from the configuration and following additional options:

- the path to the question file (which is copied from the [question repository][qdir]) (first positional argument);
- the path to the file containing student's answer (second positional argument);
- `-I<PATH>` arguments for each path in which testing-related modules for the course should be searched in (currently only the [question repository][qdir];
- `-o<OPT>` if the question ID contains an [additional option][qopt], it will be passed in using this argument;
- `--hint` if the checker should run in a [hint mode](#hint-mode) (e.g., a before-submission syntax check);
- `-p<FILE_DESCRIPTOR_NUMBER>` if the evaluator should run in the [extended mode](#extended-interface) (used for sending points information and finer control of the evaluation).

## Basic Checker Interface {#basic-interface}

With the basic interface, the following is used to determine the result of evaluation:

- the exit code of the checker indicates if the test passed -- exitcode 0 means it passed, all other exit codes indicate test failure;
- anything written to the standard output of the checker is sent to the student;
- anything written to the standard error of the checker is sent to the evaluator log.

If hint mode is allowed by the [main configuration][mainconf], the checker should respect it and modify its behaviour accordingly.

## Extended Interface {#extended-interface}

[mainconf]: main_configuration.md
[qdir]: question_repository.md
