# Assignment Checkers

A checker is a script which takes an assignment and student's solution, and produces a response consisting of

* an indication whether the solution passed the tests;
* a feedback for the student;
* and optionally points for sub-assignments, if there are any (using the *extended interface*).

Currently, the sub-assignments are not available in the IS MU mode (due to limitations on the IS side).

The checker to be executed for given course is set by the [main configuration file][mainconf].
When the evaluator receives a solution of an assignment, it will execute the checker with the command from the configuration and following additional options:

- the path to the question file (which is copied from the [question repository][qdir]) (first positional argument);
- the path to the file containing student's answer (second positional argument);
- `-I<PATH>` arguments for each path in which testing-related modules for the course should be searched in (currently only the [question repository][qdir]);
- `-o<OPT>` if the question ID contains an [additional option][qopt], it will be passed in using this argument;
- `--hint` if the checker should run in a [hint mode][concepts-hint] (e.g., a before-submission syntax check);
- `-p<FILE_DESCRIPTOR_NUMBER>` if the evaluator should run in the [extended mode](#extended-interface) (used for sending points information and finer control of the evaluation) this argument is used to indicate extended mode is used and to pass to the checker the file descriptor to which the checker can send information during the evaluation.

## Basic Checker Interface

With the basic interface, the following is used to determine the result of evaluation:

- the exit code of the checker indicates if the test passed – exit code 0 means the test had passed, all other exit codes indicate test failure;
- anything written to the standard output of the checker is sent to the student;
- anything written to the standard error of the checker is sent to the evaluator log.

If hint mode is allowed by the [main configuration][mainconf], and the `--hint` argument for checker is used, the checker should respect it and modify its behaviour accordingly.
More can be found in [concepts → hint mode][concepts-hint].

## Extended Interface

Extended interface allows more communication between the checker and the evaluator.
It still preserves the communication described by the basic interface, but adds to it.
Currently, it is possible to set points for sub-assignments in this way.

The interface works by sending JSON elements over the file extended mode descriptor (the file descriptor number is send to the checker using the `-p` argument).
Each sent element should be a JSON map at the top level – we will call this element a *packet*.
It is not possible to combine several packets into one, even if they have different types with disjoined keys.

### Supported Packets

* **Points** – packet format is `{ points = <N>, out_of = <N>, comment = <STRING> }` where `<N>` is a (possibly floating-point) number, and `<STRING>` is a string.
    The `comment` field should contain information about this sub-assignment.

[mainconf]: main_configuration.md
[qdir]: questions.md#question-repository
[concepts-hint]: concepts.md#hint-mode
[qopt]: questions.md#options
