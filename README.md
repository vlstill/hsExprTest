ExprTest
==========

[![Build Status](https://travis-ci.org/vlstill/hsExprTest.svg?branch=master)](https://travis-ci.org/vlstill/hsExprTest)

Automatic testing of small programming assignments.

This project consists of two parts, an programming-language-agnostic driver for
testing which invokes teacher-provided script on student's solution and a tool
for comparison of Haskell expressions and types based on QuickCheck.

This tool was initially used in the [Non-Imperative
Programming](https://is.muni.cz/predmet/fi/podzim2020/IB015?lang=en) course on
[Faculty of Informatics, Masaryk University (FI MU)](https://www.fi.muni.cz) and
is now used in a few courses on the same faculty (with different-teacher
provided components).  It can be used either locally (but that is not very
useful) or connected to a testing frontend, such as the frontend of [IS
MU](https://is.muni.cz).  In either case, testing requires [assignment
files](#assignment-files).

The work began with a bachelor thesis of Martin Jonáš on FI MU, the code was
later extended by Vladimír Štill and is used in the course from autumn 2014.
Since autumn 2018, a complete rewrite (version 3) of Haskell testing framework
is used.

## Evaluators / Language Backends

There is support for multiple evaluators. The original & residing in this
repository is Haskell evaluator: [hsExprTest](/src/hs/)
([documentation](/src/hs/README.md)).

Other evaluators are mostly private to the courses which use them.
Documentation for writing evaluators will be added.

## IS MU Integration

For integration with IS MU, you will need a server capable of running a
web server, the [`exprtest` service](/src/pyserv/)
([documentation](/src/pyserv/README.md)) and your language backends of choice.
For running the service, you can start with [the systemd unit
file](src/systemd/exprtest.service).

### Questionnaire Frontend

The original frontend is the questionnaire frontend which uses ability of IS to
send questionnaire evaluation to an external server.

TODO: doc

### Submission Folder Frontend

The [`ispol`](/src/ispol) ([documentation](/src/ispol/README.md)) is an
alternative frontend/glue for IS MU. It allows students to submit a file to
submission folder which is checked by `ispol` and evaluated using ExprTest
service. The results are written to student's notebooks.

### Writing questions for IS MUNI integration

Questions are stored in question directory (defaults to `/var/lib/checker/qdir`),
they have extension `.q.hs`, or other `.q.??` extension.


## Bug Reporting

Use GitHub bug reporting for bugs (I'm sure there are some).
