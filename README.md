ExprTest
==========

[![Build Status](https://travis-ci.org/vlstill/hsExprTest.svg?branch=master)](https://travis-ci.org/vlstill/hsExprTest)

Automatic testing of small programming assignments.

This project consists of two parts, an programming-language-agnostic driver for
testing which invokes teacher-provided script on student's solution and a tool
for comparison of Haskell expressions and types based on QuickCheck.

This tool was initially used in the [Non-Imperative
Programming](https://is.muni.cz/auth/predmet/fi/podzim2018/IB015) course on
[Faculty of Informatics, Masaryk University (FI MU)](https://www.fi.muni.cz) and
is now used in 3 more courses on the same faculty (with different-teacher
provided components).  It can be used either locally (but that is not very
useful) or connected to a testing frontend, such as the frontend of [IS
MU](https://is.muni.cz).  In either case, testing requires [assignment
files](#assignment-files).

The work began with a bachelor thesis of Martin Jonáš on FI MU, the code was
later extended by Vladimír Štill and is used in the couse from autumn 2014.
As of autumn 2018, a complete rewrite (version 3) of Haskell testing framework
is going to be used.

## Evaluators / Language Backends

There is support for multiple evaluators. The original & residing in this
repository is Haskell evaluator: [hsExprTest](/src/hs/)
([documentation](/src/hs/README.md)).

Other evaluators are mostly private to the courses which use them.
Documentation for writing evaluators will be added.

## IS MU Integration

For integration with IS MU, you will need a server capable of running a
web server, the `exprtest` service and your language backends of choice. For
running the service, you can start with [the systemd unit
file](src/systemd/exprtest.service).

The service does not, however communicate with the webserver directly, it uses [a
small Perl frontend script which runs as CGI](src/frontend/is.pl).

The service is configured by [a YAML file](exprtest.yaml). This file designates
the socket of the IS Perl wrapper, the root for test declaration directories and
the courses provided by the service. For each course it is possible to enable
user-based isolation (requires existence of user `rc-COUSE_NAME` which has to
have reading access to the teacher files and the driver/checker) and a hint
mode. If a hint mode is enabled, queries marked as unauthorized by the frontend
are processed but with the additional --hint argument to the checker. The
checker should then check for this argument and provide partial answer, e.g.
typechecking result in the case of Haskell. If hint mode is disable unauthorized
queries are refused.

The `exprtest` service has a hot restart feature to facilitate config reloads
and service updates -- if it receives `SIGUSR1` signal, it will finish all
running requests and the re-exec itself, reloading the config in process. The
reload process should be robust enough to be performed under load.

### Writing questions for IS MUNI integration

Questions are stored in question directory (defaults to `/var/lib/checker/qdir`),
they have extension `.q.hs`, or other `.q.??` extension.


## Bug Reporting

Use GitHub bug reporting for bugs (I'm sure there are some).
