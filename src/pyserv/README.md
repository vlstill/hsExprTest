# ExprTest service

TODO: this documentation is outdated

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

