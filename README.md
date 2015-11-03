Command Line Parser Generator [1]
=================================

A tool for generating a driver intended to call one of the procedures declared
in a package specification, depending on the parameters passed to the driver.

The accepted command line arguments are derived from the formal parameters of
the available procedures.


Build dependencies
------------------

+ Zsh
* GNU Parallel
* GNAT (including the `gnatmetric` tool)
* Mercurial (hg)
* Perl
* Wget
* Tar
* ASIS (the Ada Semantic Interface)


Installing
----------

```
   OS_VERSION=unix make install
```

Builds and tests the executable before installing it in
"${DESTDIR}${PREFIX}/bin" (where "${PREFIX}" defaults to "${HOME}").

Installing may also work on Windows, if you substitute "OS_VERSION=unix" with
"OS_VERSION=windows".


Testing
-------

```
   OS_VERSION=unix make test
```

Builds and tests the executable.


Building
--------

```
   OS_VERSION=unix make build
```

Builds the executable.


Examples
--------

The project comes with some examples (located in the `examples/` directory).
The program `command_line_parser_generator-run` can be used to analyse them:

    command_line_parser_generator-run bad_declares_function

Notice that `command_line_parser_generator-run` uses the environment variable
ADA_INCLUDE_PATH to decide which directories to search for source files in.
If ADA_INCLUDE_PATH isn't set, the current directory will be used.


Links
-----

If you want to find free Ada tools or libraries AdaIC [2] is an excellent
starting point.  You can also take a look at my other source text
repositories [3] or my web site [4].

[1] Source text repository:
    http://repositories.jacob-sparre.dk/command-line-parser-generator

[2] Free Ada Tools and Libraries:
    http://www.adaic.org/ada-resources/tools-libraries/

[3] My repositories on Bitbucket:
    http://repositories.jacob-sparre.dk/

[4] My web site:
    http://www.jacob-sparre.dk/
