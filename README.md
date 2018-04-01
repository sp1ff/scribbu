scribbu - The extensible tool for tagging your music collection
===============================================================

This directory contains the 0.4 release of scribbu.

See the file NEWS for the user-visible changes from previous releases.

Please check the system-specific notes below for any caveats related to
your operating system.

For general building and installation instructions, see the file [INSTALL]().

scribbu is free software.  See the file COPYING for copying
conditions.  scribbu is copyrighted by Michael Herstine.  Copyright
notices condense sequential years into a range; e.g. "1987-1994" means
all years from 1987 to 1994 inclusive.

What is it?
-----------

scribbu is a C++ library & associated command-line tool for working
with ID3-tagged files (music, presumably). In addition to assorted
sub-commands, the scribbu command
embeds [Guile](https://www.gnu.org/software/guile/ "Guile"), allowing
it to be scripted in Scheme.

Downloading
-----------

You can find the project at https://github.com/sp1ff/scribbu. You can
clone it by typing `git clone https://github.com/sp1ff/scribbu.git`.

Documentation
-------------

scribbu was born when I retired my last Windows machine & could no
longer use Winamp to manage my library of digital music. While
everyone else is streaming their music, I still manage my own library
(harumph).  The project is still crude; I'm building it out as I
discover features I want.

scribbu can be invoked in a few ways:

  - with a Scheme expression (`-e`, `--expression`) or Scheme file
    (`-f`, `--file`).  In this case, scribbu will evaluate the given
    code & exit.

  - with no arguments at all. In this case, scribbu will drop into a
    Scheme shell in which the user can evaluate arbitrary Scheme
    expressions.

  - with a sub-command & relevant options & arguments.  scribbu
    implements the following sub-commands:

	+ `scribbu dump` will write the contents of any & all ID3 tags
      found in one or more files to stdout.

    + `scribbu rename` will rename one or more files based on the
      contents of their ID3 tags; e.g. `scribbu rename -t %A-%T.mp3 *.mp3`
	  will rename all the files matching *.mp3 to
      <artist>-<title>.mp3 where "artist" and "title" are derived from
      their ID3 tags (if any).

    + `scribbu report` will generate a report listing ID3 attributes
      on one or more files on stdout.  Only CSV format is supported
      currently (on the basis that there are better querying/reporting
      tools out there already; they can just import the .csv & do
      better than scribbu would)

    + `scribbu split` will split one file into many; with each new
      file corresponding to some component of the input file: ID3v1
      tag, data, and/or ID3v2 tag(s).

Any sub-command can be invoked with `--help` or `-h` for more
information. Add the `--info` option to display the Info manual.

I got the project name from this cool project name
[http://mrsharpoblunto.github.io/foswig.js/](generator).

### Scripting Examples

Let's illustrate the power of scripting scribbu
using [Guile](https://www.gnu.org/software/guile/ "Guile") using a
worked example.  Suppose that we have a directory full of .mp3 files
ripped by Winamp some time ago & that we noted that fact by setting
their ID3v1 comment to "Ripped by Winamp".

We would like to update these files by ensuring that:

  - They have an ID3v2 tag

  - That tag has a @code{TENC} frame set to "Winamp"

We begin experimenting:

``` common-lisp
scheme@@(guile-user)> (define t (scribbu/make-track "/vagrant/test/data/elliot-goldenthal.id3v1.tag"))
scheme@@(guile-user)> (scribbu/get-path t)
$5 = "/vagrant/test/data/elliot-goldenthal.id3v1.tag"
scheme@@(guile-user)> (scribbu/get-id3v1-string t 'scribbu/comment)
$6 = "Ripped by Winamp on Pimperne"
scheme@@(guile-user)> (scribbu/get-id3v2-tag-count t)
$7 = 0
```

So this track has an ID3v1 tag with the comment we wrote when we
ripped it using Winamp, but no ID3v2 tags. Let's fix that:

``` common-lisp
scheme@@(guile-user)> (scribbu/make-id3v2-tag t 0)
$1 = ()
scheme@@(guile-user)> (scribbu/set-frame t 0 'scribbu/encoded-by "Winamp")
$2 = ()
scheme@@(guile-user)> (scribbu/write-id3v2-tag t 0 "test.out")
$3 = 27
```

In a shell, we see that an ID3v2 tag has been written to "test.out":

``` shell
vagrant@@vagrant:~/build$ od -Ax -t x1z test.out
000000 49 44 33 03 00 00 00 00 00 11 54 45 4e 43 00 00  >ID3.......TENC..<
000010 00 07 00 00 00 57 69 6e 61 6d 70                 >.....Winamp<
00001b
```

We attempt to write the new track to file:

``` common-lisp
@example
scheme@@(guile-user)> (scribbu/write-track t "test.mp3")
Cannot write a track that wasn't created with load-data
```

By default, `scribbu/make-track` will *not* load the track data into
memory (just the ID3 tags), and so `scribbu/write-track` refuses to
re-write it (see `scribbu/replace-tags` for an alternative approach
here).

We're just experimenting, however, so let's try this again:

``` common-lisp
scheme@@(guile-user)> (define t (scribbu/make-track "/vagrant/test/data/elliot-goldenthal.id3v1.tag" #:load-data #t))
scheme@@(guile-user)> (scribbu/make-id3v2-tag t 0)
$1 = ()
scheme@@(guile-user)> (scribbu/set-frame t 0 'scribbu/encoded-by "Winamp")
$2 = ()
scheme@@(guile-user)> (scribbu/write-track t "test.mp3")
$3 = ()
```

Checking in the shell, we see that the entire track has been written
out (this is a contrived example, so there's no audio data-- just the
new ID3v2 tag & the old ID3v1 tag):

``` shell
vagrant@@vagrant:~/build$ od -Ax -t x1z test.mp3
000000 49 44 33 03 00 00 00 00 00 11 54 45 4e 43 00 00  >ID3.......TENC..<
000010 00 07 00 00 00 57 69 6e 61 6d 70 54 41 47 45 61  >.....WinampTAGEa<
000020 73 74 65 72 20 52 65 62 65 6c 6c 69 6f 6e 20 28  >ster Rebellion (<
000030 50 65 72 66 6f 72 6d 65 64 20 62 79 53 69 6e 65  >Performed bySine<
000040 61 64 20 4f 27 43 6f 6e 6e 6f 72 00 00 00 00 00  >ad O'Connor.....<
000050 00 00 00 00 00 00 00 00 00 00 4d 69 63 68 61 65  >..........Michae<
000060 6c 20 43 6f 6c 6c 69 6e 73 00 00 00 00 00 00 00  >l Collins.......<
000070 00 00 00 00 00 00 00 00 31 39 39 36 52 69 70 70  >........1996Ripp<
000080 65 64 20 62 79 20 57 69 6e 61 6d 70 20 6f 6e 20  >ed by Winamp on <
000090 50 69 6d 70 65 72 6e 65 00 01 ff                 >Pimperne...<
00009b
```

Refer to the unit test `test-cleanup-encoded-by` for a Scheme program
that will do this for all files in a given directory.

Development
-----------

You can obtain the code by doing `git clone https://github.com/sp1ff/scribbu.git`:

``` shell
git clone https://github.com/sp1ff/scribbu.git
cd scribbu
./autogen.sh
```

I like to build in a separate directory:

``` shell
cd /tmp
mkdir build && cd build
.../scribbu/configure CXXFLAGS="-g -O0"
make check
```

Alternatively, you can just use Vagrant:

``` shell
vagrant up
vagrant ssh
# In the Vagrant VM:
mkdir build && cd build
/vagrant/configure CXXFLAGS="-g -O0"
make check
```

You can generate doxygen docs by cd'ing to doc & typing 'make doxygen-doc'

Bug Reporting
-------------

sp1ff@pobox.com


System-specific Notes
---------------------

Linux only, Mac coming.

Ports
-----

None.

-------------------------------------------------------------------------------
Copyright (C) 2015-2018 Michael Herstine <sp1ff@pobox.com>
