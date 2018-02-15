"""Fill out boilerplate throughout the project.

scribbu files share certain boilerplate:

  - version
  - author
  - copyrights
  - various documentation verbiage

This script will take information defined in one place (`bp', by default) and
propagate it throughout.
"""

import argparse
import datetime
import logging
import os
import re
import six
import shutil
import sys
import tempfile


log = logging.getLogger()
log.setLevel(logging.INFO)
log.addHandler(logging.StreamHandler(sys.stdout))


MAN_PAGES = ['scribbu.1', 'scribbu-dump.1', 'scribbu-rename.1', 'scribbu-report.1',
             'scribbu-split.1']
SOURCE_DIRS = ['scribbu', 'scheme', 'src', 'test']

LICENSE = """This file is part of scribbu.

scribbu is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

scribbu is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with scribbu.  If not, see <http://www.gnu.org/licenses/>."""


def _find_root_directory():
    """Find the scribbu root directory, assuming that the pwd is a descendent
    thereof.
    """

    cand = os.path.abspath(os.getcwd())
    d, b = os.path.split(cand)
    while b:
        if 'scribbu' == b and 'bp.py' in os.listdir(cand):
            break
        cand = d
        d, b = os.path.split(cand)

    if cand == '/':
        raise RuntimeError('{} must be run from within the scribbu source tree.'.format(__file__))

    return cand


def replace_file_contents(filename, lines, make_backup=True, dry_run=True):
    """Replace the contents of FILENAME with LINES.

    :param str filename: Name of the file to be replaced
    :param iterable lines: Iterable containing the lines with which the
                           contents of FILENAME shall be replaced
    :param bool make_backup: If true, leave a backup copy of the original file
    :param bool dry_run: If true, don't actually change anything; just print
                         what would be done.

    """

    if dry_run:
        log.info('Would write the following to {0} {1} backup:'.format(
            filename, "with" if make_backup else "without"))
        for line in lines:
            log.info(line)
    else:
        if make_backup:
            shutil.copy(filename, '{}.bpbak'.format(filename))
        with open(filename, 'w') as fh:
            fh.write('\n'.join(lines) + '\n')


def process_file_via_regexes(filename, regexes):
    """Update a file through the application of one or more regex
    replacements.

    :param str filename: Path of the file to be processed
    :param regexes: Sequence of replacements
    :type regexes: iterable(re.regex, str, [count])
    :return: a list of the resulting lines & the total number of replacements

    The implementation leverages re.sub to do its work. For each line
    in FILENAME, each pair or triple in REGEXES will be used to
    call re.sub. Every element of REGEXES will be tried, so a
    given regex will be applied to the output of all previous
    regexes (i.e. order matters).

    Note that each line is stripped of trailing whitespace, including
    the newline, before having the regexes applied.

    N.B. All processing is done in-memory, and the resulting lines are returned
    as a list (i.e. this implementation is not suitable for very large files;
    there are however, certain conveniences associated with this approach, such
    as being able to readily compute the total number of replacements and
    easily knowing when the input file has been closed).

    """

    out_lines = []
    num_replacements = 0
    with open(filename, 'r') as f:
        for line in f.readlines():
            line = line.rstrip()
            for regex in regexes:
                if len(regex) <= 2 or regex[2] is None:
                    count = 0
                else:
                    count = regex[2]
                line, num_subs = re.subn(regex[0], regex[1], line, count)
                num_replacements += num_subs
            out_lines.append(line)

    return out_lines, num_replacements


def update_configure_ac(ver, email, root, make_backups ,dry_run):
    """Update boilerplate in configure.ac.

    :param str ver: Current version number, e.g. '0.4'
    :param str email: Maintainer e-mail version
    :param str root: Root directory for the scribbu project
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.

    This method looks for the line:

        AC_INIT([scribbu], [x.y], [XXX@YYY])

    and updates it with VER and EMAIL.
    """

    regex = re.compile('^AC_INIT\(\[scribbu\], \[[.0-9]+\], \[[-a-zA-Z0-9@_.]+\]\)$')
    repl = 'AC_INIT([scribbu], [{0}], [{1}])'.format(ver, email)
    cfgac = os.path.join(root, 'configure.ac')
    out, num = process_file_via_regexes(cfgac, [(regex, repl, 1)])
    if num != 1:
        raise RuntimeError('{} replacements were made in `configure.ac\'; should have been 1.'.format(num))
    replace_file_contents(cfgac, out, make_backups, dry_run)


def update_readme_md(ver, author, root, make_backups, dry_run):
    """Update boilerplate in README.md.

    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param str root: Root directory for the scribbu project
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.

    This method looks for lines of the form:

        This directory contains the x.y release of scribbu.

    and

        Copyright (C) xxx-yyy XXXX

    and updates them with VER, AUTHOR, and the current year.
    """

    today = datetime.datetime.today().strftime('%Y')
    re1 = re.compile('This directory contains the [0-9.]+ release of scribbu.')
    rp1 = 'This directory contains the {} release of scribbu.'.format(ver)
    re2 = re.compile('Copyright \(C\) ([0-9]{4})-[0-9]{4}\s+(.*)')
    rp2 = 'Copyright (C) \\1-{0} {1}'.format(today, author)
    rm = os.path.join(root, 'README.md')
    out, _ = process_file_via_regexes(rm, [(re1, rp1), (re2, rp2)])
    replace_file_contents(rm, out, make_backups, dry_run)


def update_man_pages(ver, author, root, make_backups, dry_run):
    """Update boilerplate in the scribbu man pages.

    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param str root: Root directory for the scribbu project
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.

    This method looks for lines of the form:

        .TH scribbu 1 YYYY-MM-DD "scribbu x.y" "scribbu Manual"

    and

        Copyright (C) xxx-yyy XXXX

    and updates them with VER, AUTHOR, and the current year.
    """

    year = datetime.datetime.today().strftime('%Y')
    today = datetime.datetime.today().strftime('%Y-%m-%d')
    re1 = re.compile('^.TH (scribbu(-[a-zA-Z0-9]+)?) 1 [-0-9]+ "scribbu [0-9.]+" "scribbu Manual"$')

    def _repl_header(what):
        return '.TH {0} 1 {1} "scribbu {2}" "scribbu Manual"'.format(what.group(1), today, ver)

    rp1 = _repl_header

    re2 = re.compile('Copyright \(C\) ([0-9]{4})-[0-9]{4}\s+(.*)')
    rp2 = 'Copyright (C) \\1-{0} {1}'.format(today, author)

    re3 = re.compile('Copyright \(C\) ([0-9]{4})\s+(.*)')

    def _repl_year(what):
        if what.group(1) == year:
            yrs = year
        else:
            yrs = '{0}-{1}'.format(what.group(1), year)
        return 'Copyright (C) {0} {1}'.format(yrs, author)

    rp3 = _repl_year

    for man in MAN_PAGES:
        fn = os.path.join(root, 'doc', man)
        log.info('{0}...'.format(man))
        out, num = process_file_via_regexes(fn, [(re1, rp1), (re2, rp2), (re3, rp3)])
        replace_file_contents(fn, out, make_backups, dry_run)
        if num != 2:
            raise RuntimeError('{0} replacements in {1}; should be 2.'.format(num, man))
        log.info('{0}...done.'.format(man))


def update_texinfo(ver, author, root, make_backups, dry_run):
    """Update boilerplate in the scribbu texinfo manual.

    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param str root: Root directory for the scribbu project
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.

    This method looks for lines of the form:

        Copyright @copyright{} xxx-yyy XXXX

    and

        This manual corresponds to scribbu version 0.4.

    and updates them with VER, AUTHOR, and the current year.
    """

    year = datetime.datetime.today().strftime('%Y')
    re1 = re.compile('^Copyright @copyright{} ([0-9]+)\s+(.*)$')

    def _repl_year(what):
        if year == what.group(1):
            yrs = what.group(1)
        else:
            yrs = '{0}-{1}'.format(what.group(1), year)
        return 'Copyright @copyright{{}} {0} {1}'.format(yrs, author)

    rp1 = _repl_year

    re2 = re.compile('This manual corresponds to scribbu version [0-9.]+')
    rp2 = 'This manual corresponds to scribbu version {}.'.format(ver)

    texi = os.path.join(root, 'doc/scribbu.texi')
    out, num = process_file_via_regexes(texi, [(re1, rp1), (re2, rp2)])
    replace_file_contents(texi, out, make_backups, dry_run)
    if num != 2:
        raise RuntimeError('{0} replacements in {1}; should be 2.', num, texi)


def update_source_code(ver, author, root, make_backups, dry_run):
    """Update all source code boilerplate.

    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param str root: Root directory for the scribbu project
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.
    """

    for d in SOURCE_DIRS:
        update_source_in(os.path.join(root, d), ver, author, make_backups, dry_run)


def update_source_in(directory, ver, author, make_backups, dry_run):
    """Update source code boilerplate in a given directory.

    :param str directory: Directory in which to update source
    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.
    """

    _EXTS = set(['.hh', '.cc', '.yy', '.ll', '.scm'])

    files = filter(lambda x: os.path.splitext(x)[1] in _EXTS, os.listdir(directory))
    for f in files:
        update_source(os.path.join(directory, f), ver, author, make_backups, dry_run)


def update_source(filename, ver, author, make_backups, dry_run):
    """Update a source file.

    :param str filename: File to be updated
    :param str ver: Current version number, e.g. '0.4'
    :param str author: Maintainer name.
    :param bool dry_run: If true, don't actually change anything; just print
                         what *would* be done.

    This function will search for a file header in FILENAME. If it is not
    present, it will be added at the top.

    If it is present, a copyright statement will be added or updated, and
    the licensing verbiage will be added if not found.
    """

    log.info('Updating {}...'.format(filename))

    basename = os.path.basename(filename)
    year = datetime.datetime.today().strftime('%Y')

    cc = '*'
    ecc = '\*'
    doxy = '/**'
    termdoxy = '*/'
    filehdr = '^(\s+){}\s+\\\\file'.format(ecc)
    fileopen = '\\file {}'.format(basename)

    if os.path.splitext(filename)[1] == '.scm':
        cc = ';;;;'
        ecc = ';;;;'
        doxy = ';;;;'
        termdoxy = 'Commentary'
        filehdr = '^;;;; {} ---'.format(basename)
        fileopen = basename

    # States:
    #  -2: done; parsed file header, found copyright but no license; line on which
    #      the file header ends in CLOSING
    #  -1: done; parsed file header, no copyright or license; line on which
    #      the file header ends in CLOSING
    #   0: init
    #   1: found "\file"
    #   2; Found copyright
    #   3: Found license, no copyright; license begins on line LICENSE
    #   4: Found copyright & license; license begins on line LICENSE
    indent = ''
    closing = None
    license = None
    state = 0
    with open(filename, 'r') as fh:
        lines = fh.readlines()

    for i in range(len(lines)):
        line = lines[i].rstrip()
        if state == 0:
            m = re.search(filehdr, line)
            if m:
                state = 1
                if len(m.groups()) > 0:
                    indent = m.group(1)
        elif state == 1:
            text = '^(\s*){}\s+Copyright \(C\) ([0-9]{{4}}-)?([0-9]{{4}})\s+(.*)'.format(ecc)
            m = re.search(text, line)
            if m:
                lines[i] = '{0}{4} Copyright (C) {1}{2} {3}'.format(
                    m.group(1), m.group(2) if m.group(2) else '', year, author, cc)
                state = 2
            if termdoxy in line:
                state = -1
                closing = i
                break
            elif 'This file is part of scribbu.' in line:
                state = 3
                license = i
        elif state == 2:
            if termdoxy in line:
                state = -2
                closing = i
                break
            elif 'This file is part of scribbu.' in line:
                state = 4
                license = i
                break

    if state == 0:
        # Never found header-- insert one before line 0
        hdr = """{4}
 {3} {0}
 {3}
 {3} Copyright (C) 2015-{1} {2}
 {3}
""".format(fileopen, year, author, cc, doxy)
        hdr += '\n'.join(map(lambda x: indent + cc + ' ' + x, LICENSE.split('\n')))
        hdr += """{0}{1}
{0}{1}
{0}{1}
{0}{1}
""".format(indent, cc)
        lines[0:0] = hdr.split('\n')
    elif state == -2:
        # Insert license above line CLOSING
        lines[closing-1:closing-1] = map(lambda x: indent + cc + ' ' + x, LICENSE.split('\n'))
    elif state == -1:
        # Insert copyright & license above line CLOSING
        addnl = """{0}{3}
{0}{3} Copyright (C) 2015-{1} {2}
{0}{3}
""".format(indent, year, author, cc)
        addnl += '\n'.join(map(lambda x: indent + cc + ' ' + x, LICENSE.split('\n')))
        lines[closing-1:closing-1] = addnl.split('\n')
    elif state != 4:
        raise RuntimeError('Failed to parse {0}; state is {1}.'.format(filename, state))

    replace_file_contents(filename, lines, make_backups, dry_run)

    log.info('Updating {}...done.'.format(filename))


def main():
    parser = argparse.ArgumentParser(description='Fill out scribbu boilerplate')
    parser.add_argument('-b', '--boilerplate-file', type=str,
                        help='file containing Python variables defining', default='bp')
    parser.add_argument('-c', '--make-backups', action='store_true',
                        help='make a backup copy before altering a file')
    parser.add_argument('-d', '--scribbu-dir', type=str,
                        help='scribbu root directory')
    parser.add_argument('-n', '--dry-run', action='store_true',
                        help='don\'t change any files; just print what would be done')
    args = parser.parse_args()

    root = args.scribbu_dir
    if root is None:
        root = _find_root_directory()

    R = {'VERSION': None, 'AUTHOR': None, 'EMAIL': None}
    with open(os.path.join(root, args.boilerplate_file), 'r') as bp:
        six.exec_(bp.read(), R)

    log.info('directory: {}'.format(root))
    log.info('  version: {}'.format(R['VERSION']))
    log.info('   author: {}'.format(R['AUTHOR' ]))
    log.info('   e-mail: {}'.format(R['EMAIL' ]))

    if any(map(lambda x: x is None, [y for y in R.itervalues()])):
        raise RuntimeError('missing configuration')

    update_configure_ac(R['VERSION'], R['EMAIL'], root, args.make_backups,
                        args.dry_run)
    update_readme_md(R['VERSION'], R['AUTHOR'], root, args.make_backups,
                     args.dry_run)
    update_man_pages(R['VERSION'], R['AUTHOR'], root, args.make_backups,
                     args.dry_run)
    update_texinfo(R['VERSION'], R['AUTHOR'], root, args.make_backups,
                   args.dry_run)
    update_source_code(R['VERSION'], R['AUTHOR'], root, args.make_backups,
                       args.dry_run)


if __name__ == '__main__':
    main()
