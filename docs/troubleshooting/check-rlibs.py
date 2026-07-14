#!/usr/bin/env python3
"""Report Rust build artifacts truncated by an unclean shutdown.

Usage:
    python3 docs/troubleshooting/check-rlibs.py [dir ...]   # default: target/debug/deps

WHY THIS EXISTS, AND WHY THE OBVIOUS CHECKS DON'T WORK
------------------------------------------------------
An .rlib is an `ar` archive: a chain of member headers, each declaring its
own size. A file truncated by a power loss keeps an intact-looking chain at
the FRONT of the file, so:

  - `ar t` still lists the members and exits 0. It reads headers, not data.
  - The tail is real data, not NULs, so a NUL-scan finds nothing.
  - Cargo has already written the fingerprint saying the artifact is fresh,
    so `cargo build` will not rebuild it. It just hands it to the linker,
    which fails with the cryptic "file too short".

The only reliable test is the one below: walk the member chain and confirm
it lands exactly on EOF. If a member claims to run past the end of the file,
the file was truncated.
"""
import sys, os, glob

def first_error(path):
    """Return a description of the corruption, or None if the archive is intact."""
    size = os.path.getsize(path)
    with open(path, 'rb') as f:
        if f.read(8) != b'!<arch>\n':
            return 'not an ar archive (bad magic)'
        pos = 8
        while pos < size:
            f.seek(pos)
            hdr = f.read(60)
            if len(hdr) < 60:
                return f'member header at offset {pos} is cut off (file is {size} bytes)'
            try:
                member_size = int(hdr[48:58].decode('ascii').strip())
            except ValueError:
                return f'unreadable member size field at offset {pos}'
            end = pos + 60 + member_size
            if end > size:
                return (f'a member runs to byte {end}, but the file ends at {size} '
                        f'-- SHORT BY {end - size} bytes')
            pos = end + (member_size & 1)  # members are 2-byte aligned
    return None

def main():
    roots = sys.argv[1:] or ['target/debug/deps']
    corrupt, checked = [], 0
    for root in roots:
        for path in sorted(glob.glob(os.path.join(root, '*.rlib'))):
            checked += 1
            err = first_error(path)
            if err:
                corrupt.append((path, err))

    print(f'checked {checked} rlibs in: {", ".join(roots)}')
    if not corrupt:
        print('all intact')
        return 0

    for path, err in corrupt:
        print(f'\nCORRUPT: {path}\n  {err}')
    print(f'\n{len(corrupt)} corrupt. To fix, force Cargo to rebuild exactly these '
          f'packages:\n  cargo clean -p <pkg> [-p <pkg> ...]   && cargo build --bin skg')
    print('Map a file like "libtypedb_driver-<hash>.rlib" to the package name '
          '"typedb-driver" (underscores become hyphens).')
    return 1

if __name__ == '__main__':
    sys.exit(main())
