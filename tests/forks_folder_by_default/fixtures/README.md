# Test fixtures for the forks folder drawn by default.

Two repos: 'home' (owned) and 'away' (foreign).

Structure:
  home/
    1         contains 11, 12, 13, 14
    11        contains 111, forks 11-sees
    111       (leaf)
    12        forks 12-sees
    13        (leaf, no forks)
    14        contains 141
    141       (leaf)

  away/
    11-sees   (forkee of 11)
    12-sees   (forkee of 12)
    invisible (not referenced)

Expected content view from node 1:
  * 1
  ** 11
  *** forks folder    <-- because 11 forks something
  *** 111
  ** 12
  *** forks folder    <-- because 12 forks something
  ** 13               <-- no forks folder (it forks nothing)
  ** 14               <-- no forks folder (it forks nothing)
  *** 141
