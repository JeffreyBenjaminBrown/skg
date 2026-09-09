# The files 1.skg through 5.skg

The `contains` relatinoship forms a simple tree:

1.skg
├── 2.skg
└── 3.skg
    ├── 4.skg
    └── 5.skg

w/r/t the contains relationship, files 4 and 5 are islands.

# The files a.skg through c.skg

They are cyclic:

- A contains B. (Nothing contains A.)
- B contains C.
- C contains B.
