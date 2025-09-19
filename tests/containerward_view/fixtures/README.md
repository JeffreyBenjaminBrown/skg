## The idea of this graph

The detailed containment graph is drawn below. Outside of (or overlapping) that, the interesting properties are:

- Each node's title matches its ID
- Node 11 has an extra-id, "11-extra-id".
- **shared** is contained by both **1** and **2** (multiple containers)
- **shared_1** has some body text. Nothing else does.
- **2** → **21** → **211** → **2** forms a cycle

# Test Fixture Structure

This directory contains test fixtures for the robust search functionality. The nodes form the following containment structure:

```
1
├── 11
|   └── 111
└── shared

2
├── 21
│   └── 211
│       └── 2
└── shared

shared
├── shared_1
└── shared_2

uncyclic_container
└── shared_cyclic

shared_cyclic
└── shared_cyclic_1
    └── shared_cyclic
```

## Some of that same information in words

- **1** has two children: **11** and **shared**
- **2** has two children: **21** and **shared**
- **21** has one child: **211**
- **211** has one child: **2** (forming a cycle)
- **shared** has two children: **shared_1** and **shared_2**
