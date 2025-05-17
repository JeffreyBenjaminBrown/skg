# Trees and hyperlinks: Some simple but important definitions
Each document is a `tree` with `hyperlinks`. All that means is the document consists of ordinary text with hyperlinks, plus the ability (this is the "tree" part) to be divided into sections, subsections, sub-subsections, etc. The divisions don't have to be balanced -- some branches can be much deeper than others.

Each spot in the tree is a `node`, and the relationship that connects nodes in the tree is called `containment`. Each tree has exactly one special node called the `root`, which can contain other nodes, but which nothing contains. Each node in the tree has a `title`, which is ordinary text with hyperlinks. Some nodes contain other nodes (which is sometimes called "having branches"). A node that contains no nodes is called a `leaf` -- that is, leaves are where branching stops. A node's `descendents` include its branches, its branches' branches, etc.

"Branch" and "node" mean almost the same thing -- a branch is just a node together with all its descendents. The difference can often be ignored.

Each branch and each sub-branch of a tree is itself a tree -- even the leaves.

Every hyperlink has a `source`, which is where you find it (in HTML online, the source usually looks like blue text) and a `target`, which is where the link takes you.
