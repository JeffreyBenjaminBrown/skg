"""Rearranging a skg-for-ai compact view without retyping a handled headline.

A compact view is a list of headlines, each followed by its body lines.
Moving a node means moving its headline line, handle and all, with its
subtree. These helpers do that by handle, so a script never types a
handled headline afresh (which would make a new node; see
'tools/skg-for-ai/README.org', "Retyped headlines").
"""
import re

HANDLE = re.compile(r'⟨([0-9a-z]+)(?: [^⟩]*)?⟩\s*$')


def level(line):
    "The headline level of LINE, or 0 if it is a body line."
    m = re.match(r'^(\*+) ', line)
    return len(m.group(1)) if m else 0


def handle(line):
    "The viewnode ID in the handle that ends headline LINE, or None."
    m = HANDLE.search(line) if level(line) else None
    return m.group(1) if m else None


class CompactView:
    def __init__(self, path):
        self.path = path
        self.lines = open(path, encoding='utf-8').read().split('\n')

    def index(self, vnid):
        for i, line in enumerate(self.lines):
            if handle(line) == vnid:
                return i
        raise KeyError(vnid)

    def end(self, i):
        "The index just past the subtree whose headline is at I."
        lv = level(self.lines[i])
        j = i + 1
        while j < len(self.lines) and not (0 < level(self.lines[j]) <= lv):
            j += 1
        return j

    def subtree(self, vnid, at_level=None, with_children=True):
        """The lines of the subtree at handle VNID, shifted so that its
        headline sits AT_LEVEL. Without children, only the headline and
        its body."""
        i = self.index(vnid)
        j = self.end(i)
        block = self.lines[i:j]
        if not with_children:
            k = 1
            while k < len(block) and level(block[k]) == 0:
                k += 1
            block = block[:k]
        if at_level is None:
            return block
        shift = at_level - level(block[0])
        return [('*' * (level(l) + shift) + l[level(l):]) if level(l) else l
                for l in block]

    def replace_subtree(self, vnid, new_lines):
        i = self.index(vnid)
        self.lines[i:self.end(i)] = new_lines

    def headline(self, vnid):
        return self.lines[self.index(vnid)]

    def write(self):
        open(self.path, 'w', encoding='utf-8').write('\n'.join(self.lines))


def add_first(view, vnid, text):
    """Insert TEXT (body lines, then child headlines) right under the
    headline at handle VNID, before its existing children. Appending to
    the end of a view file is wrong: a view may end in a write-protected
    folder (the root's containers), and text there edits that folder."""
    i = view.index(vnid) + 1
    while i < len(view.lines) and level(view.lines[i]) == 0:
        i += 1
    view.lines[i:i] = text.strip('\n').split('\n')
