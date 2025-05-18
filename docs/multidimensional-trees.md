# Multidimensional trees

(This is why the app uses TypeDB.)

We are used to viewing documents one at a time. In traditional writing, any view is associated with a single document, and the relationship between every part and subpart of the document was always the same: "[part] contains [subpart] in [document]". (A usueful synonym for the relationship might be "[node] contains [branch] in [view]".)

But the tree depicted does not have to correspond to any one view. The branches of one node might be its contents in the document the node represents, but they might also appear alongside that node's contents in a different docuemnt, or backlinks pointing to the spots where other documnents link to the node.
