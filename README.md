# dtamin
DTA minimization

A frontier-to-root deterministic finite-state tree automaton
(DTA) can be used as a compact data structure to store collections of
unranked ordered trees. DTAs are usually sparser than string automata,
as most transitions are undefined and therefore, special care must be
taken in order to minimize them efficiently. However, it is difficult to
find simple and detailed descriptions of the minimization procedure in
the published literature. Here, we fully describe a simple implementation
of the standard minimization algorithm that needs a time in O(|A|^2),
with |A| being the size of the DTA.

Reference: 	Rafael C. Carrasco, Jan Daciuk, Mikel L. Forcada:
An Implementation of Deterministic Tree Automata Minimization. CIAA 2007: 122-129
