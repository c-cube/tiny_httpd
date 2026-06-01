# lwt-dllist - Mutable doubly-linked list

An `Lwt_dllist` is an object holding a list of elements which support
the following operations:

- adding an element to the left or the right in time and space O(1)
- taking an element from the left or the right in time and space O(1)
- removing a previously added element from a sequence in time and space O(1)
- removing an element while the sequence is being traversed.

## History

This module was formerly part of the Lwt core distribution as the
`Lwt_sequence` module, but has been pulled out into a separate library since it
is really just an implementation detail of Lwt.

You can migrate existing uses of `Lwt_sequence` into `Lwt_dllist` by simply
renaming the module.  The implementation of the module remains unchanged, but
the name reflects the fact that the implementation is a doubly-linked list.

## Further Reading

- Docs: <https://mirage.github.io/lwt-dllist>
- Issues: <https://github.com/mirage/lwt-dllist/issues>
- Discussion: <https://discuss.ocaml.org> with the MirageOS tag.
