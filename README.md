
The design goal is to have an efficient pretty-printer that, although it uses an allocator that has to be passed explicitly, maximizes the use of generic programming and provides a reasonable API while remaining fast and efficient.
There are also built-in facilities for doing brackets/parenthesization, and for pretty-printing structured data both using rust object notation and more generic objects.

To get started, you can either use the `Printer` type directly, or you can add `Printer` to something, like putting it in a field of some struct, and then implement the HasPrinter trait for whatever you gave the printer to, which just requires that you tell how to get the printer from your struct both mutably and immutably.
After that you're pretty much off to the races. 

The main trait that defines what can be treated like a document without having to first turn it into one explicitly is `Doclike`. Most string types (&str, String, Cow<'_, str>), Doc, DocPtr, and anything that has `From<A> for Cow<'_, str>` can directly be treated as a document.

In order to implement `Doclike` for a type of your own, you only need to implement the `alloc` method, which explains how the arena is supposed to track your element. The easiest way to do this is just figure out how to get a string that represents your thing, and then call `my_thing_as_string.alloc(printer)`, and let the implementation of Doclike for string types take over. An example of this can be seen in /examples/lined, where we allow for a span/slice type to be treated directly as a document by specifying that the Printer type also has information about the underlying file where the span comes from.
