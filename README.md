# Alcolang

## Preface

Alcolang is not a good programming language like, say, Haskell. It's not even a bad programming language like, say, PHP. It is not even esoteric, like Brainfuck. It was not made to achieve some great purpose, or fullfill some real-world need. It is just... weird.

Alcolang is not practical - it is experimental. It is not smart - it is ridiculous. Writing anything meaningful in it will twist any sane and sober mind. It was inspired by Haskell, Verilog, Lisp, Prolog and JavaScript.

This language has been designed to achieve the most with the smallest amount of syntactical constructs. Thanks to that, the core of the language is just a few lines of Haskell with a simple Parsec parser, while most of the actual code resides in the standard library, constantly under construction. This made the language easy to potentially extend! This also resulted in an overly-simplistic system, where the lines between code, data and text are blurred extremely thin. Is this a good thing or a bad thing? I don't know, I'm drunk!

## Using Alcolang:

I am currently working on a Haskell implementation of the interpreter in this repo.

## Language Spec:

Alcolang is an intrinsically-interpreted, pure, reactive, strongly and dynamically-typed, lazy, pattern-matching language. It is effectively multi-paradigm, since it allows for object-oriented, functional (and even imperative, coming soon to the standard library) code to be written.

There are 3 language-level reserved symbols in Alcolang: (, ) and =. Oh, and whitespace, I guess whitespace also counts. Any whitespace goes and has the same meaning, space, \r, \n, \t, you know the drill.

Just like everything in Python is an object, everything in Alcolang is an alco. A parenthesized alco is not parsed until it is necessary, I refer to this as lazy parsing. I know, this is very weird. But anything between a ( and a ), if its insides have a set of matching parentheses, is lazily parsed. Reasons why this is useful are covered in more detail below in "Pattern Matching".

An alco can be a collection of one or more literals and/or unparsed alcos. A literal is any sequence of characters that is not a whitespace, =, ( or ). An alco can also be a binding, where one alco is bound to another with = (the one on the left "bound" to the one on the right, if you want me to spell it out). An alco can be bound to and from any number of alcos. Writing A = B = C is the same as A = (B = C). Additionally, in this case, there will be an extra binding of A to B. This is done to make it easier to create traversable and searchable (doubly-)linked lists with this feature, but this is also explained later, in Pattern Matching.

As you will soon see, an alco is a powerful abstraction. It can easily be used as a linked list, a graph, a matrix, text, a pure computation, a non-pure computation, a pattern, an object, a function... You name it!

### Pattern Matching

At the core of the language lies pattern-matching. Basically, all control flow is multiple-dispatch.

The runtime could attempt to "match" any alco, a "value", to any other alco, a "pattern".

Before the actual pattern-matching happens, the runtime attempts to find some bindings containing the the pattern and the value, going up the lexical hierarchy. The pattern is searched for in right-hand-sides of bindings, and the value is searched for in the left-hand-sides of bindings. If nothing is found for the pattern but something for the value, the pattern is searched for via prototyping (see below). After that, the checks are made.

The first check made is "prototyping". If the value has been bound to the pattern, the match is successful. If the value has been bound to an alco that has been bound to *** that has been bound to the pattern, the match is also successful. Bindings are discovered lazily, so, if the existing bindings for the value do not satisfy the pattern, they are searched for. The search is a depth-first-search along all chains of bindings starting from the value. The path corresponding to the sequence of pattern matches for this value recorded on the stack, described below, is always searched first. Then, the search covers bindings in other children of the alco where the value (or the alco the value was directly or indirectly bound to) was originally parsed, going left to right. If none of them satisfy the pattern, the search continues up one level, to the alco in which the "parent" alco was defined, etc.

The moral of the story is, do not scatter a complex structure of bindings all over, it might get inefficient to process.

If both the value and the pattern have been or are to be lazily parsed (or one or both of them could be an alphanumeric literal), they are first compared for text equality. Any type of literal, be it a string, integer or whatever, can be expressed as a parenthesized value and directly matched. It doesn't even have to be parsed! Of course, a string containing some undesirable code *could* get parsed, causing undesirable behavior, but it is also very easy to prevent, as we'll see later.

If the text match fails, both the value and the pattern are parsed, and, if either of them parses to something, the entire matching process is repeated.

If the repeat fails as well, and both the pattern and the value consist of multiple alcos, and the number of alcos in the value is greater than or equal to the number of alcos in the pattern, the match is attempted for each respective pair of alcos in the value and the pattern. Prior to this, the value is padded with the "pattern-end" build-in value and an unlimited number of "*any*" values (see the Standard Library for these). When attempting a match, if padding is necessary, the "pattern-end" is always tried first. 

If that fails as well, and both the pattern and the value are a binding, the left and right hand sides of the bindings are matched respectively.

If {builtin: match-any} (see below) matches an alco, the match is always successful.

An unbound alco can be matched to any pattern. An unbound pattern can be matched by any value.

### Binding Propagation

Alcolang is a reactive language, which means that binding chains can act like "wires" in a digital citcuit, propagating data from a value to something that is bound to it. Patterns containing bindings can act as a form of "junctions", receiving a "signal" into a value via a binding and binding it to other values, propagating it into other patterns matched to it. Pattern matching essentialy is a process for dynamically establishing a network of such "wires".

For all of this to work, during pattern matching, stacks of pattern *instances* are kept, where an instance is a replica of the original pattern definition, but with the propagation process complete for each "value" alco.

The actual propagation process only happens for unbound values: if, somewhere up the stack, after several matches, an unbound value is bound to something, that value is propagated all the way down to all pattern instances to all instances in them of that unbound value. You can think of it as being similar to an "out" parameter in C#.

The stack is also needed for prototyping, as described above.

And now we get to the most important idea: a pattern match only happens if there is an unbound value in the pattern. The pattern itself binding the value doesn't count (e.g. in the pattern A = B, A is not considered to be bound). Essentially, we only pattern-match while we have unknowns to find, "wires" to connect.

Note: the elements of any pattern, including a binding, constitute a single scope; so in A (A = x) and in A = (A = x), if the former is matched by A \*, and the latter by A = \*, x will get propagated to A either way. This also implies that, when we are matching a binding A = ? to some pattern, that binding is not considered to exist inside the pattern.

An important idea is that pattern matching is repeated for patterns created inside a pattern instance, until all values have been propagated, and there is no un-bound alco anywhere inside the pattern instance.

## Standard Library:

When an alco program from a file is loaded, the runtime provides a "framework", where an array of builtins is contained in a virtual alco along the actual alco that the file represents. This way, any search when resolving a pattern will eventually end there.

```
(# (builtins are not actually accessible from regular code. {builting: end} occurs right past the end of any value being matched to a pattern.))
({builtin: end} = where)
({builtin: end} = ;)

@ = nonpure = {builtin: match-any}
(# (nonpure will provide for sequencing non-pure computations via = in later revisions; the idea is to make it impossible for a non-pure computation in a pattern to be matched by a match-any, so that any value matching the pattern would have to be tainted with one of the non-pure patterns; this will be used to compute non-pure code correctly.))

({builtin: matched-by-any} = _)

(# This is part of the module system. When an alco is to be exported from a file, it should be bound to export.)
export

(# (This imports all exports of a file. After that, x will be bound to all exported alcos of filename.
   If any of them is used by name as (part of) a pattern (and that name does not occur in the surrounding lexical hierarchy),
   it is thus guaranteed to be matched by x.))
(x = import filename where (x = {builtin: open filename, interpret, bind x = export from the filename}))

(x = a + b where (a = int) (b = int) (x = {builtin: a + b}) = {any integer computable by addition} = int)
(# (below are aliases))
(x = + int int = {any integer computable by addition} = int)
(x = add int int = {any integer computable by addition} = int)

(# (Same for *, /, all regular Java operators for int, float and bool. Bitwise operators included.
   There is one additional operator:))
(x = a /% b c where (a = int) (b = int) (x = {builtin: a / b}) (c = {builtin: a % b}))
(# Alias:)
(x = divmod a b c)
((x c) = a /% b)
((x c) = divmod a b)

(# (Only form of I/O we have before non-pure computations are added: prints x as a side-effect upon being matched. If x is a thunk of expressions, it is evaluated.))
(trace x where (x = int) = {builtin: trace x})
(trace x where (x = float) = {builtin: trace x})
(trace x = {builtin: trace x}) (# that one is for strings)
```

The framework matches the alco corresponding to the code with a special virtual alco value which looks like this:

```
(main = *)

and an infinite number of {builtin: match-any}.
```


## Example Code:

### OOP:

```
(main = tracelegs Dog)

(Cat = Animal)
(Dog = Animal)

(vocalize where (trace (woof)) = Dog)
(x = legs where (x = 4) = Dog)

(vocalize where (trace (meow)) = Cat)
(x = legs where (x = 4) = Cat)

(tracelegs a where (a = Animal) (vocalize = a))
```

### Graphs:

```
(main = trace (dfs a (Node 3)))

(a = b = c = d)
    (b = c)
        (c = d)

(a (Node 1))
(b (Node 2))
(c (Node 3))
(d (Node 4))

(# (for this pattern to be matched, a DFS has to be performed from root to find (root val)))
(node = dfs node val where (node val))
```
Note: the above can also be seen as an example of simple logic programming.

## Future Work:

* Adding non-pure computations
* Adding more powerful handling for strings through additional pattern-matching rules
* Expanding the standard library with non-pure I/O
* Expanding the standard library with a computation pattern which can execute computations in a linked = list in a sequence to allow for some imperative programming (as a non-pure computation, similar to Haskell's do blocks)
* Enabling modifications of unparsed source code through registering preprocessor code and through non-pure I/O to actually make the language reflective
* Figuring out a good garbage-collection strategy
* Multithreading? Maybe?
