
# PA6, Pattern Matching
First, complete PA5. PA6 extends that project and you cannot make progress on PA6
without PA5.
There is only thing to do in this PA, but do not underestimate the complexity of
the task.
# Part 2: Pattern Matching
Let's add the pattern matching functionality.
```ocaml
let f 0 = 0
let f 1 = 1
let f n = ...
let g (x:xs) = ...
```
Similar to pa5, start with the lexer, parser, then move on to infer and eval.
Eval in particular will require some larger edits. This is because we need to allow
for multi-line definitions for a function. We will not be running completeness
checks for coverage of inputs.
i.e. defining a function `f 0 = 0` won't cause an error, until you evaluate it with
`f 1`.
The biggest change will be in how we store a closure. Instead of `String Expr
TermEnv`,
we want the closure to associate `Pattern`s with `Expr`s and a `TermEnv`, or in
other words,
the closure contains a mapping between input patterns and function body with a
global environment map.
When matching against a pattern, you can check:
1. Is the pattern a PVar? -> always match, this is the generic case
2. Is the pattern a PLit? -> match if the argument is equivalent to the literal
3. Is the pattern a PCons (x:xs) structure? -> match if the argument is a non-empty
list
4. Otherwise, check another pattern
# Extra Credit
Handle recursive function defintions, e.g.
```ocaml
let g [] = 0
let g (x:xs) = 1 + g xs
```
# Debugging with Trace
Tracing execution is tricky in a language which values purity.
The Trace monad can help.
First import trace
```hs
import Debug.Trace
```
Then, in your `do` block, simply chain the trace call to your existing execution
block
```hs
-- before
eval env expr = do
case expr of
...
-- after
eval env expr = do
trace ("Log: " ++ show expr) do
case expr of
...
```
Read more about
[tracing](https://hackage.haskell.org/package/base-4.21.0.0/docs/Debug-Trace.html).
