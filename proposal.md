
# What will you do?

We will explore higher-order functions (HOFs) in Bril by implementing a lambda lifter and a defunctionalizer. Our HOFs are designed to be arbitrarily higher order, meaning that they can take functions as arguments. We will do Hindley-Milner type inference. We will also need to extend the Bril syntax to support local function definitions. A stretch goal is to get programs to return functions themselves.

# How will you do it?

We will need to add closures to Bril, as well as implement typechecking for HOFs. Additionally, we will have to do lambda lifting. A limitation of our work is that we will only be able to support int ptrs/int ptr ptrs/int ptr ptr ptrs… no fancy “branching” types here.

# How will you empirically measure success?

We will run our lambda lifter on the Bril benchmarks and measure the performance of the HOF versions against the normal ones. Moreover, we will write some programs using HOFs and defunctionalize them, producing new source code we can then compare to the projects.