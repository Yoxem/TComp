 1. **OK**: uniquify deals with the shadowing of variables by renaming every variable to a
unique name.
 2. **OK** remove_complex_operands ensures that each subexpression of a primitive operation or
 function call is a variable or integer, that is, an atomic expression. We refer
to nonatomic expressions as complex. This pass introduces temporary variables
to hold the results of complex subexpressions.
 3. **OK** explicate_control makes the execution order of the program explicit. It converts
the abstract syntax tree representation into a graph in which each node is a
labeled sequence of statements and the edges are goto statements.
 4. select_instructions handles the difference between LVar operations and x86
instructions. This pass converts each LVar operation to a short sequence of
instructions that accomplishes the same task.
assign_homes