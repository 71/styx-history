//! Styx IR analyzers and optimizers.
//!
//! The `Analyzer` can be used to compute an `Analysis` that describes variables
//! and their different uses.
//! A "hot" variable which is used more often should be faster to access, especially
//! if computations are often applied to it.

use arch::{Architecture, Operand, OperandLocation as Location};
use assembler::AssemblyGraph;
use builder::{Value, ValueKind};
use typesystem::{Fun, Typed};

use std::collections::HashMap;


/// The Control-Flow Graph of a function, built by analyzing
/// a `Builder`.
#[derive(Debug, Clone)]
pub struct Cfg {

}

/// A value stored on the stack or on the register.
#[derive(Debug, Copy, Clone)]
pub struct Variable<'a, 'cx: 'a> {
    /// Number of times the variable is used.
    pub uses: u32,

    /// Value described by that variable.
    pub value: Value<'a, 'cx>
}

/// An architecture-independant structure that represents the analysis
/// of IR code in order for the `Assembler` to produce an optimal machine code.
///
/// # Panics
/// All lookups are considered valid, and no checks are made to ensure they are.
/// Looking up an invalid graph or variable will result in a panic.
#[derive(Debug)]
pub struct Analysis<'a, 'g: 'a, 'cx: 'g> {
    /// Target architecture.
    pub arch: Architecture,

    /// Declared variables and their architecture-dependant operands.
    pub variables: HashMap<Value<'a, 'cx>, Operand>,

    /// Approximate sizes (in bytes) of the analyzed bodies.
    pub sizes: HashMap<&'a AssemblyGraph<'g, 'cx>, usize>,

    /// Return operands of the analyzed bodies.
    pub return_operands: HashMap<&'a AssemblyGraph<'g, 'cx>, Operand>
}

impl<'a, 'g, 'cx> Analysis<'a, 'g, 'cx> {
    /// Performs the analysis of an assembly graph.
    pub fn perform(arch: Architecture, graph: &'a AssemblyGraph<'g, 'cx>) -> Self {
        let mut vars = Vec::new();
        let mut sizes = HashMap::default();
        let mut return_operands = HashMap::default();

        // analyze graph
        for graph in graph.iter_codependencies() {
            let mut size = 0;
            let builder = &graph.builder;

            // analyze blocks in builder
            for block in builder.blocks() {
                size += block.instructions().len() * arch.average_instruction_size() as usize;

                // analyze instructions in block
                for instr in block.instructions() {
                    for operand in instr.operands() {
                        if operand.must_be_processed() {
                            size += 4;
                            continue
                        }
                        if let ValueKind::Immediate(_) = operand.kind() {
                            size += 2;
                            continue
                        }

                        if let Some(var) = dup!(mut &mut vars => Vec<Variable<'a, 'cx>>).iter_mut().find(|v| v.value == *operand) {
                            var.uses += 1;
                        } else {
                            vars.push(Variable { value: *operand, uses: 1 });
                        }
                    }
                }
            }

            // set return operand and size
            sizes.insert(graph, size);
        }

        // assign variables to map
        vars.sort_unstable_by_key(|k| -(k.uses as isize));

        let (mut rstate, mut sstate) = arch.default_state();
        let mut variables = HashMap::with_capacity(vars.len());

        for var in &vars {
            if var.uses == 0 {
                // unused variable, who cares?
                break
            }

            let operand = arch.get_native_operand(&var.value, &mut rstate, &mut sstate);

            variables.insert(var.value, operand);
        }

        // find optimal return value for each function
        for graph in graph.iter_codependencies() {
            let mut best_ret = arch.default_return_operand(take_ok!(graph.target.ty().size()));
            let mut best_uses = 0;

            // find most used variable in graph that's also returned
            for ret in &graph.builder.rets {
                if ret.size() == 0 {
                    continue
                }

                let var = match vars.iter().find(|v| v.value == *ret) {
                    Some(var) => var,
                    None => continue
                };

                if var.uses <= best_uses {
                    continue
                }

                best_ret = variables[&var.value];
                best_uses = var.uses;
            }

            println!("Found return operand for {}", graph.target);

            return_operands.insert(graph, best_ret);
            println!("State: {:?}", return_operands);
        }

        Analysis { arch, variables, sizes, return_operands }
    }

    /// Gets the (logical) maximum size of the body produced by the assembly
    /// of the specified graph.
    pub fn get_size(&self, graph: &AssemblyGraph<'g, 'cx>) -> usize {
        *self.sizes.get(graph).unwrap()
    }

    /// Gets the operand corresponding to the specified operand.
    pub fn get_operand<'v>(&self, variable: &Value<'v, 'cx>) -> Operand where 'v: 'a {
        match self.variables.get(variable) {
            Some(var) => *var,
            None => match variable.kind() {
                ValueKind::Immediate(i) => Operand::new(variable.size(), Location::Immediate(i as _)),
                _ => unreachable!()
            }
        }
    }

    /// Gets the operand corresponding to the return value of the specified graph.
    pub fn get_return_operand(&self, graph: &AssemblyGraph<'g, 'cx>) -> Operand {
        *self.return_operands.get(graph).unwrap()
    }

    /// Gets the operand corresponding to the return value of the specified graph.
    pub fn get_return_operand_of_function(&self, fun: &Fun<'cx>) -> Operand {
        println!("Trying to get operand for {}", fun);

        for k in self.return_operands.keys() {
            println!("  Comparing with {}", k.target);

            if k.target == fun {
                return *self.return_operands.get(k).unwrap()
            }
        }

        println!("Nothing found in {:?}", self);

        panic!("Could not find operand for given function.")
    }
}
