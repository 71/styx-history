//! The Styx IR builder.
//!
//! Styx IR is a structure very close to actual assembly.
//!
//! It consists of raw assembly with the following added features:
//!   - **Macros**: Instructions that get expanded to one or more other instructions.
//!   - **Variables**: Abstract variables that are automatically transformed into register
//!     or stack variables during encoding.
//!   - **Blocks**: Labels that allow simple jumping inside a function, by passing values.
//!   - **Calls**: Easier function calls with automatic argument insertion.
//!
//! Additionally, the produced IR can be easily converted to machine code in multiple
//! architectures with little effort.
//!
//! The builder defined thereafter allows the emission of the described language
//! using a friendly API.

use prelude::*;
use arch::Architecture;
use assembler::AssemblyGraph;
use context::Context;
use diagnostics::{Diagnostic, DiagnosticBag};
use lexer::Span;
use typesystem::{Fun, Typed};

use std::fmt::{self, Display, Formatter, Write};
use std::marker::PhantomData;

use string_interner::StringInterner;
use yansi::Paint;


/// Panics if the given value is null or not a multiple of two.
macro_rules! check_size {
    ( $value: expr ) => (
        assert!($value == 0 || $value.is_power_of_two(), "The specified value is null or odd.")
    )
}

/// Writes all the given arguments, separated by a comma.
macro_rules! write_separated {
    ( $f: expr, $elements: expr, $elem: ident => $body: expr ) => {
        #[allow(never_loop)]
        'block: loop {
            if $elements.len() == 0 {
                break 'block Ok(())
            }

            for i in 0..$elements.len() - 1 {
                let $elem = unsafe { $elements.get_unchecked(i) };
                $body;
                if let Err(err) = write!($f, ", ") {
                    break 'block Err(err)
                }
            }

            let $elem = unsafe { $elements.get_unchecked($elements.len() - 1) };
            $body;
            break 'block Ok(())
        }
    }
}


// //==========================================================================//
// // INSTR & OPERAND                                                          //
// //==========================================================================//

/// An instruction, as seen by the `Builder`.
#[derive(Debug, Clone, PartialEq)]
pub struct Instr<'b, 'cx> {
    name: usize,
    operands: Vec<Value<'b, 'cx>>,
    terminates: bool
}

impl<'b, 'cx> Instr<'b, 'cx> {
    fn new(name: usize, operands: Vec<Value<'b, 'cx>>) -> Self {
        Instr { name, operands, terminates: false }
    }

    fn that_terminates(mut self) -> Self {
        self.terminates = true;
        self
    }

    pub(crate) fn as_jmp<'a>(&'a self, builder: &'a Builder<'b, 'cx>) -> Option<(usize, &'a str)> {
        let target = match &self.operands.get(0)?.kind {
            &ValueKind::BlockReference(r) => r as usize,
            _ => return None
        };

        let kind = builder.get_name(self.name as _).unwrap();

        Some((target, kind))
    }

    pub(crate) fn as_call(&self) -> Option<&'cx Fun<'cx>> {
        if self.name == Builder::CALL_INDEX {
            match &self.operands.get(0)?.kind {
                &ValueKind::FunctionReference(f) => Some(f),
                _ => None
            }
        } else {
            None
        }
    }

    pub(crate) fn as_ret(&self) -> Option<Value<'b, 'cx>> {
        if self.name == Builder::RET_INDEX {
            Some(*self.operands.get(0).unwrap_or(&Value::null()))
        } else {
            None
        }
    }

    /// Returns the name of the instruction, if it has one.
    ///
    /// # Note
    /// The `Builder` is required because the actual string is stored in it.
    pub fn name<'a>(&'a self, builder: &'a Builder<'b, 'cx>) -> Option<&'a str> {
        builder.get_name(self.name)
    }

    /// Returns a slice of all the operands passed to this instruction.
    pub fn operands(&self) -> &[Value<'b, 'cx>] {
        &self.operands
    }

    fn display(&self, f: &mut Write, builder: &Builder<'b, 'cx>) -> fmt::Result {
        write!(f, "{} ", Paint::white(builder.get_name(self.name).unwrap()))?;
        if self.name == Builder::CALL_INDEX {
            self.operands[0].display(f, builder)?;
        } else {
            write_separated!(f, &self.operands, operand => operand.display(f, builder)?)?;
        }

        Ok(())
    }
}

/// The kind of a `Value`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ValueKind<'cx> {
    /// A local variable or function parameter.
    Variable(bool, u32),

    /// An immediate value.
    Immediate(u64),

    /// A pointer to a memory address.
    Memory(u64),

    /// A reference to a `Block`.
    BlockReference(u16),

    /// A reference to a function.
    FunctionReference(&'cx Fun<'cx>),

    /// A phi-value.
    Phi(u64),

    /// A block argument.
    Argument(u16),

    /// A null-operand.
    Null
}

impl<'cx> Display for ValueKind<'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::ValueKind::*;

        match *self {
            Variable(..)         => f.write_str("var"),
            Immediate(_)         => f.write_str("imm"),
            BlockReference(_)    => f.write_str("blockref"),
            Memory(_)            => f.write_str("mem"),
            Phi(_)               => f.write_str("phi"),
            FunctionReference(_) => f.write_str("fn"),
            Argument(_)          => f.write_str("arg"),
            Null                 => f.write_str("null")
        }
    }
}

/// An operand taken by an instruction emitted by a `Builder`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Value<'b, 'cx> {
    size: u16,
    kind: ValueKind<'cx>,
    symbol: u32,
    _data: PhantomData<&'b ()>
}

impl<'a, 'b, 'cx> AsRef<Value<'b, 'cx>> for &'a Value<'b, 'cx> {
    fn as_ref(&self) -> &Value<'b, 'cx> {
        self
    }
}

impl<'b, 'cx> Value<'b, 'cx> {
    /// Creates a new `Value`, given its size, kind and name.
    fn new(size: u16, kind: ValueKind<'cx>, symbol: usize) -> Self {
        check_size!(size);

        if size == 1 {
            //panic!("Created value of size 1");
        }

        Value { size, symbol: symbol as _, kind, _data: PhantomData }
    }

    /// Creates a null `Value` with a null size, no name, and a `ValueKind::Null' kind.
    fn null() -> Self {
        Value { size: 0, symbol: Builder::EMPTY_INDEX as _, kind: ValueKind::Null, _data: PhantomData }
    }

    fn display(&self, f: &mut Write, builder: &Builder<'b, 'cx>) -> fmt::Result {
        if self.symbol == 0 {
            write!(f, "null")
        } else {
            write!(f, "{} '{}'", self.kind, builder.get_name(self.symbol as _).unwrap())
        }
    }

    /// Returns a `bool` that indicates whether the value is null (it has a null size).
    #[inline]
    pub fn is_null(&self) -> bool {
        self.kind == ValueKind::Null
    }

    /// Returns the size of the value in bytes.
    pub fn size(&self) -> u16 {
        self.size
    }

    /// Returns the kind of the value.
    pub fn kind(&self) -> ValueKind<'cx> {
        self.kind
    }

    /// Returns a `bool` that indicates whether the value must be processed by the `Analysis`.
    #[inline]
    pub(crate) fn must_be_processed(&self) -> bool {
        use self::ValueKind::*;

        match self.kind {
            Variable(_, _) | Immediate(_) | Memory(_) | Argument(_) => false,
            _ => true
        }
    }
}


// //==========================================================================//
// // BUILDER                                                                  //
// //==========================================================================//


/// A structure that enables easy procedure creation during runtime.
pub struct Builder<'b, 'cx: 'b> {
    pub(crate) blocks: Vec<Block<'b, 'cx>>,
    pub(crate) diagnostics: &'b mut DiagnosticBag,
    pub(crate) calls: Vec<&'cx Fun<'cx>>,
    pub(crate) rets: Vec<Value<'b, 'cx>>,
    pub(crate) paramsc: usize,
    pub(crate) ctx: &'b Context<'cx>,

    inlining: Vec<usize>,
    variables: Vec<Value<'b, 'cx>>,
    interner: StringInterner<usize>,
    arch: Architecture,
    block: usize,
    span: Span
}

impl<'b, 'cx> Builder<'b, 'cx> {
    const EMPTY_INDEX: usize = 0;
    const NAME_INDEX: usize  = 1;
    const CALL_INDEX: usize  = 2;
    const RET_INDEX:  usize  = 3;
    const ARG_INDEX:  usize  = 4;

    /// Creates a new `Binder`.
    pub fn new<N: Into<String>, S: ToString>(
        ctx: &'b Context<'cx>,
        arch: Architecture,
        span: Span,
        diagnostics: &'b mut DiagnosticBag,
        name: N,
        parameters: &[(S, u16)])
        -> Self {
        let mut interner = StringInterner::with_capacity(16);
        let mut name = name.into();

        if name.is_empty() {
            name.push_str("<anonymous>");
        }

        interner.get_or_intern("");
        interner.get_or_intern(name);

        // Special instructions:
        // THE ORDER IS IMPORTANT! special instructions
        // can be easily resolved using their name,
        // which depends on the order of declaration of their name.
        interner.get_or_intern("call*");
        interner.get_or_intern("ret*");
        interner.get_or_intern("blockarg");

        let name = interner.get_or_intern("entry");
        let mut params = Vec::with_capacity(parameters.len());
        let mut i = 0;

        for &(ref name, size) in parameters.iter() {
            let name = interner.get_or_intern(name.to_string());
            params.push(Value::new(size, ValueKind::Variable(false, i), name));

            i += 1;
        }

        Builder {
            ctx,
            blocks: vec!(Block::new(0, name, Some(0))),
            calls: vec!(), rets: vec!(), inlining: vec!(),
            variables: params,
            block: 0,
            interner,
            arch,
            span,
            diagnostics,
            paramsc: i as _
        }
    }

    /// Returns the name of the function to which this builder corresponds.
    #[inline]
    pub fn name(&self) -> &str {
        self.interner.resolve(Self::NAME_INDEX).unwrap()
    }

    /// Returns the target architecture of the builder.
    #[inline]
    pub fn arch(&self) -> Architecture {
        self.arch
    }

    /// Returns the span in which the expressions making up this builder
    /// were defined.
    #[inline]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Defines a variable with a specific size and an optional name,
    /// and returns an `Value` that describes this variable.
    ///
    /// # Panics
    /// - `size` is null or not a multiple of two.
    pub fn define<N: Into<String> + AsRef<str>>(&mut self, size: u16, name: N) -> Value<'b, 'cx> {
        check_size!(size);

        let length = self.vars().len();
        let var = Value::new(size, ValueKind::Variable(false, length as u32), self.interner.get_or_intern(name));

        self.variables.push(var);

        var
    }

    /// Returns a name for a variable to be defined.
    pub fn variable_name(&mut self) -> String {
        // name defaults to 'i%' where % is the position of the variable in the variables array
        format!("i{}", self.vars().len())
    }

    /// Updates the specified variable, giving it a new value and marking it as mutable.
    pub fn update<N: Into<String> + AsRef<str>>(&mut self, _variable: Value<'b, 'cx>, _value: Value<'b, 'cx>) {
        unimplemented!()
    }

    /// Returns the parameter defined at the given position.
    ///
    /// # Errors
    /// Returns `None` if no parameter matching this position exists.
    pub fn parameter(&self, position: usize) -> Option<Value<'b, 'cx>> {
        if self.paramsc <= position {
            None
        } else {
            Some(self.variables[position])
        }
    }

    /// Returns the variable defined at the given position.
    pub fn variable(&self, position: usize) -> Option<Value<'b, 'cx>> {
        self.variables.get(position + self.paramsc).cloned()
    }

    /// Defines a new instruction, and returns its value.
    pub fn new_instruction<N: Into<String> + AsRef<str>>(&mut self, name: N, operands: Vec<Value<'b, 'cx>>) -> Instr<'b, 'cx> {
        Instr::new(self.interner.get_or_intern(name), operands)
    }

    /// Defines a new operand, and returns its value.
    pub fn new_value<N: Into<String> + AsRef<str>>(&mut self, size: u16, name: N, kind: ValueKind<'cx>) -> Value<'b, 'cx> {
        Value::new(size, kind, self.interner.get_or_intern(name))
    }

    /// Defines a null operand with a null size and no name, and returns its value.
    pub fn null_value(&mut self) -> Value<'b, 'cx> {
        Value::null()
    }

    /// Emits a 'jump' instruction to the given block.
    pub fn emit_jump(&mut self, block: usize, value: Option<Value<'b, 'cx>>) {
        let jump = Instr::new(self.interner.get_or_intern("jmp"), vec!(Value {
            symbol: self.blocks[block].name as _,
            size: 0,
            kind: ValueKind::BlockReference(block as u16),
            _data: PhantomData
        }, value.unwrap_or_else(Value::null)));
        let currblock = self.block;

        self.blck().is_terminated = true;
        self.blocks[block].imm_dominators.push(currblock);
        self.instrs().push(jump.that_terminates())
    }

    /// Emits a conditional 'jump' instruction to the given block.
    pub fn emit_cond_jump<N: Into<String> + AsRef<str>>(&mut self, jmpkind: N, block: usize, value: Option<Value<'b, 'cx>>) {
        let jump = Instr::new(self.interner.get_or_intern(jmpkind), vec!(Value {
            symbol: self.blocks[block].name as _,
            size: 0,
            kind: ValueKind::BlockReference(block as u16),
            _data: PhantomData
        }, value.unwrap_or_else(Value::null)));

        let currblock = self.block;

        self.blocks[block].imm_dominators.push(currblock);
        self.instrs().push(jump)
    }

    /// Emits a 'ret' instruction which returns the specified variable.
    pub fn emit_ret(&mut self, value: Value<'b, 'cx>) {
        let inliningc = self.inlining.len();

        if inliningc == 0 {
            self.rets.push(value);

            self.blck().is_terminated = true;
            self.instrs().push(Instr::new(Self::RET_INDEX, vec!(value)).that_terminates());
        } else {
            // currently inlining an expression, jump to its end block with the value instead
            let target = unsafe { *self.inlining.get_unchecked(inliningc - 1) };
            let value = if value.size() == 0 { None } else { Some(value) };

            self.emit_jump(target, value);
        }
    }

    /// Emits a 'call' instruction to the specified function.
    pub fn emit_call(&mut self, fun: &'cx Fun<'cx>, args: &[Value<'b, 'cx>]) -> Value<'b, 'cx> {
        let mut operands = Vec::with_capacity(args.len() + 1);
        let ret_size = fun.ty().size().unwrap();
        let varname = self.variable_name();
        let name = {
            let mut name = format!("{}(", fun.name());

            write_separated!(name, args, v => v.display(&mut name, self).expect("Could not write argument."))
                .expect("Could not write argument.");

            if ret_size == 0 {
                name.push(')');
            } else {
                write!(name, ") -> {}", varname).expect("Could not write name");
            }

            name
        };

        operands.push(Value {
            symbol: self.interner.get_or_intern(name) as _,
            size: 0,
            kind: ValueKind::FunctionReference(fun),
            _data: PhantomData
        });

        operands.extend_from_slice(args);

        self.calls.push(fun);
        self.instrs().push(Instr::new(Self::CALL_INDEX, operands));

        if ret_size == 0 {
            self.null_value()
        } else {
            self.define(ret_size, varname)
        }
    }

    /// Inlines the given function in the current body.
    ///
    /// The function cannot depend on the current block.
    pub fn emit_inline<'g>(&mut self, graph: &AssemblyGraph<'g, 'cx>, args: &[Value<'b, 'cx>]) -> Value<'b, 'cx> {
        let target = graph.target;
        let builder: &Self = unsafe {
            &*(&graph.builder as *const _ as *const () as *const Self)
        };

        let ret_size = target.ty().size().unwrap();
        let mut varc = self.variables.len();

        // copy variables
        for (i, var) in builder.variables.iter().enumerate() {
            let var = self.new_value(var.size, builder.get_name(var.symbol as _).unwrap(), ValueKind::Variable(false, (varc + i) as u32));
            self.variables.push(var);
        }

        // create after-call block, which contains the result of the "call"
        let curr = self.block_index();
        let then = self.create_block("after-call", if ret_size == 0 { None } else { Some(ret_size) }).index();

        let diff = self.blocks().len();

        // copy body
        for block in builder.blocks() {
            block.clone_to(diff - 1, varc as _, builder, self);
        }

        // move arguments
        self.position_at_nth(curr);

        for arg in args {
            let var = self.variables[varc];
            self.emit_move(*arg, var);
            varc += 1;
        }

        // jump to after-call, and return value
        self.emit_jump(then + 1, None);
        self.position_at_nth(then);
        self.block_argument().unwrap_or_else(Value::null)
    }

    /// Emits a 'mov' instruction.
    pub fn emit_move(&mut self, source: Value<'b, 'cx>, destination: Value<'b, 'cx>) {
        if destination != source {
            self.emit2("mov", destination, source)
        }
    }

    /// Emits the given expression inline.
    ///
    /// If the expression returns a value, the function will not exit,
    /// and the parameter of the 'after-expr' block will be set to the return value.
    pub fn emit_expr(&mut self, expr: &Expr<'cx>) -> Value<'b, 'cx> {
        let ty = expr.ty();
        let size = ty.size().unwrap();
        let after = self.create_block("after-expr", if size == 0 { None } else { Some(size) }).index();

        // notify builder that we're building
        // this is important, and changes the behavior of "ret"
        self.inlining.push(after);

        // emit expression
        let result = expr.build(self);

        // pop last element from inlining vec
        self.inlining.pop();

        // return value corresponding to block param
        self.emit_jump(after, Some(result));
        self.block_argument().unwrap_or_else(Value::null)
    }

    /// Emits the instruction described by the specified opcode.
    ///
    /// Returns `Err` if no instruction matching the given opcode could be found.
    pub fn emit(&mut self, opcode: &str, operands: Vec<Value<'b, 'cx>>) {
        let name = self.interner.get_or_intern(opcode);

        self.instrs().push(Instr::new(name, operands));
    }

    /// Emits the instruction described by the specified opcode.
    ///
    /// Returns `Err` if no instruction matching the given opcode could be found.
    pub fn emit0(&mut self, opcode: &str) {
        let name = self.interner.get_or_intern(opcode);

        self.instrs().push(Instr::new(name, vec!()));
    }

    /// Emits the instruction described by the specified opcode and operand.
    ///
    /// Returns `Err` if no instruction matching the given opcode could be found.
    pub fn emit1(&mut self, opcode: &str, op: Value<'b, 'cx>) {
        let name = self.interner.get_or_intern(opcode);

        self.instrs().push(Instr::new(name, vec!(op)));
    }

    /// Emits the instruction described by the specified opcode and operands.
    ///
    /// Returns `Err` if no instruction matching the given opcode could be found.
    pub fn emit2(&mut self, opcode: &str, op1: Value<'b, 'cx>, op2: Value<'b, 'cx>) {
        let name = self.interner.get_or_intern(opcode);

        self.instrs().push(Instr::new(name, vec!(op1, op2)));
    }

    /// Emits the instruction described by the specified opcode and operands.
    ///
    /// Returns `Err` if no instruction matching the given opcode could be found.
    pub fn emit3(&mut self, opcode: &str, op1: Value<'b, 'cx>, op2: Value<'b, 'cx>, op3: Value<'b, 'cx>) {
        let name = self.interner.get_or_intern(opcode);

        self.instrs().push(Instr::new(name, vec!(op1, op2, op3)));
    }

    /// Creates a block with no parameter, given its name.
    #[inline]
    pub fn create_block<N: Into<String> + AsRef<str>>(&mut self, name: N, argsize: Option<u16>) -> &'b Block<'b, 'cx> {
        let name = self.interner.get_or_intern(name);
        let index = self.blocks.len();
        self.blocks.push(Block::new(index, name, argsize));

        unsafe {
            &*(self.blocks.get_unchecked(index) as *const _)
        }
    }

    /// Creates a block with no parameter, and moves the builder to its end.
    #[inline]
    pub fn position_at_new_block<N: Into<String> + AsRef<str>>(&mut self, name: N, argsize: Option<u16>) -> &mut Self {
        let name = self.interner.get_or_intern(name);
        self.block = self.blocks.len();
        self.blocks.push(Block::new(self.block, name, argsize));
        self
    }

    /// Positions the builder at the end of the specified block.
    #[inline]
    pub fn position(&mut self, block: &Block<'b, 'cx>) -> &mut Self {
        self.position_at_nth(block.index)
    }

    /// Positions the builder at the end of the nth block.
    #[inline]
    pub fn position_at_nth(&mut self, index: usize) -> &mut Self {
        if index >= self.blocks.len() {
            panic!("Index ouf of bounds.")
        } else {
            self.block = index;
            self
        }
    }

    #[inline]
    fn blck<'a>(&'a mut self) -> &'a mut Block<'b, 'cx> {
        &mut self.blocks[self.block]
    }

    #[inline]
    fn instrs<'a>(&'a mut self) -> &'a mut Vec<Instr<'b, 'cx>> {
        &mut self.blck().instructions
    }

    #[inline]
    fn vars<'a>(&'a mut self) -> &'a mut Vec<Value<'b, 'cx>> {
        &mut self.blck().variables
    }

    /// Gets the argument passed to the current block.
    #[inline]
    pub fn block_argument(&self) -> Option<Value<'b, 'cx>> {
        match self.block().argsize {
            Some(size) => Some(Value::new(size, ValueKind::Argument(self.block as _), Self::ARG_INDEX)),
            None => None
        }
    }

    /// Returns a reference to the block in which the builder is currently emitting.
    #[inline]
    pub fn block(&self) -> &Block<'b, 'cx> {
        unsafe {
            self.blocks.get_unchecked(self.block)
        }
    }

    /// Returns the index of the block in which the builder is currently emitting.
    #[inline]
    pub fn block_index(&self) -> usize {
        self.block
    }

    /// Returns a slice containing all built blocks.
    #[inline]
    pub fn blocks(&self) -> &[Block<'b, 'cx>] {
        &self.blocks
    }

    /// Returns the nth block.
    #[inline]
    pub fn nth_block(&self, nth: usize) -> Option<&Block<'b, 'cx>> {
        self.blocks.get(nth)
    }

    /// Returns the name associated with the specified symbol,
    /// or `None` if it couldn't be resolved.
    #[inline]
    pub(crate) fn get_name(&self, symbol: usize) -> Option<&str> {
        self.interner.resolve(symbol)
    }
}

impl<'b, 'cx> Builder<'b, 'cx> {
    /// Finalizes the builder, expanding macros and inlining method calls.
    /// Furthermore, errors may be reported.
    pub fn finalize(&mut self) {
        // TODO: Right now, this doesn't do anything.
        let diagnostics = unsafe {
            &mut *(self.diagnostics as *mut DiagnosticBag)
        };

        for block in self.blocks() {
            if !block.is_terminated {
                diagnostics.report(Diagnostic::non_terminated_block(self.span));
            }
        }
    }
}

impl<'b, 'cx> Display for Builder<'b, 'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // write name
        write!(f, "{} {{\n\n", Paint::white(self.name()).bold())?;

        for block in &self.blocks {
            block.display(f, self)?;
        }

        f.write_char('}')
    }
}


// //==========================================================================//
// // BLOCK                                                                    //
// //==========================================================================//

/// A group of `Instr`s prefixed with a block header, which can be jumped to.
/// A block may be identified by a name, and may have parameters.
#[derive(Clone, Debug, PartialEq)]
pub struct Block<'b, 'cx: 'b> {
    name: usize,
    index: usize,
    variables: Vec<Value<'b, 'cx>>,
    instructions: Vec<Instr<'b, 'cx>>,
    is_terminated: bool,
    imm_dominators: Vec<usize>,
    argsize: Option<u16>
}

impl<'b, 'cx> Block<'b, 'cx> {
    /// Creates a new parameter-less `Block`, given its name symbol.
    #[inline]
    fn new(index: usize, name: usize, argsize: Option<u16>) -> Self {
        Block {
            index, name,
            variables: Vec::new(),
            instructions: Vec::new(),
            is_terminated: false,
            imm_dominators: Vec::new(),
            argsize
        }
    }

    fn clone_to(&self, then: usize, vardiff: u32, origin: &Builder<'b, 'cx>, to: &mut Builder<'b, 'cx>) {
        to.position_at_new_block(format!("inlined-{}", self.name(origin)), self.argsize);

        // copy variables
        for (i, var) in self.variables.iter().enumerate() {
            let var = to.new_value(var.size, origin.get_name(var.symbol as _).unwrap(), ValueKind::Variable(false, i as _));
            to.vars().push(var);
        }

        // copy instructions
        for mut instr in self.instructions.clone() {
            instr.name = to.interner.get_or_intern(origin.get_name(instr.name).unwrap());

            for operand in &mut instr.operands {
                operand.symbol = to.interner.get_or_intern(origin.get_name(operand.symbol as _).unwrap()) as _;
                operand.kind = match operand.kind {
                    ValueKind::BlockReference(nth) => ValueKind::BlockReference(then as u16 + nth),
                    ValueKind::Variable(m, nth) => ValueKind::Variable(m, nth + vardiff),
                    ValueKind::Argument(nth) => ValueKind::Argument(then as u16 + nth),
                    other => other
                };
            }

            if let Some(call) = instr.as_call() {
                to.calls.push(call);
                to.instrs().push(instr);
            }
            else if let Some(ret) = instr.as_ret() {
                to.emit_jump(then, Some(ret));
            }
            else if let Some((jmp, kind)) = instr.as_jmp(origin) {
                to.emit_cond_jump(kind, then + jmp, instr.operands().get(1).cloned());
            }
            else {
                assert!(!instr.terminates, "A non-jump / ret instruction cannot terminate.");

                to.instrs().push(instr.clone());
            }
        }
    }

    /// Returns the index of the block, relative to its parent body.
    #[inline]
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the name of the block.
    #[inline]
    pub fn name<'a>(&'a self, builder: &'a Builder<'b, 'cx>) -> &'a str {
        builder.get_name(self.name).unwrap()
    }

    /// Returns a slice containing all variables declared in this block.
    #[inline]
    pub fn variables(&self) -> &[Value<'b, 'cx>] {
        &self.variables
    }

    /// Returns a slice containing all instructions that make up this block.
    #[inline]
    pub fn instructions(&self) -> &[Instr<'b, 'cx>] {
        &self.instructions
    }

    /// Returns whether the block contains a terminating instruction.
    #[inline]
    pub fn is_terminated(&self) -> bool {
        self.is_terminated
    }

    /// Returns the dominators of this block.
    #[inline]
    pub fn dominators(&self) -> &[usize] {
        &self.imm_dominators
    }

    fn display(&self, f: &mut Write, builder: &Builder<'b, 'cx>) -> fmt::Result {
        // block header:
        writeln!(f, "  {}:", Paint::white(builder.get_name(self.name).unwrap()).underline())?;

        // instructions:
        for ins in &self.instructions {
            f.write_str("    ")?;
            ins.display(f, builder)?;
            f.write_char('\n')?;
        }

        f.write_str("\n")
    }
}

