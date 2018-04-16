//! The Styx assembler.
//!
//! The assembler translates Styx IR into machine code for a
//! specific architecture, either in memory or in a file.
//!
//! It is in charge of emitting offsets, variables, stack allocations, and more.

use arch::{Architecture, Operand as NO, OperandLocation as Location};
use analysis::Analysis;
use builder::{Block, Builder, Instr, Value};
use context::Context;
use diagnostics::{Diagnostic, DiagnosticBag};
use emit::Emitter;
use lexer::HasSpan;
use procedures::{Proc, Procs};
use typesystem::{Fun, Typed};
use vm::Vm;

use std::hash::{Hash, Hasher};
use std::fmt::{self, Debug, Formatter};
use std::mem;


#[derive(Debug)]
struct BlockFixup {
    pub offset: usize,
    pub index: usize,
    pub kind: String
}

#[derive(Debug)]
struct ProcFixup<'b, 'cx: 'b> {
    pub offset: usize,
    pub fun: &'cx Fun<'cx>,
    pub operands: Vec<Value<'b, 'cx>>,
    pub next: Option<Instr<'b, 'cx>>
}

/// The state of an `AssemblyGraph` after its initialization.
enum GraphState<'g, 'cx: 'g> {
    /// The graph has dependencies on a function that is set to be compiled,
    /// and will thus become a dependency.
    Join(u32, AssemblyGraph<'g, 'cx>),

    /// The graph does not have any dependency on other functions in this graph, and can
    /// thus be compiled.
    Ready(AssemblyGraph<'g, 'cx>),

    /// Fatal error encountered which keeps the graph from being built and processed.
    Fatal
}

/// The graph of functions that the `Assembler` must compile.
pub struct AssemblyGraph<'g, 'cx: 'g> {
    pub(crate) arch: Architecture,
    pub(crate) builder: Builder<'g, 'cx>,
    pub(crate) target: &'cx Fun<'cx>,
    pub(crate) context: &'g mut Context<'cx>,
    codependencies: Vec<AssemblyGraph<'g, 'cx>>,
    dependencies: Vec<AssemblyGraph<'g, 'cx>>
}

impl<'g, 'cx, 'vm> AssemblyGraph<'g, 'cx> {
    /// Creates a new assembly graph, and initializes it.
    pub fn new(arch: Architecture,
               diagnostics: &mut DiagnosticBag,
               ctx: &'g mut Context<'cx>,
               f: &'cx Fun<'cx>)
               -> Result<Self, ()> {
        match Self::init(arch, diagnostics, ctx, f, &|_| -1) {
            GraphState::Ready(graph) => Ok(graph),

            GraphState::Join(_, _)   => panic!("This should not happen. Please report this."),
            GraphState::Fatal        => Err(())
        }
    }

    fn init(arch: Architecture,
            diagnostics: &mut DiagnosticBag,
            ctx: &'g mut Context<'cx>,
            f: &'cx Fun<'cx>,
            get_depth: &Fn(&'cx Fun<'cx>) -> i32)
            -> GraphState<'g, 'cx> {
        let span = f.span();
        let name = f.name();
        let mut parameters = Vec::new();

        for param in f.parameters() {
            parameters.push( (param.name(), param.ty().size().unwrap()) );
        }

        // build function to IR
        // note: we're duplicating the mutable reference to diagnostics here, since the builder
        //       won't log diagnostics after being finalized, but we need to get back the ownership
        //       afterwards
        let mut builder = Builder::new(dup!(ctx), arch, span, dup!(mut diagnostics), name, &parameters);
        let body = f.body();
        let retv = body.build(&mut builder);

        builder.emit_ret(retv);
        builder.finalize();

        // keep going even if there are errors; at this point we're only catching them
        // to display them to the user

        // analyze dependencies
        let mut dependencies   = Vec::new();
        let mut codependencies = Vec::new();
        let mut join_depth = -1;

        for call in &builder.calls {
            // if already compiled, keep going
            if dup!(ctx => Context<'cx>).get_procs(call).is_some() {
                continue
            }

            // if already logged as dependency, keep going
            if dependencies.iter().chain(codependencies.iter()).any(|g: &Self| g.target == *call) {
                continue
            }

            // ensure the target function is not set to be compiled
            match get_depth(call) {
                -1 => (),
                n => {
                    if n > join_depth {
                        join_depth = n
                    }

                    continue
                }
            }

            match AssemblyGraph::init(arch, diagnostics, dup!(mut ctx), call,
                                      &|d| if f == d ||
                                              dependencies.iter().any(|dep: &Self| dep.target == d) ||
                                              codependencies.iter().any(|dep: &Self| dep.target == d) {
                                              -1
                                          } else {
                                              let depth = get_depth(d);

                                              if depth == -1 {
                                                  -1
                                              } else {
                                                  depth + 1
                                              }
                                          }
                ) {
                GraphState::Ready(graph) => dependencies.push(graph),
                GraphState::Join(_jd, graph) => codependencies.push(graph),
                GraphState::Fatal => ()
            }
        }

        if diagnostics.has_error() {
            return GraphState::Fatal
        }

        // create graph depending on dependencies
        let graph = AssemblyGraph { arch, builder, dependencies, codependencies, target: f, context: ctx };

        if join_depth == -1 {
            GraphState::Ready(graph)
        } else {
            GraphState::Join(join_depth as _, graph)
        }
    }

    /// Processes the content of this graph, creating and compiling a procedure.
    pub fn process(&mut self, style: AssemblyStyle, diagnostics: &mut DiagnosticBag, vm: Vm<'vm>) {
        // build dependencies that do not depend on the current graph
        for dep in &mut self.dependencies {
            dep.process(style, diagnostics, vm);
        }

        // build all the current graph
        let analysis = Analysis::perform(self.arch, dup!(self));
        let mut fixes = Vec::new();

        self.process_recursive(&analysis, &mut fixes, style, diagnostics, vm);
    }

    fn process_recursive<'a, 'b>(&mut self,
                                 analysis: &Analysis<'a, 'g, 'cx>,
                                 fixes: &mut Vec<ProcFixup<'b, 'cx>>,
                                 style: AssemblyStyle,
                                 diagnostics: &mut DiagnosticBag,
                                 vm: Vm<'vm>) {
        // process mutual dependencies
        for dep in &mut self.codependencies {
            dep.process_recursive(analysis, fixes, style, diagnostics, vm);
        }

        // build final method
        let rproc = {
            let ret_operand = analysis.return_operands.get(self).expect("Could not find return operand of graph.");
            let mut emitter = dup!(mut self.context => Context<'cx>).create_emitter(analysis.get_size(self) * 2);

            Assembler::translate(
                (unsafe { mem::transmute(&self.builder) }),
                analysis, self.context, &mut emitter, style, self.target, *ret_operand,
                fixes, diagnostics);

            let proc_params = (0..self.builder.paramsc)
                .map(|i| analysis.get_operand(&self.builder.parameter(i).unwrap()))
                .collect();

            Proc::new(unsafe {
                &*(emitter.start() as *const ())
            }, emitter.len() as _, proc_params, *ret_operand)
        };

        // set procedure
        let procs = self.context.get_or_insert_procs(self.target);

        procs.set_proc(rproc, style);
    }

    /// Returns an iterator over the graph.
    pub fn iter_dependencies<'w>(&'w self) -> GraphWalker<'w, 'g, 'cx> {
        GraphWalker::new(self, false)
    }

    /// Returns an iterator over the graph.
    pub fn iter_codependencies<'w>(&'w self) -> GraphWalker<'w, 'g, 'cx> {
        GraphWalker::new(self, true)
    }

    /// Returns a reference to the builder used within the graph.
    pub fn builder(&self) -> &Builder<'g, 'cx> {
        &self.builder
    }
}

impl<'g, 'cx> Debug for AssemblyGraph<'g, 'cx> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[{}] {} Graph {{ codeps: {:?} ; deps: {:?} }}", self.arch, self.target, self.codependencies, self.dependencies)
    }
}

impl<'g, 'cx> Hash for AssemblyGraph<'g, 'cx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.target.hash(state)
    }
}

impl<'g, 'cx> PartialEq for AssemblyGraph<'g, 'cx> {
    fn eq(&self, other: &Self) -> bool {
        self.target.eq(other.target)
    }
}

impl<'g, 'cx> Eq for AssemblyGraph<'g, 'cx> {}


/// The style of the `Proc` that is to be generated.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum AssemblyStyle {
    /// Continuation-Passing Style.
    CPS,

    /// Direct Style.
    DS
}


fn get_next_power_of_two(val: i64) -> usize {
    let val = if val < 0 { -val } else { val };

    if val < 2 {
        1
    } else if val < 2i64.pow(2) {
        2
    } else if val < 2i64.pow(4) {
        4
    } else if val < 2i64.pow(8) {
        8
    } else if val < 2i64.pow(16) {
        16
    } else if val < 2i64.pow(32) {
        32
    } else {
        64
    }
}

/// Asserts that the used opcode exists.
macro_rules! assert_exists {
    ( $op: ident, $body: expr ) => {
        assert!($body, format!("The built-in opcode {} could not be found.", stringify!($op)));
    }
}

/// Returns the offset of a jump from the current location to a remote location.
#[inline]
fn get_call_offset(here: usize, target: usize) -> isize {
    if target > here {
        (target - here + 5) as isize
    } else {
        -((here - target + 5) as isize)
    }
}

/// Returns the offset of a jump from the current location to a remote location.
#[inline]
fn get_jmp_offset(here: usize, target: usize) -> isize {
    if target > here {
        (target - here) as isize
    } else {
        -((here - target) as isize)
    }
}

/// Returns an operand that represents an offset to a relative position.
#[inline]
fn offset_to_imm(offset: isize) -> NO {
    let len = get_next_power_of_two(offset as _);
    let len = len / 8;
    NO::new((len + 1) as _, Location::Relative((offset - len as isize - 2) as _))
}

/// A structure that translates IR code (defined in a `Procedure`)
/// into machine code.
struct Assembler;

impl<'e, 'cx, 'vm> Assembler {
    /// Translates the given function and all of its dependencies into machine code.
    #[allow(too_many_arguments)]
    fn translate<'a, 'g, 'b>(builder: &Builder<'b, 'cx>,
                             analysis: &Analysis<'a, 'g, 'cx>,
                             ctx: &Context<'cx>,
                             emitter: &mut Emitter<'e, 'cx>,
                             style: AssemblyStyle,
                             fun: &'cx Fun<'cx>,
                             return_operand: NO,
                             fixes: &mut Vec<ProcFixup<'b, 'cx>>,
                             diagnostics: &mut DiagnosticBag) {
        let mut offsets = Vec::new();
        let mut block_fixups = Vec::new();

        let offset = emitter.offset();

        // fix calls to current function
        for fix in dup!(mut fixes => Vec<ProcFixup<'b, 'cx>>).drain_filter(move |fix| fix.fun == fun) {
            emitter.goto(fix.offset);

            // now we know that the proc exists, and thus emit_call won't modify fixes,
            // we can dup the reference without worrying
            Self::emit_call(style, ctx, fun, analysis, dup!(mut fixes), &fix.operands, fix.next.as_ref(), emitter);
        }

        emitter.goto(offset);

        // emit blocks one by one
        for (i, block) in builder.blocks().iter().enumerate() {
            Self::translate_block(i, return_operand, block, builder, style, analysis, ctx, emitter, fixes, &mut block_fixups, &mut offsets, diagnostics)
        }
    }

    /// Translates a single block into machine code.
    #[allow(too_many_arguments)]
    fn translate_block<'a, 'g, 'b>(index: usize,
                                   return_operand: NO,
                                   block: &Block<'b, 'cx>,
                                   builder: &Builder<'b, 'cx>,
                                   style: AssemblyStyle,
                                   analysis: &Analysis<'a, 'g, 'cx>,
                                   ctx: &Context<'cx>,
                                   emitter: &mut Emitter<'e, 'cx>,
                                   fixups: &mut Vec<ProcFixup<'b, 'cx>>,
                                   block_fixups: &mut Vec<BlockFixup>,
                                   block_offsets: &mut Vec<usize>,
                                   diagnostics: &mut DiagnosticBag) {
        let offset = emitter.offset();
        let arch = analysis.arch;
        let span = builder.span();

        macro_rules! get_operand {
            ( $maybe: expr ) => (match $maybe {
                Some(operand) => analysis.get_operand(operand),
                None => NO::null()
            })
        }

        // fix jumps to the current block
        block_offsets.push(offset);

        for fixup in block_fixups.drain_filter(move |fixup| fixup.index == index) {
            emitter.goto(fixup.offset);

            let offset = get_jmp_offset(fixup.offset, offset);

            assert_exists!(jmp, arch.encode_instr(fixup.kind.as_str(), offset_to_imm(offset), NO::null(), NO::null(), emitter));
        }

        emitter.goto(offset);

        let mut skip_next = false;

        // emit instructions one by one
        let len = block.instructions().len();
        let mut i = 0;

        while i < len {
            i += 1;

            if skip_next {
                skip_next = false;
                continue
            }

            let instr = unsafe {
                block.instructions().get_unchecked(i - 1)
            };

            if let Some((jump, kind)) = instr.as_jmp(builder) {
                // instruction is a jump
                match block_offsets.get(jump) {
                    Some(block_offset) => {
                        let offset = get_jmp_offset(offset, *block_offset) - 4;

                        // Emit jump to block
                        assert_exists!(jmp, arch.encode_instr(kind, offset_to_imm(offset), NO::null(), NO::null(), emitter));
                    },
                    None => {
                        block_fixups.push(BlockFixup { offset: emitter.offset(), index: jump, kind: kind.to_string() });

                        for _ in 0..5 {
                            assert_exists!(nop, arch.encode_instr("nop", NO::null(), NO::null(), NO::null(), emitter));
                        }
                    }
                }
            }

            else if let Some(call) = instr.as_call() {
                // instruction is a call
                skip_next = Self::emit_call(style, ctx, call, analysis, fixups, &instr.operands()[1..], block.instructions().get(i), emitter);
            }

            else if let Some(value) = instr.as_ret() {
                // instruction is a return statement
                if !value.is_null() {
                    // non-null value is returned, move it to the return register / stack if needed
                    let src = analysis.get_operand(&value);
                    let dest = return_operand;

                    if src != dest {
                        assert_exists!(mov, arch.encode_instr("mov", dest, src, NO::null(), emitter));
                    }
                }

                assert_exists!(ret, arch.encode_instr("ret", NO::null(), NO::null(), NO::null(), emitter));
            }

            else {
                // instruction is normal
                let operands = instr.operands();
                let op0 = get_operand!(operands.get(0));
                let op1 = get_operand!(operands.get(1));
                let op2 = get_operand!(operands.get(2));

                if !arch.encode_instr(instr.name(builder).unwrap(), op0, op1, op2, emitter) {
                    let display = format!("{} {} {} {}", instr.name(builder).unwrap(), op0, op1, op2);

                    diagnostics.report(Diagnostic::unknown_opcode(span, display.as_str()));
                }
            }

        }
    }

    fn emit_call<'a, 'b, 'g>(style: AssemblyStyle,
                             ctx: &Context<'cx>,
                             fun: &'cx Fun<'cx>,
                             analysis: &Analysis<'a, 'g, 'cx>,
                             fixups: &mut Vec<ProcFixup<'b, 'cx>>,
                             operands: &[Value<'b, 'cx>],
                             next: Option<&Instr<'b, 'cx>>,
                             emitter: &mut Emitter) -> bool {

        let arch = analysis.arch;
        let procs = ctx.get_procs(fun);
        let procd = procs.and_then(match style {
            AssemblyStyle::CPS => Procs::cps_proc,
            AssemblyStyle::DS  => Procs::ds_proc
        });

        if let Some(procd) = procd {
            // emit call
            for (i, operand) in operands.iter().enumerate() {
                let src = analysis.get_operand(operand);
                let dest = procd.parameters[i];

                if src != dest {
                    assert_exists!(mov, arch.encode_instr("mov", dest, src, NO::null(), emitter));
                }
            }

            let target = procd.ptr as *const () as usize;
            let here = emitter.offset();

            let offset = get_call_offset(here, target);

            if style == AssemblyStyle::DS {
                let (tail, mnemo) = if let Some(instr) = next {
                    if let Some(v) = instr.as_ret() {
                        if false && analysis.get_operand(&v) == analysis.get_return_operand_of_function(fun) {
                            // tail recursive call
                            (true, "jmp")
                        } else {
                            (false, "call")
                        }
                    } else {
                        (false, "call")
                    }
                } else {
                    (false, "call")
                };

                assert_exists!(call, arch.encode_instr(mnemo, NO::new(4, Location::Relative(offset as _)), NO::null(), NO::null(), emitter));

                return tail
            } else {
                unimplemented!()
            }
        } else {
            for operand in operands.iter() {
                let src = analysis.get_operand(operand);
                let dest = analysis.get_return_operand_of_function(fun);

                if src != dest {
                    assert_exists!(mov, arch.encode_instr("mov", dest, src, NO::null(), emitter));
                }
            }

            // target hasn't been compiled yet, wait...
            let mut vec = Vec::with_capacity(operands.len());
            vec.copy_from_slice(operands);

            fixups.push(ProcFixup { fun, offset: emitter.offset(), operands: vec, next: next.cloned() });

            // make room for call
            emitter.emit_bytes(&[0, 0, 0, 0, 0]).expect("Could not emit call bytes.");
        }

        false
    }
}

/// An iterator that recursively walks through an assembly graph.
pub struct GraphWalker<'w, 'g: 'w, 'cx: 'g> {
    idx: isize,
    graph: &'w AssemblyGraph<'g, 'cx>,
    rec_walker: Option<Box<GraphWalker<'w, 'g, 'cx>>>,
    codeps: bool
}

impl<'w, 'g, 'cx> GraphWalker<'w, 'g, 'cx> {
    fn new(graph: &'w AssemblyGraph<'g, 'cx>, codeps: bool) -> Self {
        GraphWalker { graph, codeps, idx: -1, rec_walker: None }
    }
}

impl<'w, 'g, 'cx> Iterator for GraphWalker<'w, 'g, 'cx> {
    type Item = &'w AssemblyGraph<'g, 'cx>;

    fn next(&mut self) -> Option<&'w AssemblyGraph<'g, 'cx>> {
        let index = self.idx;

        macro_rules! deps {
            () => (if self.codeps {
                &self.graph.codependencies
            } else {
                &self.graph.dependencies
            })
        }

        // if index == -1, we first return the root graph itself
        if index == -1 {
            self.idx = 0;

            let deps = deps!();

            if !deps.is_empty() {
                self.rec_walker = Some(box GraphWalker::new(unsafe { deps.get_unchecked(0) }, self.codeps));
            }

            return Some(self.graph)
        }

        // if we already started iterating over another graph, take care of it.
        match mem::replace(&mut self.rec_walker, None) {
            Some(mut walker) => match walker.next() {
                Some(dep) => {
                    // dependency can be returned
                    self.rec_walker = Some(walker);

                    Some(dep)
                },
                None => {
                    // end of previous graph, go to next
                    let deps = deps!();
                    let dep = match deps.get(index as usize) {
                        Some(dep) => dep,
                        None => {
                            self.rec_walker = None;
                            return None
                        }
                    };

                    self.idx = index + 1;

                    self.rec_walker = Some(box GraphWalker::new(dep, self.codeps));
                    self.rec_walker.as_mut().unwrap().next()
                }
            },
            None => None // no graph left, means we're done
        }
    }
}

