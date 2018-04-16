//! Utilities used to host the Styx deamon, which provides a HTTP interface for communicating with
//! Styx contexts.

use prelude::*;
use arch::Architecture;
use assembler::{AssemblyGraph, AssemblyStyle};
use binder::{BinderState, Bound};
use diagnostics::DiagnosticPrinter;
use input::InputFile;

use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::io;
use std::mem;
use std::sync::RwLock;

use futures::future::{self, Future};
use futures::stream::Stream;

use hyper::{self, Method, StatusCode};
use hyper::server::{NewService, Request, Response, Service};

use uuid::Uuid;
use qstring::QString;


/// The Styx deamon.
pub struct Deamon<'vm> {
    vm: Vm<'vm>,
    contexts: RwLock<HashMap<Uuid, (Context<'vm>, Binder<'vm, 'vm>)>>
}

impl<'vm> Deamon<'vm> {
    /// Creates a new deamon, given its [`Vm`].
    pub fn new(vm: Vm<'vm>) -> Self {
        Deamon { vm, contexts: RwLock::new(HashMap::default()) }
    }
}

impl<'vm> NewService for Deamon<'vm> {
    type Request  = Request;
    type Response = Response;
    type Error    = hyper::Error;

    type Instance = DeamonService<'vm, 'vm>;

    fn new_service(&self) -> Result<DeamonService<'vm, 'vm>, io::Error> {
        Ok(unsafe {
            DeamonService::new(self.vm, &*(&self.contexts as *const _))
        })
    }
}


/// A Styx deamon `Service`.
pub struct DeamonService<'s, 'vm: 's> {
    vm: Vm<'vm>,
    contexts: &'s RwLock<HashMap<Uuid, (Context<'vm>, Binder<'vm, 'vm>)>>
}

impl<'s, 'vm> DeamonService<'s, 'vm> {
    /// Creates a new deamon service, given its VM and contexts.
    pub fn new(vm: Vm<'vm>, contexts: &'s RwLock<HashMap<Uuid, (Context<'vm>, Binder<'vm, 'vm>)>>) -> Self {
        DeamonService { vm, contexts }
    }

    /// Creates a context managed by this deamon.
    pub fn create_context(&self) -> Result<String, Error> {
        if let Ok(mut contexts) = self.contexts.write() {
            let id = Uuid::new_v4();
            let ctx = Context::new(self.vm);
            let binder = Binder::empty(self.vm);

            contexts.insert(id, (ctx, binder));

            Ok(format!("{}", id.simple()))
        } else {
            Err(Error::Poisoned)
        }
    }

    /// Removes the context that bears the given name.
    ///
    /// If it cannot be found, `false` will be returned.
    pub fn remove_context(&self, name: &str) -> Result<(), Error> {
        let id = match Uuid::parse_str(name) {
            Ok(id) => id,
            Err(_) => return Err(Error::InvalidId)
        };

        if let Ok(mut contexts) = self.contexts.write() {
            match contexts.remove(&id) {
                Some(_) => Ok(()),
                None => Err(Error::UnknownContext)
            }
        } else {
            Err(Error::Poisoned)
        }
    }

    /// Gets the context associated with the given name.
    pub fn get_context(&self, name: &str) -> Result<(&mut Context<'vm>, &mut Binder<'vm, 'vm>), Error> {
        use std::ops::DerefMut;

        let id = match Uuid::parse_str(name) {
            Ok(id) => id,
            Err(_) => return Err(Error::InvalidId)
        };

        if let Ok(mut guard) = self.contexts.write() {
            match unsafe { (&mut *(guard.deref_mut() as *mut HashMap<_, _>)).get_mut(&id) } {
                Some(&mut (ref mut ctx, ref mut binder)) => Ok((ctx, binder)),
                None => Err(Error::UnknownContext)
            }
        } else {
            Err(Error::Poisoned)
        }
    }

    /// Executes the given code in the specified context, and returns a string representing
    /// the result.
    pub fn execute_in_context(&self, name: &str, code: String) -> Result<String, Error> {
        let (mut ctx, binder) = self.get_context(name)?;

        let file = InputFile::new(name.to_string(), code, 0);

        let arch = match Architecture::current() {
            Some(arch) => arch,
            None => return Err(Error::UnsupportedArchitecture)
        };

        let mut diagnostics = DiagnosticBag::default();

        // Update binder
        if let Err(_) = Binder::resume(binder, file, &mut diagnostics) {
            return Err(Error::InvalidContent)
        }

        // Bind input
        let (expr, file, mut diagnostics) = {
            let success = match binder.process(true, &mut diagnostics) {
                BinderState::Done => true,
                BinderState::Stalled | BinderState::Pending => false
            };

            let mut bound = Bound::new(binder, diagnostics);

            if !success {
                bound.generate_missing_symbols_diagnostics();
            }
            if !success || bound.diagnostics.has_error() {
                return Err(Error::Diagnostics(bound.diagnostics, bound.file))
            }

            (bound.expression, bound.file, bound.diagnostics)
        };

        // Compile input
        {
            let fun = Fun::anonymous(expr);
            // Function doesn't live long enough for context, but we don't really care, we won't
            // use it anymore
            let mut graph = AssemblyGraph::
                 new(arch, &mut diagnostics, &mut ctx, unsafe { mem::transmute(&fun) })
                .expect("Could not create graph.");

            graph.process(AssemblyStyle::DS, &mut diagnostics, self.vm);

            if diagnostics.has_error() {
                return Err(Error::Diagnostics(diagnostics, file))
            }
        }

        // Compute and display output
        let proced = ctx.procedures().iter().find(|p| p.function().is_anonymous());
        let proced = proced.unwrap().ds_proc().unwrap();
        let addr = proced.ptr as *const () as *mut ();

        unsafe {
            let f: fn() -> i32 = mem::transmute(addr);

            Ok(format!("{}", f()))
        }
    }
}

impl<'s, 'vm> Service for DeamonService<'s, 'vm> {
    type Request  = Request;
    type Response = Response;
    type Error    = hyper::Error;

    type Future   = Box<Future<Item = Response, Error = hyper::Error>>;

    fn call(&self, req: Request) -> Self::Future {
        let (method, uri, _, _, body) = req.deconstruct();
        let mut res = Response::new();

        info!("Replying to Kernel request.");

        macro_rules! err {
            ( $err: expr ) => {{
                res.set_status(StatusCode::BadRequest);
                res.set_body(format!("{}", $err));

                return box future::ok(res)
            }}
        }

        macro_rules! try {
            ( $expr: expr ) => (match $expr {
                Ok(ok) => ok,
                Err(err) => err!(err)
            });
        }

        macro_rules! query {
            () => (
                QString::from(match uri.query() {
                    Some(q) => q,
                    None => err!(Error::MissingQuery)
                })
            );

            ($q: expr, context) => (
                match $q.get("context") {
                    Some(cx) => cx,
                    None => err!(Error::MissingArgument("context"))
                };
            );
        }

        match (method, uri.path()) {
            (Method::Get, "/") => (),

            (Method::Post, "/ctx/new") => {
                let name = try!(self.create_context());

                res.set_body(name);
            },

            (Method::Post, "/ctx/remove") => {
                let query = query!();
                let name = query!(query, context);

                try!(self.remove_context(name.as_str()));
            },

            (Method::Post, "/ctx/execute") => {
                let query = query!();
                let name = query!(query, context);

                let this: &'static DeamonService<'static, 'static> = unsafe { mem::transmute(self) };

                return box
                    body.concat2()
                        .map(|chunk| chunk.to_vec())
                        .map(|vec| String::from_utf8(vec).unwrap())
                        .map(move |input| {
                            info!("Executing input... {}", input);
                            match this.execute_in_context(name.as_str(), input) {
                                Ok(output) => res.with_body(output),
                                Err(err) => {
                                    res.with_status(StatusCode::BadRequest)
                                       .with_body(format!("{}", err))
                                }
                            }
                        });
            },

            _ => {
                res.set_status(StatusCode::NotFound)
            }
        }

        box future::ok(res)
    }
}


/// An error encountered by the [`Deamon`].
#[derive(Debug)]
pub enum Error {
    /// Lock on contexts poisoned.
    Poisoned,

    /// Identifier of context invalid.
    InvalidId,

    /// Invalid content.
    InvalidContent,

    /// Could not find context defined by the given id.
    UnknownContext,

    /// Missing query string.
    MissingQuery,

    /// Missing content.
    MissingContent,

    /// Unsupported architecture.
    UnsupportedArchitecture,

    /// Missing query argument.
    MissingArgument(&'static str),

    /// Diagnostics resulting of an execution.
    Diagnostics(DiagnosticBag, InputFile)
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use self::Error::*;

        match self {
            &Poisoned => f.write_str("Deamon is poisoned."),
            &InvalidId => f.write_str("Invalid context identifier."),
            &InvalidContent => f.write_str("Invalid content cannot be parsed."),
            &UnknownContext => f.write_str("Unknown context identifier."),
            &MissingQuery => f.write_str("Missing query string."),
            &MissingContent => f.write_str("Missing body."),
            &UnsupportedArchitecture => f.write_str("Unsupported architecture."),

            &MissingArgument(arg) => write!(f, "Missing argument {}.", arg),

            &Diagnostics(ref bag, ref file) => {
                let mut vec = Vec::new();

                DiagnosticPrinter::new(&mut vec, file).print_bag(bag);

                let string = String::from_utf8(vec).unwrap();

                f.write_str(string.as_str())
            }
        }
    }
}

