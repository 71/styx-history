//! This module declares all inputs that can be given to a `VirtualMachine` and their
//! relationship.

use lexer::{HasSpan, Span};

use std::fs::File;
use std::io::{self, Read};

use futures::prelude::*;

/// An input entity, which either wraps a file or a string.
pub enum InputEntity {
    /// A file that is to be read.
    File {
        /// Name of the file to read.
        filename: String,
        /// Readable file object.
        file: File,
        /// Identifier of the file, when multiple files are parsed at once.
        id: u64
    },

    /// A string.
    Str {
        /// A user-friendly display name for the file.
        display: &'static str,
        /// The content of the file as a `String`.
        content: String,
        /// Identifier of the file, when multiple files are parsed at once.
        id: u64
    }
}

/// A fully-read input file.
#[derive(Debug)]
pub struct InputFile {
    pub(crate) name: String,
    pub(crate) content: String,
    id: u64
}

impl InputFile {
    /// Creates a new input file, given its name and content.
    pub fn new(name: String, content: String, id: u64) -> Self {
        InputFile { name, content, id }
    }

    /// Creates a new empty input file.
    pub fn empty() -> Self {
        InputFile { name: String::from("<empty>"), content: String::new(), id: 0 }
    }

    /// Returns the name of the file defined by this struct, as a `str`.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Returns the content of the source file.
    pub fn source(&self) -> &str {
        self.content.as_str()
    }

    /// Returns a hash identifying this file.
    pub fn hash(&self) -> u64 {
        self.id
    }
}

impl HasSpan for InputFile {
    fn span(&self) -> Span {
        span!(0 => self.content.len())
    }
}

/// An input accepted by `VirtualMachine.bind`.
pub struct Input {
    files:       Vec<InputFile>,
    entities:    Vec<InputEntity>
}

impl Input {
    /// Creates a new `Input`, given the input files it wraps.
    pub fn new<I: Iterator<Item = InputEntity>>(files: I) -> Self {
        Input {
            entities: files.collect::<Vec<_>>(),
            files: Vec::new()
        }
    }

    /// Returns the length of the input files.
    pub fn len(&self) -> usize {
        self.files.len()
    }

    /// Returns whether no file is in this input.
    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Synchronously reads the next entity from the input iterator.
    fn read_entity(&mut self) -> Result<Option<InputFile>, io::Error> {
        if self.entities.is_empty() {
            return Ok(None)
        }

        Ok(match self.entities.swap_remove(0) {
            InputEntity::Str {  display, content, id  } => Some(InputFile::new(display.to_string(), content, id)),

            InputEntity::File { filename, mut file, id  } => Some({
                let mut buffer = String::new();

                file.read_to_string(&mut buffer)?;

                InputFile::new(filename, buffer, id)
            })
        })
    }
}

impl Stream for Input {
    type Item  = InputFile;
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Option<Self::Item>, Self::Error> {
        // We could potentially read every file asynchronously,
        // but right now reading multiple huge files isn't a concern, which is why
        // we're doing everything at once.
        Ok(Async::Ready(self.read_entity()?))
    }
}


