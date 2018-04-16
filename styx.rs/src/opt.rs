//! [`Optimization`] level, and related [`OptimizationOptions`].


/// Represents a set of options that define how a function and its dependencies should be optimized.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OptimizationOptions {
    /// How many recursive calls can be made to compute a value during compilation.
    ///
    /// When a method call is made with constant arguments to a pure function,
    /// the method can be computed during compilation to improve runtime performance.
    /// However, making those computations can be slow.
    pub depth: u32
}

impl Default for OptimizationOptions {
    fn default() -> Self {
        OptimizationOptions { depth: 100 }
    }
}

/// Defines the level of optimization that should be used to compile a function and its
/// dependencies.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Optimization {
    /// No optimization -- compile as fast as possible.
    None,

    /// Optimization for speed -- make the runtime as fast as possible, even if it means
    /// a larger binary.
    Speed(OptimizationOptions),

    /// Optimization for size -- make the executable as small as possible, even if it leads to
    /// slower execution.
    Size(OptimizationOptions)
}

impl Optimization {
    /// Returns a `bool` representing if no optimization should be applied.
    #[inline]
    pub fn is_none(&self) -> bool {
        self == &Optimization::None
    }

    /// Returns the specific `OptimizationOptions` related to this optimization.
    /// If no optimization is to be performed, `None` will be returned.
    #[inline]
    pub fn options(&self) -> Option<&OptimizationOptions> {
        match self {
            &Optimization::Speed(ref opts) | &Optimization::Size(ref opts) => Some(opts),
            &Optimization::None => None
        }
    }
}

impl Default for Optimization {
    #[inline]
    fn default() -> Self {
        Optimization::None
    }
}

