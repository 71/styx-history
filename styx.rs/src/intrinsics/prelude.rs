//! Parsers, functions and types imported by default in every binder.

use prelude::*;
use prelude::parse::*;
use expr::{BuiltIns, Call, Conditional};
use super::syntax;

syntaxty!(using_type of size 0: "Styx.Prelude.Using");

/// Imports the symbols specified in the given module.
pub fn using<'cx>() -> Syntax<'cx> {
    syntax! {
        type using_type() =>

        fn parse(parser, diagnostics) {
            let span = match parser.current_or_report(diagnostics)? {
                &Token::Ident(span, ref ident) if ident == "using" => span,
                tok => return Err(Failure::Match(tok.span()))
            };

            // parse path
            let path_parser = syntax::path();
            let _path: Sym = match path_parser.parse(parser, diagnostics) {
                Ok(path) => path,
                Err(Failure::Match(_)) => report!(diagnostics, Diagnostic::expected_token(span, "import path")),
                Err(err) => return Err(err)
            };

            // import path
            unimplemented!()
        }
    }
}

/// Calls a method identified by a name, using the specified arguments.
pub fn call<'cx>() -> Syntax<'cx> {
    syntax! {
        type BuiltIns::call() =>

        fn parse(parser, diagnostics) {
            // get path of call
            let start = parser.current_span(diagnostics)?.start();
            let path: Spanned<Sym> = parser.parse(syntax::path_type(), diagnostics)?;

            if path.parts().len() > 1 {
                unimplemented!()
            }

            // parse args (enclosed in parenthesis)
            match parser.current_or_report(diagnostics)? {
                &Token::LParen(_) => (),
                token => return Err(Failure::Match(token.span()))
            }

            // at this point, the signature matches a function call;
            // any parsing error that might happen after this is an ERROR,
            // and not a syntax mismatch

            let mut args = Vec::new();

            let end = loop {
                parser.advance();

                let arg = parser.parse_binary(diagnostics, &|token| match token {
                    &Token::Comma(_) | &Token::RParen(_) => true,
                    _ => false
                });

                if let Err(Failure::Match(span)) = arg {
                    diagnostics.report(Diagnostic::no_match(span));
                }

                args.push(match arg {
                    Ok(arg) => arg,
                    _ => loop {
                        // enter recovery
                        match parser.current_or_report(diagnostics)? {
                            &Token::Comma(_) | &Token::RParen(_) => break Expr::default(),
                            _ => parser.advance()
                        }
                    }
                });

                // expect comma or right paren
                match parser.current_or_report(diagnostics)? {
                    &Token::Comma(_) => continue,
                    &Token::RParen(end) => break end,

                    _ => unreachable!()
                }
            };

            // loop broken? we have encountered a ')'
            parser.advance();

            // find target function and return
            let span = span!(start => end + 1);
            let target = {
                let args_refs: Vec<_> = args.iter().by_ref().collect();
                parser.binder().lookup_function(span, path.full_name(), &args_refs, diagnostics)
            };

            Ok(Expr::native(Call::new(target, args), span, BuiltIns::call()))
        }
    }
}


/// Performs a ternary if-then-else operation.
pub fn ternary<'cx>() -> Syntax<'cx> {
    syntax! {
        type using_type() =>

        fn parse(parser, diagnostics) {
            // parse 'if'
            let start = match parser.current_or_report(diagnostics)? {
                &Token::Ident(span, ref ident) if ident.as_str() == "if" => span,
                token => return Err(Failure::Match(token.span()))
            };

            parser.advance();

            // parse condition
            let condition = parser.parse_binary(diagnostics, &|token| match token {
                &Token::Ident(_, ref ident) => ident.as_str() == "then",
                _ => false
            })?;

            // parse then
            let then = parser.parse_binary(diagnostics, &|token| match token {
                &Token::Ident(_, ref ident) => ident.as_str() == "else",
                _ => false
            })?;

            // parse otherwise
            let otherwise = parser.parse_binary(diagnostics, &|_| false)?;

            // construct conditional
            if then.ty() != otherwise.ty() {
                diagnostics.report(Diagnostic::error(0, "Else expression does not return a value compatible with then condition.", otherwise.span()))
            }

            if condition.ty() != parser.binder().lookup_type(condition.span(), "System.Boolean", diagnostics) {
                diagnostics.report(Diagnostic::error(0, "Condition does not return a boolean.", condition.span()))
            }

            let span = (start .. otherwise.span()).into();

            Ok(Expr::native(Conditional::new(condition, then, otherwise), span, BuiltIns::conditional()))
        }
    }
}
